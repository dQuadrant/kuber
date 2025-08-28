{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Websocket.SocketConnection where

import Cardano.Kuber.Api
import Control.Concurrent
import Control.Concurrent.Async (race, race_)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS, setRequestBodyLBS, setRequestHeader, setRequestMethod)
import qualified Network.HTTP.Simple as HTTP
import Network.Wai (Request, requestHeaders)
import qualified Network.WebSockets as WS
import Websocket.Aeson
import Websocket.Utils

data AppConfig = AppConfig
  { hydraUrl :: String
  , serverIp :: Maybe String
  , serverPort :: Int
  , chainInfo :: ChainConnectInfo
  }

evaluateL1Kontract :: AppConfig -> Kontract ChainConnectInfo w FrameworkError r -> IO  (Either FrameworkError r)
evaluateL1Kontract config = evaluateKontract config.chainInfo

getHydraIpAndPort :: String -> (String, Int)
getHydraIpAndPort hydraUrl = case break (== ':') hydraUrl of
                               (ip, ':' : p) -> (ip, read p)
                               (ip, _)       -> (ip, 8080) -- Default Hydra port if not specified

hydraBaseUrl :: AppConfig -> [Char]
hydraBaseUrl appConfig = "http://" ++ ip ++ ":" ++ show port ++ "/"
  where (ip, port) = getHydraIpAndPort (hydraUrl appConfig)

errorTags :: [(T.Text, Int)]
errorTags =
  [ ("CommandFailed", 500), -- Internal Server Error
    ("PostTxOnChainFailed", 400), -- Internal Server Error
    ("PeerHandshakeFailure", 502), -- Bad Gateway
    ("TxInvalid", 400), -- Bad Request
    ("InvalidInput", 400), -- Bad Request
    ("IgnoredHeadInitializing", 409), -- Conflict
    ("DecommitInvalid", 400), -- Bad Request
    ("CommitIgnored", 409), -- Conflict
    ("FailedToDraftTxNotInitializing", 409) -- Conflict
  ]

skipTags :: [(T.Text, Int)]
skipTags = [("Greetings", 201), ("TxValid", 201)] -- Created

-- WebSocket Proxy Server
proxyServer :: AppConfig -> WS.ServerApp
proxyServer host pending = do
  conn <- WS.acceptRequest pending
  putStrLn "New client connected, forwarding to Hydra WebSocket..."
  let (hydraIp, hydraPort) = getHydraIpAndPort (hydraUrl host)
  WS.runClient hydraIp hydraPort "/" $ \remote -> do
    putStrLn "Connected to Hydra WebSocket server."
    -- Race both directions, end when either one stops
    race_
      (forwardMessageWS conn remote) -- client -> Hydra
      (forwardMessageWS remote conn) -- Hydra -> client
    putStrLn "Connection closed."

-- Forward Messages between Client and Hydra
forwardMessageWS :: WS.Connection -> WS.Connection -> IO ()
forwardMessageWS srcConn dstConn = forever $ do
  msg <- WS.receiveData srcConn
  WS.sendTextData dstConn (msg :: T.Text)

-- Collect and filter WebSocket messages
getLatestMessage ::
  AppConfig ->
  -- | current connection
  WS.Connection ->
  -- | expected tags
  [(T.Text, Int)] ->
  -- | wait indefinitely?
  Bool ->
  -- | response
  IO (Maybe (T.Text, Int))
getLatestMessage appConfig conn0 expectedTags wait = do
  putStrLn "Trying to get latest message from Hydra WebSocket..."
  let timeoutMicroseconds = 8 * 10 ^ 6
      go conn = do
        -- wait for 8 seconds. If no message received, return 201
        result <-
          if wait
            then Right <$> (try (WS.receiveData conn) :: IO (Either SomeException T.Text))
            else do
              putStrLn "Waiting for a message from Hydra..."
              race (threadDelay timeoutMicroseconds) (try (WS.receiveData conn) :: IO (Either SomeException T.Text))
        case result of
          Left _ -> do
            -- No response received in 15 seconds, send 201
            let response201 =
                  object
                    [ "message" .= T.pack "Request created"
                    ]
            return $ Just (T.decodeUtf8 $ BSL.toStrict $ encode response201, 201)
          Right (Left _) -> do
            putStrLn "WebSocket disconnected, attempting to reconnect..."
            let (hydraIp, hydraPort) = getHydraIpAndPort (hydraUrl appConfig)
            WS.runClient hydraIp hydraPort "/" $ \newConn -> do
              putStrLn "Reconnected"
              go newConn
          Right (Right msg) -> do
            let decoded = decode (BSL.fromStrict (T.encodeUtf8 msg)) :: Maybe WSMessage
            case decoded of
              Just wsmsg
                | wsmsg.tag `elem` map fst expectedTags ->do
                  putStrLn $ "Received expected message: " ++ T.unpack wsmsg.tag
                  lookupTag msg wsmsg expectedTags
                | wsmsg.tag `elem` map fst errorTags -> lookupTag msg wsmsg errorTags
                | wsmsg.tag `elem` map fst skipTags -> go conn
                | otherwise -> do
                    let wrapper =
                          object
                            [ "expected" .= map fst expectedTags,
                              "wsMessage" .= msg
                            ]
                    return (Just (T.decodeUtf8 $ BSL.toStrict $ encode wrapper, 500))
              Nothing -> do
                go conn -- Try again if decoding fails
  go conn0
  where
    lookupTag :: T.Text -> WSMessage -> [(T.Text, Int)] -> IO (Maybe (T.Text, Int))
    lookupTag msg wsmsg tagSet = do
      let code = maybe 500 fromIntegral (lookup wsmsg.tag tagSet)
      return (Just (msg, code))

forwardCommands :: AppConfig -> T.Text -> [(T.Text, Int)] -> Bool -> IO (T.Text, Int)
forwardCommands appConfig command tag wait = do
  let serverIpStr = fromMaybe "0.0.0.0" (serverIp appConfig)
  WS.runClient serverIpStr (serverPort appConfig) "/" $ \conn -> do
    WS.sendTextData conn command
    getLatestMessage appConfig conn tag wait >>= \msg -> return (fromMaybe ("No message received", 503) msg)

-- Check if Request is a WebSocket Request
isWebSocketRequest :: Request -> Bool
isWebSocketRequest req =
  case lookup "upgrade" (map (\(k, v) -> (T.toLower (T.pack (show k)), v)) (requestHeaders req)) of
    Just "websocket" -> True
    _ -> False

fetcher :: AppConfig -> IO (T.Text -> IO T.Text)
fetcher appConfig = do
  return $ \endpoint -> do
    let url = hydraBaseUrl appConfig ++ T.unpack endpoint
    request <- parseRequest url
    response <- httpLBS request
    let responseBody = T.pack $ BS8.unpack $ BSL.toStrict $ getResponseBody response
    return responseBody

fetcher' :: AppConfig -> IO (T.Text -> IO (HTTP.Response BSL.ByteString))
fetcher' appConfig = do
  return $ \endpoint -> do
    let url = hydraBaseUrl appConfig ++ T.unpack endpoint
    request <- parseRequest url
    httpLBS request

fetchRaw :: (ToJSON p) => AppConfig -> T.Text -> T.Text -> Maybe p -> IO (HTTP.Response BSL.ByteString)
fetchRaw appConfig method endpoint body = do
  let url = hydraBaseUrl appConfig ++ T.unpack endpoint
  initialRequest <- parseRequest url
  let request =
        (if method == "POST" then setRequestMethod "POST" else id) $
          (case body of
            Just d -> setRequestBodyLBS (A.encode d)
            Nothing -> id) $
          setRequestHeader "Content-Type" ["application/json"] $
          initialRequest
  httpLBS request

fetch :: (ToJSON p) => AppConfig -> T.Text -> T.Text -> Maybe p -> IO BSL.ByteString
fetch appConfig method endpoint body = do
  response <- fetchRaw appConfig method endpoint body
  let responseBody =  HTTP.getResponseBody response
  return responseBody

getHydra :: AppConfig -> T.Text -> IO BSL.ByteString
getHydra appConfig endpoint = fetch appConfig "GET" endpoint (Nothing :: Maybe A.Value)

postHydra :: (ToJSON p) => AppConfig -> T.Text -> p -> IO BSL.ByteString
postHydra appConfig endpoint jsonData = fetch appConfig "POST" endpoint (Just jsonData)

getHydra' :: AppConfig -> T.Text -> IO (HTTP.Response BSL.ByteString)
getHydra' appConfig endpoint = fetchRaw appConfig "GET" endpoint (Nothing :: Maybe A.Value)

postHydra' :: (ToJSON p) => AppConfig -> T.Text -> p -> IO (HTTP.Response BSL.ByteString)
postHydra' appConfig endpoint jsonData = fetchRaw appConfig "POST" endpoint (Just jsonData)

getHydraCommitTx :: AppConfig -> T.Text -> IO (Either FrameworkError BSL.ByteString)
getHydraCommitTx appConfig utxoSchema = do
  let jsonResponseOrError = textToJSON utxoSchema
  case jsonResponseOrError of
    Left fe -> pure $ Left fe
    Right jsonResponse -> do
      response <- liftIO $ postHydra appConfig "commit" jsonResponse
      pure $ Right response

getHead :: AppConfig -> IO BSL.ByteString
getHead appConfig = getHydra appConfig "head"

getHead' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getHead' appConfig = getHydra' appConfig "head"

postCommit :: (ToJSON p) => AppConfig -> p -> IO BSL.ByteString
postCommit appConfig = postHydra appConfig "commit"

postCommit' :: (ToJSON p) => AppConfig -> p -> IO (HTTP.Response BSL.ByteString)
postCommit' appConfig = postHydra' appConfig "commit"

getCommits :: AppConfig -> IO BSL.ByteString
getCommits appConfig = getHydra appConfig "commits"

getCommits' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getCommits' appConfig = getHydra' appConfig "commits"

getCommitsByTxId :: AppConfig -> T.Text -> IO BSL.ByteString
getCommitsByTxId appConfig txId = getHydra appConfig ("commits/" <> txId)

getCommitsByTxId' :: AppConfig -> T.Text -> IO (HTTP.Response BSL.ByteString)
getCommitsByTxId' appConfig txId = getHydra' appConfig ("commits/" <> txId)

getSnapshotsLastSeen :: AppConfig -> IO BSL.ByteString
getSnapshotsLastSeen appConfig = getHydra appConfig "snapshots/last-seen"

getSnapshotsLastSeen' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getSnapshotsLastSeen' appConfig = getHydra' appConfig "snapshots/last-seen"

getSnapshotUtxo :: AppConfig -> IO BSL.ByteString
getSnapshotUtxo appConfig = getHydra appConfig "snapshot/utxo"

getSnapshotUtxo' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getSnapshotUtxo' appConfig = getHydra' appConfig "snapshot/utxo"

postSnapshot :: (ToJSON p) => AppConfig -> p -> IO BSL.ByteString
postSnapshot appConfig = postHydra appConfig "snapshot"

postSnapshot' :: (ToJSON p) => AppConfig -> p -> IO (HTTP.Response BSL.ByteString)
postSnapshot' appConfig = postHydra' appConfig "snapshot"

getSnapshot :: AppConfig -> IO BSL.ByteString
getSnapshot appConfig = getHydra appConfig "snapshot"

getSnapshot' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getSnapshot' appConfig = getHydra' appConfig "snapshot"

postDecommit :: (ToJSON p) => AppConfig -> p -> IO BSL.ByteString
postDecommit appConfig = postHydra appConfig "decommit"

postDecommit' :: (ToJSON p) => AppConfig -> p -> IO (HTTP.Response BSL.ByteString)
postDecommit' appConfig = postHydra' appConfig "decommit"

getProtocolParameters :: AppConfig -> IO BSL.ByteString
getProtocolParameters appConfig = getHydra appConfig "protocol-parameters"

getProtocolParameters' :: AppConfig -> IO (HTTP.Response BSL.ByteString)
getProtocolParameters' appConfig = getHydra' appConfig "protocol-parameters"

postCardanoTransaction :: (ToJSON p) => AppConfig -> p -> IO BSL.ByteString
postCardanoTransaction appConfig = postHydra appConfig "cardano-transaction"

postCardanoTransaction' :: (ToJSON p) => AppConfig -> p -> IO (HTTP.Response BSL.ByteString)
postCardanoTransaction' appConfig = postHydra' appConfig "cardano-transaction"
