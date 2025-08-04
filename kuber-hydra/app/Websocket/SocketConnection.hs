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
import Network.Wai
import qualified Network.WebSockets as WS
import Websocket.Aeson
import Websocket.Utils

data AppConfig = AppConfig
  { hydraUrl :: String
  , serverIp :: Maybe String
  , serverPort :: Int
  , chainInfo :: ChainConnectInfo
  }

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
  putStrLn $ "Forwarded: " ++ T.unpack msg ++ "\n"

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
  start <- getCurrentTime
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
            putStrLn $ "Received message: " ++ T.unpack msg
            
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
    putStrLn $ "Forwarding command to Hydra WebSocket: " ++ T.unpack command
    WS.sendTextData conn command
    putStrLn $ "Sent command to Hydra WebSocket: " ++ T.unpack command
    getLatestMessage appConfig conn tag wait >>= \msg -> return (fromMaybe ("No message received", 503) msg)

validateLatestWebsocketTag :: AppConfig -> [(T.Text, Int)] -> Bool -> IO (T.Text, Int)
validateLatestWebsocketTag appConfig tag wait = do
  let serverIpStr = fromMaybe "0.0.0.0" (serverIp appConfig)
  WS.runClient serverIpStr (serverPort appConfig) "/" $ \conn -> do
    getLatestMessage appConfig conn tag wait >>= \msg -> return (fromMaybe ("No message received", 503) msg)

-- Check if Request is a WebSocket Request
isWebSocketRequest :: Request -> Bool
isWebSocketRequest req =
  case lookup "upgrade" (map (\(k, v) -> (T.toLower (T.pack (show k)), v)) (requestHeaders req)) of
    Just "websocket" -> True
    _ -> False

fetch :: AppConfig -> IO (T.Text -> IO T.Text)
fetch appConfig = do
  return $ \endpoint -> do
    let url = hydraBaseUrl appConfig ++ T.unpack endpoint
    request <- parseRequest url
    response <- httpLBS request
    let responseBody = T.pack $ BS8.unpack $ BSL.toStrict $ getResponseBody response
    return responseBody

post :: (ToJSON p) => AppConfig -> [Char] -> p -> IO T.Text
post appConfig path jsonData = do
  let url = hydraBaseUrl appConfig ++ path
  initialRequest <- parseRequest url
  let request =
        setRequestMethod "POST" $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestBodyLBS (A.encode jsonData) $
              initialRequest
  response <- httpLBS request
  let responseBody = T.pack $ BS8.unpack $ BSL.toStrict $ getResponseBody response
  return responseBody

getHydraCommitTx :: AppConfig -> T.Text -> IO (Either FrameworkError T.Text)
getHydraCommitTx appConfig utxoSchema = do
  let jsonResponseOrError = textToJSON utxoSchema
  case jsonResponseOrError of
    Left fe -> pure $ Left fe
    Right jsonResponse -> do
      response <- liftIO $ post appConfig "commit" jsonResponse
      pure $ Right response
