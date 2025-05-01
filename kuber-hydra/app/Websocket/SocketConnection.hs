{-# LANGUAGE OverloadedStrings #-}

module Websocket.SocketConnection where

import Cardano.Kuber.Api (FrameworkError)
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
import qualified Debug.Trace as Debug
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS, setRequestBodyLBS, setRequestHeader, setRequestMethod)
import Network.Wai
import qualified Network.WebSockets as WS
import System.Environment (getEnv)
import Websocket.Aeson
import Websocket.Utils

serverIP :: String
serverIP = "0.0.0.0"

serverPort :: Int
serverPort = 8081

hydraIP :: IO String
hydraIP = getEnv "HYDRA_IP"

hydraPort :: IO String
hydraPort = getEnv "HYDRA_PORT"

errorTags :: [(T.Text, Int)]
errorTags =
  [ ("CommandFailed", 500), -- Internal Server Error
    ("PostTxOnChainFailed", 500), -- Internal Server Error
    ("PeerHandshakeFailure", 502), -- Bad Gateway
    ("TxInvalid", 400), -- Bad Request
    ("InvalidInput", 400), -- Bad Request
    ("IgnoredHeadInitializing", 409), -- Conflict
    ("DecommitInvalid", 400), -- Bad Request
    ("CommitIgnored", 409), -- Conflict
    ("FailedToDraftTxNotInitializing", 409) -- Conflict
  ]

skipTags :: [(T.Text, Int)]
skipTags = [("Greetings", 201)] -- Created

-- WebSocket Proxy Server
proxyServer :: String -> Int -> WS.ServerApp
proxyServer ip port pending = do
  conn <- WS.acceptRequest pending
  putStrLn "New client connected, forwarding to Hydra WebSocket..."
  WS.runClient ip port "/" $ \remote -> do
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
  -- | current connection
  WS.Connection ->
  -- | expected tags
  [(T.Text, Int)] ->
  IO (Maybe (T.Text, Int))
getLatestMessage conn0 expectedTags = do
  start <- getCurrentTime
  let timeoutMicroseconds = 15 * 10 ^ 6
      go conn = do
        -- wait for 15 seconds. If no message received, return 201
        result <- race (threadDelay timeoutMicroseconds) (try (WS.receiveData conn) :: IO (Either SomeException T.Text))
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
            ip <- hydraIP
            port <- hydraPort
            WS.runClient ip (read port) "/" $ \newConn -> do
              putStrLn "Reconnected"
              go newConn
          Right (Right msg) -> do
            let decoded = decode (BSL.fromStrict (T.encodeUtf8 msg)) :: Maybe WSMessage
            case decoded of
              Just wsmsg
                | timestamp wsmsg <= start -> go conn -- Wait for fresh message
                | tag wsmsg `elem` map fst expectedTags -> lookupTag msg wsmsg expectedTags
                | tag wsmsg `elem` map fst errorTags -> lookupTag msg wsmsg errorTags
                | tag wsmsg `elem` map fst skipTags -> go conn
                | otherwise -> do
                    let wrapper =
                          object
                            [ "expected" .= map fst expectedTags,
                              "wsMessage" .= msg
                            ]
                    return (Just (T.decodeUtf8 $ BSL.toStrict $ encode wrapper, 500))
              Nothing -> go conn -- Try again if decoding fails
  go conn0
  where
    lookupTag msg wsmsg tagSet = do
      let code = maybe 500 fromIntegral (lookup (tag wsmsg) tagSet)
      return (Just (msg, code))

forwardCommands :: T.Text -> [(T.Text, Int)] -> IO (T.Text, Int)
forwardCommands command tag = do
  WS.runClient serverIP serverPort "/" $ \conn -> do
    WS.sendTextData conn command
    getLatestMessage conn tag >>= \msg -> return (fromMaybe ("No message received", 503) msg)

validateLatestWebsocketTag :: [(T.Text, Int)] -> IO (T.Text, Int)
validateLatestWebsocketTag tag = do
  WS.runClient serverIP serverPort "/" $ \conn -> do
    getLatestMessage conn tag >>= \msg -> return (fromMaybe ("No message received", 503) msg)

-- Check if Request is a WebSocket Request
isWebSocketRequest :: Request -> Bool
isWebSocketRequest req =
  case lookup "upgrade" (map (\(k, v) -> (T.toLower (T.pack (show k)), v)) (requestHeaders req)) of
    Just "websocket" -> True
    _ -> False

fetch :: IO (T.Text -> IO T.Text)
fetch = do
  ip <- hydraIP
  port <- hydraPort
  let baseUrl = "http://" ++ ip ++ ":" ++ port ++ "/"
  return $ \endpoint -> do
    let url = baseUrl ++ T.unpack endpoint
    request <- parseRequest url
    response <- httpLBS request
    let responseBody = T.pack $ BS8.unpack $ BSL.toStrict $ getResponseBody response
    return responseBody

post :: (ToJSON p) => [Char] -> p -> IO T.Text
post path jsonData = do
  ip <- hydraIP
  port <- hydraPort
  let url = "http://" ++ ip ++ ":" ++ port ++ "/" ++ path
  initialRequest <- parseRequest url
  let request =
        setRequestMethod "POST" $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestBodyLBS (A.encode jsonData) $
              initialRequest
  response <- httpLBS request
  let responseBody = T.pack $ BS8.unpack $ BSL.toStrict $ getResponseBody response
  return responseBody

getHydraCommitTx :: T.Text -> IO (Either FrameworkError T.Text)
getHydraCommitTx utxoSchema = do
  let jsonResponseOrError = textToJSON utxoSchema
  case jsonResponseOrError of
    Left fe -> pure $ Left fe
    Right jsonResponse -> do
      response <- liftIO $ post "commit" jsonResponse
      pure $ Right response