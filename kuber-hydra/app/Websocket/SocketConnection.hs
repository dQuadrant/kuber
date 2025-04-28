{-# LANGUAGE OverloadedStrings #-}

module Websocket.SocketConnection where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Debug.Trace as Debug
import GHC.Conc (threadDelay)
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS, setRequestBodyLBS, setRequestHeader, setRequestMethod)
import Network.Wai
import qualified Network.WebSockets as WS
import System.Environment (getEnv)
import System.Timeout
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

errorTags :: [T.Text]
errorTags = ["CommandFailed", "PostTxOnChainFailed", "PeerHandshakeFailure", "TxInvalid", "InvalidInput", "IgnoredHeadInitializing", "DecommitInvalid", "CommitIgnored"]

skipTags :: [T.Text]
skipTags = ["Greetings"]

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
getLatestMessage :: WS.Connection -> [T.Text] -> IO (Maybe T.Text)
getLatestMessage conn expectedTag = do
  start <- getCurrentTime
  let go = do
        result <- try (WS.receiveData conn) :: IO (Either SomeException T.Text)
        case result of
          Left _ -> go -- Retry on failure
          Right msg -> do
            let decoded = decode (BSL.fromStrict (T.encodeUtf8 msg)) :: Maybe WSMessage
            case decoded of
              Just wsmsg
                | timestamp wsmsg <= start -> go -- Wait for fresh message
                | tag wsmsg `elem` expectedTag -> return (Just msg)
                | tag wsmsg `elem` errorTags -> return (Just msg)
                | tag wsmsg `elem` skipTags -> go
                | otherwise -> do
                    let wrapper =
                          object
                            [ "expected" .= expectedTag,
                              "wsMessage" .= msg
                            ]
                    return (Just $ T.decodeUtf8 $ BSL.toStrict $ encode wrapper)
              Nothing -> go -- Try again if decoding fails
  go

forwardCommands :: T.Text -> [T.Text] -> IO T.Text
forwardCommands command tag = do
  WS.runClient serverIP serverPort "/" $ \conn -> do
    WS.sendTextData conn command
    getLatestMessage conn tag >>= \msg -> return (maybe "No message received" id msg)

validateLatestWebsocketTag :: [T.Text] -> IO T.Text
validateLatestWebsocketTag tag =
  WS.runClient serverIP serverPort "/" $ \conn -> do
    getLatestMessage conn tag >>= \msg -> return (maybe "No message received" id msg)

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

getHydraCommitTx :: T.Text -> IO T.Text
getHydraCommitTx utxoSchema = do
  let jsonResponse = textToJSON utxoSchema
  post "commit" jsonResponse