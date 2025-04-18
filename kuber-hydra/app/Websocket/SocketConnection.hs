{-# LANGUAGE OverloadedStrings #-}

module Websocket.SocketConnection where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Debug.Trace as Debug
import GHC.Conc (threadDelay)
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import Network.Wai
import qualified Network.WebSockets as WS
import System.Environment (getEnv)
import System.Timeout
import Websocket.Aeson

serverIP :: String
serverIP = "0.0.0.0"

serverPort :: Int
serverPort = 8081

hydraIP :: IO String
hydraIP = getEnv "HYDRA_IP"

hydraPort :: IO String
hydraPort = getEnv "HYDRA_PORT"

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
getLatestMessage :: WS.Connection -> T.Text -> IO (Maybe T.Text)
getLatestMessage conn tag_ = do
  start <- getPOSIXTime
  let duration = 0.3 -- 300ms
  let go latest = do
        now <- getPOSIXTime
        if now - start > duration
          then return latest
          else do
            result <- timeout 50000 $ try (WS.receiveData conn) :: IO (Maybe (Either SomeException T.Text))
            case result of
              Nothing -> go latest
              Just (Left _) -> return latest
              Just (Right msg) -> do
                let decoded = decode (BSL.fromStrict (T.encodeUtf8 msg)) :: Maybe WSMessage
                case decoded of
                  Just wsmsg | tag wsmsg == tag_ -> go (Just msg)
                             | tag wsmsg == tag_ -> go (Just msg)
                  _ -> go latest -- ignore other tags
  go Nothing

-- getLatestMessage :: WS.Connection -> IO (Maybe T.Text)
-- getLatestMessage conn = go Nothing
--   where
--     go latest = do
--       result <- try (WS.receiveData conn) :: IO (Either SomeException T.Text)
--       case result of
--         Left _  -> return latest  -- Return the latest message received
--         Right msg -> go (Just msg)  -- Update latest message and keep going

forwardCommands :: T.Text -> T.Text -> IO T.Text
forwardCommands command tag = do
  WS.runClient serverIP serverPort "/" $ \conn -> do
    WS.sendTextData conn command
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
    let responseBody = T.pack $ show $ getResponseBody response
    return responseBody