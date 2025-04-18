{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Websocket.Forwarder where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.SocketConnection

data Action
  = InitializeHead
  | CommitUTxO
  | DeCommitUTxO
  | Abort
  | GetUTxO
  | CloseHead
  | ContestHead
  | FanOut

generateResponseTag :: Action -> T.Text
generateResponseTag action = case action of
  InitializeHead -> "Greetings"
  CommitUTxO -> ""
  DeCommitUTxO -> ""
  Abort -> "HeadIsAborted"
  GetUTxO -> "GetUTxOResponse"
  CloseHead -> "HeadIsClosed"
  ContestHead -> "HeadIsContested"
  FanOut -> "ReadyToFanout"

hydraHeadInitialized :: T.Text
hydraHeadInitialized = T.pack "Hydra Head Initialized"

hydraHeadAborted :: T.Text
hydraHeadAborted = T.pack "Hydra Head Aborted"

sendCommandToHydraNodeSocket message = do
  let jsonMessage :: T.Text = case message of
        InitializeHead -> "{\"tag\": \"Init\"}"
        Abort -> "{\"tag\": \"Abort\"}"
        GetUTxO -> "{\"tag\": \"GetUTxO\"}"
        _ -> ""
  let validResponseTag :: T.Text = generateResponseTag message
  forwardCommands jsonMessage validResponseTag

-- Function to listen for messages for a specified duration in seconds
-- listenForMessages :: WS.Connection -> Int -> IO (Maybe T.Text)
-- listenForMessages conn duration = do
--     let listenLoop remainingTime lastMsg = do
--             if remainingTime <= 0
--                 then return lastMsg
--                 else do
--                     msg <- WS.receiveData conn
--                     threadDelay 1000000
--                     listenLoop (remainingTime - 1) (Just msg)
--     listenLoop duration Nothing

-- webSocketProxy :: WS.ServerApp
-- webSocketProxy pendingConn = do
--     conn <- WS.acceptRequest pendingConn
--     putStrLn "Client connected to WebSocket proxy."

--     -- Get Hydra WebSocket address
--     ip <- hydraIp
--     port <- hydraPort
--     let hydraWsUrl = "ws://" ++ ip ++ ":" ++ port

--     -- Connect to Hydra WebSocket
--     WS.runClient ip (read port) "/" $ \hydraConn -> do
--         putStrLn $ "Connected to Hydra WebSocket at " ++ hydraWsUrl

--         -- Start bi-directional forwarding
--         _ <- forkIO $ forwardMessages conn hydraConn
--         forwardMessages hydraConn conn

-- Forward messages between WebSocket connections
-- forwardMessages :: WS.Connection -> WS.Connection -> IO ()
-- forwardMessages fromConn toConn = catch loop handleException
--   where
--     loop = do
--         msg <- WS.receiveData fromConn
--         TIO.putStrLn $ "Forwarding message: " <> msg
--         WS.sendTextData toConn msg
--         loop
--     handleException :: SomeException -> IO ()
--     handleException e = putStrLn $ "WebSocket connection closed: " ++ show e
