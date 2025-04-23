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

generateResponseTag :: Action -> [T.Text] 
generateResponseTag action = case action of
  InitializeHead -> ["HeadIsInitializing", "Greetings"]
  CommitUTxO -> [""]
  DeCommitUTxO -> [""]
  Abort -> ["HeadIsAborted"]
  GetUTxO -> ["GetUTxOResponse"]
  CloseHead -> ["HeadIsClosed"]
  ContestHead -> ["HeadIsContested"]
  FanOut -> ["ReadyToFanout"]

hydraHeadInitialized :: T.Text
hydraHeadInitialized = T.pack "Hydra Head Initialized"

hydraHeadAborted :: T.Text
hydraHeadAborted = T.pack "Hydra Head Aborted"

sendCommandToHydraNodeSocket :: Action -> IO T.Text
sendCommandToHydraNodeSocket message = do
  let jsonMessage :: T.Text = case message of
        InitializeHead -> "{\"tag\": \"Init\"}"
        Abort -> "{\"tag\": \"Abort\"}"
        GetUTxO -> "{\"tag\": \"GetUTxO\"}"
        _ -> ""
  let validResponseTag :: [T.Text] = generateResponseTag message
  forwardCommands jsonMessage validResponseTag
