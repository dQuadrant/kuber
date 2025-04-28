{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Websocket.Forwarder where

import qualified Data.Text as T
import qualified Debug.Trace as Debug
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

generateResponseTag :: Action -> [(T.Text, Int)]
generateResponseTag action = case action of
  InitializeHead -> [("HeadIsInitializing", 200)]
  CommitUTxO -> [("", 00)]
  DeCommitUTxO -> [("DecommitRequested", 201), ("DecommitFinalized", 200)]
  Abort -> [("HeadIsAborted", 200)]
  GetUTxO -> [("GetUTxOResponse", 200)]
  CloseHead -> [("HeadIsClosed", 200)]
  ContestHead -> [("HeadIsContested", 200)]
  FanOut -> [("HeadIsFinalized", 200)]

hydraHeadInitialized :: T.Text
hydraHeadInitialized = T.pack "Hydra Head Initialized"

hydraHeadAborted :: T.Text
hydraHeadAborted = T.pack "Hydra Head Aborted"

sendCommandToHydraNodeSocket :: Action -> IO (T.Text, Int)
sendCommandToHydraNodeSocket message = do
  let jsonMessage :: T.Text = case message of
        InitializeHead -> "{\"tag\": \"Init\"}"
        Abort -> "{\"tag\": \"Abort\"}"
        GetUTxO -> "{\"tag\": \"GetUTxO\"}"
        CloseHead -> "{\"tag\": \"Close\"}"
        FanOut -> "{\"tag\": \"Fanout\"}"
        _ -> ""
  let validResponseTag :: [(T.Text, Int)] = generateResponseTag message
  forwardCommands jsonMessage validResponseTag

-- [\"Init\",
-- \"Abort\",
-- \"NewTx\",
-- \"GetUTxO\",
-- \"Decommit\",
-- \"Close\",
-- \"Contest\",
-- \"Fanout\"],
