{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Websocket.Forwarder where

import qualified Data.Text as T
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
  DeCommitUTxO -> [("DecommitRequested", 201), ("DecommitApproved", 201), ("DecommitFinalized", 200)]
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
  let responseTag = generateResponseTag message
  case message of
    InitializeHead -> forwardCommands "{\"tag\": \"Init\"}" responseTag False
    Abort -> forwardCommands "{\"tag\": \"Abort\"}" responseTag False
    GetUTxO -> forwardCommands "{\"tag\": \"GetUTxO\"}" responseTag False
    CloseHead -> handleHydraHeadClose "{\"tag\": \"Close\"}" responseTag
    FanOut -> forwardCommands "{\"tag\": \"Fanout\"}" responseTag False

-- [\"Init\",
-- \"Abort\",
-- \"NewTx\",
-- \"GetUTxO\",
-- \"Decommit\",
-- \"Close\",
-- \"Contest\",
-- \"Fanout\"],
