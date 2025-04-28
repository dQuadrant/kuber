{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

generateResponseTag :: Action -> [T.Text]
generateResponseTag action = case action of
  InitializeHead -> ["HeadIsInitializing", "Greetings"]
  CommitUTxO -> [""]
  DeCommitUTxO -> ["DecommitRequested", "DecommitFinalized"]
  Abort -> ["HeadIsAborted"]
  GetUTxO -> ["GetUTxOResponse"]
  CloseHead -> ["HeadIsClosed"]
  ContestHead -> ["HeadIsContested"]
  FanOut -> ["HeadIsFinalized"]

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
        CloseHead -> "{\"tag\": \"Close\"}"
        FanOut -> "{\"tag\": \"Fanout\"}"
        _ -> ""
  let validResponseTag :: [T.Text] = generateResponseTag message
  forwardCommands jsonMessage validResponseTag

-- "reason": "Error in $.tag: parsing Hydra.API.ClientInput.ClientInput failed, expected tag field to be one of
-- [\"Init\",
-- \"Abort\",
-- \"NewTx\",
-- \"GetUTxO\",
-- \"Decommit\",
-- \"Close\",
-- \"Contest\",
-- \"Fanout\"],
