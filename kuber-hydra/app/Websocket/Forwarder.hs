{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Websocket.Forwarder where

import Cardano.Kuber.Data.Models
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as AT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Websocket.SocketConnection
import Websocket.Aeson

data Action
  = InitializeHead
  | CommitUTxO
  | DeCommitUTxO
  | Abort
  | GetUTxO
  | CloseHead
  | ContestHead
  | FanOut
  | NewTx

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
  NewTx -> [("SnapshotConfirmed", 200)]

hydraHeadInitialized :: T.Text
hydraHeadInitialized = T.pack "Hydra Head Initialized"

hydraHeadAborted :: T.Text
hydraHeadAborted = T.pack "Hydra Head Aborted"

sendCommandToHydraNodeSocket :: Host -> Action -> Bool -> IO (T.Text, Int)
sendCommandToHydraNodeSocket hydraHost message wait = do
  let responseTag = generateResponseTag message
  case message of
    InitializeHead -> forwardCommands hydraHost "{\"tag\": \"Init\"}" responseTag wait
    Abort -> forwardCommands hydraHost "{\"tag\": \"Abort\"}" responseTag wait
    GetUTxO -> forwardCommands hydraHost "{\"tag\": \"GetUTxO\"}" responseTag wait
    CloseHead -> forwardCommands hydraHost "{\"tag\": \"Close\"}" responseTag wait
    ContestHead -> forwardCommands hydraHost "{\"tag\": \"Contest\"}" responseTag wait
    FanOut -> forwardCommands hydraHost "{\"tag\": \"Fanout\"}" responseTag wait

submitHydraTx :: Host -> TxModal -> IO (T.Text, Int)
submitHydraTx hydraHost txm = forwardCommands hydraHost newTxCommand (generateResponseTag NewTx) False
  where
    newTxCommand =
      TL.toStrict . AT.encodeToLazyText $
        A.object
          [ "tag" .= ("NewTx" :: T.Text),
            "transaction" .= txm
          ]

-- [\"Init\",
-- \"Abort\",
-- \"NewTx\",
-- \"GetUTxO\",
-- \"Decommit\",
-- \"Close\",
-- \"Contest\",
-- \"Fanout\"],
