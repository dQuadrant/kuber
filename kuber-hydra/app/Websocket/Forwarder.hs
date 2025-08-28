{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Websocket.Forwarder where

import Cardano.Kuber.Api (ErrorType (TxSubmissionError), FrameworkError (FrameworkError))
import Cardano.Kuber.Data.Models
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as AT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Websocket.Aeson
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
  | NewTx

generateResponseTag :: Action -> [(T.Text, Int)]
generateResponseTag action = case action of
  InitializeHead -> [("HeadIsInitializing", 200),("PostTxOnChainFailed",400)]
  CommitUTxO -> [("", 00)]
  DeCommitUTxO -> [("DecommitInvalid",400),("DecommitRequested", 201), ("DecommitApproved", 201), ("DecommitFinalized", 200)]
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

sendCommandToHydraNodeSocket :: AppConfig -> Action -> Bool -> IO (T.Text, Int)
sendCommandToHydraNodeSocket appConfig message wait = do
  let responseTag = generateResponseTag message
  case message of
    InitializeHead -> forwardCommands appConfig "{\"tag\": \"Init\"}" responseTag wait
    Abort -> forwardCommands appConfig "{\"tag\": \"Abort\"}" responseTag wait
    GetUTxO -> forwardCommands appConfig "{\"tag\": \"GetUTxO\"}" responseTag wait
    CloseHead -> forwardCommands appConfig "{\"tag\": \"Close\"}" responseTag wait
    ContestHead -> forwardCommands appConfig "{\"tag\": \"Contest\"}" responseTag wait
    FanOut -> forwardCommands appConfig "{\"tag\": \"Fanout\"}" responseTag wait

submitHydraTx :: AppConfig -> TxModal -> Bool -> IO (Either FrameworkError TxModal)
submitHydraTx appConfig txm wait = do
  hydraResponse <- forwardCommands appConfig newTxCommand (generateResponseTag NewTx) wait
  case snd hydraResponse of
    200 -> pure $ Right txm
    x ->
      pure $
        Left $
          FrameworkError TxSubmissionError $
            "Hydra responsded with status: " <> show x <> " and message: " <> show (fst hydraResponse)
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
