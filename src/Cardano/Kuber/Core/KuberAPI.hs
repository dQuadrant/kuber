{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Kuber.Core.KuberAPI where

import Cardano.Api
import Cardano.Api.Shelley (LedgerProtocolParameters (unLedgerProtocolParameters), TxBody (ShelleyTxBody), convertToLedgerProtocolParameters, fromShelleyTxIn)
import Cardano.Kuber.Core.ChainAPI
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.LocalNodeChainApi (ChainConnectInfo, HasLocalNodeAPI (..), kEvaluateExUnits')
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Core.TxFramework (executeTxBuilder, executeRawTxBuilder)
import Cardano.Kuber.Error
import Cardano.Kuber.Utility.Misc
import Cardano.Ledger.Api (EraTxBody (inputsTxBodyL))
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (referenceInputsTxBodyL))
import Control.Lens ((^.))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Cardano.Api.Ledger (Coin)

type ShelleyWitCount = Word

type ByronWitCount = Word

class HasKuberAPI a where
  kTxBuildTxBody :: IsTxBuilderEra era => TxBuilder_ era -> Kontract a w FrameworkError (TxBody era)
  kBuildTx :: IsTxBuilderEra era => TxBuilder_ era -> Kontract a w FrameworkError (Tx era)
  kBuildRawTx :: IsTxBuilderEra era => TxBuilder_ era -> LedgerProtocolParameters era -> Kontract a w FrameworkError (Tx era) 
  kTimeToSlot :: POSIXTime -> Kontract a w FrameworkError SlotNo
  kSlotToTime :: SlotNo -> Kontract a w FrameworkError POSIXTime
  kEvaluateExUnits :: IsTxBuilderEra era => Tx era -> Kontract a w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kCalculateMinFee :: IsTxBuilderEra era => Tx era -> Kontract a w FrameworkError Coin
  kBuildAndSubmit :: IsTxBuilderEra era => TxBuilder_ era -> Kontract a w FrameworkError (Tx era)

instance HasKuberAPI LocalNodeConnectInfo where
  kTxBuildTxBody = kTxBuildTxBody'
  kBuildTx = kBuildTx'
  kBuildRawTx = kBuildRawTx'
  kTimeToSlot = kTimeToSlot'
  kSlotToTime = kSlotToTime'
  kEvaluateExUnits = kEvaluateExUnits''
  kCalculateMinFee = kCalculateMinFee'
  kBuildAndSubmit = kBuildAndSubmit'

instance HasKuberAPI ChainConnectInfo where
  kTxBuildTxBody = kTxBuildTxBody'
  kBuildTx = kBuildTx'
  kBuildRawTx = kBuildRawTx'
  kTimeToSlot = kTimeToSlot'
  kSlotToTime = kSlotToTime'
  kEvaluateExUnits = kEvaluateExUnits''
  kCalculateMinFee = kCalculateMinFee'
  kBuildAndSubmit = kBuildAndSubmit'

kTxBuildTxBody' builder = executeTxBuilder builder <&> fst

kBuildTx' builder = executeTxBuilder builder <&> snd

kBuildRawTx' builder pparams = executeRawTxBuilder builder pparams <&> snd

kTimeToSlot' time = timestampToSlot <$> kQuerySystemStart <*> kQueryEraHistory <*> pure time

kSlotToTime' slot = slotToTimestamp <$> kQuerySystemStart <*> kQueryEraHistory <*> pure slot

kEvaluateExUnits'' tx = do
  let txBody = getTxBody tx
      allInputs = resolveEra bShelleyBasedEra txBody

  utxos <- kQueryUtxoByTxin allInputs
  kEvaluateExUnits' (getTxBody tx) utxos

resolveEra :: ShelleyBasedEra era -> TxBody era -> Set.Set TxIn
resolveEra sbera body = case sbera of
  ShelleyBasedEraBabbage -> getTxUnknownInputs (body :: TxBody BabbageEra)
  ShelleyBasedEraConway -> getTxUnknownInputs (body :: TxBody ConwayEra)
  _ -> error "Unexpected"

getTxUnknownInputs txBody =
  let ledgerTxBody = case txBody of ShelleyTxBody sbe tb scs tbsd m_ad tsv -> tb
      ins = ledgerTxBody ^. inputsTxBodyL
      refs = ledgerTxBody ^. referenceInputsTxBodyL --case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> btbReferenceInputs tb }
    in Set.map fromShelleyTxIn (ins <> refs)

kCalculateMinFee' :: (HasChainQueryAPI a, IsTxBuilderEra era) => Tx era -> Kontract a w FrameworkError Coin
kCalculateMinFee' tx = do
  kCalculateMinFee'' (getTxBody tx) (fromInteger $ toInteger $ length (getTxWitnesses tx)) 0

kCalculateMinFee'' :: (HasChainQueryAPI a, IsTxBuilderEra era) => TxBody era -> Word -> Word -> Kontract a w FrameworkError Coin
kCalculateMinFee'' txbody shelleyWitnesses byronWitnesses = do
  protocolParams <- kQueryProtocolParams
  let 
    allInputs = resolveEra bShelleyBasedEra txbody

  utxo <- kQueryUtxoByTxin allInputs

  -- todo: fix this to support reference scripts
  pure $ calculateMinTxFee shelleyBasedEra (unLedgerProtocolParameters protocolParams) utxo txbody shelleyWitnesses 

kBuildAndSubmit' :: (HasChainQueryAPI api, HasLocalNodeAPI api, IsTxBuilderEra era,  HasSubmitApi api) => TxBuilder_ era -> Kontract api w FrameworkError (Tx era)
kBuildAndSubmit' builder = do
  tx <- executeTxBuilder builder <&> snd
  kSubmitTx (InAnyCardanoEra bCardanoEra tx)
  pure tx
