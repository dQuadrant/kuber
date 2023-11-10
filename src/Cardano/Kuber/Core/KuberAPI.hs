{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Kuber.Core.KuberAPI where
import Cardano.Kuber.Core.ChainAPI
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.TxBuilder

import Cardano.Kuber.Error
import Cardano.Api
import Data.Time.Clock.POSIX (POSIXTime)
import Cardano.Kuber.Utility.Misc
import Data.Functor ((<&>))
import Data.Map (Map)
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromShelleyTxIn, convertToLedgerProtocolParameters, LedgerProtocolParameters (unLedgerProtocolParameters), ShelleyLedgerEra)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import Cardano.Kuber.Core.LocalNodeChainApi (kEvaluateExUnits', HasLocalNodeAPI (..), ChainConnectInfo)
import Cardano.Kuber.Core.TxFramework (executeTxBuilder)
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (btbInputs, btbReferenceInputs), BabbageEraTxBody (referenceInputsTxBodyL))
import Cardano.Ledger.Api (EraTxBody(inputsTxBodyL))
import Control.Lens ((^.))
import Cardano.Kuber.Data.EraUpdate (updateUtxoEra)



type ShelleyWitCount  = Word
type ByronWitCount    = Word

class HasKuberAPI a  where
  kTxBuildTxBody    :: IsTxBuilderEra era =>  TxBuilder_ era ->  Kontract a w FrameworkError (TxBody era)
  kBuildTx       ::  IsTxBuilderEra era => TxBuilder_ era -> Kontract a w FrameworkError (Tx era)
  kTimeToSlot           :: POSIXTime -> Kontract a w FrameworkError SlotNo
  kSlotToTime           ::  SlotNo    -> Kontract a  w FrameworkError POSIXTime
  kEvaluateExUnits :: IsTxBuilderEra era => Tx era -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kCalculateMinFee :: IsTxBuilderEra era => Tx era -> Kontract a  w FrameworkError  Lovelace
  kBuildAndSubmit :: IsTxBuilderEra era => TxBuilder_ era -> Kontract a w FrameworkError (Tx era)


instance   HasKuberAPI (LocalNodeConnectInfo CardanoMode) where
  kTxBuildTxBody = kTxBuildTxBody'
  kBuildTx = kBuildTx'
  kTimeToSlot = kTimeToSlot'
  kSlotToTime = kSlotToTime'
  kEvaluateExUnits = kEvaluateExUnits''
  kCalculateMinFee = kCalculateMinFee'
  kBuildAndSubmit = kBuildAndSubmit'


instance   HasKuberAPI ChainConnectInfo where
  kTxBuildTxBody = kTxBuildTxBody'
  kBuildTx = kBuildTx'
  kTimeToSlot = kTimeToSlot'
  kSlotToTime = kSlotToTime'
  kEvaluateExUnits = kEvaluateExUnits''
  kCalculateMinFee = kCalculateMinFee'
  kBuildAndSubmit = kBuildAndSubmit'

kTxBuildTxBody' builder = executeTxBuilder builder <&> fst

kBuildTx'  builder =  executeTxBuilder builder <&> snd

kTimeToSlot'  time = timestampToSlot <$> kQuerySystemStart   <*> kQueryEraHistory <*> pure time

kSlotToTime'   slot = slotToTimestamp <$> kQuerySystemStart <*> kQueryEraHistory <*>  pure slot

kEvaluateExUnits'' tx  = do
    let txBody = getTxBody tx
        allInputs = resolveEra bShelleyBasedEra txBody
        
    utxos <-kQueryUtxoByTxin allInputs
    kEvaluateExUnits' (getTxBody tx)  utxos
    
  where
    resolveEra :: IsShelleyBasedEra era => ShelleyBasedEra era -> TxBody era -> Set.Set TxIn
    resolveEra sbera body = case sbera of 
      ShelleyBasedEraBabbage -> getTxUnknownInputs  (body ::TxBody BabbageEra)
      ShelleyBasedEraConway -> getTxUnknownInputs  (body ::TxBody ConwayEra)
      _ -> error "Unexpected"

    getTxUnknownInputs txBody = let 
          ledgerTxBody = case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> tb}
          ins =   ledgerTxBody ^. inputsTxBodyL
          refs =   ledgerTxBody ^. referenceInputsTxBodyL --case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> btbReferenceInputs tb } 
      in Set.map fromShelleyTxIn (ins <> refs)

kCalculateMinFee' :: (HasChainQueryAPI a, IsShelleyBasedEra era) => Tx era -> Kontract a w FrameworkError Lovelace
kCalculateMinFee' tx = do 
    kCalculateMinFee'' (getTxBody tx) (fromInteger $ toInteger $  length (getTxWitnesses tx)) 0

kCalculateMinFee'' :: (HasChainQueryAPI a, IsShelleyBasedEra era) => TxBody era -> Word -> Word -> Kontract a w FrameworkError Lovelace
kCalculateMinFee'' txbody shelleyWitnesses byronWitnesses = do 
    protocolParams <- kQueryProtocolParams
    let capiParams = fromLedgerPParams ShelleyBasedEraConway (unLedgerProtocolParameters protocolParams)
    bpparams <- case convertToLedgerProtocolParameters shelleyBasedEra capiParams  of
        Left ppce -> error "Couldn't Convert protocol parameters."
        Right bpp -> pure bpp
    pure $ evaluateTransactionFee shelleyBasedEra (unLedgerProtocolParameters bpparams)  txbody shelleyWitnesses byronWitnesses


kBuildAndSubmit'  builder =  do
    tx <- executeTxBuilder builder <&> snd
    kSubmitTx (InAnyCardanoEra bCardanoEra tx)
    pure tx
