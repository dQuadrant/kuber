{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}


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
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromShelleyTxIn)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import Cardano.Kuber.Core.LocalNodeChainApi (kEvaluateExUnits', HasLocalNodeAPI (..), ChainConnectInfo)
import Cardano.Kuber.Core.TxFramework (executeTxBuilder)
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (btbInputs, btbReferenceInputs))



type ShelleyWitCount  = Word
type ByronWitCount    = Word

class HasKuberAPI a  where
  kTxBuildTxBody    :: TxBuilder ->  Kontract a w FrameworkError (TxBody BabbageEra)
  kBuildTx       :: TxBuilder -> Kontract a w FrameworkError (Tx BabbageEra)
  kTimeToSlot           :: POSIXTime -> Kontract a w FrameworkError SlotNo
  kSlotToTime           ::  SlotNo    -> Kontract a  w FrameworkError POSIXTime
  kEvaluateExUnits :: Tx BabbageEra -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kCalculateMinFee :: Tx BabbageEra -> Kontract a  w FrameworkError  Lovelace
  kBuildAndSubmit :: TxBuilder -> Kontract a w FrameworkError (Tx BabbageEra)



-- instance {-# OVERLAPPABLE #-}   (HasChainQueryAPI a,HasSubmitApi a,HasLocalNodeAPI a ) => HasKuberAPI a where

--   kTxBuildTxBody    :: TxBuilder ->  Kontract a w FrameworkError (TxBody BabbageEra)
--   kTxBuildTxBody builder = executeTxBuilder builder <&> fst

--   kBuildTx       :: TxBuilder -> Kontract a w FrameworkError (Tx BabbageEra)
--   kBuildTx  builder =  executeTxBuilder builder <&> snd

--   kTimeToSlot           :: POSIXTime -> Kontract a w FrameworkError SlotNo
--   kTimeToSlot  time = timestampToSlot <$> kQuerySystemStart   <*> kQueryEraHistory <*> pure time

--   kSlotToTime           ::  SlotNo    -> Kontract a  w FrameworkError POSIXTime
--   kSlotToTime   slot = slotToTimestamp <$> kQuerySystemStart <*> kQueryEraHistory <*>  pure slot

--   kEvaluateExUnits :: Tx BabbageEra -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
--   kEvaluateExUnits tx  = do
--     let txBody = getTxBody tx
--         ins =  case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> Ledger.inputs tb } 
--         refs = case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> Ledger.referenceInputs tb } 
--         allInputs =  Set.map fromShelleyTxIn (ins <> refs)
--     utxos <-kQueryUtxoByTxin allInputs
--     kEvaluateExUnits' (getTxBody tx) utxos


--   kCalculateMinFee :: Tx BabbageEra -> Kontract a  w FrameworkError  Lovelace
--   kCalculateMinFee tx = do 
--       kCalculateMinFee' (getTxBody tx) (fromInteger $ toInteger $  length (getTxWitnesses tx)) 0


--   kBuildAndSubmit :: TxBuilder -> Kontract a w FrameworkError (Tx BabbageEra)
--   kBuildAndSubmit  builder =  do
--       tx <- executeTxBuilder builder <&> snd
--       kSubmitTx tx
--       pure tx


-- instance(HasChainQueryAPI a,HasSubmitApi a ) => HasKuberAPI (CachedApi a)


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
        ins =  case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> btbInputs tb } 
        refs = case txBody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> btbReferenceInputs tb } 
        allInputs =  Set.map fromShelleyTxIn (ins <> refs)
    utxos <-kQueryUtxoByTxin allInputs
    kEvaluateExUnits' (getTxBody tx) utxos

kCalculateMinFee' tx = do 
    kCalculateMinFee'' (getTxBody tx) (fromInteger $ toInteger $  length (getTxWitnesses tx)) 0

kCalculateMinFee'' txbody shelleyWitnesses byronWitnesses = do 
    protocolParams <- kQueryProtocolParams
    bpparams <- case bundleProtocolParams cardanoEra protocolParams of
        Left ppce -> error "Couldn't Convert protocol parameters."
        Right bpp -> pure bpp
    pure $ evaluateTransactionFee bpparams  txbody shelleyWitnesses byronWitnesses


kBuildAndSubmit'  builder =  do
    tx <- executeTxBuilder builder <&> snd
    kSubmitTx tx
    pure tx
