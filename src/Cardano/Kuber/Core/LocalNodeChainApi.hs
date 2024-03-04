{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Kuber.Core.LocalNodeChainApi where
import Cardano.Api hiding (queryGovState, queryCurrentEra, queryEraHistory, queryChainPoint, querySystemStart)
import Cardano.Api.Shelley hiding (queryGovState, queryCurrentEra, queryEraHistory, queryChainPoint, querySystemStart)

import Cardano.Kuber.Core.ChainAPI
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Error
import Cardano.Slotting.Time (SystemStart)
import Cardano.Kuber.Utility.QueryHelper (queryProtocolParam, querySystemStart, queryEraHistory, queryGenesesisParams, queryUtxos, queryTxins, queryChainPoint, submitTx, queryGenesesisParams', queryCurrentEra, queryGovState, queryStakeDeposits, queryDRepState, queryDRepDistribution)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Map (Map)
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Data.Set as Set
import Cardano.Kuber.Core.TxBuilder (TxBuilder, IsTxBuilderEra (bCardanoEra))
import qualified Data.Map as Map
import Data.Functor ((<&>))
import Cardano.Kuber.Data.EraUpdate (updatePParamEra)

class ChainInfo v where
  getConnectInfo :: v-> LocalNodeConnectInfo
  getNetworkId :: v -> NetworkId

class HasLocalNodeAPI a where
    kQueryEraHistory      :: Kontract a w FrameworkError EraHistory

-- ChainConnectInfo wraps (LocalNodeConnectInfo CardanoMode)
-- This is the minimal information required to connect to a cardano node
newtype ChainConnectInfo= ChainConnectInfo LocalNodeConnectInfo

instance HasChainQueryAPI LocalNodeConnectInfo where
    kQueryProtocolParams = liftLnciQuery queryProtocolParam
    kQuerySystemStart = liftLnciQuery querySystemStart
    kQueryGenesisParams = liftLnciQuery queryGenesesisParams'
    kQueryUtxoByAddress = liftLnciQuery2 queryUtxos
    kQueryUtxoByTxin = liftLnciQuery2 queryTxins
    kQueryChainPoint  = liftLnciQuery queryChainPoint
    kGetNetworkId  = KLift $ \c  -> pure  $pure $ localNodeNetworkId c
    kQueryCurrentEra = liftLnciQuery queryCurrentEra
    kQueryGovState = liftLnciQuery queryGovState
    kQueryStakeDeposit = liftLnciQuery2 (queryStakeDeposits ShelleyBasedEraConway)
    kQueryDrepState  = liftLnciQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepState ShelleyBasedEraConway)
    kQueryDRepDistribution = liftLnciQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepDistribution ShelleyBasedEraConway)

instance HasLocalNodeAPI LocalNodeConnectInfo where
    kQueryEraHistory = liftLnciQuery queryEraHistory
instance HasSubmitApi LocalNodeConnectInfo where
    kSubmitTx  = liftLnciQuery2 submitTx


instance HasChainQueryAPI ChainConnectInfo where
    kQueryProtocolParams = liftCinfoQuery queryProtocolParam
    kQuerySystemStart = liftCinfoQuery querySystemStart
    kQueryGenesisParams = liftCinfoQuery queryGenesesisParams'
    kQueryUtxoByAddress = liftCinfoQuery2 queryUtxos
    kQueryUtxoByTxin = liftCinfoQuery2 queryTxins
    kQueryChainPoint  = liftCinfoQuery queryChainPoint
    kGetNetworkId = KLift $ \(ChainConnectInfo c)  -> pure  $pure $ localNodeNetworkId c
    kQueryCurrentEra = liftCinfoQuery queryCurrentEra
    kQueryGovState = liftCinfoQuery queryGovState
    kQueryStakeDeposit = liftCinfoQuery2 (queryStakeDeposits ShelleyBasedEraConway)
    kQueryDrepState  = liftCinfoQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepState ShelleyBasedEraConway)
    kQueryDRepDistribution = liftCinfoQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepDistribution ShelleyBasedEraConway)
    
instance HasLocalNodeAPI ChainConnectInfo where
    kQueryEraHistory = liftCinfoQuery queryEraHistory

instance HasSubmitApi ChainConnectInfo where
  kSubmitTx tx  = KLift $ \(ChainConnectInfo c)  ->submitTx  c tx

liftCinfoQuery q =  KLift $ \(ChainConnectInfo c)  -> q c
liftCinfoQuery2 q p =  KLift $ \(ChainConnectInfo c)  -> q c p

liftLnciQuery q =  KLift $ \c  -> q c
liftLnciQuery2 q p =  KLift $ \c  -> q c p

kEvaluateExUnits' ::  (HasChainQueryAPI a, HasLocalNodeAPI a,IsTxBuilderEra era) =>   TxBody era -> UTxO era -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
kEvaluateExUnits' txbody utxos = do
  sStart <- kQuerySystemStart
  eHhistory <- kQueryEraHistory
  pParams <- kQueryProtocolParams
  case evaluateTransactionExecutionUnits
    cardanoEra
    sStart
    (toLedgerEpochInfo eHhistory)
    pParams
    utxos
    txbody of
      Left tve -> KError $  FrameworkError ExUnitCalculationError (show tve)
      Right map -> pure $ Map.map (\case
             Left see -> Left (fromScriptExecutionError  see txbody)
             Right eu -> pure eu ) map