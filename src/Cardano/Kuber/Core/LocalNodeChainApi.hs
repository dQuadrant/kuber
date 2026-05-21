{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Kuber.Core.LocalNodeChainApi where

import Cardano.Api hiding (queryChainPoint, queryCurrentEra, queryEraHistory, queryGovState, querySystemStart)
import Cardano.Api.Shelley hiding (queryChainPoint, queryCurrentEra, queryEraHistory, queryGovState, querySystemStart)
import Cardano.Kuber.Core.ChainAPI
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (bCardanoEra), TxBuilder)
import Cardano.Kuber.Data.EraUpdate (updatePParamEra, upgradeBabbagePParams)
import Cardano.Kuber.Error
import Cardano.Kuber.Utility.QueryHelper (queryChainPoint, queryCurrentEra, queryDRepDistribution, queryDRepState, queryEraHistory, queryGenesesisParams, queryGenesesisParams', queryGovState, queryProtocolParam, queryStakeDeposits, querySystemStart, queryTxins, queryUtxos, submitTx)
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import Cardano.Slotting.Time (SystemStart)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)

class ChainInfo v where
  getConnectInfo :: v -> LocalNodeConnectInfo
  getNetworkId :: v -> NetworkId

class HasLocalNodeAPI a where
  kQueryEraHistory :: Kontract a w FrameworkError EraHistory

-- ChainConnectInfo wraps (LocalNodeConnectInfo CardanoMode)
-- This is the minimal information required to connect to a cardano node
newtype ChainConnectInfo = ChainConnectInfo LocalNodeConnectInfo

instance HasChainQueryAPI LocalNodeConnectInfo where
  kQueryProtocolParams = KLift queryProtocolParamsForNodeEra
  kQueryUtxoByAddress = liftLnciQuery2 queryUtxos
  kQueryUtxoByTxin = liftLnciQuery2 queryTxins
  kQueryChainPoint = liftLnciQuery queryChainPoint

instance HasCardanoQueryApi LocalNodeConnectInfo where
  kQuerySystemStart = liftLnciQuery querySystemStart
  kQueryGenesisParams = liftLnciQuery queryGenesesisParams'
  kGetNetworkId = KLift $ \c -> pure $ pure $ localNodeNetworkId c
  kQueryCurrentEra = liftLnciQuery queryCurrentEra
  kQueryGovState = liftLnciQuery queryGovState
  kQueryStakeDeposit = liftLnciQuery2 (queryStakeDeposits ShelleyBasedEraConway)
  kQueryDrepState = liftLnciQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepState ShelleyBasedEraConway)
  kQueryDRepDistribution = liftLnciQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepDistribution ShelleyBasedEraConway)

instance HasLocalNodeAPI LocalNodeConnectInfo where
  kQueryEraHistory = liftLnciQuery queryEraHistory

instance HasSubmitApi LocalNodeConnectInfo where
  kSubmitTx = liftLnciQuery2 submitTx

instance HasChainQueryAPI ChainConnectInfo where
  kQueryProtocolParams = KLift $ \(ChainConnectInfo c) -> queryProtocolParamsForNodeEra c
  kQueryUtxoByAddress = liftCinfoQuery2 queryUtxos
  kQueryUtxoByTxin = liftCinfoQuery2 queryTxins
  kQueryChainPoint = liftCinfoQuery queryChainPoint

instance HasCardanoQueryApi ChainConnectInfo where
  kQuerySystemStart = liftCinfoQuery querySystemStart
  kQueryGenesisParams = liftCinfoQuery queryGenesesisParams'
  kGetNetworkId = KLift $ \(ChainConnectInfo c) -> pure $ pure $ localNodeNetworkId c
  kQueryCurrentEra = liftCinfoQuery queryCurrentEra
  kQueryGovState = liftCinfoQuery queryGovState
  kQueryStakeDeposit = liftCinfoQuery2 (queryStakeDeposits ShelleyBasedEraConway)
  kQueryDrepState = liftCinfoQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepState ShelleyBasedEraConway)
  kQueryDRepDistribution = liftCinfoQuery2 (Cardano.Kuber.Utility.QueryHelper.queryDRepDistribution ShelleyBasedEraConway)

instance HasLocalNodeAPI ChainConnectInfo where
  kQueryEraHistory = liftCinfoQuery queryEraHistory

instance HasSubmitApi ChainConnectInfo where
  kSubmitTx tx = KLift $ \(ChainConnectInfo c) -> submitTx c tx

liftCinfoQuery q = KLift $ \(ChainConnectInfo c) -> q c

liftCinfoQuery2 q p = KLift $ \(ChainConnectInfo c) -> q c p

liftLnciQuery q = KLift $ \c -> q c

liftLnciQuery2 q p = KLift $ \c -> q c p

queryProtocolParamsForNodeEra :: forall era. IsTxBuilderEra era => LocalNodeConnectInfo -> IO (Either FrameworkError (LedgerProtocolParameters era))
queryProtocolParamsForNodeEra conn = do
  queryCurrentEra conn >>= \case
    Left fe -> pure $ Left fe
    Right (AnyCardanoEra currentEra) -> case bCardanoEra @era of
      BabbageEra -> case currentEra of
        BabbageEra -> queryProtocolParam @BabbageEra conn
        ConwayEra -> queryProtocolParam @ConwayEra conn <&> fmap (updatePParamEra BabbageEra)
        _ -> pure $ Left $ FrameworkError FeatureNotSupported ("Protocol parameter queries require a Babbage-or-later node, current era is " ++ show currentEra)
      ConwayEra -> case currentEra of
        BabbageEra -> queryProtocolParam @BabbageEra conn <&> (>>= upgradeBabbagePParams)
        ConwayEra -> queryProtocolParam @ConwayEra conn
        _ -> pure $ Left $ FrameworkError FeatureNotSupported ("Protocol parameter queries require a Babbage-or-later node, current era is " ++ show currentEra)
      requestedEra -> pure $ Left $ FrameworkError FeatureNotSupported ("Protocol parameter queries require a Babbage-or-Conway target era, requested " ++ show requestedEra)

kEvaluateExUnits' :: (HasChainQueryAPI a, HasCardanoQueryApi a, HasLocalNodeAPI a, IsTxBuilderEra era) => TxBody era -> UTxO era -> Kontract a w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
kEvaluateExUnits' txbody utxos = do
  sStart <- kQuerySystemStart
  eHhistory <- kQueryEraHistory
  pParams <- kQueryProtocolParams
  let mp =
        evaluateTransactionExecutionUnits
          cardanoEra
          sStart
          (toLedgerEpochInfo eHhistory)
          pParams
          utxos
          txbody
  pure $
    Map.map
      ( \case
          Left see -> Left (fromScriptExecutionError see txbody)
          Right (_, eu) -> pure eu
      )
      mp
