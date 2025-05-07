{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Websocket.Aeson where

import Cardano.Api (ExecutionUnitPrices)
import Cardano.Api.Shelley (ExecutionUnits)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.Natural
import qualified Data.Aeson as A

newtype HydraGetUTxOResponse = HydraGetUTxOResponse
  { utxo :: M.Map T.Text A.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GroupedUTXO = GroupedUTXO
  { address :: T.Text,
    utxos :: [T.Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data WSMessage = WSMessage
  { tag :: T.Text,
    seq :: Maybe Int,
    timestamp :: UTCTime
  }
  deriving (Show)

instance FromJSON WSMessage where
  parseJSON = withObject "WSMessage" $ \v -> do
    tagVal <- v .: "tag"
    seqVal <- v Aeson..:? "seq"
    tsVal <- v .: "timestamp"
    ts <- iso8601ParseM tsVal
    return $ WSMessage tagVal seqVal ts

data HydraProtocolParameters = HydraProtocolParameters
  { collateralPercentage :: Maybe Natural,
    committeeMaxTermLength :: Maybe Int,
    committeeMinSize :: Maybe Int,
    costModels :: M.Map T.Text [Int],
    dRepActivity :: Maybe Int,
    dRepDeposit :: Maybe Int,
    dRepVotingThresholds :: Maybe DRepVotingThresholds,
    executionUnitPrices :: Maybe ExecutionUnitPrices,
    govActionDeposit :: Maybe Int,
    govActionLifetime :: Maybe Int,
    maxBlockBodySize :: Natural,
    maxBlockExecutionUnits :: Maybe ExecutionUnits,
    maxBlockHeaderSize :: Natural,
    maxCollateralInputs :: Maybe Natural,
    maxTxExecutionUnits :: Maybe ExecutionUnits,
    maxTxSize :: Natural,
    maxValueSize :: Maybe Natural,
    minFeeRefScriptCostPerByte :: Maybe Int,
    minPoolCost :: Int,
    monetaryExpansion :: Rational,
    poolPledgeInfluence :: Rational,
    poolRetireMaxEpoch :: Int,
    poolVotingThresholds :: Maybe PoolVotingThresholds,
    protocolVersion :: Maybe ProtocolVersion,
    extraPraosEntropy :: Maybe Int,
    stakeAddressDeposit :: Int,
    stakePoolDeposit :: Int,
    stakePoolTargetNum :: Int,
    minUTxOValue :: Maybe Int,
    decentralization :: Maybe Rational,
    treasuryCut :: Rational,
    txFeeFixed :: Int,
    txFeePerByte :: Int,
    utxoCostPerByte :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON HydraProtocolParameters

data DRepVotingThresholds = DRepVotingThresholds
  { committeeNoConfidence :: Maybe Double,
    committeeNormal :: Maybe Double,
    hardForkInitiation :: Maybe Double,
    motionNoConfidence :: Maybe Double,
    ppEconomicGroup :: Maybe Double,
    ppGovGroup :: Maybe Double,
    ppNetworkGroup :: Maybe Double,
    ppTechnicalGroup :: Maybe Double,
    treasuryWithdrawal :: Maybe Double,
    updateToConstitution :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON DRepVotingThresholds

data PoolVotingThresholds = PoolVotingThresholds
  { committeeNoConfidence :: Maybe Double,
    committeeNormal :: Maybe Double,
    hardForkInitiation :: Maybe Double,
    motionNoConfidence :: Maybe Double,
    ppSecurityGroup :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON PoolVotingThresholds

data ProtocolVersion = ProtocolVersion
  { major :: Natural,
    minor :: Natural
  }
  deriving (Show, Generic)

instance FromJSON ProtocolVersion