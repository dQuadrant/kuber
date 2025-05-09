{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Websocket.Aeson where

import Cardano.Api (ExecutionUnitPrices)
import Cardano.Api.Shelley (ExecutionUnits)
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.Natural

data Host = Host
  { ip :: String,
    port :: Int
  }

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
  deriving (Generic, Show, ToJSON)

data ContentsAndTag = ContentsAndTag
  { contents :: A.Value,
    tag :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data HydraState = HydraState
  { state :: ContentsAndTag
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data InitializedHeadParameters = InitializedHeadParameters
  { parties :: [VKeys]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data InitializedHeadContents = InitializedHeadContents
  { parameters :: InitializedHeadParameters,
    pendingCommits :: Maybe [VKeys]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data VKeys = VKeys
  { vkey :: T.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data InitializedHeadContentsAndTag = InitializedHeadContentsAndTag
  { tag :: T.Text,
    contents :: InitializedHeadContents
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data InitializedHeadResponse = InitializedHeadResponse
  { seq :: Maybe Int,
    timestamp :: UTCTime,
    state :: InitializedHeadContentsAndTag
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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

data HeadState
  = HeadIsIdle
  | WaitingCommitments
  | PartiallyCommitted
  | HeadIsReady
  | HeadIsClosed
  | HeadIsContested