{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Kuber.Test.Testnet where

import Cardano.Api (ConwayEraOnwards)
import Cardano.Api qualified as C
import Cardano.Api.Ledger (
    Coin (Coin),
    CoinPerWord (..),
    Credential (KeyHashObj),
    EpochInterval (..),
    EraCrypto,
    KeyHash,
    KeyRole (StakePool),
    StandardCrypto,
    Voter (StakePoolVoter),
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.PParams
import Cardano.Testnet (Conf (tempAbsPath))
import Cardano.Testnet qualified as CTN
import Control.Lens ((&), (.~))
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity)
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as Time
import Debug.Trace qualified as Debug
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock (waitSecondsForProcess)
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H

import Cardano.Api.Shelley (pshow)
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Plutus qualified as L
import Data.Bifunctor
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Ratio
import Data.Word

import Prettyprinter (Doc)
import System.Directory
import System.Directory qualified as IO
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (cleanupProcess)
import System.Process.Internals (
    PHANDLE,
    ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
    withProcessHandle,
 )
import Testnet.Defaults qualified as CTN
import Testnet.Runtime qualified as CTN
import Testnet.Types qualified as CTN
import Data.Ratio
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Data.Data (Typeable)
import Data.Default.Class (def)
import Data.IORef (IORef, newIORef)
import GHC.IO.Exception (ExitCode)
import Test.Tasty
import Test.Tasty.Hedgehog

import Control.Exception (try, SomeException (SomeException), finally)
import qualified System.Info as IO
import qualified System.Environment as IO
import GHC.Base (when)
import GHC.Conc (threadDelay)
import Control.Exception.Base (throw)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS8L
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams(appMinFeeA), ppCoinsPerUTxOWordL)
import Cardano.Ledger.Api.Transition (ShelleyGenesis(..))
import Cardano.Ledger.Api (ppMinFeeAL, ppMinFeeBL, ppCoinsPerUTxOByteL)

clusterFilePath :: IO FilePath
clusterFilePath = do
    currentDir <- getCurrentDirectory
    return $ currentDir </> ".cluster"

data TestEnvironmentOptions era
    = TestnetOptions
        { testnetEra :: C.CardanoEra era
        , testnetProtocolVersion :: Int
        , testnetCardanoOptions :: CTN.CardanoTestnetOptions
        }
    | LocalNodeOptions
        { localNodeEra :: C.CardanoEra era
        , localNodeProtocolVersion :: Int
        , localNodeEnvDir :: FilePath -- path to directory containing 'utxo-keys' and 'ipc' directories
        , localNodeTestnetMagic :: Int
        }
    deriving (Show)

defAlonzoTestnetOptions :: TestEnvironmentOptions C.AlonzoEra
defAlonzoTestnetOptions =
    TestnetOptions
        { testnetEra = C.AlonzoEra
        , testnetProtocolVersion = 6
        , testnetCardanoOptions =
            CTN.cardanoDefaultTestnetOptions
                { CTN.cardanoNodeEra = C.AnyCardanoEra C.AlonzoEra
                , CTN.cardanoActiveSlotsCoeff = 0.1
                , CTN.cardanoSlotLength = 0.1
                , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
                }
        }

defBabbageTestnetOptions :: Int -> TestEnvironmentOptions C.BabbageEra
defBabbageTestnetOptions protocolVersion =
    TestnetOptions
        { testnetEra = C.BabbageEra
        , testnetProtocolVersion = protocolVersion
        , testnetCardanoOptions =
            CTN.cardanoDefaultTestnetOptions
                { CTN.cardanoNodeEra = C.AnyCardanoEra C.BabbageEra
                , CTN.cardanoActiveSlotsCoeff = 0.1
                , CTN.cardanoSlotLength = 0.1
                , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
                }
        }

defConwayTestnetOptions :: TestEnvironmentOptions C.ConwayEra
defConwayTestnetOptions =
    TestnetOptions
        { testnetEra = C.ConwayEra
        , testnetProtocolVersion = 9
        , testnetCardanoOptions =
            CTN.cardanoDefaultTestnetOptions
                { CTN.cardanoNodeEra = C.AnyCardanoEra C.ConwayEra
                , CTN.cardanoActiveSlotsCoeff = 0.1
                , CTN.cardanoSlotLength = 0.1
                , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
                }
        }

shortEpochConwayTestnetOptions :: TestEnvironmentOptions C.ConwayEra
shortEpochConwayTestnetOptions =
    defConwayTestnetOptions
        { testnetCardanoOptions =
            (testnetCardanoOptions defConwayTestnetOptions)
                { CTN.cardanoActiveSlotsCoeff = 0.1 -- adjusted from default due to short epoch length
                -- 200 second epoch for testing outcome of governance actions (shorter is unstable)
                , CTN.cardanoEpochLength = 2_000
                , CTN.cardanoSlotLength = 0.1
                }
        }

localNodeOptionsConway :: FilePath -> TestEnvironmentOptions C.ConwayEra
localNodeOptionsConway filePath =
    LocalNodeOptions
        { localNodeEra = C.ConwayEra
        , localNodeProtocolVersion = 9
        , localNodeEnvDir = filePath
        , localNodeTestnetMagic = 42
        }

localNodeOptionsPreview :: TestEnvironmentOptions C.BabbageEra
localNodeOptionsPreview =
    LocalNodeOptions
        { localNodeEra = C.BabbageEra
        , localNodeProtocolVersion = 8
        , localNodeEnvDir = "/tmp/preview"
        , localNodeTestnetMagic = 2
        }

testnetOptionsAlonzo6 :: TestEnvironmentOptions C.AlonzoEra
testnetOptionsAlonzo6 = defAlonzoTestnetOptions

testnetOptionsBabbage7 :: TestEnvironmentOptions C.BabbageEra
testnetOptionsBabbage7 = defBabbageTestnetOptions 7

testnetOptionsBabbage8 :: TestEnvironmentOptions C.BabbageEra
testnetOptionsBabbage8 = defBabbageTestnetOptions 8

testnetOptionsConway9 :: TestEnvironmentOptions C.ConwayEra
testnetOptionsConway9 = defConwayTestnetOptions

testnetOptionsConway9Governance :: TestEnvironmentOptions C.ConwayEra
testnetOptionsConway9Governance = shortEpochConwayTestnetOptions

eraFromOptions :: TestEnvironmentOptions era -> C.CardanoEra era
eraFromOptions options = case options of
    TestnetOptions era _ _ -> era
    LocalNodeOptions era _ _ _ -> era

eraFromOptionsM :: (MonadTest m) => TestEnvironmentOptions era -> m (C.CardanoEra era)
eraFromOptionsM = return . eraFromOptions

pvFromOptions :: (MonadTest m) => TestEnvironmentOptions era -> m Int
pvFromOptions (TestnetOptions _ pv _) = pure pv
pvFromOptions (LocalNodeOptions _ pv _ _) = pure pv

-- | Get path to where cardano-testnet files are
getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

plutusV3CostModel :: Map.Map Word8 [Int64]
plutusV3CostModel = Map.singleton 2 pv3CostModelInteger

pv3CostModelInteger :: [Int64]
pv3CostModelInteger =
    -- These cost models are causing plutus v3 transactions to use a lot of execution units
    [ 205665
    , 812
    , 1
    , 1
    , 1000
    , 571
    , 0
    , 1
    , 1000
    , 24177
    , 4
    , 1
    , 1000
    , 32
    , 117366
    , 10475
    , 4
    , 23000
    , 100
    , 23000
    , 100
    , 23000
    , 100
    , 23000
    , 100
    , 23000
    , 100
    , 23000
    , 100
    , 100
    , 100
    , 23000
    , 100
    , 19537
    , 32
    , 175354
    , 32
    , 46417
    , 4
    , 221973
    , 511
    , 0
    , 1
    , 89141
    , 32
    , 497525
    , 14068
    , 4
    , 2
    , 196500
    , 453240
    , 220
    , 0
    , 1
    , 1
    , 1000
    , 28662
    , 4
    , 2
    , 245000
    , 216773
    , 62
    , 1
    , 1060367
    , 12586
    , 1
    , 208512
    , 421
    , 1
    , 187000
    , 1000
    , 52998
    , 1
    , 80436
    , 32
    , 43249
    , 32
    , 1000
    , 32
    , 80556
    , 1
    , 57667
    , 4
    , 1000
    , 10
    , 197145
    , 156
    , 1
    , 197145
    , 156
    , 1
    , 204924
    , 473
    , 1
    , 208896
    , 511
    , 1
    , 52467
    , 32
    , 64832
    , 32
    , 65493
    , 32
    , 22558
    , 32
    , 16563
    , 32
    , 76511
    , 32
    , 196500
    , 453240
    , 220
    , 0
    , 1
    , 1
    , 69522
    , 11687
    , 0
    , 1
    , 60091
    , 32
    , 196500
    , 453240
    , 220
    , 0
    , 1
    , 1
    , 196500
    , 453240
    , 220
    , 0
    , 1
    , 1
    , 1159724
    , 392670
    , 0
    , 2
    , 806990
    , 30482
    , 4
    , 1927926
    , 82523
    , 4
    , 265318
    , 0
    , 4
    , 0
    , 85931
    , 32
    , 205665
    , 812
    , 1
    , 1
    , 41182
    , 32
    , 212342
    , 32
    , 31220
    , 32
    , 32696
    , 32
    , 43357
    , 32
    , 32247
    , 32
    , 38314
    , 32
    , 35190005
    , 10
    , 57996947
    , 18975
    , 10
    , 39121781
    , 32260
    , 10
    , 23000
    , 100
    , 23000
    , 100
    , 832808
    , 18
    , 3209094
    , 6
    , 331451
    , 1
    , 65990684
    , 23097
    , 18
    , 114242
    , 18
    , 94393407
    , 87060
    , 18
    , 16420089
    , 18
    , 2145798
    , 36
    , 3795345
    , 12
    , 889023
    , 1
    , 204237282
    , 23271
    , 36
    , 129165
    , 36
    , 189977790
    , 85902
    , 36
    , 33012864
    , 36
    , 388443360
    , 1
    , 401885761
    , 72
    , 2331379
    , 72
    , 1927926
    , 82523
    , 4
    , 117366
    , 10475
    , 4
    , 1292075
    , 24469
    , 74
    , 0
    , 1
    , 936157
    , 49601
    , 237
    , 0
    , 1
    ]

pv3CostModel =
    case L.mkCostModelsLenient plutusV3CostModel >>= Map.lookup L.PlutusV3 . L.costModelsValid of
        Nothing -> error "Incorrect cost model"
        Just cm -> cm

updatedConwayGenesis :: ConwayGenesis StandardCrypto
updatedConwayGenesis =
    let -- upPParams :: UpgradeConwayPParams Identity
        -- upPParams =
        --     UpgradeConwayPParams
        --         { ucppPoolVotingThresholds = poolVotingThresholds
        --         , ucppDRepVotingThresholds = drepVotingThresholds
        --         , ucppCommitteeMinSize = 0
        --         , ucppCommitteeMaxTermLength = EpochInterval 200
        --         , ucppGovActionLifetime = EpochInterval 2 -- One Epoch
        --         , ucppGovActionDeposit = Coin 1_000_000
        --         , ucppDRepDeposit = Coin 1_000_000
        --         , ucppDRepActivity = EpochInterval 100
        --         , ucppMinFeeRefScriptCostPerByte = 0 %! 1 -- FIXME GARBAGE VALUE
        --         , ucppPlutusV3CostModel = ucppPlutusV3CostModel $   CTN.defaultConwayGenesis
        --         }
        -- ucppPlutusV3CostModel $ cgUpgradePParams CTN.defaultConwayGenesis
        drepVotingThresholds =
            DRepVotingThresholds
                { dvtMotionNoConfidence = 0 %! 10
                , dvtCommitteeNormal = 0 %! 10
                , dvtCommitteeNoConfidence = 0 %! 10
                , dvtUpdateToConstitution = 0 %! 10
                , dvtHardForkInitiation = 0 %! 10
                , dvtPPNetworkGroup = 0 %! 10
                , dvtPPEconomicGroup = 0 %! 10
                , dvtPPTechnicalGroup = 0 %! 10
                , dvtPPGovGroup = 0 %! 10
                , dvtTreasuryWithdrawal = 0 %! 10
                }
        poolVotingThresholds =
            PoolVotingThresholds
                { pvtMotionNoConfidence = 0 %! 10
                , pvtCommitteeNormal = 0 %! 10
                , pvtCommitteeNoConfidence = 0 %! 10
                , pvtHardForkInitiation = 0 %! 10
                , pvtPPSecurityGroup = 0 %! 10
                }
        defaultPparam=cgUpgradePParams CTN.defaultConwayGenesis
     in CTN.defaultConwayGenesis
            { cgUpgradePParams = defaultPparam{
                                 ucppPlutusV3CostModel = pv3CostModel
                                , ucppMinFeeRefScriptCostPerByte = 44 %! 1

                     }
            -- , cgConstitution = def
            -- , cgCommittee = def
            -- , cgDelegs = mempty
            -- , cgInitialDReps = mempty
            }


data TestResult = TestResult
    { resultTestName :: String
    , resultTestDescription :: String
    , resultSuccessful :: Bool
    , resultFailure :: Maybe String -- TODO: use this in failureElement in xml output
    , resultTime :: Double
    }
    deriving (Show)

data TestnetRef = TestnetRef
    {testnetResultsRef :: IORef [TestResult]}

integrationClusterWorkspace ::
    (HasCallStack) => Int -> FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationClusterWorkspace n workspaceName f = withFrozenCallStack $
    CTN.integration $
        H.retry n $ \i ->
            (liftIO setTestnetTmpDir >>) $ H.runFinallies $ H.workspace (workspaceName <> "-" <> show i) f

setTestnetTmpDir :: IO ()
setTestnetTmpDir = do
    cluster <- liftIO  clusterFilePath
    when (IO.os == "darwin") $ IO.setEnv "TMPDIR" cluster

testnetTest :: IORef [TestResult] -> H.Property
testnetTest resultsRef = integrationClusterWorkspace 0 "testnet" $ \tempAbsPath -> do
    let options = testnetOptionsConway9Governance
    cluster <- liftIO $ clusterFilePath
    (localNodeConnectInfo, networkId, mPoolNodes) <-
        setupTestEnvironment options cluster
    liftIO $ do
        putStrLn ("\n\nStarted testnet on " ++ cluster)
        putStrLn ("\n\nSocket path " ++ (cluster<>".cluster/socket/pool1/sock " ))
        putStrLn ("\nNetworkId: " ++ (case networkId of Api.Testnet (Api.NetworkMagic num) -> show num))
        finally  (threadDelay (24 * 60 * 60 * 1000000)) (handler mPoolNodes) -- lasts one day

    where
        handler ::Maybe [CTN.PoolNode]  -> IO ()
        handler mPoolNodes  = do
            cleanupTestnet mPoolNodes
            pure ()

getTestTree TestnetRef{..} =
    testGroup
        "TESTNET"
        [ testProperty "Start Testnet" (testnetTest testnetResultsRef)
        ]

conwayGenesisString=
  "{"
  ++ "\"poolVotingThresholds\": {"
  ++ "  \"committeeNormal\": 0.51,"
  ++ "  \"committeeNoConfidence\": 0.51,"
  ++ "  \"hardForkInitiation\": 0.51,"
  ++ "  \"motionNoConfidence\": 0.51,"
  ++ "  \"ppSecurityGroup\": 0.51"
  ++ "},"
  ++ "\"dRepVotingThresholds\": {"
  ++ "  \"motionNoConfidence\": 0.67,"
  ++ "  \"committeeNormal\": 0.67,"
  ++ "  \"committeeNoConfidence\": 0.6,"
  ++ "  \"updateToConstitution\": 0.75,"
  ++ "  \"hardForkInitiation\": 0.6,"
  ++ "  \"ppNetworkGroup\": 0.67,"
  ++ "  \"ppEconomicGroup\": 0.67,"
  ++ "  \"ppTechnicalGroup\": 0.67,"
  ++ "  \"ppGovGroup\": 0.75,"
  ++ "  \"treasuryWithdrawal\": 0.67"
  ++ "},"
  ++ "\"committeeMinSize\": 7,"
  ++ "\"committeeMaxTermLength\": 146,"
  ++ "\"govActionLifetime\": 6,"
  ++ "\"govActionDeposit\": 100000000000,"
  ++ "\"dRepDeposit\": 500000000,"
  ++ "\"dRepActivity\": 20,"
  ++ "\"minFeeRefScriptCostPerByte\": 15,"
  ++ "\"plutusV3CostModel\": [100788,420,1,1,1000,173,0,1,1000,59957,4,1,11183,32,201305,8356,4,16000,100,16000,100,16000,100,16000,100,16000,100,16000,100,100,100,16000,100,94375,32,132994,32,61462,4,72010,178,0,1,22151,32,91189,769,4,2,85848,123203,7305,-900,1716,549,57,85848,0,1,1,1000,42921,4,2,24548,29498,38,1,898148,27279,1,51775,558,1,39184,1000,60594,1,141895,32,83150,32,15299,32,76049,1,13169,4,22100,10,28999,74,1,28999,74,1,43285,552,1,44749,541,1,33852,32,68246,32,72362,32,7243,32,7391,32,11546,32,85848,123203,7305,-900,1716,549,57,85848,0,1,90434,519,0,1,74433,32,85848,123203,7305,-900,1716,549,57,85848,0,1,1,85848,123203,7305,-900,1716,549,57,85848,0,1,955506,213312,0,2,270652,22588,4,1457325,64566,4,20467,1,4,0,141992,32,100788,420,1,1,81663,32,59498,32,20142,32,24588,32,20744,32,25933,32,24623,32,43053543,10,53384111,14333,10,43574283,26308,10,16000,100,16000,100,962335,18,2780678,6,442008,1,52538055,3756,18,267929,18,76433006,8868,18,52948122,18,1995836,36,3227919,12,901022,1,166917843,4307,36,284546,36,158221314,26549,36,74698472,36,333849714,1,254006273,72,2174038,72,2261318,64571,4,207616,8310,4,1293828,28716,63,0,1,1006041,43623,251,0,1],"
  ++ "\"constitution\": {"
  ++ "  \"anchor\": {"
  ++ "    \"dataHash\": \"ca41a91f399259bcefe57f9858e91f6d00e1a38d6d9c63d4052914ea7bd70cb2\","
  ++ "    \"url\": \"ipfs://bafkreifnwj6zpu3ixa4siz2lndqybyc5wnnt3jkwyutci4e2tmbnj3xrdm\""
  ++ "  },"
  ++ "  \"script\": \"fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64\""
  ++ "},"
  ++ "\"committee\": {"
  ++ "  \"members\": {"
  ++ "    \"scriptHash-df0e83bde65416dade5b1f97e7f115cc1ff999550ad968850783fe50\": 580,"
  ++ "    \"scriptHash-b6012034ba0a7e4afbbf2c7a1432f8824aee5299a48e38e41a952686\": 580,"
  ++ "    \"scriptHash-ce8b37a72b178a37bbd3236daa7b2c158c9d3604e7aa667e6c6004b7\": 580,"
  ++ "    \"scriptHash-f0dc2c00d92a45521267be2d5de1c485f6f9d14466d7e16062897cf7\": 580,"
  ++ "    \"scriptHash-349e55f83e9af24813e6cb368df6a80d38951b2a334dfcdf26815558\": 580,"
  ++ "    \"scriptHash-84aebcfd3e00d0f87af918fc4b5e00135f407e379893df7e7d392c6a\": 580,"
  ++ "    \"scriptHash-e8165b3328027ee0d74b1f07298cb092fd99aa7697a1436f5997f625\": 580"
  ++ "  },"
  ++ "  \"threshold\": {"
  ++ "    \"numerator\": 2,"
  ++ "    \"denominator\": 3"
  ++ "  }"
  ++ "}"
  ++ "}"

alonzoGenesisString = jsonString :: String
jsonString = "{\"lovelacePerUTxOWord\":34482,"
              ++ "\"executionPrices\":{"
              ++ "\"prSteps\":{"
              ++ "\"numerator\":721,"
              ++ "\"denominator\":10000000"
              ++ "},"
              ++ "\"prMem\":{"
              ++ "\"numerator\":577,"
              ++ "\"denominator\":10000"
              ++ "},"
              ++ "},"
              ++ "\"maxTxExUnits\":{"
              ++ "\"exUnitsMem\":10000000,"
              ++ "\"exUnitsSteps\":10000000000"
              ++ "},"
              ++ "\"maxBlockExUnits\":{"
              ++ "\"exUnitsMem\":50000000,"
              ++ "\"exUnitsSteps\":40000000000"
              ++ "},"
              ++ "\"maxValueSize\":5000,"
              ++ "\"collateralPercentage\":150,"
              ++ "\"maxCollateralInputs\":3,"
              ++ "\"costModels\":{"
              ++ "\"PlutusV1\":{"
              ++ "\"sha2_256-memory-arguments\":4,"
              ++ "\"equalsString-cpu-arguments-constant\":1000,"
              ++ "\"cekDelayCost-exBudgetMemory\":100,"
              ++ "\"lessThanEqualsByteString-cpu-arguments-intercept\":103599,"
              ++ "\"divideInteger-memory-arguments-minimum\":1,"
              ++ "\"appendByteString-cpu-arguments-slope\":621,"
              ++ "\"blake2b-cpu-arguments-slope\":29175,"
              ++ "\"iData-cpu-arguments\":150000,"
              ++ "\"encodeUtf8-cpu-arguments-slope\":1000,"
              ++ "\"unBData-cpu-arguments\":150000,"
              ++ "\"multiplyInteger-cpu-arguments-intercept\":61516,"
              ++ "\"cekConstCost-exBudgetMemory\":100,"
              ++ "\"nullList-cpu-arguments\":150000,"
              ++ "\"equalsString-cpu-arguments-intercept\":150000,"
              ++ "\"trace-cpu-arguments\":150000,"
              ++ "\"mkNilData-memory-arguments\":32,"
              ++ "\"lengthOfByteString-cpu-arguments\":150000,"
              ++ "\"cekBuiltinCost-exBudgetCPU\":29773,"
              ++ "\"bData-cpu-arguments\":150000,"
              ++ "\"subtractInteger-cpu-arguments-slope\":0,"
              ++ "\"unIData-cpu-arguments\":150000,"
              ++ "\"consByteString-memory-arguments-intercept\":0,"
              ++ "\"divideInteger-memory-arguments-slope\":1,"
              ++ "\"divideInteger-cpu-arguments-model-arguments-slope\":118,"
              ++ "\"listData-cpu-arguments\":150000,"
              ++ "\"headList-cpu-arguments\":150000,"
              ++ "\"chooseData-memory-arguments\":32,"
              ++ "\"equalsInteger-cpu-arguments-intercept\":136542,"
              ++ "\"sha3_256-cpu-arguments-slope\":82363,"
              ++ "\"sliceByteString-cpu-arguments-slope\":5000,"
              ++ "\"unMapData-cpu-arguments\":150000,"
              ++ "\"lessThanInteger-cpu-arguments-intercept\":179690,"
              ++ "\"mkCons-cpu-arguments\":150000,"
              ++ "\"appendString-memory-arguments-intercept\":0,"
              ++ "\"modInteger-cpu-arguments-model-arguments-slope\":118,"
              ++ "\"ifThenElse-cpu-arguments\":1,"
              ++ "\"mkNilPairData-cpu-arguments\":150000,"
              ++ "\"lessThanEqualsInteger-cpu-arguments-intercept\":145276,"
              ++ "\"addInteger-memory-arguments-slope\":1,"
              ++ "\"chooseList-memory-arguments\":32,"
              ++ "\"constrData-memory-arguments\":32,"
              ++ "\"decodeUtf8-cpu-arguments-intercept\":150000,"
              ++ "\"equalsData-memory-arguments\":1,"
              ++ "\"subtractInteger-memory-arguments-slope\":1,"
              ++ "\"appendByteString-memory-arguments-intercept\":0,"
              ++ "\"lengthOfByteString-memory-arguments\":4,"
              ++ "\"headList-memory-arguments\":32,"
              ++ "\"listData-memory-arguments\":32,"
              ++ "\"consByteString-cpu-arguments-intercept\":150000,"
              ++ "\"unIData-memory-arguments\":32,"
              ++ "\"remainderInteger-memory-arguments-minimum\":1,"
              ++ "\"bData-memory-arguments\":32,"
              ++ "\"lessThanByteString-cpu-arguments-slope\":248,"
              ++ "\"encodeUtf8-memory-arguments-intercept\":0,"
              ++ "\"cekStartupCost-exBudgetCPU\":100,"
              ++ "\"multiplyInteger-memory-arguments-intercept\":0,"
              ++ "\"unListData-memory-arguments\":32,"
              ++ "\"remainderInteger-cpu-arguments-model-arguments-slope\":118,"
              ++ "\"cekVarCost-exBudgetCPU\":29773,"
              ++ "\"remainderInteger-memory-arguments-slope\":1,"
              ++ "\"cekForceCost-exBudgetCPU\":29773,"
              ++ "\"sha2_256-cpu-arguments-slope\":29175,"
              ++ "\"equalsInteger-memory-arguments\":1,"
              ++ "\"indexByteString-memory-arguments\":1,"
              ++ "\"addInteger-memory-arguments-intercept\":1,"
              ++ "\"chooseUnit-cpu-arguments\":150000,"
              ++ "\"sndPair-cpu-arguments\":150000,"
              ++ "\"cekLamCost-exBudgetCPU\":29773,"
              ++ "\"fstPair-cpu-arguments\":150000,"
              ++ "\"quotientInteger-memory-arguments-minimum\":1,"
              ++ "\"decodeUtf8-cpu-arguments-slope\":1000,"
              ++ "\"lessThanInteger-memory-arguments\":1,"
              ++ "\"lessThanEqualsInteger-cpu-arguments-slope\":1366,"
              ++ "\"fstPair-memory-arguments\":32,"
              ++ "\"modInteger-cpu-arguments-intercept\":0,"
              ++ "\"unConstrData-cpu-arguments\":150000,"
              ++ "\"lessThanEqualsInteger-memory-arguments\":1,"
              ++ "\"chooseUnit-memory-arguments\":32,"
              ++ "\"sndPair-memory-arguments\":32,"
              ++ "\"addInteger-cpu-arguments-intercept\":197209,"
              ++ "\"decodeUtf8-memory-arguments-slope\":8,"
              ++ "\"equalsData-cpu-arguments-intercept\":150000,"
              ++ "\"mapData-cpu-arguments\":150000,"
              ++ "\"mkPairData-cpu-arguments\":150000,"
              ++ "\"quotientInteger-cpu-arguments-constant\":148000,"
              ++ "\"consByteString-memory-arguments-slope\":1,"
              ++ "\"cekVarCost-exBudgetMemory\":100,"
              ++ "\"indexByteString-cpu-arguments\":150000,"
              ++ "\"unListData-cpu-arguments\":150000,"
              ++ "\"equalsInteger-cpu-arguments-slope\":1326,"
              ++ "\"cekStartupCost-exBudgetMemory\":100,"
              ++ "\"subtractInteger-cpu-arguments-intercept\":197209,"
              ++ "\"divideInteger-cpu-arguments-model-arguments-intercept\":425507,"
              ++ "\"divideInteger-memory-arguments-intercept\":0,"
              ++ "\"cekForceCost-exBudgetMemory\":100,"
              ++ "\"blake2b-cpu-arguments-intercept\":2477736,"
              ++ "\"remainderInteger-cpu-arguments-constant\":148000,"
              ++ "\"tailList-cpu-arguments\":150000,"
              ++ "\"encodeUtf8-cpu-arguments-intercept\":150000,"
              ++ "\"equalsString-cpu-arguments-slope\":1000,"
              ++ "\"lessThanByteString-memory-arguments\":1,"
              ++ "\"multiplyInteger-cpu-arguments-slope\":11218,"
              ++ "\"appendByteString-cpu-arguments-intercept\":396231,"
              ++ "\"lessThanEqualsByteString-cpu-arguments-slope\":248,"
              ++ "\"modInteger-memory-arguments-slope\":1,"
              ++ "\"addInteger-cpu-arguments-slope\":0,"
              ++ "\"equalsData-cpu-arguments-slope\":10000,"
              ++ "\"decodeUtf8-memory-arguments-intercept\":0,"
              ++ "\"chooseList-cpu-arguments\":150000,"
              ++ "\"constrData-cpu-arguments\":150000,"
              ++ "\"equalsByteString-memory-arguments\":1,"
              ++ "\"cekApplyCost-exBudgetCPU\":29773,"
              ++ "\"quotientInteger-memory-arguments-slope\":1,"
              ++ "\"verifySignature-cpu-arguments-intercept\":3345831,"
              ++ "\"unMapData-memory-arguments\":32,"
              ++ "\"mkCons-memory-arguments\":32,"
              ++ "\"sliceByteString-memory-arguments-slope\":1,"
              ++ "\"sha3_256-memory-arguments\":4,"
              ++ "\"ifThenElse-memory-arguments\":1,"
              ++ "\"mkNilPairData-memory-arguments\":32,"
              ++ "\"equalsByteString-cpu-arguments-slope\":247,"
              ++ "\"appendString-cpu-arguments-intercept\":150000,"
              ++ "\"quotientInteger-cpu-arguments-model-arguments-slope\":118,"
              ++ "\"cekApplyCost-exBudgetMemory\":100,"
              ++ "\"equalsString-memory-arguments\":1,"
              ++ "\"multiplyInteger-memory-arguments-slope\":1,"
              ++ "\"cekBuiltinCost-exBudgetMemory\":100,"
              ++ "\"remainderInteger-memory-arguments-intercept\":0,"
              ++ "\"sha2_256-cpu-arguments-intercept\":2477736,"
              ++ "\"remainderInteger-cpu-arguments-model-arguments-intercept\":425507,"
              ++ "\"lessThanEqualsByteString-memory-arguments\":1,"
              ++ "\"tailList-memory-arguments\":32,"
              ++ "\"mkNilData-cpu-arguments\":150000,"
              ++ "\"chooseData-cpu-arguments\":150000,"
              ++ "\"unBData-memory-arguments\":32,"
              ++ "\"blake2b-memory-arguments\":4,"
              ++ "\"iData-memory-arguments\":32,"
              ++ "\"nullList-memory-arguments\":32,"
              ++ "\"cekDelayCost-exBudgetCPU\":29773,"
              ++ "\"subtractInteger-memory-arguments-intercept\":1,"
              ++ "\"lessThanByteString-cpu-arguments-intercept\":103599,"
              ++ "\"consByteString-cpu-arguments-slope\":1000,"
              ++ "\"appendByteString-memory-arguments-slope\":1,"
              ++ "\"trace-memory-arguments\":32,"
              ++ "\"divideInteger-cpu-arguments-constant\":148000,"
              ++ "\"cekConstCost-exBudgetCPU\":29773,"
              ++ "\"encodeUtf8-memory-arguments-slope\":8,"
              ++ "\"quotientInteger-cpu-arguments-model-arguments-intercept\":425507,"
              ++ "\"mapData-memory-arguments\":32,"
              ++ "\"appendString-cpu-arguments-slope\":1000,"
              ++ "\"modInteger-cpu-arguments-constant\":148000,"
              ++ "\"verifySignature-cpu-arguments-slope\":1,"
              ++ "\"unConstrData-memory-arguments\":32,"
              ++ "\"quotientInteger-memory-arguments-intercept\":0,"
              ++ "\"equalsByteString-cpu-arguments-constant\":150000,"
              ++ "\"sliceByteString-memory-arguments-intercept\":0,"
              ++ "\"mkPairData-memory-arguments\":32,"
              ++ "\"equalsByteString-cpu-arguments-intercept\":112536,"
              ++ "\"appendString-memory-arguments-slope\":1,"
              ++ "\"lessThanInteger-cpu-arguments-slope\":497,"
              ++ "\"modInteger-cpu-arguments-model-arguments-intercept\":425507,"
              ++ "\"modInteger-memory-arguments-minimum\":1,"
              ++ "\"sha3_256-cpu-arguments-intercept\":0,"
              ++ "\"verifySignature-memory-arguments\":1,"
              ++ "\"cekLamCost-exBudgetMemory\":100,"
              ++ "\"sliceByteString-cpu-arguments-intercept\":150000"
              ++ "}"
              ++ "}"



runTestnet :: IO ()
runTestnet = do
    testnetRef@[testnet] <- traverse newIORef $ replicate 1 []
    defaultMain $ getTestTree $ TestnetRef testnet


-- | Start a testnet with provided testnet options (including era and protocol version)
startTestnet ::
    TestEnvironmentOptions era ->
    FilePath ->
    H.Integration
        ( C.LocalNodeConnectInfo
        , C.NetworkId
        , Maybe [CTN.PoolNode]
        )
startTestnet LocalNodeOptions{} _ = error "LocalNodeOptions not supported"
startTestnet TestnetOptions{..} tempAbsBasePath = do
    conf :: CTN.Conf <-
        HE.noteShowM $
            CTN.mkConf tempAbsBasePath
    currentTime <- liftIO Time.getCurrentTime
    let sg = (CTN.defaultShelleyGenesis currentTime testnetCardanoOptions)
                {
                    sgProtocolParams = def  & ppMinFeeAL .~ 44 
                                        & ppMinFeeBL .~ 155381 
                }
        agDef = forceRight $ CTN.defaultAlonzoGenesis (C.ConwayEra)
        updatedAg=agDef {
            agCoinsPerUTxOWord = CoinPerWord (Coin 4310)

        }

        -- cg = defaultConwayGenesis
    decodedGenesis<-case A.decode (BS8L.pack conwayGenesisString) of
        Nothing -> fail "Unexpected"
        Just v -> pure v
    -- decodedAlonzoGenesis<-case A.decode (BS8L.pack conwayGenesisString) of
    --     Nothing -> fail "Unexpected"
    --     Just v -> pure v
    tn <- CTN.cardanoTestnet testnetCardanoOptions conf currentTime sg agDef decodedGenesis
    -- needed to avoid duplication of directory in filepath
    let tmpAbsBasePath' = CTN.makeTmpBaseAbsPath $ CTN.tempAbsPath conf

    -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
    -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
    -- to the Byron era.
    socketPathAbs <- getPoolSocketPathAbs tmpAbsBasePath' tn
    let epochSlots = C.EpochSlots 21_600
        localNodeConnectInfo =
            C.LocalNodeConnectInfo
                { C.localConsensusModeParams = C.CardanoModeParams epochSlots
                , C.localNodeNetworkId = getNetworkId tn
                , C.localNodeSocketPath = socketPathAbs
                }
        networkId = getNetworkId tn
    pure (localNodeConnectInfo, networkId, Just $ CTN.poolNodes tn)

forceRight (Right r) = r

cleanupTestnet ::  Maybe [CTN.PoolNode] -> IO [Either String ()]
cleanupTestnet mPoolNodes =
    case mPoolNodes of
        Just poolNodes -> do
            forM_ poolNodes $ \(CTN.PoolNode poolRuntime _) -> do
                -- graceful SIGTERM all nodes
                    cleanupProcess
                        (Just (CTN.nodeStdinHandle poolRuntime), Nothing, Nothing, CTN.nodeProcessHandle poolRuntime)
            forM poolNodes $ \node ->
                -- kill signal for any node unix handles still open
                killUnixHandle $ CTN.nodeProcessHandle $ CTN.poolRuntime node
        _ ->
            return []
  where
    killUnixHandle ph = withProcessHandle ph $ \case
        OpenHandle pid -> do
            signalProcess sigKILL pid -- send kill signal if handle still open
            eTimeOut <- waitSecondsForProcess 60 ph -- wait 60s for process to exit
            case eTimeOut of
                Left _ -> return $ Left $ "ProcessExitTimedOut 60 "++ show pid
                Right _ -> return $ Right ()
        OpenExtHandle _ _ -> return $ Right () -- do nothing on Windows
        ClosedHandle _ -> return $ Right () -- do nothing if already closed

connectToLocalNode ::
    TestEnvironmentOptions era ->
    FilePath ->
    H.Integration
        ( C.LocalNodeConnectInfo
        , C.NetworkId
        , Maybe [CTN.PoolNode]
        )
connectToLocalNode TestnetOptions{} _ = error "TestnetOptions not supported"
connectToLocalNode LocalNodeOptions{..} tempAbsPath = do
    -- HE.createDirectoryIfMissing (tempAbsPath </> "utxo-keys")
    -- HE.createDirectoryIfMissing (localNodeEnvDir </> "sockets")
    -- HE.createFileLink (localNodeEnvDir </> "test2.skey") (tempAbsPath </> "utxo-keys/utxo2.skey")
    -- HE.createFileLink (localNodeEnvDir </> "test2.vkey") (tempAbsPath </> "utxo-keys/utxo2.vkey")
    -- HE.createFileLink (localNodeEnvDir </> "test1.skey") (tempAbsPath </> "utxo-keys/utxo1.skey")
    -- HE.createFileLink (localNodeEnvDir </> "test1.vkey") (tempAbsPath </> "utxo-keys/utxo1.vkey")
    -- HE.createFileLink (localNodeEnvDir </> "socket/pool1/sock") (localNodeEnvDir </> "sockets/node.socket")
    let socketPathAbs = C.File $ localNodeEnvDir </> "socket/pool1/sock"
        networkId = C.Testnet $ C.NetworkMagic $ fromIntegral localNodeTestnetMagic
    -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
    -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
    -- to the Byron era.
    let epochSlots = C.EpochSlots 21600
        localNodeConnectInfo =
            C.LocalNodeConnectInfo
                { C.localConsensusModeParams = C.CardanoModeParams epochSlots
                , C.localNodeNetworkId = networkId
                , C.localNodeSocketPath = socketPathAbs
                }
    pure (localNodeConnectInfo, networkId, Nothing)

{- | Start testnet with cardano-testnet or use local node that's already
  connected to a public testnet
-}
setupTestEnvironment ::
    TestEnvironmentOptions era ->
    FilePath ->
    H.Integration
        ( C.LocalNodeConnectInfo
        , C.NetworkId
        , Maybe [CTN.PoolNode]
        )
setupTestEnvironment testnetOptions@TestnetOptions{..} tempAbsPath = do
    liftIO $
        putStrLn $
            "\nStarting local testnet in "
                ++ show testnetEra
                ++ " PV"
                ++ show testnetProtocolVersion
                ++ "..."
    startTestnet testnetOptions tempAbsPath
setupTestEnvironment localNodeOptions@LocalNodeOptions{} tempAbsPath = do
    liftIO $ putStrLn "\nConnecting to local node..."
    connectToLocalNode localNodeOptions tempAbsPath

-- | Network ID of the testnet
getNetworkId :: CTN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (CTN.testnetMagic tn)

-- | Path to a pool node's unix socket
getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => FilePath -> CTN.TestnetRuntime -> m C.SocketPath
getPoolSocketPathAbs tempAbsPath tn = do
    socketPath <- IO.sprocketArgumentName <$> H.headM (CTN.poolSprockets tn)
    fp <- liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath
    H.annotate fp
    return $ C.File fp


class IsRatio r where
    (%!) :: (HasCallStack) => Integer -> Integer -> r
    default (%!) :: (HasCallStack, Typeable r, BoundedRational r) => Integer -> Integer -> r
    n %! d = Api.unsafeBoundedRational $ n Data.Ratio.% d

instance IsRatio UnitInterval

instance IsRatio Rational where
    (%!) = (Data.Ratio.%)

instance IsRatio NonNegativeInterval

instance IsRatio PositiveInterval

instance IsRatio PositiveUnitInterval