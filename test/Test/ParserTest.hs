{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.ParserTest where

import Cardano.Api
-- import Plutus.V2.Ledger.Api (fromBuiltin, PubKeyHash (PubKeyHash))

import Cardano.Kuber.Api
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable (toConsoleText, toConsoleTextNoPrefix))
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util hiding (toHexString)
import Cardano.Ledger.Shelley.API (ScriptHash (ScriptHash))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Debug.Trace as Debug
import Test.ChainApiTests (test_kGetNetworkId, test_kQueryChainPoint, test_kQueryCurrentEra, test_kQueryGenesisParams, test_kQueryProtocolParams, test_kQuerySystemStart, test_kQueryUtxoByAddress, test_kQueryUtxoByTxin)
import Test.KuberApiTests
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do
  (networkName, network) <- getNetworkFromEnv "NETWORK"
  createRemoteKuberConnection network "http://172.31.6.14:8081/" Nothing

evaluateFromRemoteKuber test = do
  cInfo <- remoteKuberConnection
  evaluateKontract cInfo $ do test

evaluateFromLocalKuber test = do
  cInfo <- chainInfoFromEnv
  evaluateKontract cInfo $ do test

testGetNetworkId :: TestTree
testGetNetworkId =
  testGroup
    "should get network ID"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kGetNetworkId
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kGetNetworkId
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure ()
    ]

testQueryProtocolParams :: TestTree
testQueryProtocolParams =
  testGroup
    "should get protocol params"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryProtocolParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryProtocolParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure ()
    ]

testQuerySystemStart :: TestTree
testQuerySystemStart =
  testGroup
    "should get System Start details"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQuerySystemStart
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ss -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQuerySystemStart
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ss -> pure ()
    ]

testQueryGenesisParams :: TestTree
testQueryGenesisParams =
  testGroup
    "should get Genesis Params"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryGenesisParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right gp -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryGenesisParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right gp -> pure ()
    ]

testQueryUtxoByAddress :: TestTree
testQueryUtxoByAddress =
  testGroup
    "should query UTxO by Address"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryUtxoByAddress
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryUtxoByAddress
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure ()
    ]

testQueryUtxoByTxin :: TestTree
testQueryUtxoByTxin =
  testGroup
    "should query UTxO by TxIn"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryUtxoByTxin
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryUtxoByTxin
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure ()
    ]

testQueryChainPoint :: TestTree
testQueryChainPoint =
  testGroup
    "should get chain point"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryChainPoint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right cp -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryChainPoint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right cp -> pure ()
    ]

testQueryCurrentEra :: TestTree
testQueryCurrentEra =
  testGroup
    "should get current era"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kQueryCurrentEra
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ace -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kQueryCurrentEra
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ace -> pure ()
    ]

testBuildTxSimplePay :: TestTree
testBuildTxSimplePay =
  testGroup
    "should pay to address"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_simplePay
        case maybeFe of
          Left fe -> assertFailure $ "Test Case failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_simplePay
        case maybeFe of
          Left fe -> assertFailure $ "Test Case failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSimpleMint :: TestTree
testBuildTxSimpleMint =
  testGroup
    "should simply mint"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_simpleMint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_simpleMint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSimpleRedeem :: TestTree
testBuildTxSimpleRedeem =
  testGroup
    "should redeem with reference input"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_redeemWithReferenceInput
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_redeemWithReferenceInput
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxRedeemFromSmartContract :: TestTree
testBuildTxRedeemFromSmartContract =
  testGroup
    "should redeem from smart contract"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_redeemFromSmartContract
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_redeemFromSmartContract
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSupportMetadata :: TestTree
testBuildTxSupportMetadata =
  testGroup
    "should support metadata"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_supportMetadata
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_supportMetadata
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]


