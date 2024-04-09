{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.ApiTest where

import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Ledger.Alonzo.Scripts hiding ()
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxWits
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import Test.ChainApiTests (test_kGetNetworkId, test_kQueryChainPoint, test_kQueryCurrentEra, test_kQueryGenesisParams, test_kQueryProtocolParams, test_kQuerySystemStart, test_kQueryUtxoByAddress, test_kQueryUtxoByTxin)
import Test.KuberApiTests
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as RP

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

testBuildTxSupportDatumInAuxData :: TestTree
testBuildTxSupportDatumInAuxData =
  testGroup
    "should support datum in auxiliary data"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_kBuildTx_supportDatumInAuxData
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_kBuildTx_supportDatumInAuxData
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testExUnits :: TestTree
testExUnits =
  testGroup
    "should pass"
    [ testCase "Remote" $ do
        maybeFe <- evaluateFromRemoteKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> do
            let mem_steps = memsAndSteps tx
                (memSum, stepsSum) = sumTuples mem_steps
            Debug.traceM("memSum: " ++ show memSum ++ " stepsum: " ++ show stepsSum)
            let checkExceed
                  | memSum > 14000000 =
                    error $
                      "\n" ++ "Mem has exceeded by " ++ show (memSum - 14000000)
                  | stepsSum > 10000000000 =
                    error $
                      "\n" ++ "step has exceeded by  " ++ show (stepsSum - 10000000000)
                  | otherwise = mem_steps
            case checkExceed of
              [(-1,-1)] -> error "failed"
              memsAndSteps -> pure ()
    ]

sumTuples :: [(Integer, Integer)] -> (Integer, Integer)
sumTuples = foldr (\x (mem, steps) -> case x of (n, i) -> (mem + n, steps + i)) (0, 0)

memsAndSteps txBabbage = case txBabbage of
  ShelleyTx sbe tx -> case tx of
    AlonzoTx _ wit _ _ -> case wit of
      AlonzoTxWits a b c d e ->
        Prelude.map
          ( ( \(WrapExUnits eu) -> case extractValues (show eu) of
                Nothing -> error "could not parse string to ex-units"
                Just x1 -> x1
            )
              . (snd . snd)
          )
          (Map.toList (unRedeemers e))

extractValues :: String -> Maybe (Integer, Integer)
extractValues str =
  case readP_to_S parseExUnits' str of
    [(exUnits, _)] -> Just (exUnitsMem'' exUnits, exUnitsSteps'' exUnits)
    _ -> Nothing

data ExUnits'' = ExUnits''
  { exUnitsMem'' :: Integer,
    exUnitsSteps'' :: Integer
  }
  deriving (Show)

parseExUnits' :: ReadP ExUnits''
parseExUnits' = do
  _ <- RP.string "ExUnits' {exUnitsMem' = "
  mem <- read <$> munch1 (`elem` ['0' .. '9'])
  _ <- RP.string ", exUnitsSteps' = "
  steps <- read <$> munch1 (`elem` ['0' .. '9'])
  _ <- RP.string "}"
  return $ ExUnits'' mem steps