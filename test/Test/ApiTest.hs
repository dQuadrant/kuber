{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ApiTest where

import Cardano.Api (AddressAny, AsType (AsPlutusScriptV2), CardanoEra (ConwayEra), ConwayEra, CtxUTxO, InAnyCardanoEra (InAnyCardanoEra), PaymentKey, PlutusScript, PlutusScriptV2, SigningKey, StakeAddressReference (NoStakeAddress), TxIn, TxOut, UTxO (UTxO), lovelaceToValue, unsafeHashableScriptData)
import Cardano.Api.Plutus (ExecutionUnits(..))
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers (parsePlutusScriptCborHex)
import Cardano.Kuber.Util (addressInEraToAddressAny, dataToScriptData, readSignKey, skeyToAddrInEra)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import qualified Data.ByteString.Lazy.Char8 as BS8
import Control.Concurrent (threadDelay)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.ChainApiTests (test_kGetNetworkId, test_kQueryChainPoint, test_kQueryCurrentEra, test_kQueryGenesisParams, test_kQueryProtocolParams, test_kQuerySystemStart, test_kQueryUtxoByAddress, test_kQueryUtxoByTxin)
import Test.KuberApiTests
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as RP

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do
  (_networkName, network) <- getNetworkFromEnv "NETWORK"
  remoteUrl <- maybe "http://127.0.0.1:8081/" id <$> lookupEnv "KUBER_REMOTE_URL"
  createRemoteKuberConnection network remoteUrl Nothing

evaluateFromRemoteKuber test = do
  cInfo <- remoteKuberConnection
  evaluateKontract cInfo $ do test

evaluateFromLocalKuber test = do
  cInfo <- chainInfoFromEnv
  evaluateKontract cInfo $ do test

remoteClientCase :: IO () -> IO ()
remoteClientCase action = do
  enabled <- maybe True (`notElem` ["0", "false", "no", "off"]) <$> lookupEnv "KUBER_ENABLE_REMOTE_CLIENT"
  if enabled
    then action
    else Debug.traceM "Skipping remote Haskell client test because KUBER_ENABLE_REMOTE_CLIENT is disabled."

testGetNetworkId :: TestTree
testGetNetworkId =
  testGroup
    "should get network ID"
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    "should build redeem from smart contract"
    [ testCase "Remote" $ remoteClientCase $ do
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

testRedeemFromSmartContractE2E :: TestTree
testRedeemFromSmartContractE2E =
  testGroup
    "should redeem from smart contract end to end"
    [ testCase "Remote" $ remoteClientCase $ do
        maybeSignKey <- getDefaultWalletSignKey
        case maybeSignKey of
          Nothing -> assertFailure "Remote smart contract E2E failed: ~/.cardano/keys/payment.sk is missing"
          Just signKey -> do
            maybeResult <- evaluateFromRemoteKuber (smartContractRoundTrip signKey)
            case maybeResult of
              Left fe -> assertFailure $ "Remote smart contract E2E failed: " ++ show fe
              Right () -> pure (),
      testCase "Local" $ do
        maybeSignKey <- getDefaultWalletSignKey
        case maybeSignKey of
          Nothing -> assertFailure "Local smart contract E2E failed: ~/.cardano/keys/payment.sk is missing"
          Just signKey -> do
            maybeResult <- evaluateFromLocalKuber (smartContractRoundTrip signKey)
            case maybeResult of
              Left fe -> assertFailure $ "Local smart contract E2E failed: " ++ show fe
              Right () -> pure ()
    ]

testBuildTxSupportMetadata :: TestTree
testBuildTxSupportMetadata =
  testGroup
    "should support metadata"
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
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
    [ testCase "Remote" $ remoteClientCase $ do
        maybeFe <- evaluateFromRemoteKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> do
            maybeExUnits <- evaluateFromRemoteKuber (kEvaluateExUnits tx)
            case maybeExUnits of
              Left fe -> assertFailure $ "Remote ExUnits evaluation failed: " ++ show fe
              Right exUnitsMap -> assertExUnitMap exUnitsMap,
      testCase "Local" $ do
        maybeFe <- evaluateFromLocalKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> do
            maybeExUnits <- evaluateFromLocalKuber (kEvaluateExUnits tx)
            case maybeExUnits of
              Left fe -> assertFailure $ "ExUnits evaluation failed: " ++ show fe
              Right exUnitsMap -> assertExUnitMap exUnitsMap
    ]

testCalculateFee :: TestTree
testCalculateFee =
  testGroup
    "should calculate fee"
    [ testCase "Remote" $ remoteClientCase $ do
        maybeTx <- evaluateFromRemoteKuber test_kBuildTx_simplePay
        case maybeTx of
          Left fe -> assertFailure $ "BuildTx failed: " ++ show fe
          Right tx -> do
            maybeFee <- evaluateFromRemoteKuber (kCalculateMinFee tx)
            case maybeFee of
              Left fe -> assertFailure $ "Remote fee calculation failed: " ++ show fe
              Right fee ->
                if fee > 0
                  then pure ()
                  else assertFailure "Remote fee calculation returned non-positive fee",
      testCase "Local" $ do
        maybeTx <- evaluateFromLocalKuber test_kBuildTx_simplePay
        case maybeTx of
          Left fe -> assertFailure $ "BuildTx failed: " ++ show fe
          Right tx -> do
            maybeFee <- evaluateFromLocalKuber (kCalculateMinFee tx)
            case maybeFee of
              Left fe -> assertFailure $ "Local fee calculation failed: " ++ show fe
              Right fee ->
                if fee > 0
                  then pure ()
                  else assertFailure "Local fee calculation returned non-positive fee"
    ]

testSubmitTx :: TestTree
testSubmitTx =
  testGroup
    "should submit tx"
    [ testCase "Remote" $ remoteClientCase $ do
        maybeSignKey <- getDefaultWalletSignKey
        case maybeSignKey of
          Nothing -> assertFailure "Remote submit failed: ~/.cardano/keys/payment.sk is missing"
          Just signKey -> do
            maybeResult <- evaluateFromRemoteKuber (walletSubmitTx signKey)
            case maybeResult of
              Left fe -> assertFailure $ "Remote submit failed: " ++ show fe
              Right submitted ->
                if submitted
                  then pure ()
                  else assertFailure "Remote submit failed: wallet has no spendable UTxO",
      testCase "Local" $ do
        maybeSignKey <- getDefaultWalletSignKey
        case maybeSignKey of
          Nothing -> assertFailure "Local submit failed: ~/.cardano/keys/payment.sk is missing"
          Just signKey -> do
            maybeResult <- evaluateFromLocalKuber (walletSubmitTx signKey)
            case maybeResult of
              Left fe -> assertFailure $ "Local submit failed: " ++ show fe
              Right submitted ->
                if submitted
                  then pure ()
                  else assertFailure "Local submit failed: wallet has no spendable UTxO"
    ]

walletSubmitTx :: forall api w. (HasKuberAPI api, HasChainQueryAPI api, HasCardanoQueryApi api, HasSubmitApi api) => SigningKey PaymentKey -> Kontract api w FrameworkError Bool
walletSubmitTx signKey = do
  network <- kGetNetworkId
  let walletAddress = skeyToAddrInEra @ConwayEra signKey network
      walletAddressAny = addressInEraToAddressAny walletAddress
  UTxO utxos <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  if Map.null utxos
    then pure False
    else do
      tx <-
        kBuildTx $
          txWalletSignKey signKey
            <> txPayTo walletAddress (lovelaceToValue 2_000_000)
      kSubmitTx (InAnyCardanoEra ConwayEra tx)
      waitForWalletConfirmation walletAddressAny (UTxO utxos)

scriptTestDatum = unsafeHashableScriptData $ dataToScriptData ()

scriptTestRedeemer = unsafeHashableScriptData $ dataToScriptData ()

smartContractRoundTrip :: forall api w. (HasKuberAPI api, HasChainQueryAPI api, HasCardanoQueryApi api, HasSubmitApi api) => SigningKey PaymentKey -> Kontract api w FrameworkError ()
smartContractRoundTrip signKey = do
  network <- kGetNetworkId
  script <- liftIO loadFixtureSmartContractScript
  let walletAddress = skeyToAddrInEra @ConwayEra signKey network
      walletAddressAny = addressInEraToAddressAny walletAddress
      scriptAddress = txScriptAddress (TxScriptPlutus $ toTxPlutusScript script) network NoStakeAddress
      scriptAddressAny = addressInEraToAddressAny scriptAddress
      lockedValue = lovelaceToValue 3_000_000

  UTxO walletUtxos <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  if Map.null walletUtxos
    then kError ConnectionError "Smart contract E2E failed: wallet has no spendable UTxO"
    else pure ()

  initialScriptUtxo <- (kQueryUtxoByAddress $ Set.singleton scriptAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  lockTx <-
    kBuildTx $
      txWalletSignKey signKey
        <> txPayToScriptWithData scriptAddress lockedValue scriptTestDatum
  kSubmitTx (InAnyCardanoEra ConwayEra lockTx)
  (scriptTxIn, scriptTxOut) <- waitForNewUtxoAtAddress scriptAddressAny initialScriptUtxo

  redeemTx <-
    kBuildTx $
      txWalletSignKey signKey
        <> txRedeemUtxo scriptTxIn scriptTxOut script scriptTestRedeemer Nothing
        <> txPayTo walletAddress lockedValue
  kSubmitTx (InAnyCardanoEra ConwayEra redeemTx)
  redeemed <- waitForUtxoSpent scriptAddressAny scriptTxIn
  if redeemed
    then pure ()
    else kError ConnectionError "Smart contract E2E failed: redeemed script UTxO still present after timeout"

waitForWalletConfirmation :: forall api w. HasChainQueryAPI api => AddressAny -> UTxO ConwayEra -> Kontract api w FrameworkError Bool
waitForWalletConfirmation walletAddressAny initialUtxo = poll 60
  where
    initialEntries = utxoEntries initialUtxo

    poll :: Int -> Kontract api w FrameworkError Bool
    poll remaining
      | remaining <= 0 = pure False
      | otherwise = do
          liftIO $ threadDelay 1_000_000
          currentUtxo <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
          if utxoEntries currentUtxo /= initialEntries
            then do
              liftIO $ putStrLn "Submit test: transaction confirmed on-chain."
              pure True
            else poll (remaining - 1)

    utxoEntries (UTxO entries) = entries

loadFixtureSmartContractScript :: IO (PlutusScript PlutusScriptV2)
loadFixtureSmartContractScript = do
  cwd <- getCurrentDirectory
  let filePath = cwd </> "test" </> "Test" </> "TransactionJSON" </> "redeemFromSmartContract.json"
  fixture <- BS8.readFile filePath
  value <- case A.decode fixture of
    Just v -> pure v
    Nothing -> error "Failed to decode redeemFromSmartContract.json"
  cborHex <- case A.parseEither fixtureScriptCborHex value of
    Right hexText -> pure hexText
    Left err -> error $ "Failed to extract smart contract script from fixture: " ++ err
  case (parsePlutusScriptCborHex AsPlutusScriptV2 cborHex :: Maybe (PlutusScript PlutusScriptV2)) of
    Just script -> pure script
    Nothing -> error "Failed to parse smart contract script CBOR from fixture"

fixtureScriptCborHex :: A.Value -> A.Parser T.Text
fixtureScriptCborHex = A.withObject "tx fixture" $ \o -> do
  inputs :: [A.Value] <- o A..: "inputs"
  case inputs of
    firstInput : _ -> A.withObject "tx input" (\inputObj -> do
      scriptValue <- inputObj A..: "script"
      A.withObject "script" (A..: "cborHex") scriptValue) firstInput
    [] -> fail "Fixture has no inputs"

waitForNewUtxoAtAddress :: forall api w. HasChainQueryAPI api => AddressAny -> UTxO ConwayEra -> Kontract api w FrameworkError (TxIn, TxOut CtxUTxO ConwayEra)
waitForNewUtxoAtAddress addressAny initialUtxo = poll 60
  where
    initialEntries = utxoEntries initialUtxo

    poll :: Int -> Kontract api w FrameworkError (TxIn, TxOut CtxUTxO ConwayEra)
    poll remaining
      | remaining <= 0 = kError ConnectionError "Timed out waiting for script UTxO to appear"
      | otherwise = do
          liftIO $ threadDelay 1_000_000
          currentUtxo <- (kQueryUtxoByAddress $ Set.singleton addressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
          let newEntries = Map.toList $ Map.difference (utxoEntries currentUtxo) initialEntries
          case newEntries of
            entry : _ -> pure entry
            [] -> poll (remaining - 1)

    utxoEntries (UTxO entries) = entries

waitForUtxoSpent :: forall api w. HasChainQueryAPI api => AddressAny -> TxIn -> Kontract api w FrameworkError Bool
waitForUtxoSpent addressAny targetTxIn = poll 60
  where
    poll :: Int -> Kontract api w FrameworkError Bool
    poll remaining
      | remaining <= 0 = pure False
      | otherwise = do
          liftIO $ threadDelay 1_000_000
          currentUtxo <- (kQueryUtxoByAddress $ Set.singleton addressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
          if Map.notMember targetTxIn (utxoEntries currentUtxo)
            then pure True
            else poll (remaining - 1)

    utxoEntries (UTxO entries) = entries

getDefaultWalletSignKey :: IO (Maybe (SigningKey PaymentKey))
getDefaultWalletSignKey = do
  home <- getHomeDirectory
  let signKeyPath = home </> ".cardano" </> "keys" </> "payment.sk"
  exists <- doesFileExist signKeyPath
  if exists
    then Just <$> readSignKey signKeyPath
    else pure Nothing

assertExUnitMap :: Map.Map k (Either a ExecutionUnits) -> IO ()
assertExUnitMap exUnitsMap = do
  let memSteps =
        Prelude.map
          ( \entry -> case entry of
              Right (ExecutionUnits mem steps) -> (toInteger mem, toInteger steps)
              Left _ -> (-1, -1)
          )
          (Map.elems exUnitsMap)
      (memSum, stepsSum) = sumTuples memSteps
  if memSum > 14000000
    then error $ "\nMem has exceeded by " ++ show (memSum - 14000000)
    else
      if stepsSum > 10000000000
        then error $ "\nstep has exceeded by  " ++ show (stepsSum - 10000000000)
        else case memSteps of
          [(-1, -1)] -> error "failed"
          _ -> pure ()

sumTuples :: [(Integer, Integer)] -> (Integer, Integer)
sumTuples = foldr (\x (mem, steps) -> case x of (n, i) -> (mem + n, steps + i)) (0, 0)

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
