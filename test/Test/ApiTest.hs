{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ApiTest where

import Cardano.Api (AddressAny, AddressInEra, AsType (AsAddressInEra, AsConwayEra, AsPlutusScriptV2), AssetId (AdaAssetId), CardanoEra (ConwayEra), ConwayEra, CtxUTxO, EraHistory, InAnyCardanoEra (InAnyCardanoEra), PaymentKey, PlutusScript, PlutusScriptV2, Quantity (Quantity), SigningKey, StakeAddressReference (NoStakeAddress), TxIn (TxIn), TxOut (TxOut), TxId, TxOutValue (TxOutValueByron, TxOutValueShelleyBased), UTxO (UTxO), deserialiseAddress, fromLedgerValue, getTxBody, getTxId, lovelaceToValue, selectAsset, serialiseToCBOR, unsafeHashableScriptData, valueToList)
import Cardano.Api.Ledger (Coin (Coin))
import Cardano.Api.Plutus (ExecutionUnits(..))
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (EraHistoryModal (..), Wrapper (unWrap))
import Cardano.Kuber.Data.Parsers (parsePlutusScriptCborHex)
import Cardano.Kuber.Util (addressInEraToAddressAny, dataToScriptData, readSignKey, skeyToAddrInEra)
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower)
import Data.List (isInfixOf, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Control.Concurrent (threadDelay)
import Cardano.Slotting.Time (SystemStart)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Maybe (mapMaybe)
import Test.ChainApiTests (test_kGetNetworkId, test_kQueryChainPoint, test_kQueryCurrentEra, test_kQueryEraHistory, test_kQueryGenesisParams, test_kQueryProtocolParams, test_kQuerySystemStart, test_kQueryUtxoByAddress, test_kQueryUtxoByTxin)
import Test.KuberApiTests
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as RP

data DefaultWallet = DefaultWallet
  { dwSignKey :: SigningKey PaymentKey
  , dwAddress :: AddressInEra ConwayEra
  }

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

evaluateReadOnlyFromRemoteKuber test =
  retryTransientFrameworkErrors $ evaluateFromRemoteKuber test

evaluateReadOnlyFromLocalKuber test =
  retryTransientFrameworkErrors $ evaluateFromLocalKuber test

remoteClientCase :: IO () -> IO ()
remoteClientCase action = do
  enabled <- maybe True (`notElem` ["0", "false", "no", "off"]) <$> lookupEnv "KUBER_ENABLE_REMOTE_CLIENT"
  if enabled
    then action
    else Debug.traceM "Skipping remote Haskell client test because KUBER_ENABLE_REMOTE_CLIENT is disabled."

testCooldownMicros :: Int
testCooldownMicros = 400_000

transientRetryCount :: Int
transientRetryCount = 3

transientRetryDelayMicros :: Int
transientRetryDelayMicros = 750_000

retryTransientFrameworkErrors :: IO (Either FrameworkError a) -> IO (Either FrameworkError a)
retryTransientFrameworkErrors action = go transientRetryCount
  where
    go remaining = do
      result <- action
      case result of
        Left fe | remaining > 1 && isTransientFrameworkError fe -> do
          threadDelay transientRetryDelayMicros
          go (remaining - 1)
        _ -> pure result

isTransientFrameworkError :: FrameworkError -> Bool
isTransientFrameworkError fe =
  let message = map toLower (show fe)
   in any (`isInfixOf` message)
        [ "resource exhausted",
          "resource temporarily unavailable",
          "bearerclosed",
          "closed when reading data",
          "network.socket.connect",
          "connection was closed",
          "connection refused",
          "does not exist",
          "socket:"
        ]

pacedTestCase :: String -> IO () -> TestTree
pacedTestCase label action =
  testCase label (action `finally` threadDelay testCooldownMicros)

remoteTestCase :: String -> IO () -> TestTree
remoteTestCase label action = pacedTestCase label (remoteClientCase action)

localTestCase :: String -> IO () -> TestTree
localTestCase = pacedTestCase

testGetNetworkId :: TestTree
testGetNetworkId =
  testGroup
    "should get network ID"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kGetNetworkId
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kGetNetworkId
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure ()
    ]

testQueryProtocolParams :: TestTree
testQueryProtocolParams =
  testGroup
    "should get protocol params"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryProtocolParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryProtocolParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ni -> pure ()
    ]

testQuerySystemStart :: TestTree
testQuerySystemStart =
  testGroup
    "should get System Start details"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQuerySystemStart
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ss -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQuerySystemStart
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ss -> pure ()
    ]

testQueryGenesisParams :: TestTree
testQueryGenesisParams =
  testGroup
    "should get Genesis Params"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryGenesisParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right gp -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryGenesisParams
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right gp -> pure ()
    ]

testQueryUtxoByAddress :: TestTree
testQueryUtxoByAddress =
  testGroup
    "should query UTxO by Address"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryUtxoByAddress
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryUtxoByAddress
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure ()
    ]

testQueryUtxoByTxin :: TestTree
testQueryUtxoByTxin =
  testGroup
    "should query UTxO by TxIn"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryUtxoByTxin
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryUtxoByTxin
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right uto -> pure ()
    ]

testQueryChainPoint :: TestTree
testQueryChainPoint =
  testGroup
    "should get chain point"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryChainPoint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right cp -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryChainPoint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right cp -> pure ()
    ]

testQueryCurrentEra :: TestTree
testQueryCurrentEra =
  testGroup
    "should get current era"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kQueryCurrentEra
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ace -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kQueryCurrentEra
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right ace -> pure ()
    ]

testQueryEraHistory :: TestTree
testQueryEraHistory =
  testGroup
    "should get era history"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber $ do
          systemStart <- kQuerySystemStart
          eraHistory <- test_kQueryEraHistory
          pure (systemStart, eraHistory)
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right (systemStart, eraHistory) -> assertEraHistoryRoundTrip systemStart eraHistory,
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber $ do
          systemStart <- kQuerySystemStart
          eraHistory <- test_kQueryEraHistory
          pure (systemStart, eraHistory)
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right (systemStart, eraHistory) -> assertEraHistoryRoundTrip systemStart eraHistory
    ]

assertEraHistoryRoundTrip :: SystemStart -> EraHistory -> IO ()
assertEraHistoryRoundTrip systemStart eraHistory =
  case (A.eitherDecode (A.encode $ EraHistoryModal (Just systemStart) eraHistory) :: Either String EraHistoryModal) of
    Left err -> assertFailure $ "EraHistory JSON round-trip failed: " ++ err
    Right modal ->
      if serialiseToCBOR (unWrap modal :: EraHistory) == serialiseToCBOR eraHistory
        then pure ()
        else assertFailure "EraHistory JSON round-trip changed the underlying CBOR payload."

testBuildTxSimplePay :: TestTree
testBuildTxSimplePay =
  testGroup
    "should pay to address"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_simplePay
        case maybeFe of
          Left fe -> assertFailure $ "Test Case failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_simplePay
        case maybeFe of
          Left fe -> assertFailure $ "Test Case failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSimpleMint :: TestTree
testBuildTxSimpleMint =
  testGroup
    "should simply mint"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_simpleMint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_simpleMint
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSimpleRedeem :: TestTree
testBuildTxSimpleRedeem =
  testGroup
    "should redeem with reference input"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_redeemWithReferenceInput
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_redeemWithReferenceInput
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxRedeemFromSmartContract :: TestTree
testBuildTxRedeemFromSmartContract =
  testGroup
    "should build redeem from smart contract"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_redeemFromSmartContract
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_redeemFromSmartContract
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testRedeemFromSmartContractE2E :: TestTree
testRedeemFromSmartContractE2E =
  testGroup
    "should redeem from smart contract end to end"
    [ remoteTestCase "Remote" $ do
        maybeWallet <- getDefaultWallet
        case maybeWallet of
          Left err -> assertFailure $ "Remote smart contract E2E failed: " ++ err
          Right Nothing -> assertFailure "Remote smart contract E2E failed: ~/.cardano/keys/payment.sk is missing"
          Right (Just wallet) -> do
            maybeResult <- evaluateFromRemoteKuber (smartContractRoundTrip wallet)
            case maybeResult of
              Left fe -> assertFailure $ "Remote smart contract E2E failed: " ++ show fe
              Right () -> pure (),
      localTestCase "Local" $ do
        maybeWallet <- getDefaultWallet
        case maybeWallet of
          Left err -> assertFailure $ "Local smart contract E2E failed: " ++ err
          Right Nothing -> assertFailure "Local smart contract E2E failed: ~/.cardano/keys/payment.sk is missing"
          Right (Just wallet) -> do
            maybeResult <- evaluateFromLocalKuber (smartContractRoundTrip wallet)
            case maybeResult of
              Left fe -> assertFailure $ "Local smart contract E2E failed: " ++ show fe
              Right () -> pure ()
    ]

testBuildTxSupportMetadata :: TestTree
testBuildTxSupportMetadata =
  testGroup
    "should support metadata"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_supportMetadata
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_supportMetadata
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testBuildTxSupportDatumInAuxData :: TestTree
testBuildTxSupportDatumInAuxData =
  testGroup
    "should support datum in auxiliary data"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_supportDatumInAuxData
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure (),
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_kBuildTx_supportDatumInAuxData
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> pure ()
    ]

testExUnits :: TestTree
testExUnits =
  testGroup
    "should pass"
    [ remoteTestCase "Remote" $ do
        maybeFe <- evaluateReadOnlyFromRemoteKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> do
            maybeExUnits <- evaluateReadOnlyFromRemoteKuber (kEvaluateExUnits tx)
            case maybeExUnits of
              Left fe -> assertFailure $ "Remote ExUnits evaluation failed: " ++ show fe
              Right exUnitsMap -> assertExUnitMap exUnitsMap,
      localTestCase "Local" $ do
        maybeFe <- evaluateReadOnlyFromLocalKuber test_ex_units
        case maybeFe of
          Left fe -> assertFailure $ "Test Case Failed: " ++ show fe
          Right tx -> do
            maybeExUnits <- evaluateReadOnlyFromLocalKuber (kEvaluateExUnits tx)
            case maybeExUnits of
              Left fe -> assertFailure $ "ExUnits evaluation failed: " ++ show fe
              Right exUnitsMap -> assertExUnitMap exUnitsMap
    ]

testCalculateFee :: TestTree
testCalculateFee =
  testGroup
    "should calculate fee"
    [ remoteTestCase "Remote" $ do
        maybeTx <- evaluateReadOnlyFromRemoteKuber test_kBuildTx_simplePay
        case maybeTx of
          Left fe -> assertFailure $ "BuildTx failed: " ++ show fe
          Right tx -> do
            maybeFee <- evaluateReadOnlyFromRemoteKuber (kCalculateMinFee tx)
            case maybeFee of
              Left fe -> assertFailure $ "Remote fee calculation failed: " ++ show fe
              Right fee ->
                if fee > 0
                  then pure ()
                  else assertFailure "Remote fee calculation returned non-positive fee",
      localTestCase "Local" $ do
        maybeTx <- evaluateReadOnlyFromLocalKuber test_kBuildTx_simplePay
        case maybeTx of
          Left fe -> assertFailure $ "BuildTx failed: " ++ show fe
          Right tx -> do
            maybeFee <- evaluateReadOnlyFromLocalKuber (kCalculateMinFee tx)
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
    [ remoteTestCase "Remote" $ do
        maybeWallet <- getDefaultWallet
        case maybeWallet of
          Left err -> assertFailure $ "Remote submit failed: " ++ err
          Right Nothing -> assertFailure "Remote submit failed: ~/.cardano/keys/payment.sk is missing"
          Right (Just wallet) -> do
            maybeResult <- evaluateFromRemoteKuber (walletSubmitTx wallet)
            case maybeResult of
              Left fe -> assertFailure $ "Remote submit failed: " ++ show fe
              Right submitted ->
                if submitted
                  then pure ()
                  else assertFailure "Remote submit failed: wallet has no spendable UTxO",
      localTestCase "Local" $ do
        maybeWallet <- getDefaultWallet
        case maybeWallet of
          Left err -> assertFailure $ "Local submit failed: " ++ err
          Right Nothing -> assertFailure "Local submit failed: ~/.cardano/keys/payment.sk is missing"
          Right (Just wallet) -> do
            maybeResult <- evaluateFromLocalKuber (walletSubmitTx wallet)
            case maybeResult of
              Left fe -> assertFailure $ "Local submit failed: " ++ show fe
              Right submitted ->
                if submitted
                  then pure ()
                  else assertFailure "Local submit failed: wallet has no spendable UTxO"
    ]

walletSubmitTx :: forall api w. (HasKuberAPI api, HasChainQueryAPI api, HasCardanoQueryApi api, HasSubmitApi api) => DefaultWallet -> Kontract api w FrameworkError Bool
walletSubmitTx (DefaultWallet signKey walletAddress) = do
  let walletAddressAny = addressInEraToAddressAny walletAddress
  UTxO walletUtxos <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  if Map.null walletUtxos
    then pure False
    else do
      fundingUtxos <- selectFundingUtxosOrErr "Submit test" (Coin 5_000_000) (UTxO walletUtxos)
      tx <-
        kBuildTx $
          txWalletUtxos fundingUtxos
            <> txSign signKey
            <> txChangeAddress walletAddress
            <> txPayTo walletAddress (lovelaceToValue 2_000_000)
      kSubmitTx (InAnyCardanoEra ConwayEra tx)
      waitForTxAtAddress walletAddressAny (getTxId $ getTxBody tx)

scriptTestDatum = unsafeHashableScriptData $ dataToScriptData ()

scriptTestRedeemer = unsafeHashableScriptData $ dataToScriptData ()

smartContractRoundTrip :: forall api w. (HasKuberAPI api, HasChainQueryAPI api, HasCardanoQueryApi api, HasSubmitApi api) => DefaultWallet -> Kontract api w FrameworkError ()
smartContractRoundTrip (DefaultWallet signKey walletAddress) = do
  network <- kGetNetworkId
  script <- liftIO loadFixtureSmartContractScript
  let walletAddressAny = addressInEraToAddressAny walletAddress
      scriptAddress = txScriptAddress (TxScriptPlutus $ toTxPlutusScript script) network NoStakeAddress
      scriptAddressAny = addressInEraToAddressAny scriptAddress
      lockedValue = lovelaceToValue 3_000_000

  UTxO walletUtxos <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  if Map.null walletUtxos
    then kError ConnectionError "Smart contract E2E failed: wallet has no spendable UTxO"
    else pure ()

  initialScriptUtxo <- (kQueryUtxoByAddress $ Set.singleton scriptAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  lockFundingUtxos <- selectFundingUtxosOrErr "Smart contract E2E lock" (Coin 6_000_000) (UTxO walletUtxos)
  lockTx <-
    kBuildTx $
      txWalletUtxos lockFundingUtxos
        <> txSign signKey
        <> txChangeAddress walletAddress
        <> txPayToScriptWithData scriptAddress lockedValue scriptTestDatum
  kSubmitTx (InAnyCardanoEra ConwayEra lockTx)
  lockConfirmed <- waitForTxAtAddress scriptAddressAny (getTxId $ getTxBody lockTx)
  if not lockConfirmed
    then kError ConnectionError "Smart contract E2E failed: lock transaction was not confirmed at the script address before timeout"
    else pure ()
  (scriptTxIn, scriptTxOut) <- waitForNewUtxoAtAddress scriptAddressAny initialScriptUtxo

  UTxO refreshedWalletUtxos <- (kQueryUtxoByAddress $ Set.singleton walletAddressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
  redeemFundingUtxos <- selectFundingUtxosOrErr "Smart contract E2E redeem" (Coin 3_000_000) (UTxO refreshedWalletUtxos)
  redeemTx <-
    kBuildTx $
      txWalletUtxos redeemFundingUtxos
        <> txSign signKey
        <> txChangeAddress walletAddress
        <> txRedeemUtxo scriptTxIn scriptTxOut script scriptTestRedeemer Nothing
        <> txPayTo walletAddress lockedValue
  kSubmitTx (InAnyCardanoEra ConwayEra redeemTx)
  redeemConfirmed <- waitForTxAtAddress walletAddressAny (getTxId $ getTxBody redeemTx)
  if not redeemConfirmed
    then kError ConnectionError "Smart contract E2E failed: redeem transaction was not confirmed at the wallet address before timeout"
    else pure ()
  redeemed <- waitForUtxoSpent scriptAddressAny scriptTxIn
  if redeemed
    then pure ()
    else kError ConnectionError "Smart contract E2E failed: redeemed script UTxO still present after timeout"

waitForTxAtAddress :: forall api w. HasChainQueryAPI api => AddressAny -> TxId -> Kontract api w FrameworkError Bool
waitForTxAtAddress addressAny targetTxId = poll 60
  where
    poll :: Int -> Kontract api w FrameworkError Bool
    poll remaining
      | remaining <= 0 = pure False
      | otherwise = do
          liftIO $ threadDelay 1_000_000
          UTxO currentUtxo <- (kQueryUtxoByAddress $ Set.singleton addressAny :: Kontract api w FrameworkError (UTxO ConwayEra))
          if any (\(TxIn txId _, _) -> txId == targetTxId) (Map.toList currentUtxo)
            then pure True
            else poll (remaining - 1)

loadFixtureSmartContractScript :: IO (PlutusScript PlutusScriptV2)
loadFixtureSmartContractScript =
  case parsePlutusScriptCborHex AsPlutusScriptV2 "49480100002221200101" of
    Just script -> pure script
    Nothing -> error "Failed to parse provided always-succeeds Plutus V2 script CBOR"

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

selectFundingUtxosOrErr :: forall api w. String -> Coin -> UTxO ConwayEra -> Kontract api w FrameworkError (UTxO ConwayEra)
selectFundingUtxosOrErr label minAda walletUtxos =
  case selectFundingUtxos minAda walletUtxos of
    Left err -> kError ConnectionError (label ++ ": " ++ err)
    Right selected -> pure selected

selectFundingUtxos :: Coin -> UTxO ConwayEra -> Either String (UTxO ConwayEra)
selectFundingUtxos minAda (UTxO entries) =
  case accumulate mempty 0 sortedCandidates of
    Just selected -> Right (UTxO selected)
    Nothing -> Left ("could not find enough ADA-only UTxOs to cover " ++ show minAda)
  where
    minRequired = coinToInteger minAda
    sortedCandidates =
      reverse $
        sortOn snd $
          filter ((> 0) . snd) $
            mapMaybe
              ( \(txIn, txOut) ->
                  case txOutAdaOnly txOut of
                    Just adaAmount -> Just ((txIn, txOut), adaAmount)
                    Nothing -> Nothing
              )
              (Map.toList entries)

    accumulate selected total [] =
      if total >= minRequired
        then Just selected
        else Nothing
    accumulate selected total (((txIn, txOut), adaAmount) : rest)
      | total >= minRequired = Just selected
      | otherwise = accumulate (Map.insert txIn txOut selected) (total + adaAmount) rest

coinToInteger :: Coin -> Integer
coinToInteger (Coin amount) = amount

txOutAdaOnly :: TxOut CtxUTxO ConwayEra -> Maybe Integer
txOutAdaOnly (TxOut _ txOutValue _ _) =
  case txOutValue of
    TxOutValueByron (Coin amount) -> Just amount
    TxOutValueShelleyBased sbe ledgerValue ->
      let value = fromLedgerValue sbe ledgerValue
       in if length (valueToList value) == 1
            then case selectAsset value AdaAssetId of
              Quantity amount -> Just amount
            else Nothing

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

getDefaultWallet :: IO (Either String (Maybe DefaultWallet))
getDefaultWallet = do
  home <- getHomeDirectory
  let signKeyPath = home </> ".cardano" </> "keys" </> "payment.sk"
      addressPath = home </> ".cardano" </> "keys" </> "payment.addr"
  exists <- doesFileExist signKeyPath
  if exists
    then do
      signKey <- readSignKey signKeyPath
      addressExists <- doesFileExist addressPath
      walletAddress <-
        if addressExists
          then do
            addressText <- Text.strip <$> Text.readFile addressPath
            pure $
              case deserialiseAddress (AsAddressInEra AsConwayEra) addressText of
                Just address -> Right address
                Nothing ->
                  Left $
                    "~/.cardano/keys/payment.addr exists but is not a valid Conway-era address: "
                      ++ Text.unpack addressText
          else do
            (_networkName, network) <- getNetworkFromEnv "NETWORK"
            pure $ Right $ skeyToAddrInEra @ConwayEra signKey network
      pure $ fmap (Just . DefaultWallet signKey) walletAddress
    else pure $ Right Nothing

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
