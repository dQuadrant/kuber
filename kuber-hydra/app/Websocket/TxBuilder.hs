{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Websocket.TxBuilder where

import Cardano.Api
import qualified Cardano.Api.Ledger as L
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Util
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import Websocket.Forwarder
import Websocket.SocketConnection (AppConfig, fetch, validateLatestWebsocketTag, getSnapshotUtxo, getProtocolParameters, getSnapshotUtxo', postDecommit')
import Websocket.Utils
import qualified Data.Text.IO as T
import Websocket.Aeson (HydraProtocolParameters(..), ProtocolVersion(..))
import Network.HTTP.Client.Conduit (Response(responseStatus, responseBody))
import Network.HTTP.Types (Status(statusCode))
import qualified Data.ByteString.Lazy.Char8 as BSL8


handleHydraDecommitTx :: AppConfig -> [TxIn] -> Maybe (SigningKey PaymentKey) -> Bool -> Bool -> IO (Either FrameworkError A.Value)
handleHydraDecommitTx appConfig utxosToDecommit sk wait submit = do
  allUTxOsText <-  getSnapshotUtxo appConfig
  let allHydraUTxOs = decode allUTxOsText :: Maybe (UTxO ConwayEra)
  case allHydraUTxOs of
    Nothing -> return $ Left $ FrameworkError ParserError "buildHydraDecommitTx: Error parsing Hydra UTxOs"
    Just hydraUTxOs -> do
          let hydraTxIns = M.keys ( unUTxO hydraUTxOs)
          let notMissing = utxosToDecommit `isSubsetOf` hydraTxIns
          if not notMissing
            then
              return
                $ Left
                $ FrameworkError
                  NodeQueryError
                $ "Missing UTxOs in Hydra: " <> show (utxosToDecommit `notPresentIn` hydraTxIns)
            else do
              let utxosToDecommitTexts =  utxosToDecommit
                  hydraUTxOsToDecommit = M.filterWithKey (\k _ -> k `elem` utxosToDecommitTexts) (unUTxO hydraUTxOs)
                  encodedUTxOs = A.encode hydraUTxOsToDecommit
              let txb =
                    mconcat
                      ( map
                          ( \(tin, tout) ->
                              txConsumeUtxo tin tout
                                <> txChangeAddress
                                  ( case tout of
                                      TxOut addr _ _ _ -> addr
                                  )
                          )
                          $ Map.toList
                          $ hydraUTxOsToDecommit
                      )
                      <> maybe mempty txSign sk
              protocolParamText <- hydraProtocolParams appConfig
              case protocolParamText of
                Left err -> return $ Left err
                Right (hpp :: HydraProtocolParameters) -> do
                  cardanoTxBody <- toCardanoTxBody txb hpp
                  case cardanoTxBody of
                    Left fe -> return $ Left fe
                    Right tx -> do
                      let decommitTxModalObject = TxModal (InAnyCardanoEra ConwayEra tx)
                      if submit
                        then do
                          putStrLn "Starting to decommit"
                          decommitResponse <- postDecommit' appConfig (Just decommitTxModalObject)
                          if statusCode ( responseStatus decommitResponse) `elem` [200,20]
                            then do
                              putStrLn "Validating websocket tag"
                              wsResult <- validateLatestWebsocketTag appConfig (generateResponseTag DeCommitUTxO) wait
                              return $ textToJSON $ fst wsResult
                            else
                              return $
                                Left $
                                  FrameworkError TxSubmissionError $
                                    "submitHydraDecommitTx: Error submitting decommit transaction to Hydra: " ++ BSL8.unpack (responseBody decommitResponse)
                        else
                          pure $ bytestringToJSON $ A.encode decommitTxModalObject

hydraProtocolParams :: AppConfig -> IO (Either FrameworkError HydraProtocolParameters)
hydraProtocolParams appConfig = do
  protocoalParams <- getProtocolParameters appConfig
  case A.eitherDecode protocoalParams of
    Left err -> pure $ Left $ FrameworkError ParserError err
    Right (hpp :: HydraProtocolParameters) -> pure $ Right hpp

submitHandler :: Kontract ChainConnectInfo w FrameworkError r -> IO (Either FrameworkError r)
submitHandler tx = do
  localChain <- chainInfoFromEnv
  evaluateKontract localChain tx

toCardanoTxBody :: TxBuilder_ ConwayEra -> HydraProtocolParameters -> IO (Either FrameworkError (Tx ConwayEra))
toCardanoTxBody txb hpp = do
  case hydraProtocolParamsToLedgerParams hpp of
    Left fe -> pure $ Left fe
    Right pp -> do
      let builtTx = submitHandler $ kBuildRawTx txb pp
      builtTx

hydraProtocolParamsToLedgerParams :: HydraProtocolParameters -> Either FrameworkError (LedgerProtocolParameters ConwayEra)
hydraProtocolParamsToLedgerParams hpp = case convertToLedgerProtocolParameters ShelleyBasedEraConway $
  ProtocolParameters
    ( major ppVersion,
      minor ppVersion
    )
    (toRational <$> decentralization hpp)
    Nothing
    (maxBlockHeaderSize hpp)
    (maxBlockBodySize hpp)
    (maxTxSize hpp)
    (L.Coin $ toInteger $ txFeeFixed hpp)
    (L.Coin $ toInteger $ txFeePerByte hpp)
    ( case minUTxOValue hpp of
        Nothing -> Nothing
        Just x -> Just $ L.Coin $ toInteger x
    )
    (L.Coin $ toInteger $ stakeAddressDeposit hpp)
    (L.Coin $ toInteger $ stakePoolDeposit hpp)
    (L.Coin $ toInteger $ minPoolCost hpp)
    (L.EpochInterval $ fromIntegral $ poolRetireMaxEpoch hpp)
    (fromIntegral $ stakePoolTargetNum hpp)
    (poolPledgeInfluence hpp)
    (monetaryExpansion hpp)
    (treasuryCut hpp)
    costModel
    (executionUnitPrices hpp)
    (maxTxExecutionUnits hpp)
    (maxBlockExecutionUnits hpp)
    (maxValueSize hpp)
    (collateralPercentage hpp)
    (maxCollateralInputs hpp)
    ( case utxoCostPerByte hpp of
        Nothing -> Nothing
        Just x -> Just $ L.Coin $ toInteger x
    ) of
  Left err -> Left $ FrameworkError ParserError ("hydraProtocolParamsToLedgerParams: Conversion error: " <> show err)
  Right lpp -> Right lpp
  where
    ppVersion = fromJust $ protocolVersion hpp
    costModel =
      Map.fromList $
        Maybe.mapMaybe
          ( \(pv, cm) -> do
              let parsedCostModel = CostModel (fmap fromIntegral cm)
               in if pv == "PlutusV1"
                    then Just (AnyPlutusScriptVersion PlutusScriptV1, parsedCostModel)
                    else
                      if pv == "PlutusV2"
                        then Just (AnyPlutusScriptVersion PlutusScriptV2, parsedCostModel)
                        else
                          if pv == "PlutusV2"
                            then Just (AnyPlutusScriptVersion PlutusScriptV3, parsedCostModel)
                            else Nothing
          )
          (Map.toList $ costModels hpp)

rawBuildHydraTx :: AppConfig -> TxBuilder_ ConwayEra -> IO (Either FrameworkError (Tx ConwayEra))
rawBuildHydraTx appConfig txb = do
  protocolParamText <- hydraProtocolParams appConfig
  case protocolParamText of
    Left err -> return $ Left err
    Right (hpp :: HydraProtocolParameters) -> do
      cardanoTxBody <- toCardanoTxBody txb hpp
      case cardanoTxBody of
        Left fe -> return $ Left fe
        Right tx -> return $ Right tx

queryUTxO :: AppConfig -> [AddressInEra ConwayEra] -> [TxIn] -> IO (Either FrameworkError (UTxO ConwayEra))
queryUTxO appConfig address txin = do
  utxoResponse <- getSnapshotUtxo' appConfig
  if 404 == statusCode (responseStatus utxoResponse)
    then pure $ Right $ UTxO mempty
    else do
    let allHydraUTxOs = decode (responseBody utxoResponse) :: Maybe (UTxO ConwayEra)
    case allHydraUTxOs of
      Nothing -> return $ Left $ FrameworkError ParserError "queryUTxO: Error parsing Hydra UTxOs"
      Just  (UTxO hydraUTxOs) -> do
        let allHydraTxins = M.keys  hydraUTxOs
        if not (null txin)
        then do
            let notMissing = txin `isSubsetOf` allHydraTxins
            if notMissing
              then
                (
                    let hydraTxInFilteredUTxOs = Map.filterWithKey (\k _ -> k `elem` txin) hydraUTxOs
                        converted = UTxO hydraTxInFilteredUTxOs
                    in
                    pure $ pure converted
                )
              else return  $ Right $ UTxO mempty
          -- Case: Addresses are provided
          else if not (null address)
            then
              let hydraAddressFilteredUTxOs =
                    M.filter
                      ( \(TxOut addr _ _ _ ) -> addr `elem` address )
                      hydraUTxOs
                  converted = UTxO hydraAddressFilteredUTxOs
              in
                pure $  pure  converted

            -- Fallback: Return all UTxOs parsed from Hydra response
          else return $ pure $ UTxO hydraUTxOs

toValidHydraTxBuilder :: AppConfig -> TxBuilder_ ConwayEra -> Bool -> IO (Either FrameworkError TxModal)
toValidHydraTxBuilder appConfig txb submit = do
  let selections = txSelections txb
      inputs = txInputs txb
      specifiedChangeAddress = case txDefaultChangeAddr txb of
        Just chAddr -> Right chAddr
        Nothing -> do
          let changeAddressFromSelection = getChangeAddressFromSelections selections :: Maybe (AddressInEra ConwayEra)
          case changeAddressFromSelection of
            Just chAddr -> Right chAddr
            Nothing -> Left $ FrameworkError TxValidationError "Missing Change Address"
  selectionUTxOs <- mapM (getUTxOsFromSelection appConfig) selections
  sKeyFromSelections <- mapM getKeysFromSelections selections
  sKeyFromInputs <- mapM getKeysFromInputs inputs
  inputUTxOs <- mapM (getUTxOFromInputs appConfig) inputs
  let (utxosFromSelections, errorsFromSelections) = (rights selectionUTxOs, lefts selectionUTxOs)
      (utxosFromInputs, errorsFromInputs) = (rights inputUTxOs, lefts inputUTxOs)
  case specifiedChangeAddress of
    Left err -> return $ Left err
    Right changeAddress -> do
      let selectionTxBuilder =
            mconcat $ map txWalletUtxos utxosFromSelections
          inputTxBuilder =
            mconcat $ map txConsumeUtxos utxosFromInputs
          skeyTxBuilder =
            mconcat $ map txSign $ concat sKeyFromSelections <> concat sKeyFromInputs
          changeAddressTxBuilder = txChangeAddress changeAddress
      if not (null $ errorsFromSelections <> errorsFromInputs)
        then
          return $
            Left $
              FrameworkError
                NodeQueryError
                ("toValidHydraTxBuilder: Error building Hydra Tx." <> concatMap ((++ "\n") . show) (errorsFromSelections <> errorsFromInputs))
        else do
          let validHydraTxBuilder =
                -- clearing selections from the original txbuilder becuase the required extration of utxos and signing keys has been done
                txb {txSelections = []}
                  <> selectionTxBuilder
                  <> inputTxBuilder
                  <> skeyTxBuilder
                  <> changeAddressTxBuilder
          Debug.traceM (BS8.unpack $ prettyPrintJSON validHydraTxBuilder)
          txBuilderResponse <- liftIO $ rawBuildHydraTx appConfig validHydraTxBuilder
          case txBuilderResponse of
            Left fe -> pure $ Left fe
            Right tx -> do
              if submit
                then do
                  let txModalToSubmit = TxModal $ InAnyCardanoEra bCardanoEra tx
                  submitHydraTx appConfig txModalToSubmit True
                else
                  pure $ Right $ TxModal $ InAnyCardanoEra bCardanoEra tx

getUTxOsFromSelection :: AppConfig -> TxInputSelection ConwayEra -> IO (Either FrameworkError (UTxO ConwayEra))
getUTxOsFromSelection appConfig selection = case selection of
  TxSelectableAddresses addrs -> do
    let capiAddress = map fromLedgerAddress addrs :: [AddressInEra ConwayEra]
    queryUTxO appConfig capiAddress []
  TxSelectableUtxos utxo' -> pure $ Right $ utxo'
  TxSelectableTxIn txins -> do
    queryUTxO appConfig [] txins
  _ -> pure $ Right $ UTxO mempty

getKeysFromSelections :: TxInputSelection ConwayEra -> IO [SigningKey PaymentKey]
getKeysFromSelections selection = case selection of
  TxSelectableSkey skeys -> pure skeys
  _ -> pure []

getChangeAddressFromSelections :: [TxInputSelection era] -> Maybe (AddressInEra ConwayEra)
getChangeAddressFromSelections selections =
  let parsedSelectionAddresses =
        concat $
          mapMaybe
            ( \s -> case s of
                TxSelectableAddresses addrs -> Just (map fromLedgerAddress addrs :: [AddressInEra ConwayEra])
                _ -> Nothing
            )
            selections
   in case parsedSelectionAddresses of
        [] -> Nothing
        a : _ -> Just a

getUTxOFromInputs :: AppConfig -> TxInput ConwayEra -> IO (Either FrameworkError (UTxO ConwayEra))
getUTxOFromInputs appConfig inputs = validateTxInUTxOs $ getTxInsAndAddressFromInputs inputs
  where
    validateTxInUTxOs :: Either [AddressInEra ConwayEra] [TxIn] -> IO (Either FrameworkError (UTxO ConwayEra))
    validateTxInUTxOs inputs = do
      case inputs of
        Left addrs -> queryUTxO appConfig addrs []
        Right txins -> queryUTxO appConfig [] txins

    getTxInsAndAddressFromInputs :: TxInput ConwayEra -> Either [AddressInEra ConwayEra] [TxIn]
    getTxInsAndAddressFromInputs input = case input of
      TxInputResolved resolvedTxIn -> case resolvedTxIn of
        TxInputUtxo (UTxO utxoMap) -> Right $ map fst $ M.toList utxoMap
        TxInputScriptUtxo _ _ _ _ (tin, _) -> Right [tin]
        TxInputReferenceScriptUtxo tin _ _ _ _ -> Right [tin]
      TxInputUnResolved unresolvedTxIn -> case unresolvedTxIn of
        TxInputTxin tin -> Right [tin]
        TxInputSkey _ -> Right mempty
        TxInputAddr addr -> Left ([addr] :: [AddressInEra ConwayEra])
        TxInputScriptTxin _ _ _ _ tin -> Right [tin]
        TxInputReferenceScriptTxin tin _ _ _ _ -> Right [tin]

getKeysFromInputs :: TxInput ConwayEra -> IO [SigningKey PaymentKey]
getKeysFromInputs inputs = pure $ case inputs of
  TxInputUnResolved unresolvedTxIn -> case unresolvedTxIn of
    TxInputSkey skey -> [skey]
    _ -> []
  _ -> []
