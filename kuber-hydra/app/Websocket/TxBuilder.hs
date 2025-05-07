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
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.Generics
import Websocket.Aeson
import Websocket.Forwarder
import Websocket.SocketConnection (fetch, post, validateLatestWebsocketTag)
import Websocket.Utils (textToJSON)

submitHydraDecommitTx :: [T.Text] -> SigningKey PaymentKey -> Bool -> IO (Either FrameworkError A.Value)
submitHydraDecommitTx utxosToDecommit sk wait = do
  (allUTxOsText, _) <- sendCommandToHydraNodeSocket GetUTxO False
  let allHydraUTxOs = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe HydraGetUTxOResponse
  case allHydraUTxOs of
    Nothing -> return $ Left $ FrameworkError ParserError "buildHydraDecommitTx: Error parsing Hydra UTxOs"
    Just parsedHydraUTxOs -> do
      let hydraUTxOs = utxo parsedHydraUTxOs
          missing = filter (`M.notMember` hydraUTxOs) utxosToDecommit
      if not (null missing)
        then return $ Left $ FrameworkError ParserError $ "Missing UTxOs in Hydra: " ++ show missing
        else do
          let hydraUTxOsToDecommit = M.filterWithKey (\k _ -> k `elem` utxosToDecommit) hydraUTxOs
              encodedUTxOs = A.encode hydraUTxOsToDecommit
          case parseUTxO encodedUTxOs of
            Left fe -> return $ Left fe
            Right parsedUTxO -> do
              let txb =
                    mconcat
                      ( map
                          ( \(tin, tout) ->
                              txConsumeUtxo tin tout
                                <> txSetFee 0
                                <> txChangeAddress
                                  ( case tout of
                                      TxOut addr _ _ _ -> addr
                                  )
                          )
                          $ Map.toList
                          $ unUTxO parsedUTxO
                      )
                      <> txSign sk
              protocolParamText <- hydraProtocolParams
              case protocolParamText of
                Left err -> return $ Left err
                Right (hpp :: HydraProtocolParameters) -> do
                  cardanoTxBody <- toCardanoTxBody txb hpp
                  case cardanoTxBody of
                    Left fe -> return $ Left fe
                    Right tx -> do
                      let cborHex :: T.Text = T.pack $ toHexString $ serialiseToCBOR tx
                          decommitTxObject = buildWitnessedTx cborHex
                      decommitPostResponse <- post "decommit" decommitTxObject
                      if T.strip (T.filter (/= '"') decommitPostResponse) == "OK"
                        then do
                          wsResult <- validateLatestWebsocketTag (generateResponseTag DeCommitUTxO) wait
                          return $ textToJSON $ fst wsResult
                        else
                          return $
                            Left $
                              FrameworkError TxSubmissionError $
                                "submitHydraDecommitTx: Error submitting decommit transaction to Hydra: " ++ T.unpack decommitPostResponse

hydraProtocolParams :: IO (Either FrameworkError HydraProtocolParameters)
hydraProtocolParams = do
  protocolParamText <- fetch >>= \query -> query (T.pack "protocol-parameters")
  case A.eitherDecode (BSL.fromStrict $ encodeUtf8 protocolParamText) of
    Left err -> pure $ Left $ FrameworkError ParserError err
    Right (hpp :: HydraProtocolParameters) -> pure $ Right hpp

groupUtxosByAddress :: M.Map T.Text A.Value -> [GroupedUTXO]
groupUtxosByAddress utxoMap =
  let grouped = foldl' insertUTXO M.empty (M.toList utxoMap)
   in map (\(addr, utxos) -> GroupedUTXO {address = addr, utxos = utxos}) (M.toList grouped)
  where
    insertUTXO acc (utxoId, jsonVal) =
      case A.fromJSON jsonVal :: A.Result Object of
        A.Success obj -> case KM.lookup (Key.fromText (T.pack "address")) obj of
          Just (A.String addr) -> M.insertWith (++) addr [utxoId] acc
          _ -> acc -- Skip if "address" missing or not a string
        A.Error _ -> acc

parseUTxO :: BSL.ByteString -> Either FrameworkError (UTxO ConwayEra)
parseUTxO bs = case A.decode bs :: Maybe (UTxO ConwayEra) of
  Just x -> Right x
  Nothing -> Left $ FrameworkError ParserError "parseUTxO: Failulre parsing UTxO Json schema to UTxO ConwayEra"

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

buildWitnessedTx :: (ToJSON v) => v -> A.Value
buildWitnessedTx cborHex =
  object
    [ "cborHex" .= cborHex,
      "type" .= T.pack "Witnessed Tx ConwayEra",
      "description" .= T.pack "Ledger Cddl Format"
    ]

rawBuildHydraTx :: TxBuilder_ ConwayEra -> IO (Either FrameworkError (Tx ConwayEra))
rawBuildHydraTx txb = do
  protocolParamText <- hydraProtocolParams
  case protocolParamText of
    Left err -> return $ Left err
    Right (hpp :: HydraProtocolParameters) -> do
      cardanoTxBody <- toCardanoTxBody txb hpp
      case cardanoTxBody of
        Left fe -> return $ Left fe
        Right tx -> return $ Right tx

queryUTxO :: Maybe T.Text -> Maybe T.Text -> IO (Either FrameworkError A.Value)
queryUTxO address txin = do
  (allUTxOsText, _) <- sendCommandToHydraNodeSocket GetUTxO False
  let allHydraUTxOs = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe HydraGetUTxOResponse
  case allHydraUTxOs of
    Nothing -> return $ Left $ FrameworkError ParserError "queryUTxO: Error parsing Hydra UTxOs"
    Just parsedHydraUTxOs -> do
      case txin of
        Just tin -> do
          case parseTxIn tin of
            Just _ -> do
              let hydraUTxOs = utxo parsedHydraUTxOs
                  missing = filter (`M.notMember` hydraUTxOs) [tin]
              if not (null missing)
                then return $ Left $ FrameworkError ParserError $ "Missing UTxOs in Hydra: " ++ show missing
                else do
                  let hydraTxInFilteredUTxOs = M.filterWithKey (\k _ -> k == tin) hydraUTxOs
                      converted = KM.fromList [(K.fromText k, v) | (k, v) <- M.toList hydraTxInFilteredUTxOs]
                  return $ Right $ A.Object converted
            Nothing -> pure $ Left $ FrameworkError ParserError $ "queryUTxO : Unable to parse TxIn: " <> T.unpack tin
        Nothing ->
          case address of
            Just addr ->
              case parseAddress @ConwayEra addr of
                Just _ -> do
                  let hydraUTxOs = utxo parsedHydraUTxOs
                      hydraAddressFilteredUTxOs =
                        M.filter
                          ( \val ->
                              case val of
                                A.Object obj ->
                                  case KM.lookup "address" obj of
                                    Just (A.String a) -> a == addr
                                    _ -> False
                                _ -> False
                          )
                          hydraUTxOs
                      converted = KM.fromList [(K.fromText k, v) | (k, v) <- M.toList hydraAddressFilteredUTxOs]
                  return $ Right $ A.Object converted
                Nothing -> pure $ Left $ FrameworkError ParserError $ "queryUTxO : Unable to parse address: " <> T.unpack addr
            Nothing -> pure $ Right $ A.toJSON $ utxo parsedHydraUTxOs

toValidHydraTxBuilder :: TxBuilder_ ConwayEra -> IO (Either FrameworkError TxModal)
toValidHydraTxBuilder txb = do
  -- validate selections and inputs
  let selections = txSelections txb
      inputs = txInputs txb
  selectionUTxOs <- mapM getUTxOsFromSelection selections
  let (utxos, err) = (rights selectionUTxOs, lefts selectionUTxOs)
      selectionTxBuilder =
        mconcat $
          map txWalletUtxos (concat utxos)
  if not (null err)
    then
      return $
        Left $
          FrameworkError
            NodeQueryError
            ("toValidHydraTxBuilder: Missing Selection In Hydra." <> concatMap ((++ "\n") . show) err)
    else do
      Debug.traceM (BS8.unpack $ prettyPrintJSON selectionTxBuilder)
      txBuilderResponse <- liftIO $ rawBuildHydraTx $ txb <> selectionTxBuilder
      case txBuilderResponse of
        Left fe -> pure $ Left fe
        Right tx -> pure $ Right $ TxModal $ InAnyCardanoEra bCardanoEra tx

getUTxOsFromSelection :: TxInputSelection ConwayEra -> IO (Either FrameworkError [UTxO ConwayEra])
getUTxOsFromSelection selection = case selection of
  TxSelectableAddresses addrs -> do
    let capiAddress = map fromLedgerAddress addrs :: [AddressInEra ConwayEra]
        addressTexts = map serialiseAddress capiAddress
    fetchUTxOsFromQuery (\addr -> queryUTxO (Just addr) Nothing) addressTexts "Error querying addresses from Hydra"
  TxSelectableUtxos utxo -> pure $ Right [utxo]
  TxSelectableTxIn txins -> do
    let txInTexts =
          map (\(TxIn hash (TxIx num)) -> serialiseToRawBytesHexText hash <> "#" <> T.pack (show $ toInteger num)) txins
    fetchUTxOsFromQuery (queryUTxO Nothing . Just) txInTexts "Error querying TxIns from Hydra"
  _ -> pure $ Right []
  where
    fetchUTxOsFromQuery :: (a -> IO (Either FrameworkError A.Value)) -> [a] -> String -> IO (Either FrameworkError [UTxO ConwayEra])
    fetchUTxOsFromQuery queryFn inputs errorMsg = do
      results <- mapM queryFn inputs
      let (utxos, errors) = (rights results, lefts results)
      if not (null errors)
        then return $ Left $ FrameworkError NodeQueryError errorMsg
        else pure $ Right $ rights $ map (parseUTxO . A.encode) utxos