{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Websocket.TxBuilder where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.IO (unsafePerformIO)
import Websocket.Aeson
import Websocket.Forwarder
import Websocket.SocketConnection (fetch, post, validateLatestWebsocketTag)
import Websocket.Utils (textToJSON)

newtype HydraGetUTxOResponse = HydraGetUTxOResponse
  { utxo :: M.Map T.Text A.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GroupedUTXO = GroupedUTXO
  { address :: T.Text,
    utxos :: [T.Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

submitHydraDecommitTx :: [T.Text] -> SigningKey PaymentKey -> Either FrameworkError A.Value
submitHydraDecommitTx utxosToDecommit sk = do
  let allHydraUTxOs = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOs) :: Maybe HydraGetUTxOResponse
      allUTxOs = fst $ unsafePerformIO $ sendCommandToHydraNodeSocket GetUTxO
  parsedHydraUTxOs <- case allHydraUTxOs of
    Just res -> Right res
    Nothing -> Left $ FrameworkError ParserError "buildHydraDecommitTx: Error parsing Hydra UTxOs"
  let hydraUTxOs = utxo parsedHydraUTxOs
  let missing = filter (`M.notMember` hydraUTxOs) utxosToDecommit
  if not (null missing)
    then Left $ FrameworkError ParserError $ "Missing UTxOs in Hydra: " ++ show missing
    else do
      let hydraUTxOsToDecommit = M.filterWithKey (\k _ -> k `elem` utxosToDecommit) hydraUTxOs
          encodedUTxOs = A.encode hydraUTxOsToDecommit
      case parseUTxO encodedUTxOs of
        Left fe -> Left fe
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
          let protocolParamText = fetch >>= \query -> query (T.pack "protocol-parameters")
          case A.eitherDecode (BSL.fromStrict $ encodeUtf8 $ unsafePerformIO protocolParamText) of
            Right (hpp :: HydraProtocolParameters) -> do
              case toCardanoTxBody txb hpp of
                Left fe -> Left fe
                Right tx -> do
                  let cborHex :: T.Text = T.pack $ toHexString $ serialiseToCBOR tx
                      decommitTxObject = buildWitnessedTx cborHex
                      decommitPostResponse = unsafePerformIO $ post "decommit" decommitTxObject
                  if T.strip (T.filter (/= '"') decommitPostResponse) == "OK"
                    then do
                      Right $ textToJSON $ fst $ unsafePerformIO $ validateLatestWebsocketTag $ generateResponseTag DeCommitUTxO
                    else Left $ FrameworkError TxSubmissionError ("submitHydraDecommitTx: Error submiting decommit transaction to Hydra : " ++ T.unpack decommitPostResponse)
            Left err -> Debug.trace err $ Left $ FrameworkError ParserError err

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

submitHandler :: Kontract ChainConnectInfo w FrameworkError r -> IO (IO (Either FrameworkError r))
submitHandler tx = do
  localChain <- chainInfoFromEnv
  pure $ evaluateKontract localChain tx

toCardanoTxBody :: TxBuilder_ ConwayEra -> HydraProtocolParameters -> Either FrameworkError (Tx ConwayEra)
toCardanoTxBody txb hpp = do
  case hydraProtocolParamsToLedgerParams hpp of
    Left fe -> Left fe
    Right pp -> do
      let builtTx = unsafePerformIO $ submitHandler $ kBuildRawTx txb pp
      unsafePerformIO builtTx

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