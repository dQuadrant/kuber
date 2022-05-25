{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.Kuber.Data.TxBuilderAeson 
where


import Cardano.Api hiding(txMetadata, txFee)
import Cardano.Api.Shelley hiding (txMetadata, txFee)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Kuber.Util
import Data.Functor ((<&>))

import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut, CurrencySymbol)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser, parseMaybe)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Cardano.Kuber.Data.Models ( unAddressModal)
import Cardano.Kuber.Data.Parsers (parseValueText, parseScriptData, parseAnyScript, parseAddress, parseAssetNQuantity, parseValueToAsset, parseAssetId, scriptDataParser, txInParser, parseUtxo, parseTxIn)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as A.Object
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import qualified Data.ByteString            as B
import qualified Data.ByteString.Short      as SBS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy             as TL
import Debug.Trace (trace, traceM)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString(fromString))
import qualified Debug.Trace as Debug
import qualified Data.Aeson as Aeson
import Data.Word (Word64)
import qualified Data.HashMap.Internal.Strict as H
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Utility.ScriptUtil
import Data.Text.Internal.Fusion.CaseMapping (upperMapping)

instance FromJSON TxBuilder where
  parseJSON (A.Object v) =
    TxBuilder
      <$> (v .:? "selections" .!=[])
      <*> (v .:? "inputs" .!=[])
      <*> (v .:? "outputs" .!= [])
      <*> (v .:? "collaterals" .!= [])
      <*> v .:? "validityStart"
      <*> v .:? "validityEnd"
      <*> (v .:? "mint" .!= [])
      <*> (v .:? "signatures" .!= [])
      <*> v .:? "fee"
      <*> (v .:? "changeAddress" <&> fmap unAddressModal)
      <*> (v.:? "metadata" .!= Map.empty)
  parseJSON _ = fail "TxBuilder must be an object"

instance FromJSON TxMintData where
  parseJSON (A.Object v) = do
    mintAmountJson <- v .: "amount"
    scriptJson:: A.Object <- v .: "script"
    scriptAny <- parseAnyScript $ B.concat $ LBS.toChunks $ A.encode scriptJson

    mintRedeemer <- v .:? "redeemer"
    exUnitsM <- v .:? "executionUnits"
    mintScript <- case mintRedeemer of
      Nothing -> do
        case validateScriptSupportedInEra' AlonzoEra scriptAny of
          Left fe -> throw fe
          Right (ScriptInEra langInEra script') -> case script' of
            SimpleScript ssv ss -> pure $ TxSimpleScript scriptAny
            PlutusScript psv ps -> fail "Plutus Minting Script must have redeemer present."
      Just (A.String s) -> case parseScriptData s of
        Nothing -> fail $ "Invalid script data string: " ++ T.unpack s
        Just sData -> pure $ TxPlutusScript scriptAny sData exUnitsM
      Just _ -> fail "TxMintingScript redeemer must be a string"

    scriptWitnessE <- case mintScript of
      TxSimpleScript sial -> pure $ createSimpleMintingWitness sial
      TxPlutusScript sAny sd eM -> do
        exUnits <- case eM of
          Nothing -> pure $ ExecutionUnits 700000000 700000000
          Just eu -> pure eu
        pure $ createPlutusMintingWitness sAny sd exUnits

    (sw,policyId) <- case scriptWitnessE of
      Left fe -> throw fe
      Right sw -> pure $ (sw, getPolicyIdFromScriptWitness sw)


    case mintAmountJson of
      A.Object o -> do
        let amountList = HM.toList o

        mintValue <- mapM (mapToValue policyId) amountList

        pure $ TxMintData policyId sw (valueFromList mintValue)
      _ -> fail "Mint amount must be a object with key as token name and value as integer."
    where
      mapToValue :: MonadFail m => PolicyId -> (T.Text,A.Value) -> m (AssetId, Quantity)
      mapToValue policyId (tName, amountT) = do
        amount <- parseAmount amountT
        let assetName = fromString $ T.unpack tName
        pure (AssetId policyId assetName, Quantity amount)

      parseAmount :: MonadFail m => A.Value -> m Integer
      parseAmount v = case v of
        A.Number sci -> pure $ floor sci
        _ -> fail "Error amount value must be in integer"


      getPolicyIdFromScriptWitness :: ScriptWitness WitCtxMint  AlonzoEra  -> PolicyId
      getPolicyIdFromScriptWitness witness =
          case scriptWitnessScript witness of
            ScriptInEra _ script -> scriptPolicyId script

  parseJSON _ = fail "TxMintData must be an object"
        --   case mintValueJson of
        --     Just (A.Array mintValueArray) -> do
        --       let mintValueList = V.toList mintValueArray
        --       valueList <- mapM parseMintObject mintValueList
        --       pure $ valueFromList $ Prelude.concat valueList
        --     Just (A.String mintValueText) -> parseValueText mintValueText
        --     Just v@(A.Object mintValueObject) -> do
        --       value <- parseMintObject v
        --       pure $ valueFromList value
        --     Just _ -> fail "Failed to parse mintValue must be value object or array or value text"
        --     Nothing -> pure $ valueFromList []

        -- parseMintObject :: A.Value ->  Parser [(AssetId,Quantity)]
        -- parseMintObject (A.Object v) = do
        --   policyId <- v .: "policy"
        --   assetName <- v .: "name"
        --   assetId <- parseAssetId policyId assetName
        --   value <- v .: "amount"
        --   pure [(assetId, Quantity value)]
        -- parseMintObject (A.String mintValueText) = parseValueToAsset mintValueText
        -- parseMintObject _ = fail "Failed to parse mintValue must be value object"

        -- parseMintValue :: Parser Value
        -- parseMintValue = do
        --   mintValueJson <- v .:? "mint"
        --   case mintValueJson of
        --     Just (A.Array mintValueArray) -> do
        --       let mintValueList = V.toList mintValueArray
        --       valueList <- mapM parseMintObject mintValueList
        --       pure $ valueFromList $ Prelude.concat valueList
        --     Just (A.String mintValueText) -> parseValueText mintValueText
        --     Just v@(A.Object mintValueObject) -> do
        --       value <- parseMintObject v
        --       pure $ valueFromList value
        --     Just _ -> fail "Failed to parse mintValue must be value object or array or value text"
        --     Nothing -> pure $ valueFromList []

        -- parseMintObject :: A.Value ->  Parser [(AssetId,Quantity)]
        -- parseMintObject (A.Object v) = do
        --   policyId <- v .: "policy"
        --   assetName <- v .: "name"
        --   assetId <- parseAssetId policyId assetName
        --   value <- v .: "amount"
        --   pure [(assetId, Quantity value)]
        -- parseMintObject (A.String mintValueText) = parseValueToAsset mintValueText
        -- parseMintObject _ = fail "Failed to parse mintValue must be value object"


instance ToJSON TxBuilder where
  toJSON (TxBuilder selections inputs outputs collaterals validityStart validityEnd mintData signatures fee defaultChangeAddr metadata) =
    A.object
      [ "selections" .= selections
      , "inputs" .= inputs
      , "outputs" .= outputs
      , "collaterals" .= collaterals
      , "validityStart" .= validityStart
      , "validityEnd" .= validityEnd
      , "mint" .=  mintData
      , "signatures" .= signatures
      , "fee" .= fee
      , "defaultChangeAddr" .= defaultChangeAddr
      , "metadata" .= metadata
      ]

instance ToJSON TxMintData where
  toJSON (TxMintData policyId mintScript mintValue) =
    A.object
      [
      "policyId" .= policyId
      ,"script" .= show mintScript
      , "amount" .= mintValue
      ]

instance ToJSON TxInputSelection where
  toJSON (TxSelectableAddresses v) = A.Array $ V.fromList $ Prelude.map toJSON v
  toJSON (TxSelectableTxIn v) = A.Array $ V.fromList $ Prelude.map toJSON v
  toJSON (TxSelectableUtxos  (UTxO uMap)) = Aeson.String $ T.pack $  "WithTxout: [" ++ intercalate ", "  (map (T.unpack . renderTxIn  ) $ Map.keys uMap)++"]"

instance ToJSON TxInput where
  toJSON (TxInputUnResolved txInputTxin) = toJSON txInputTxin
  toJSON (TxInputResolved val) = toJSON val
instance ToJSON TxInputUnResolved_ where
  toJSON (TxInputTxin txin) = toJSON txin
  toJSON (TxInputScriptTxin _script _data _redeemer _exUnits _txin) =
    A.object
      [
        "script" .= _script
      , "data" .= _data
      , "redeemer" .= _redeemer
      , "exUnits" .= _exUnits
      , "txin" .= _txin
      ]
  toJSON (TxInputAddr _addr) = toJSON _addr

instance ToJSON TxInputResolved_  where
  toJSON  v = case v of 
    TxInputUtxo (UTxO umap) ->  Aeson.String $ T.pack $  "WithTxout: " ++ show (map renderTxIn $ Map.keys umap)
    TxInputScriptUtxo tvs sd sd' m_eu (UTxO umap) -> Aeson.String $ T.pack $  "WithScriptTxout: " ++ show (map renderTxIn $ Map.keys umap)

instance ToJSON TxValidatorScript where
  toJSON (TxValidatorScript script) = A.String $ T.pack $ show script

instance ToJSON ScriptData where
  toJSON scriptData = scriptDataToJson ScriptDataJsonNoSchema scriptData

instance ToJSON TxOutput where
  toJSON (TxOutput _content _addChange _deductFee) =
    A.object
      [
        "content" .= _content
      , "addChange" .= _addChange
      , "deductFee" .= _deductFee
      ]

instance ToJSON TxOutputContent where
  toJSON (TxOutAddress _address value) =
    A.object
      [
        "address" .= _address
      , "value" .= value
      ]
  toJSON (TxOutScript _script value dataHash) =
    A.object
      [
        "script" .= _script
      , "value" .= value
      , "dataHash" .= dataHash
      ]
  toJSON (TxOutScriptAddress address value dataHash) =
    A.object
      [
        "address" .= address
      , "value" .= value
      , "dataHash" .= dataHash
      ]
  toJSON _ = "TxOutPkh Not implemented."

instance ToJSON TxMintingScript where
  toJSON (TxSimpleScript _script) =
    A.object
      [
        "script" .= _script
      ]
  toJSON (TxPlutusScript _script _redeemer _exUnits) =
    A.object
      [
        "script" .= _script
      , "redeemer" .= _redeemer
      , "exUnits" .= _exUnits
      ]

instance ToJSON ScriptInAnyLang where
  toJSON script = A.String $ T.pack $ show script

instance ToJSON TxCollateral where
  toJSON (TxCollateralTxin _collateral) = toJSON _collateral

  toJSON _ = "TxCollateralUtxo Not implemented."

instance ToJSON TxSignature where
  toJSON (TxSignatureAddr _sigAddr) = toJSON _sigAddr
  toJSON _ = "TxSignaturePkh Not implemented."

instance FromJSON TxInputSelection where
  parseJSON v@(A.String s) = do
    if "addr" `T.isPrefixOf` s
      then do
        case deserialiseAddress (AsAddressInEra AsAlonzoEra) s of
          Nothing -> fail $ "Invalid address string: " ++ T.unpack s
          Just aie -> pure $ TxSelectableAddresses [aie]
      else do 
          case  parseTxIn  s of
            Just txin -> pure $ TxSelectableTxIn [txin]
            Nothing -> case parseUtxo  s of 
              Just utxo -> pure $ TxSelectableUtxos utxo
              Nothing -> fail $ "Invalid InputSelection String. It must be  address, txHash#index, txInCbor or  utxo"
          
  parseJSON v = do
    txIn <- txInParser  v
    pure $ TxSelectableTxIn [txIn]

instance FromJSON TxInput where
  parseJSON (A.Object v) = do
    utxo <- v .: "utxo"
    txIn <- txInParser  utxo
    mScript :: (Maybe A.Value) <- v .:? "script"
    case mScript of
      Nothing ->  pure $ TxInputUnResolved $ TxInputTxin txIn
      Just scriptJson -> do
        script <- parseAnyScript $ B.concat $ LBS.toChunks $ A.encode scriptJson
        datum <- v  `scriptDataParser` "datum"
        redeemer <- v `scriptDataParser` "redeemer"
        exUnits <- v .:? "executionUnits"
        pure $ TxInputUnResolved $ TxInputScriptTxin (TxValidatorScript script) datum redeemer exUnits txIn

  parseJSON v@(A.String s) = do
    if "addr" `T.isPrefixOf` s
    then do
      case deserialiseAddress (AsAddressInEra AsAlonzoEra) s of
        Nothing -> fail $ "Invalid address string: " ++ T.unpack s
        Just aie -> pure $ TxInputUnResolved $ TxInputAddr aie
    else do
      txIn <- txInParser  v
      pure $ TxInputUnResolved $ TxInputTxin txIn

  parseJSON _ = fail "TxInput must be an object or string"

instance FromJSON TxOutput where
  parseJSON (A.Object v) = do
    -- Parse TxOutput according to address type if simple
    -- then use address with value TxOutAddress otherwise use TxOutScriptAddress or TxOutScript
    -- which has script and data if there is no script given then take address
    -- as script address and get data hash from the datum or directly from dataHash
    -- if there is script given then use that script and datahash
    addChange' <- v .:? "addChange" .!= False
    deductFee' <- v .:? "deductFee" .!= False

    addressTextM <- v .:? "address"
    addressM <- case addressTextM of
      Nothing -> pure Nothing
      Just addrText -> parseAddress addrText <&> Just
    valueText :: A.Value <- v .: "value"
    value <- case valueText of
      A.String txt ->  parseValueText txt
      A.Number sci -> pure $ valueFromList [(AdaAssetId, Quantity $ round  sci)]
      _ -> fail "Expected string or number"

    scriptM <- v .:? "script"
    scriptAnyM <- case scriptM of
      Nothing -> pure Nothing
      Just scriptJson -> do
        script <- parseAnyScript scriptJson
        pure $ Just script

   
    datumHashE <- parseData
    shouldEmbed <- v .:? "embedData" .!= True
    let dHashConfigConsidered=if shouldEmbed 
                                then datumHashE
                                else (case datumHashE of
                                  Just (Left datum) -> pure $ pure $ hashScriptData datum
                                  _ -> datumHashE
                                )
    txOutputContent <- case (dHashConfigConsidered, scriptAnyM, addressM) of
      -- If there is no datum hash and no script then use address as script address
      (Nothing, Nothing, Just address) -> pure $ TxOutAddress address value
      -- If there is datum hash but no script then use address as script address
      (Just datum, Nothing, Just address) -> case datum of 
        Left sd -> pure $ TxOutScriptAddressWithData address value sd
        Right ha -> pure $ TxOutScriptAddress address value ha
      -- If there is no datum hash but there is script then it is not supported
      (Nothing, Just scriptAny,_) -> fail "TxOutput must have a data or dataHash if it has a script"
      -- If there is datum hash and script then use script and datahash
      (Just datum, Just scriptAny, Nothing) -> case datum of 
        Left sd -> pure $ TxOutScriptWithData  (TxValidatorScript scriptAny) value sd
        Right ha -> pure $ TxOutScript (TxValidatorScript scriptAny) value ha
      (_,_,_) -> fail "Unsupported output object format it must be address, value, optional datumhash for scriptaddress or script, value, datumHash"

    pure $ TxOutput txOutputContent addChange' deductFee'

    where

      parseData :: Parser  (Maybe (Either (ScriptData) (Hash ScriptData)))
      parseData = do
        dataHashM <- v .:? "datumHash"
        case dataHashM of
          Just dataHash -> do
              case deserialiseFromRawBytes (AsHash AsScriptData) dataHash of
                  Nothing -> pure  Nothing
                  Just dh -> pure $ pure $ pure dh
          Nothing -> case H.lookup "datum" v of
              Just _  -> do
                    val <- v `scriptDataParser` "datum"
                    pure   $ pure $ Right $ hashScriptData  val
              Nothing -> pure  Nothing


  parseJSON _ = fail "TxOutput must be an object"


-- instance FromJSON TxOutputContent where
--   parseJSON (A.Object v) = do
--     addressText <- v .: "address"
--     address <- case deserialiseAddress (AsAddressInEra AsAlonzoEra) addressText of
--       Nothing -> fail $ "Invalid address string: " ++ T.unpack addressText
--       Just aie -> pure aie

--     valueText <- v .: "value"
--     value <- parseValueText valueText

--     datumText <- v .:? "datum"
--     datumHashM <- case datumText of
--       Nothing -> pure Nothing
--       Just dt -> case deserialiseFromRawBytes (AsHash AsScriptData) dt of
--         Nothing -> pure Nothing
--         Just d -> pure $ Just d
--     case datumHashM of
--       Nothing -> pure $ TxOutAddress address value
--       Just dh -> pure $ TxOutScriptAddress address value dh
--     pure $ TxOutAddress address value

--   parseJSON _ = fail "TxOutputContent must be an object"

-- instance FromJSON TxMintingScript where
--   parseJSON (A.Object v) = do
--     scriptJson:: A.Object <- v .: "script"
--     scriptAny <- parseAnyScript $ B.concat $ BL.toChunks $ A.encode scriptJson

--     mintRedeemer <- v .:? "redeemer"
--     exUnits <- v .:? "executionUnits"
--     case mintRedeemer of
--       Nothing -> pure $ TxSimpleScript scriptAny
--       Just (A.String s) -> case parseScriptData s of
--         Nothing -> fail $ "Invalid script data string: " ++ T.unpack s
--         Just d -> pure $ TxPlutusScript scriptAny d exUnits
--       Just _ -> fail "TxMintingScript redeemer must be a string"
--   parseJSON _ = fail "TxMintingScript must be an object"

instance FromJSON TxCollateral where
  parseJSON v = do
      txIn <- txInParser  v
      pure $ TxCollateralTxin txIn


instance FromJSON TxSignature where
  parseJSON (A.String v) = case deserialiseAddress (AsAddressInEra AsAlonzoEra) v of
      Nothing -> fail $ "Invalid address string: " ++ T.unpack v
      Just aie -> pure $ TxSignatureAddr aie


  parseJSON _ = fail "TxSignature must be an String Address or PubKeyHash "