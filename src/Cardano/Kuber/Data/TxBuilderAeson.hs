{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Kuber.Data.TxBuilderAeson
where


import Cardano.Api hiding(txMetadata, txFee)
import Cardano.Api.Shelley hiding (txMetadata, txFee)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Kuber.Util
import Data.Functor ((<&>), ($>))

import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut, CurrencySymbol)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser, parseMaybe, parseEither)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.Types as A

import qualified Data.Text as T
import Cardano.Kuber.Data.Models ( unAddressModal)
import Cardano.Kuber.Data.Parsers (parseSignKey,parseValueText, parseScriptData, parseAnyScript, parseAddress, parseAssetNQuantity, parseValueToAsset, parseAssetId, scriptDataParser, txInParser, parseUtxo, parseTxIn, parseHexString, parseAddressBench32, parseAddressCbor, parseUtxoCbor)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON), ToJSONKey (toJSONKey))
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
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleTextNoPrefix, toConsoleText))

instance FromJSON TxBuilder where
  parseJSON (A.Object v) =
    TxBuilder
      <$> (v .:? "selections" .!=[])
      <*> (v .:? "inputs" .!=[])
      <*> (v.:?"referenceInputs".!=[])
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
        case validateScriptSupportedInEra' BabbageEra scriptAny of
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
        let amountList = A.toList o

        mintValue <- mapM (mapToValue policyId) amountList

        pure $ TxMintData policyId sw (valueFromList mintValue)
      _ -> fail "Mint amount must be a object with key as token name and value as integer."
    where
      mapToValue :: MonadFail m => PolicyId -> (A.Key,A.Value) -> m (AssetId, Quantity)
      mapToValue policyId (tName, amountT) = do
        amount <- parseAmount amountT
        let assetName = fromString $ A.toString tName
        pure (AssetId policyId assetName, Quantity amount)

      parseAmount :: MonadFail m => A.Value -> m Integer
      parseAmount v = case v of
        A.Number sci -> pure $ floor sci
        _ -> fail "Error amount value must be in integer"


      getPolicyIdFromScriptWitness :: ScriptWitness WitCtxMint  BabbageEra  -> PolicyId
      getPolicyIdFromScriptWitness witness = case scriptWitnessScript witness of
        Nothing -> error "Unexpected in era babbage or soemthing"
        Just (ScriptInEra _ script) -> scriptPolicyId  script


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



instance IsString a =>  MonadFail (Either a ) where
  fail msg = Left $ fromString msg

instance ToJSON TxBuilder where
  toJSON (TxBuilder selections inputs refInputs outputs collaterals validityStart validityEnd mintData signatures fee defaultChangeAddr metadata) =
      -- TODO: tojson for input reference
    A.object $ nonEmpyPair

    where
    appendNonEmpty :: (Foldable t, KeyValue a1, ToJSON (t a2)) => A.Key -> t a2 -> [a1] -> [a1]
    appendNonEmpty  key  val obj   =  if null val then obj else (key .= val) : obj

    nonEmpyPair :: [A.Pair]
    nonEmpyPair =  "selections"     >= selections
              <+>  "inputs"         >= inputs
              <+>  "referenceInputs">= map (\(TxInputReference tin) ->renderTxIn tin) refInputs
              <+>  "collaterals"    >= collaterals
              <+>  "mint"           >= mintData
              <+>  "output"         >= outputs
              <+>  "validityStart"  >= validityEnd
              <+>  "validityEnd"    >= validityEnd
              <+>  "signatures"     >= signatures
              <+>  "fee"            >= fee
              <+>  "changeAddress"  >= defaultChangeAddr
              <#>  "metadata"       >= metadata

    infixl 8 >=
    (>=) a b = appendNonEmpty a b
    infixr 7 <#>
    (<#>)  f  f2  =  f $ f2 []
    infixr 6 <+>
    (<+>) f  v = f v

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
  toJSON (TxSelectableUtxos  utxo) =  utxoToAeson  utxo
  toJSON (TxSelectableSkey  sk) = A.Array $ V.fromList $ map (A.String . serialiseToBech32)  sk

instance ToJSON TxInput where
  toJSON (TxInputUnResolved txInputTxin) = toJSON txInputTxin
  toJSON (TxInputResolved val) = toJSON val
instance ToJSON TxInputUnResolved_ where
  toJSON (TxInputTxin txin) = toJSON txin
  toJSON (TxInputReferenceScriptTxin  _refScript _mData _redeemer _exUnits _txin ) = A.object $ [
        "inlineDatum" .= isNothing _mData,
        "txin" .= renderTxIn _txin
      ] ++ (case _exUnits of
    Nothing -> []
    Just e@(ExecutionUnits mem step) -> ["exUnits" .= e ] )
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
    TxInputScriptUtxo tvs sd sd' m_eu utxo ->  utxoToAeson  utxo
    TxInputReferenceScriptUtxo refTxin mData r mExunit utxo -> utxoToAeson utxo

instance ToJSON TxValidatorScript where
  toJSON (TxValidatorScript script) = A.String $ T.pack $ show script

instance ToJSON ScriptData where
  toJSON scriptData = scriptDataToJson ScriptDataJsonNoSchema scriptData

instance ToJSON TxOutput where
  toJSON (TxOutput _content  _deductFee _addChange) =
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

instance ToJSON TxCollateral where
  toJSON (TxCollateralTxin _collateral) = toJSON _collateral

  toJSON (TxCollateralUtxo utxo) = utxoToAeson  utxo


instance ToJSON TxSignature where
  toJSON (TxSignatureAddr _sigAddr) = toJSON _sigAddr
  toJSON _ = "TxSignaturePkh Not implemented."

instance FromJSON TxInputSelection where
  parseJSON v@(A.String s) = do
    case parseHexString s of
      Just str -> case parseAddressCbor  str of
        Nothing -> case parseUtxoCbor str of
          Just utxo ->  pure $ TxSelectableUtxos  utxo
          Nothing -> fail $ "Invalid InputSelection Hex:  It must be  address, txHash#index or  utxoCbor"
        Just addr -> pure $ TxSelectableAddresses  [addr]
      Nothing -> case parseAddressBench32 s  of
        Just addr -> pure $ TxSelectableAddresses  [addr]
        Nothing -> case parseTxIn s of
          Just txin -> pure $ TxSelectableTxIn  [txin]
          Nothing -> case parseSignKey s of
            Just s -> pure $ TxSelectableSkey [s]
            Nothing -> fail $ "Invalid InputSelection String : It must be  address, txHash#index,  or  utxoCbor"

    -- if "addr" `T.isPrefixOf` s
    --   then do
    --     case deserialiseAddress (AsAddressInEra AsBabbageEra) s of
    --       Nothing -> fail $ "Invalid address string: " ++ T.unpack s
    --       Just aie -> pure $ TxSelectableAddresses [aie]
    --   else do
    --       case  parseTxIn  s of
    --         Just txin -> pure $ TxSelectableTxIn [txin]
    --         Nothing -> case parseUtxo  s of
    --           Just utxo -> pure $ TxSelectableUtxos utxo
    --           Nothing -> fail $ "

  parseJSON v = do
    txIn <- txInParser  v
    pure $ TxSelectableTxIn [txIn]

instance FromJSON TxInput where
  parseJSON o@(A.Object v) = do
    utxo <- v .: "utxo"
    txIn <- txInParser  utxo
    mScript :: (Maybe A.Value) <- v .:? "script"
    case mScript of
      Nothing ->  pure $ TxInputUnResolved $ TxInputTxin txIn
      Just scriptJson -> do
        inputFunc<-case scriptJson of 
          A.String txt -> parseTxIn txt <&> TxInputReferenceScriptTxin
          _  ->  parseAnyScript ( B.concat $ LBS.toChunks $ A.encode scriptJson ) 
                  <&> TxValidatorScript 
                  <&> TxInputScriptTxin 
        mData <- v .:? "datum"
        redeemer <- v .: "redeemer" >>= scriptDataParser
        _exUnits <- v .:? "executionUnits"
        exUnits <- case _exUnits of
          Nothing -> v .:? "exUnits"
          Just eu -> pure eu
        mParsedData <- case  mData of
          Nothing -> pure Nothing
          Just datum -> do
            sd <-scriptDataParser  datum
            pure $ Just sd
        pure $ TxInputUnResolved $ inputFunc mParsedData  redeemer exUnits txIn

  parseJSON v@(A.String s) = do
    case parseHexString s of
      Just str -> case parseAddressCbor  str of
        Nothing -> case parseUtxoCbor str of
          Just utxo ->  pure $ TxInputResolved $ TxInputUtxo utxo
          Nothing -> fail $ "Invalid Inpu HexString : It must be  address, txHash#index or  utxoCbor"
        Just addr -> pure $ TxInputUnResolved $ TxInputAddr  addr
      Nothing -> case parseAddressBench32 s  of
        Just addr -> pure $ TxInputUnResolved $ TxInputAddr  addr
        Nothing -> case parseTxIn s of
          Just txin -> pure $ TxInputUnResolved $ TxInputTxin txin
          Nothing -> fail $ "Invalid Input String : It must be  address, txHash#index or  utxoCbor"

  parseJSON _ = fail "TxInput must be an object or string"

instance FromJSON TxInputReference where
  parseJSON (A.String v)  =parseTxIn v <&> TxInputReference 
  parseJSON _             = fail "Expected InputReference string" 

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
        script <- parseAnyScript (T.encodeUtf8  scriptJson)
        pure $ Just script


    datumHashE <- parseData
    shouldEmbed <- v .:? "inline" .!= True
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

    pure $ TxOutput txOutputContent deductFee' addChange'

    where
      parseData :: Parser  (Maybe (Either ScriptData (Hash ScriptData)))
      parseData = do
        mDatum <- v .:? "datum"
        case mDatum of
          Just datum -> do
                  val <- scriptDataParser datum
                  pure $ pure $ Left $  val
          Nothing ->do
            datumHashM <- v .:? "datumHash"
            case datumHashM of
              Just dHash ->case deserialiseFromRawBytesHex (AsHash AsScriptData) (T.encodeUtf8 dHash) of
                  Left e -> fail "Expected hex string "
                  Right dh -> pure $ pure $ pure dh
              Nothing -> pure Nothing


  parseJSON _ = fail "TxOutput must be an object"


-- instance FromJSON TxOutputContent where
--   parseJSON (A.Object v) = do
--     addressText <- v .: "address"
--     address <- case deserialiseAddress (AsAddressInEra AsBabbageEra) addressText of
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
  parseJSON v@(A.Object o) =do
      txIn <- txInParser  v
      pure $ TxCollateralTxin txIn
  parseJSON v@(A.String s) = case parseUtxo s of
        Just x -> pure $ TxCollateralUtxo x
        Nothing -> parseTxIn s <&> TxCollateralTxin
  parseJSON _ = fail "Expected Collateral to be TxHash#index or utxoCbor or txIn object"


instance FromJSON TxSignature where
  parseJSON (A.String v) = case parseAddress v of
      Nothing -> case parseSignKey v of
        Just sk -> pure $ TxSignatureSkey sk
        Nothing -> fail $ "Invalid address string: " ++ T.unpack v
      Just aie -> pure $ TxSignatureAddr aie


  parseJSON _ = fail "TxSignature must be an String Address or PubKeyHash "


utxoToAeson (UTxO uMap) = A.Object $  A.fromList  $ map  (\(k,TxOut addr val dh _ ) ->
         A.fromString  (toConsoleTextNoPrefix k)
         .=
           Aeson.object [ "address" .=  serialiseAddress addr,
                          "value"   .= toVal val]) $ Map.toList uMap
    where
      toVal (TxOutValue _ v)= v
      toVal (TxOutAdaOnly _ l) =lovelaceToValue l
