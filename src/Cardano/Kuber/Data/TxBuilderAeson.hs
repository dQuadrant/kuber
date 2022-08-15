{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Kuber.Data.TxBuilderAeson
where


import Cardano.Api hiding(txMetadata, txFee)
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra), ReferenceScript (ReferenceScript, ReferenceScriptNone))
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
import Data.Maybe (mapMaybe, catMaybes, isNothing, fromMaybe)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V2.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut, CurrencySymbol)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser, parseMaybe, parseEither)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.Types as A

import qualified Data.Text as T
import Cardano.Kuber.Data.Models ( unAddressModal)
import Cardano.Kuber.Data.Parsers (parseSignKey,parseValueText, parseScriptData, parseAnyScript, parseAddress, parseAssetNQuantity, parseValueToAsset, parseAssetId, scriptDataParser, txInParser, parseUtxo, parseTxIn, parseHexString, parseAddressBench32, parseAddressCbor, parseUtxoCbor, anyScriptParser, parseAssetName, parseCborHex)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON), ToJSONKey (toJSONKey), fromJSON)
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
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative ((<|>))
import Data.Bifunctor (second)

instance FromJSON TxBuilder where
  parseJSON (A.Object v) =
    TxBuilder
      <$> (v .?< "selection")
      <*> v .?<  "input"
      <*> v .?< "referenceInput"
      <*> v .?< "output"
      <*> v .?< "collateral"
      <*> v .:? "validityStart"
      <*> v .:? "validityEnd"
      <*> v .?< "mint"
      <*> v .?< "signature"
      <*> v .:? "fee"
      <*> (v .:? "changeAddress" <&> fmap unAddressModal)
      <*> (v.:? "metadata" .!= Map.empty)
    where
      (.?<) :: FromJSON v=> A.Object -> T.Text -> Parser [v]
      (.?<) obj key = do
        mVal1<- obj .:? A.fromText key
        case mVal1 of
          Nothing -> do
            mVal2<- obj .:? A.fromText  (key <>"s")
            case mVal2 of
              Nothing -> pure []
              Just any -> returnVal any
          Just val  -> returnVal val

      returnVal  val =  case val of
            A.Array vec -> mapM parseJSON $ V.toList vec
            _ ->  do
              v<- parseJSON  val
              pure [v]

  parseJSON _ = fail "TxBuilder must be an object"

instance FromJSON (TxMintData TxMintingScriptSource) where
  parseJSON (A.Object v) = do
    mintAmountm <- v .:? "amount"
    mintAmount_ <-case mintAmountm of
      Nothing ->  v .: "tokens"
      Just any -> pure any

    exUnitsM <- v .:? "executionUnits"
    mintAmount <-  mapM (\(n,a :: A.Value) -> (do
              v<- parseAssetName n
              (q,meta) <- case a of
                A.Object km -> do
                  mintAmount <- km .:? "amount" .!=1
                  meta_ <- km .:? "meta"
                  meta <-case meta_ of
                    Nothing -> km .:? "metadata" .!=mempty
                    Just any -> pure any
                  pure (mintAmount,meta)
                A.Number sci ->
                  pure (Quantity $ round sci,mempty)
                _ -> fail "expected token value to be mint amount or object"
              pure (v,q,meta)
               )) $ Map.toList mintAmount_
    let quantities = map (\(t,q,_)-> (t,q)) mintAmount
        metaList ::  [Map Word64  (Map AssetName A.Value)]
        metaList =  map (\(t,_,m) -> morphMetaMap t m ) mintAmount
        metaMap =   foldl (Map.unionWith (<>))   mempty metaList
        morphMetaMap token mp =Map.fromList $  map (second (Map.singleton token))  (Map.toList  mp)
    let doReturn c = pure $ TxMintData  c  quantities  metaMap
    scriptAny <- v .: "script"

    case scriptAny of
      A.String  s-> do
        txin <- parseTxIn s
        mintRedeemerObj <- v .:? "redeemer"
        mintRedeemer <- case mintRedeemerObj of
              Nothing -> pure Nothing
              Just any -> do
                d<- scriptDataParser any
                pure $ pure d
        doReturn $ TxMintingReferenceScript txin  exUnitsM mintRedeemer
      A.Object  obj ->do
          txScript<- parseJSON scriptAny
          case   txScript of
            TxScriptPlutus psc ->do
              mintRedeemer <- v .: "redeemer" >>= scriptDataParser
              doReturn $ TxMintingPlutusScript psc exUnitsM mintRedeemer
            TxScriptSimple simpleSc -> doReturn $ TxMintingSimpleScript simpleSc
      _  -> fail "Either ReferenceInput or Script Object expected "

  parseJSON _ = fail "TxMintData must be an object"

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
              <+>  "outputs"         >= outputs
              <+>  "validityStart"  >= validityStart
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

instance ToJSON (TxMintData TxMintingScriptSource) where
  toJSON (TxMintData script value metadata) =
    A.object
      [
      "script" .= (case script of
        TxMintingPlutusScript tps m_eu sd ->toJSON  tps
        TxMintingReferenceScript ti m_eu m_sd -> A.String $  renderTxIn ti
        TxMintingSimpleScript tss -> toJSON tss)
      ,"amount" .=  Map.fromList value
      , "metadata" .= metadata
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

instance ToJSON TxPlutusScript where
  toJSON tps =case tps of
      TxPlutusScriptV1 ps -> toJSON  $ serialiseToTextEnvelope Nothing  ps
      TxPlutusScriptV2 ps -> toJSON  $ serialiseToTextEnvelope Nothing  ps

instance FromJSON  TxPlutusScript where
  parseJSON (A.Object o) = do
    _type :: T.Text <- o  .: "type"
    case _type of
      "PlutusScriptV1" -> o.: "cborHex"  >>= parseCborHex @T.Text <&> TxPlutusScriptV1
      "PlutusScriptV2" -> o.: "cborHex"  >>= parseCborHex @T.Text <&> TxPlutusScriptV2
      _ -> fail "Expected either PlutsScriptV1 or PlutusScriptV2 type"
  parseJSON  _ = error "Expected Object"

instance FromJSON  TxScript where
  parseJSON v@(A.Object o) =do
    tryPlutus <|> trySimple
    where
      tryPlutus = parseJSON v <&> TxScriptPlutus
      trySimple = do
        v ::(SimpleScript SimpleScriptV2 ) <- parseJSON v
        pure $ TxScriptSimple $TxSimpleScriptV2 v

  parseJSON  _ = error "Expected Object"

instance ToJSON TxSimpleScript where
  toJSON tss = case tss of
    TxSimpleScriptV1 ss -> toJSON ss
    TxSimpleScriptV2 ss -> toJSON ss

instance ToJSON TxScript where
  toJSON script = case script of
    TxScriptSimple tss -> toJSON tss
    TxScriptPlutus tps -> toJSON tps


instance ToJSON ScriptData where
  toJSON scriptData = scriptDataToJson ScriptDataJsonNoSchema scriptData


instance ToJSON (TxOutput TxOutputContent)  where
  toJSON (TxOutput _content  _deductFee _addChange onMinAda ) =
    A.object  $
      outputContentJsonPair _content
      ++ ["addChange" .= _addChange | _addChange]
      ++ (["deductFee" .= _deductFee | _deductFee])
      ++ (case onMinAda of
    DropOnUtxoInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "drop"]
    IncreaseOnUtxoInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "increase"]
    ErrorOnInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "error"]
    OnInsufficientUtxoAdaUnset -> []
    )
outputContentJsonPair :: KeyValue a => TxOutputContent -> [a]

outputContentJsonPair v = case v of
  TxOutPkh pkh va -> [
      "pkh" .= show pkh
    , "value" .= va]
  TxOutScript tps va ha -> [
      "script" .= tps
    , "value" .= va
    , "dataHash" .= ha
    ]
  TxOutScriptInline tps va ha -> [
      "script" .= tps
    , "value" .= va
    , "data" .= ha]
  TxOutScriptWithScript tps va ha ts -> [
      "address" .= tps,
      "script"  .= ts
    , "value" .= va
    , "data" .= ts]
  TxOutScriptWithData script va sd -> [
      "script" .= script
    , "value" .= va
    , "data" .= sd]
  TxOutScriptWithDataAndScript tps va sd ts -> [
      "address" .= tps,
      "script" .= ts
    , "value" .= va
    , "data" .= sd]
  TxOutScriptWithDataAndReference tps va sd -> [
      "script" .= tps
    , "value" .= va
    , "data" .= sd]
  TxOutNative (TxOut addr (TxOutValue _ v) txoutData refScript) ->
        ["address" .= addr]
    ++  ["value" .= v]
    ++ (case txoutData of
      TxOutDatumHash sdsie ha -> ["datumHash" .= serialiseToRawBytesHexText ha ]
      TxOutDatumInline rtisidsie sd -> ["datum" .= sd]
      _ -> []
      )
    ++ (case refScript of
     ReferenceScript rtisidsie sial -> ["referenceScript" .= sial ]
     ReferenceScriptNone -> [])
  TxOutNative _ -> error "Unexpected"

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
          Nothing -> fail "Invalid InputSelection Hex:  It must be  address, txHash#index or  utxoCbor"
        Just addr -> pure $ TxSelectableAddresses  [addr]
      Nothing -> case parseAddressBench32 s  of
        Just addr -> pure $ TxSelectableAddresses  [addr]
        Nothing -> case parseTxIn s of
          Just txin -> pure $ TxSelectableTxIn  [txin]
          Nothing -> case parseSignKey s of
            Just s -> pure $ TxSelectableSkey [s]
            Nothing -> fail "Invalid InputSelection String : It must be  address, txHash#index,  or  utxoCbor"
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
          _  ->  parseJSON   scriptJson
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

instance FromJSON InsufficientUtxoAdaAction where
  parseJSON (A.String _v)
        | v == "drop"                   = pure DropOnUtxoInsufficientUtxoAda
        | v == "increase" || v == "fix" = pure IncreaseOnUtxoInsufficientUtxoAda
        | v == "error"                  = pure ErrorOnInsufficientUtxoAda
        | otherwise  = fail $ "Expected one of drop, increase or error, got: " ++ T.unpack v
        where
          v = T.toLower _v
  parseJSON A.Null = pure OnInsufficientUtxoAdaUnset
  parseJSON _             = fail "Expected InsufficientUtxoAdaAction string : 'drop', 'increase' or 'error'"


instance FromJSON (TxOutput TxOutputContent ) where
  parseJSON json@(A.Object v) = do
    -- Parse TxOutput according to address type if simple
    -- then use address with value TxOutAddress otherwise use TxOutScriptAddress or TxOutScript
    -- which has script and data if there is no script given then take address
    -- as script address and get data hash from the datum or directly from dataHash
    -- if there is script given then use that script and datahash
    addChange' <- v .:? "addChange" .!= False
    deductFee' <- v .:? "deductFee" .!= False
    insuffientUtxoAda <- v.:? "insuffientUtxoAda" .!=OnInsufficientUtxoAdaUnset
    addressTextM  :: Maybe A.Value <- v .:? "address"

    destination :: Maybe (Either (AddressInEra  BabbageEra) TxPlutusScript) <- case addressTextM of
      Nothing -> pure Nothing
      Just v@(A.Object  o) -> do
        v<- parseJSON  v
        pure$ pure $ pure v
      Just (A.String addrTxt) -> do
        val <- parseAddress addrTxt
        pure $ pure $ Left val
      _ -> error "Unexpected value"
    maybeVal :: Maybe  A.Value <- v .:? "value"

    value <- case maybeVal of
        Just (A.String txt) ->  parseValueText txt
        Just (A.Number sci) -> pure $ valueFromList [(AdaAssetId, Quantity $ round  sci)]
        Just val -> parseJSON  val
        _ -> pure $ valueFromList []
    datumHashE <- parseData
    shouldEmbed <- v .:? "inline" .!= True
    let txOutDatum=if shouldEmbed
                                then case datumHashE of
                                  Nothing -> TxOutDatumNone
                                  Just (Left sd) -> TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra sd
                                  Just (Right dh) -> TxOutDatumHash ScriptDataInBabbageEra dh
                                else (case datumHashE of
                                  Just (Left datum) -> TxOutDatumHash ScriptDataInBabbageEra (hashScriptData datum)
                                  Just (Right dh)   -> TxOutDatumHash ScriptDataInBabbageEra dh
                                  _ -> TxOutDatumNone
                                )
    mScript_ <- v .:? "script"
    mScript <- case mScript_ of
      Nothing ->  v .:? "inlineScript"
      Just va -> pure mScript_
    -- based on outputAddress and inlineScript determine what to do. (Automatic calculation  of destination address
    -- from script code has increased the code drastically )
    output <-  case destination of
          Nothing -> case mScript of
            Nothing -> fail "missing  both inlineScript and address"
            Just obj -> do
              sc <- parseJSON   obj
              case txOutDatum of
                TxOutDatumNone -> fail "Missing datum in script output"
                TxOutDatumHash sdsie ha ->  pure $ TxOutScriptInline sc value ha
                TxOutDatumInline rtisidsie sd ->pure $  TxOutScriptWithDataAndReference sc  value sd
                _  -> error "Unexpected"
          Just (Left addr) -> case mScript of
            Nothing ->  pure $ TxOutNative $ TxOut addr (TxOutValue MultiAssetInBabbageEra value) txOutDatum ReferenceScriptNone
            Just (A.Bool _) ->  pure $ TxOutNative $ TxOut addr (TxOutValue MultiAssetInBabbageEra value) txOutDatum ReferenceScriptNone
            Just obj -> do
              sc <- anyScriptParser  obj
              pure $ TxOutNative $ TxOut addr (TxOutValue MultiAssetInBabbageEra value) txOutDatum (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra sc)
          Just (Right script) ->
            let defaultAct = case txOutDatum of
                  TxOutDatumNone -> fail "Missing datum in script output"
                  TxOutDatumHash sdsie ha ->  pure $ TxOutScript script value ha
                  TxOutDatumInline rtisidsie sd ->pure $  TxOutScriptWithData script  value sd
                  _  -> error "Unexpected"
            in case mScript of
              Nothing -> defaultAct
              Just (A.Bool False) -> defaultAct
              Just (A.Bool True) ->  case txOutDatum of
                  TxOutDatumNone -> fail "Missing datum in script output"
                  TxOutDatumHash sdsie ha ->  pure $ TxOutScriptInline script value ha
                  TxOutDatumInline rtisidsie sd ->pure $  TxOutScriptWithDataAndReference script  value sd
                  _  -> error "Unexpected"
              Just va -> do
                sc <- parseJSON   va
                case txOutDatum of
                  TxOutDatumNone -> fail "Missing datum in script output"
                  TxOutDatumHash sdsie ha ->  pure $ TxOutScriptWithScript script value ha sc
                  TxOutDatumInline rtisidsie sd ->pure $  TxOutScriptWithDataAndScript script  value sd sc
                  _  -> error "Unexpected"
    pure $ TxOutput output deductFee' addChange' insuffientUtxoAda
    where

      parseData :: Parser  (Maybe (Either ScriptData (Hash ScriptData)))
      parseData = do
        mDatum <- v .:? "datum"
        case mDatum of
          Just datum -> do
                  val <- scriptDataParser datum
                  pure $ pure $ Left $  val
          Nothing ->do
            datumHashM_ <- v .:? "datumHash"
            datumHashM <-case datumHashM_ of
              Nothing ->  v .:? "dataHash"
              Just any -> pure datumHashM_
            case datumHashM of
              Just dHash ->case deserialiseFromRawBytesHex (AsHash AsScriptData) (T.encodeUtf8 dHash) of
                  Left e -> fail "Expected hex string "
                  Right dh -> pure $ pure $ pure dh
              Nothing -> pure Nothing


  parseJSON _ = fail "TxOutput must be an object"


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


utxoToAeson (UTxO uMap) = Aeson.object  $ map  (\(k,TxOut addr val dh _ ) ->
         A.fromString  (toConsoleTextNoPrefix k)
         .=
           Aeson.object [ "address" .=  serialiseAddress addr,
                          "value"   .= toVal val]) $ Map.toList uMap
    where
      toVal (TxOutValue _ v)= v
      toVal (TxOutAdaOnly _ l) =lovelaceToValue l
