{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Kuber.Data.Parsers where

import           Cardano.Api
import Cardano.Api.Shelley ( fromShelleyAddr, fromShelleyTxOut )
import           Cardano.Binary               (FromCBOR (fromCBOR), decodeFull)
import           Cardano.Kuber.Utility.Text
import qualified Cardano.Ledger.Babbage       as Babbage
import           Cardano.Ledger.DescribeEras  (StandardCrypto, Witness (Alonzo))
import qualified Cardano.Ledger.Mary.Value    as Mary
import qualified Cardano.Ledger.Shelley.API   as Shelley
import           Control.Exception            (SomeException, catch, throw)
import           Data.Aeson                   ((.:), (.:?))
import qualified Data.Aeson                   as A
import qualified Data.Aeson.KeyMap            as A
import qualified Data.Aeson.Key as A

import           Data.Aeson.Types             (parseMaybe)
import qualified Data.Aeson.Types             as Aeson
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Char                    (isDigit, isSeparator)
import           Data.Functor                 ((<&>))
import qualified Data.HashMap.Internal.Strict as H
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Conversions        (Base16 (Base16, unBase16),
                                               FromText (fromText),
                                               ToText (toText), convertText)
import           Data.Text.Encoding           (encodeUtf8)
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Encoding           as TSE
import           Debug.Trace                  (traceM)
import           GHC.IO.Exception             (IOErrorType (UserError),
                                               IOException (IOError))
import           Text.Read                    (readMaybe)
import qualified Debug.Trace as Debug


parseSignKey :: MonadFail m => Text -> m (SigningKey PaymentKey)
parseSignKey txt
  | T.null txt = fail "Empty value for SignKey"
  | T.head txt /= '{' =
    case deserialiseFromBech32 (AsSigningKey AsPaymentKey) txt of
      Left ide -> case convertText txt <&> unBase16 of
        Nothing -> fail "SignKey is neither Bench32 nor Hex encoded"
        Just bs -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) bs of
          Left de   -> fail "SignKey is neither Bench32 nor CBOR encoded"
          Right sk' -> pure sk'
      Right sk -> pure sk
  | otherwise = case A.eitherDecode (fromStrict $ encodeUtf8 txt) of
    Right (TextEnvelope _ _ cborHex) -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) cborHex of
      Left de   -> fail "TextEnvelope doesn't contain cborHex for signKey"
      Right sk' -> pure sk'
    Left err -> fail err


 -- parse plain text for assetName
parseAssetId :: MonadFail m => T.Text -> m AssetId
parseAssetId assetText
  | T.null policy = if T.null tokenNameWithDot || T.null tokenName then pure AdaAssetId else fail "TokenName provided for AdaAssetId"
  | T.null tokenNameWithDot = parseCombined policy
  | T.null tokenName = parseCombined policy
  | otherwise = parsePair policy tokenName
  where
      (policy, tokenNameWithDot) = T.break (== '.') $ T.strip assetText
      tokenName = T.tail tokenNameWithDot
      parseCombined combined
        | T.toLower combined == "lovelace" = pure AdaAssetId
        | T.toLower combined == "l" = pure AdaAssetId
        | T.length policy < 56 = fail "ParserError : Too short input for assetId"
        | otherwise =  parsePair (T.take 56 policy ) (T.drop 56 policy)
      parsePair policyText nameText=do
              let policyId = deserialiseFromRawBytesHex AsPolicyId $ encodeUtf8 policyText
              let assetName = case deserialiseFromRawBytesHex AsAssetName $ encodeUtf8 nameText
                              of
                                Left _  -> deserialiseFromRawBytes AsAssetName $ encodeUtf8 nameText
                                Right an -> pure an
              case policyId of
                Left _  -> fail "ParserError : Invalid policy name"
                Right  pi -> case assetName of
                  Nothing -> fail "ParserError : Invalid token name"
                  Just an -> pure $ AssetId pi an

parseValueText :: MonadFail f => Text -> f Value
parseValueText valueTxt =
  mapM parseAssetNQuantity (T.split (== '+') $ T.strip valueTxt)
    <&> valueFromList

parseValueToAsset :: MonadFail f => Text -> f [(AssetId, Quantity)]
parseValueToAsset valueTxt =
  mapM parseAssetNQuantity (T.split (== '+') $ T.strip valueTxt)

parseAssetNQuantity :: MonadFail f => Text -> f (AssetId, Quantity)
parseAssetNQuantity textStr = do
  let (amountTxt, assetTxt) = performBreak
      assetTxtStripped = T.strip assetTxt
  case readMaybe (T.unpack amountTxt) of
    Just iAmount -> do
      case parseAssetId assetTxtStripped of
        Just asset -> pure (asset, Quantity iAmount)
        _ ->
          convertAdaOrLovelace
            assetTxtStripped
            iAmount
            (fail $ "Invalid AssetId in value `" ++ T.unpack textStr ++ "`")
    _ ->case parseAssetId textStr of
          Just asset -> pure (asset, Quantity 1)
          _ -> do
            let (newAmountTxt, postfix) = T.span isDigit amountTxt
            case readMaybe (T.unpack newAmountTxt) of
              Just iAmount -> convertAdaOrLovelace postfix iAmount parseFail
              _            -> parseFail
  where
    convertAdaOrLovelace str iAmount _otherwise = case T.unpack (T.toLower str) of
      "ada"      -> lovelaceAmount (iAmount * 1_000_000)
      "a"        -> lovelaceAmount (iAmount * 1_000_000)
      "l"        -> lovelaceAmount iAmount
      "lovelace" -> lovelaceAmount iAmount
      _          -> _otherwise
    lovelaceAmount v = pure (AdaAssetId, Quantity v)
    parseFail = fail $ "Value parse failed : invalid value token `" ++ T.unpack textStr ++ "`"
    performBreak = T.break isSeparator (T.stripStart textStr)

parseScriptData :: MonadFail m => Text -> m ScriptData
parseScriptData jsonText = do
  case decodeJson of
    Nothing -> fail "Script data string must be json format"
    Just v -> case scriptDataFromJson ScriptDataJsonDetailedSchema v of
      Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $ "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $ "Invalid data " ++ show sdre
      Right sd -> pure sd
  where
    decodeJson = A.decode $ fromStrict $ TSE.encodeUtf8 jsonText


parseAnyScript :: MonadFail m => ByteString -> m ScriptInAnyLang
parseAnyScript jsonText =
  case deserialiseFromJSON AsTextEnvelope jsonText of
    Left _ ->
      case deserialiseFromJSON (AsSimpleScript AsSimpleScriptV2) jsonText of
        Left err -> do
          case deserialiseFromCBOR (AsScript AsPlutusScriptV1) jsonText of
            Left de -> fail "Cannot parse the script hex as script"
            Right sc -> pure $ ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) sc
        Right script -> pure $ toMinimumSimpleScriptVersion script
    Right te ->
      case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
        Left err     -> fail "Error while decoding script text envelope"
        Right script -> pure script
  where
    textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
    textEnvTypes =
      [ FromSomeType
          (AsScript AsSimpleScriptV1)
          (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)),
        FromSomeType
          (AsScript AsSimpleScriptV2)
          (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)),
        FromSomeType
          (AsScript AsPlutusScriptV1)
          (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1)),
        FromSomeType
          (AsScript AsPlutusScriptV2)
          (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2))
      ]

    toMinimumSimpleScriptVersion ::
      SimpleScript SimpleScriptV2 ->
      ScriptInAnyLang
    toMinimumSimpleScriptVersion s =
      case adjustSimpleScriptVersion SimpleScriptV1 s of
        Nothing ->
          ScriptInAnyLang
            (SimpleScriptLanguage SimpleScriptV2)
            (SimpleScript SimpleScriptV2 s)
        Just s' ->
          ScriptInAnyLang
            (SimpleScriptLanguage SimpleScriptV1)
            (SimpleScript SimpleScriptV1 s')

parseAddressBinary :: MonadFail m => ByteString -> m (AddressInEra BabbageEra )
parseAddressBinary bs = case parseAddressCbor (LBS.fromStrict bs) of
  Just addr -> pure addr
  Nothing -> parseAddressRaw bs

parseAddressRaw :: MonadFail m =>  ByteString -> m(AddressInEra BabbageEra)
parseAddressRaw addrBs = case deserialiseFromRawBytes (AsAddressInEra AsBabbageEra) addrBs of
            Nothing -> fail  "Invalid Address Hex "
            Just addr -> pure addr

parseAddress :: MonadFail m => Text -> m (AddressInEra BabbageEra)
parseAddress addrText = case deserialiseAddress (AsAddressInEra AsBabbageEra) addrText of
  Nothing -> do
     let hex :: Maybe  ByteString = parseHexString addrText
     case  hex of
      Just hex -> case parseAddressBinary hex of
        Just addr -> pure addr
        Nothing -> case deserialiseFromRawBytes (AsAddressInEra AsBabbageEra) hex of
            Nothing -> fail  $ "Address is neither bench32 nor cborHex : "++ T.unpack addrText
            Just addr -> pure addr
      Nothing -> fail $ "Address is neither bench32 nor cborHex : "++ T.unpack addrText
  Just aie -> pure aie

parseAddressCbor :: MonadFail m => LBS.ByteString -> m (AddressInEra BabbageEra)
parseAddressCbor  cbor = do
  aie <-  parseCbor cbor
  pure $ fromShelleyAddr ShelleyBasedEraBabbage   aie

parseAddressBench32 :: MonadFail m => Text -> m (AddressInEra BabbageEra)
parseAddressBench32 txt = case deserialiseAddress (AsAddressInEra AsBabbageEra) txt of
  Nothing -> fail $ "Address is not in bench32 format"
  Just aie -> pure aie

scriptDataParser :: MonadFail m =>  Aeson.Value  -> m ScriptData
scriptDataParser v = doParsing v
  where
    doParsing (Aeson.String v) =  parseScriptData  v
    doParsing (Aeson.Object o )=case scriptDataFromJson ScriptDataJsonDetailedSchema  (Aeson.Object o) of
       Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $  "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $  "Invalid data " ++ show sdre
       Right sd -> pure  sd
    doParsing _  = fail "Script data Must be either string or object"


txInParser :: Aeson.Value -> Aeson.Parser TxIn
txInParser  (Aeson.Object o) = do
  txid' <- o .:? "hash"
  txid'' <- o .:? "txid"
  txid''' <- o.:? "txId"
  txid <-case txid' of
    Nothing -> case txid'' of
        Nothing -> case txid''' of
          Nothing -> o.: "txId"
          Just ti -> pure ti
        Just txid -> pure txid
    Just txid -> pure txid
  index <- o .: "index"
  pure $ TxIn txid index
txInParser (Aeson.String v) =parseTxIn v
txInParser _ = fail "Expected Utxo to be of type Object or String"


parseTxIn :: MonadFail m =>  Text -> m TxIn
parseTxIn txt = do
  case T.split (== '#') txt of
      [txHash, index] ->
        case deserialiseFromRawBytesHex AsTxId (TSE.encodeUtf8 txHash) of
          Right txid -> case readMaybe (T.unpack index) of
            Just txindex -> pure $ TxIn txid (TxIx txindex)
            Nothing      -> fail $ "Failed to parse txIndex in " ++ T.unpack txt
          Left _ -> fail $ "Failed to parse value as txHash " ++ T.unpack txHash
      _ -> fail $ "Expected to be of format 'txId#index' got :" ++ T.unpack txt

parseUtxo :: MonadFail m => Text -> m (UTxO BabbageEra )
parseUtxo v =parseHexString v >>= parseUtxoCbor

parseUtxoCbor :: MonadFail m => LBS.ByteString -> m (UTxO BabbageEra)
parseUtxoCbor val = do
  ((txHash,index ),txout) <- parseCbor val
  txIn <- case deserialiseFromRawBytes AsTxId txHash of
          Just txid -> pure $ TxIn txid (TxIx index)
          Nothing -> fail $ "Failed to parse value as txHash " ++ toHexString  txHash
  pure $ UTxO (Map.singleton txIn (fromShelleyTxOut ShelleyBasedEraBabbage txout))


parseTxOut :: MonadFail m => Text -> m (TxOut CtxTx   BabbageEra)
parseTxOut  val = decodeCbor <&> fromShelleyTxOut ShelleyBasedEraBabbage
  where
    decodeCbor :: MonadFail m => m (Babbage.TxOut (Babbage.BabbageEra StandardCrypto))
    decodeCbor = parseHexString  val >>= parseCbor


parseHexString :: (FromText (Maybe (Base16 a1)), ToText a2, MonadFail f) =>a2 -> f a1
parseHexString  v = case unHex v  of
  Just bs ->pure bs
  Nothing -> fail $ "Invalid Hex string: " ++ convertText v

parseCbor :: (FromCBOR a, MonadFail m) =>LBS.ByteString -> m a
parseCbor  v = case decodeFull v of
  Left de   -> fail $  "Not in required cbor format: "++ show de
  Right any -> pure any

parseCborHex :: (ToText a2, MonadFail m, FromCBOR b) => a2 -> m b
parseCborHex  v = parseHexString v >>=parseCbor
