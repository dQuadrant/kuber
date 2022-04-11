{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Contrib.Kubær.Parsers where

import Cardano.Api
import Cardano.Contrib.Kubær.Error
import Control.Exception (SomeException, catch, throw)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Char (isDigit, isSeparator)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Conversions (Base16 (unBase16), convertText)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TSE
import GHC.IO.Exception (IOErrorType (UserError), IOException (IOError))
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import Debug.Trace (traceM)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Internal.Strict as H

parseSignKey :: MonadFail m => Text -> m (SigningKey PaymentKey)
parseSignKey txt
  | T.null txt = fail "Empty value for SignKey"
  | T.head txt /= '{' =
    case deserialiseFromBech32 (AsSigningKey AsPaymentKey) txt of
      Left ide -> case convertText txt <&> unBase16 of
        Nothing -> fail "SignKey is neither Bench32 nor Hex encoded"
        Just bs -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) bs of
          Left de -> fail "SignKey is neither Bench32 nor CBOR encoded"
          Right sk' -> pure sk'
      Right sk -> pure sk
  | otherwise = case Aeson.eitherDecode (fromStrict $ encodeUtf8 txt) of
    Right (TextEnvelope _ _ cborHex) -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) cborHex of
      Left de -> fail "TextEnvelope doesn't contain cborHex for signKey"
      Right sk' -> pure sk'
    Left err -> fail err

parseAssetId :: MonadFail m => T.Text -> T.Text -> m AssetId
parseAssetId policyText nameText = do
  if T.null policyText
    then
      if T.null nameText
        then pure AdaAssetId
        else fail "Policy Id is for Ada but tokenName provided"
    else do
      let policyId = deserialiseFromRawBytesHex AsPolicyId $ encodeUtf8 policyText
      let assetName = deserialiseFromRawBytes AsAssetName $ encodeUtf8 nameText
      case policyId of
        Nothing -> fail "ParseError: invalid policyId value"
        Just pi -> case assetName of
          Nothing -> fail "ParseError: invalid tokenName"
          Just an -> pure $ AssetId pi an

parseAssetIdText :: MonadFail m => T.Text -> m AssetId
parseAssetIdText assetText = case T.split (== '.') assetText of
  [policy, tokenName] -> parseAssetId policy tokenName
  _ ->
    if T.null assetText || (T.toLower assetText == "lovelace")
      then pure AdaAssetId
      else failure
  where
    failure = fail "ParseError : Cannot construct AssetId from text. Expected format :`policyHex.tokenName`"

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
      case parseAssetIdText assetTxtStripped of
        Just asset -> pure (asset, Quantity iAmount)
        _ ->
          convertAdaOrLovelace
            assetTxtStripped
            iAmount
            (fail $ "Invalid AssetId in value `" ++ T.unpack textStr ++ "`")
    _ ->
      if T.null assetTxtStripped
        then case parseAssetIdText amountTxt of
          Just asset -> pure (asset, Quantity 1)
          _ -> do
            let (newAmountTxt, postfix) = T.span isDigit amountTxt
            case readMaybe (T.unpack newAmountTxt) of
              Just iAmount -> convertAdaOrLovelace postfix iAmount parseFail
              _ -> parseFail
        else parseFail
  where
    convertAdaOrLovelace str iAmount _otherwise = case T.unpack (T.toLower str) of
      "ada" -> lovelaceAmount (iAmount * 1_000_000)
      "a" -> lovelaceAmount (iAmount * 1_000_000)
      "l" -> lovelaceAmount iAmount
      "lovelace" -> lovelaceAmount iAmount
      _ -> _otherwise
    lovelaceAmount v = pure (AdaAssetId, Quantity v)
    parseFail = fail $ "Value parse failed : invalid value token `" ++ T.unpack textStr ++ "`"
    performBreak = T.break isSeparator (T.stripStart textStr)

parseScriptData :: MonadFail m => Text -> m ScriptData
parseScriptData jsonText = do
  case decodeJson of
    Nothing -> fail "Invalid Json for script data"
    Just v -> case scriptDataFromJson ScriptDataJsonNoSchema v of
      Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $ "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $ "Invalid data " ++ show sdre
      Right sd -> pure sd
  where
    decodeJson = Aeson.decode $ fromStrict $ TSE.encodeUtf8 jsonText


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
        Left err -> fail "Error while decoding script text envelope"
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

parseAddress :: MonadFail m => Text -> m (AddressInEra AlonzoEra)
parseAddress addrText = case deserialiseAddress (AsAddressInEra AsAlonzoEra) addrText of
  Nothing -> fail $ "Invalid address string: " ++ T.unpack addrText
  Just aie -> pure aie


scriptDataParser v key = case H.lookup key v of
  Nothing -> fail $"missing key \"" ++ T.unpack key ++ "\" if type ScriptData in json object"
  Just v -> doParsing v
  where
    doParsing (Aeson.String v) =  parseScriptData  v
    doParsing (Aeson.Object o )=case scriptDataFromJson ScriptDataJsonDetailedSchema  (Aeson.Object o) of
       Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $  "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $  "Invalid data " ++ show sdre
       Right sd -> pure  sd
    doParsing _  = fail "Script data Must be either string or object"