{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Cardano.Contrib.Easy.Parsers

where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Cardano.Api (TextEnvelope(TextEnvelope), AssetId (AssetId, AdaAssetId), ScriptDataJsonError (ScriptDataJsonSchemaError, ScriptDataRangeError), ScriptData, scriptDataFromJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), deserialiseFromRawBytesHex, AsType (AsPolicyId, AsAssetName, AsSigningKey, AsPaymentKey), SerialiseAsRawBytes (deserialiseFromRawBytes), SerialiseAsCBOR (deserialiseFromCBOR), deserialiseFromBech32, Value, Quantity (Quantity), valueFromList, SigningKey, PaymentKey)
import qualified Data.Text.Encoding as TSE
import Data.Text (Text)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Conversions (convertText, Base16 (unBase16))
import Data.Functor ((<&>))
import Data.Char (isSeparator, isDigit)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)
import GHC.IO.Exception (IOException(IOError), IOErrorType (UserError))
import Data.Aeson.Types (Parser)
parseSignKey txt
  | T.null txt = fail "Empty value for SignKey"
  | T.head txt /='{' =
    case deserialiseFromBech32 (AsSigningKey AsPaymentKey) txt of
        Left ide -> case  convertText txt <&> unBase16 of
          Nothing -> fail "SignKey is neither Bench32 nor Hex encoded"
          Just bs -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) bs of
            Left de -> fail "SignKey is neither Bench32 nor CBOR encoded"
            Right sk' -> pure sk'
        Right sk -> pure sk
  | otherwise = case Aeson.eitherDecode  (  fromStrict $ encodeUtf8 txt) of
              Right (TextEnvelope _ _ cborHex)->case deserialiseFromCBOR (AsSigningKey AsPaymentKey) cborHex of
                      Left de -> fail "TextEnvelope doesn't contain cborHex for signKey"
                      Right sk' -> pure sk'
              Left err -> fail err

parseAssetId :: MonadFail m => T.Text -> T.Text -> m AssetId
parseAssetId policyText nameText= do
    if T.null  policyText
          then if T.null nameText
                then pure  AdaAssetId
                else fail "Policy Id is for Ada but tokenName provided"
          else do
            let policyId=   deserialiseFromRawBytesHex AsPolicyId  $ encodeUtf8 policyText
            let assetName=  deserialiseFromRawBytes AsAssetName $  encodeUtf8 nameText
            case policyId of
              Nothing -> fail "ParseError: invalid policyId value"
              Just pi -> case assetName of
                Nothing -> fail "ParseError: invalid tokenName"
                Just an -> pure $ AssetId pi an

parseAssetIdText :: MonadFail m => T.Text  -> m AssetId
parseAssetIdText assetText = case T.split (== '.') assetText of
  [policy, tokenName] -> parseAssetId policy tokenName
  _                   ->  if T.null assetText || (T.toLower assetText == "lovelace")
                          then pure AdaAssetId
                          else failure
  where
    failure= fail "ParseError : Cannot construct AssetId from text. Expected format :`policyHex.tokenName`"


parseValueText ::  Text -> IO Value
parseValueText valueTxt=
  mapM parseAssetNQuantity (T.split (=='+')  $ T.strip  valueTxt)
   <&> valueFromList


parseAssetNQuantity :: MonadFail f => Text -> f (AssetId, Quantity)
parseAssetNQuantity textStr=do
    let (amountTxt,assetTxt)= performBreak
        assetTxtStripped= T.strip assetTxt
    case readMaybe (T.unpack amountTxt) of
      Just iAmount -> do
         case   parseAssetIdText assetTxtStripped  of
           Just asset -> pure (asset,Quantity iAmount)
           _ -> convertAdaOrLovelace
                  assetTxtStripped  iAmount  (fail $ "Invalid AssetId in value `"++T.unpack textStr ++"`")
      _ ->  if T.null assetTxtStripped
            then  case parseAssetIdText amountTxt of
                Just asset -> pure (asset,Quantity 1)
                _ -> do
                  let (newAmountTxt,postfix) =T.span isDigit amountTxt
                  case readMaybe (T.unpack newAmountTxt) of
                    Just iAmount -> convertAdaOrLovelace postfix iAmount parseFail
                    _     -> parseFail
            else
              parseFail
  where
  convertAdaOrLovelace str iAmount  _otherwise=  case T.unpack  (T.toLower  str) of
                              "ada"     -> lovelaceAmount (iAmount * 1_000_000)
                              "a"       -> lovelaceAmount (iAmount * 1_000_000)
                              "l"       -> lovelaceAmount iAmount
                              "lovelace"-> lovelaceAmount iAmount
                              _         -> _otherwise
  lovelaceAmount v= pure (AdaAssetId, Quantity v)
  parseFail =fail $ "Value parse failed : invalid value token `" ++ T.unpack textStr ++ "`"
  performBreak = T.break   isSeparator (T.stripStart  textStr)


parseScriptData :: MonadFail m => Text -> m ScriptData
parseScriptData jsonText=do
  case decodeJson of
    Nothing -> fail "Invalid Json for script data"
    Just v -> case scriptDataFromJson ScriptDataJsonDetailedSchema  v of
      Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $  "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $  "Invalid data " ++ show sdre
      Right sd -> pure  sd
  where
    decodeJson=Aeson.decode $ fromStrict  $ TSE.encodeUtf8 jsonText