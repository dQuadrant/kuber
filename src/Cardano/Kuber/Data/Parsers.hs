{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Cardano.Kuber.Data.Parsers where

import           Cardano.Api
import           Cardano.Binary               (FromCBOR (fromCBOR), decodeFull)
import           Cardano.Kuber.Utility.Text
import qualified Cardano.Ledger.Alonzo       as Alonzo
import qualified Cardano.Ledger.Babbage       as Babbage

-- import           Cardano.Ledger.DescribeEras  (StandardCrypto, Witness (Alonzo))
import qualified Cardano.Ledger.Shelley.API   as Shelley
import           Control.Exception            (SomeException, catch, throw)
import           Data.Aeson                   ((.:), (.:?), FromJSON (parseJSON), eitherDecode)
import qualified Data.Aeson                   as A
import qualified Data.Aeson.KeyMap            as A
import qualified Data.Aeson.Key as A

import           Data.Aeson.Types             (parseMaybe, parse)
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
import qualified Data.Aeson.Types as A
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Babbage.TxOut as Babbage

import Cardano.Ledger.Address
import qualified Cardano.Ledger.Api.Era as Conway
import Cardano.Api.Shelley (fromShelleyTxOut, ReferenceScript (..), fromShelleyAddr, toShelleyAddr)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.List (intercalate)
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (bMaryOnward, bConwayOnward, bBabbageOnward, bAlonzoOnward, bAsEra, bCardanoEra, bShelleyBasedEra))


type ErrorMessage = String

parseSignKey :: MonadFail m => Text -> m (SigningKey PaymentKey)
parseSignKey txt
  | T.null txt = fail "Empty value for SignKey"
  | T.head txt /= '{' =
    case deserialiseFromBech32 (AsSigningKey AsPaymentKey) txt of
      Left ide -> case convertText txt <&> unBase16 of
        Nothing -> fail "SignKey is neither Bech32 nor Hex encoded"
        Just bs -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) bs of
          Left de   -> fail "SignKey is neither Bech32 nor CBOR encoded"
          Right sk' -> pure sk'
      Right sk -> pure sk
  | otherwise = case A.eitherDecode (fromStrict $ encodeUtf8 txt) of
    Right (TextEnvelope _ _ cborHex) -> case deserialiseFromCBOR (AsSigningKey AsPaymentKey) cborHex of
      Left de   -> fail "TextEnvelope doesn't contain cborHex for signKey"
      Right sk' -> pure sk'
    Left err -> fail err

signKeyParser ::  A.Value -> A.Parser (SigningKey PaymentKey)
signKeyParser v@(A.Object _) = do
  textEnvelope <- parseJSON v
  case deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey) textEnvelope of
      Left tee -> case tee of
        TextEnvelopeTypeError tets tet -> fail  $ "Invalid text envelope type for "++ show tet
        TextEnvelopeDecodeError de -> fail $ "Failed to decode signKey " ++ show de
        TextEnvelopeAesonDecodeError s -> fail $ "Json ParseError : " ++ s
      Right sk -> pure sk
signKeyParser (A.String str) = parseSignKey str
signKeyParser _ = fail "Expected signKey to be either string or textEnvelope object"

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
                  Left _ -> fail "ParserError : Invalid token name"
                  Right an -> pure $ AssetId pi an

parseAssetName :: MonadFail f =>Text -> f AssetName
parseAssetName txt =case deserialiseFromRawBytesHex AsAssetName utf8
      of
        Left _  -> case deserialiseFromRawBytes AsAssetName utf8 of
          Left _ -> fail $  "Invalid assetname :" ++ T.unpack txt
          Right an -> pure an
        Right an -> pure an
  where
    utf8 =encodeUtf8 txt
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

parseScriptData :: MonadFail m => Text -> m HashableScriptData
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

parseAnyScript :: MonadFail m => LBS.ByteString -> m ScriptInAnyLang
parseAnyScript jsonText = case eitherDecode jsonText of
  Left s -> fail s
  Right any -> case parse anyScriptParser any of
    A.Error s -> fail s
    A.Success any' -> pure any'


anyScriptParser ::  A.Value  -> A.Parser ScriptInAnyLang
anyScriptParser v@(A.Object o) =do
  _type :: T.Text <- o  .: "type"
  case _type of
    "PlutusScriptV1" -> do
      sc <- o.: "cborHex"  >>= parseCborHex @T.Text
      pure $ ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) (PlutusScript PlutusScriptV1 sc)
    "PlutusScriptV2" -> do
      sc <- o.: "cborHex"  >>= parseCborHex @T.Text
      pure $ ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) (PlutusScript PlutusScriptV2 sc)
    _ -> do
      v ::SimpleScript <- parseJSON v
      pure $ ScriptInAnyLang SimpleScriptLanguage $ SimpleScript v
anyScriptParser _ = fail "Expected json object type"

parseAddressBinary :: (IsTxBuilderEra era,MonadFail m) => ByteString -> m (AddressInEra era )
parseAddressBinary bs = case parseAddressCbor (LBS.fromStrict bs) of
  Just addr -> pure addr
  Nothing -> parseAddressRaw bs

parseAddressRaw :: (IsTxBuilderEra era, MonadFail m) =>  ByteString -> m(AddressInEra era)
parseAddressRaw addrBs = case deserialiseFromRawBytes (AsAddressInEra bAsEra) addrBs of
            Left _  -> fail  "Invalid Address Hex "
            Right addr ->pure   addr

parseAddress :: (IsTxBuilderEra era , MonadFail m) => Text -> m (AddressInEra era)
parseAddress addrText = case deserialiseAddress (AsAddressInEra bAsEra) addrText of
  Nothing -> do
     let hex :: Maybe  ByteString = parseHexString addrText
     case  hex of
      Just hex -> case parseAddressBinary hex of
        Just addr -> pure addr
        Nothing -> case deserialiseFromRawBytes (AsAddressInEra bAsEra) hex of
            Left _ -> fail  $ "Address is neither bech32 nor cborHex : "++ T.unpack addrText
            Right addr -> pure addr
      Nothing -> fail $ "Address is neither bech32 nor cborHex : "++ T.unpack addrText
  Just aie -> pure aie

parseAddressCbor ::(IsTxBuilderEra era , MonadFail m) => LBS.ByteString -> m (AddressInEra era)
parseAddressCbor  cbor = do
  aie <-  case deserialiseFromRawBytes AsAddressAny (LBS.toStrict cbor) of
      Left e -> fail $ show e
      Right v -> pure v
  case anyAddressInEra bCardanoEra aie of
     Left s -> fail $ "Error parsing address CBOR " ++ s
     Right aie' -> pure aie'

parseAddressBech32 ::(IsTxBuilderEra era , MonadFail m)=> Text -> m (AddressInEra era)
parseAddressBech32 txt = case deserialiseAddress (AsAddressInEra bAsEra) txt of
  Nothing -> fail "Address is not in bech32 format"
  Just aie -> pure aie

scriptDataParser :: MonadFail m =>  Aeson.Value  -> m HashableScriptData
scriptDataParser v = doParsing v
  where
    doParsing (Aeson.String v) =  parseScriptData  v
    doParsing (Aeson.Object o )=case scriptDataFromJson ScriptDataJsonDetailedSchema  (Aeson.Object o) of
       Left sdje -> case sdje of
        ScriptDataJsonSchemaError va sdjse -> fail $  "Wrong schema" ++ show sdjse
        ScriptDataRangeError va sdre -> fail $  "Invalid data " ++ show sdre
       Right sd -> pure sd
    doParsing val  = fail $ "Script data Must be either string or object: got:" ++  show val


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


-- (.:*) :: FromJSON a => A.KeyMap A.Value  -> [A.Key] -> A.Parser a
-- (.:*) obj keys = doAccum keys
--   where
--     doAccum   [] = fail "Expecting one of the keys" ++ keys
--     doAccum   (key:ks)= do
--         val <- obj .:? key
--         case val of
--           Nothing -> doAccum  ks
--           Just any -> pure any


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


parseUtxo :: (IsTxBuilderEra era,  MonadFail m) => Text -> m (UTxO era )
parseUtxo v =parseHexString v >>=  parseUtxoCbor


parseUtxoCbor :: (IsTxBuilderEra era,MonadFail m) => LBS.ByteString -> m (UTxO era)
parseUtxoCbor = parseUtxoCbor' bShelleyBasedEra

parseUtxoCbor' :: ( MonadFail m) => ShelleyBasedEra era ->LBS.ByteString -> m (UTxO era)
parseUtxoCbor' era = case era of --Don't use this if possible
  ShelleyBasedEraShelley -> error "Unexpected"
  ShelleyBasedEraAllegra -> error "Unexpected"
  ShelleyBasedEraMary -> error "Unexpected"
  ShelleyBasedEraAlonzo -> error "Unexpected"
  ShelleyBasedEraBabbage -> parseUtxoCbor_ ShelleyBasedEraBabbage
  ShelleyBasedEraConway -> parseUtxoCbor_ ShelleyBasedEraConway


parseUtxoCbor_ sbe bs = do
  ((txHash,index ),txout) <- parseCbor bs
  txIn <- case deserialiseFromRawBytes AsTxId txHash of
          Right txid -> pure $ TxIn txid (TxIx index)
          Left _ -> fail $ "Failed to parse value as txHash " ++ toHexString  txHash
  pure $ UTxO (Map.singleton txIn (fromShelleyTxOut sbe txout))


parseTxOut :: MonadFail m => Text -> m (TxOut CtxTx   ConwayEra)
parseTxOut  val = decodeCbor <&> fromShelleyTxOut ShelleyBasedEraConway
  where
    decodeCbor :: MonadFail m => m (Babbage.TxOut (Conway.ConwayEra StandardCrypto))
    decodeCbor = parseHexString  val >>= parseCbor

parseHexString :: (FromText (Maybe (Base16 a1)), ToText a2, MonadFail f) =>a2 -> f a1
parseHexString  v = case unHex v  of
  Just bs ->pure bs
  Nothing -> fail $ "Invalid Hex string: " ++ convertText v

parseHexString' :: (FromText (Maybe (Base16 a1)), ToText a2, MonadFail f) =>a2 -> ErrorMessage-> f a1
parseHexString'  v msg= case unHex v  of
  Just bs ->pure bs
  Nothing -> fail msg

parseCbor :: (FromCBOR a, MonadFail m) =>LBS.ByteString -> m a
parseCbor  v = case decodeFull v of
  Left de   -> fail $  "Not in required cbor format: "++ show de
  Right any -> pure any

parseCbor' :: (FromCBOR a, MonadFail m) =>LBS.ByteString -> ErrorMessage -> m a
parseCbor'  v msg = case decodeFull v  of
  Left de   -> fail  msg
  Right any -> pure any

parseCborHex :: (ToText a2, MonadFail m, FromCBOR b) => a2 -> m b
parseCborHex  v = parseHexString v >>=parseCbor

parseRawBytes :: (SerialiseAsRawBytes a, MonadFail m) => AsType a -> ByteString -> m a
parseRawBytes t v = case deserialiseFromRawBytes t v of
  Left sarbe -> fail $ "ParseFail : " ++ show sarbe
  Right a -> pure a

parseRawBytes' :: (SerialiseAsRawBytes a, MonadFail m) => AsType a -> ByteString -> ErrorMessage -> m a
parseRawBytes' t v msg = case deserialiseFromRawBytes t v of
  Left _ -> fail msg
  Right a -> pure a

parseBech32Type :: (SerialiseAsBech32 a, MonadFail m) => Text -> AsType a -> m a
parseBech32Type bs  t  = case deserialiseFromBech32  t bs of
    Left e ->  fail $ "Parse Error :" ++ show e
    Right v -> pure v


parseRawBech32:: MonadFail m => Set.Set Text  ->  Text -> m ByteString
parseRawBech32  permittedPrefixes bech32Str = do
    (prefix, dataPart) <-case Bech32.decodeLenient bech32Str of
      Left de -> fail "Invalid bech 32"
      Right x0 -> pure x0

    let actualPrefix      = Bech32.humanReadablePartToText prefix
    if actualPrefix `notElem` permittedPrefixes
      then fail $ "Invalid prefix : " ++ T.unpack actualPrefix ++ " Allowed : " ++ intercalate ", "  (map T.unpack $  Set.toList  permittedPrefixes)
      else pure ()

    case Bech32.dataPartToBytes dataPart of
      Nothing -> fail "String not in Bech32 format"
      Just bs -> pure bs

parseRawBech32':: MonadFail m =>  Text -> m ByteString
parseRawBech32'   bech32Str = do
    (prefix, dataPart) <-case Bech32.decodeLenient bech32Str of
      Left de -> fail "Invalid bech 32"
      Right x0 -> pure x0

    case Bech32.dataPartToBytes dataPart of
      Nothing -> fail "String not in Bech32 format"
      Just bs -> pure bs

parseRawBech32_:: MonadFail m => Text ->   m (Text,ByteString)
parseRawBech32_   bech32Str = do
    (prefix, dataPart) <-case Bech32.decodeLenient bech32Str of
      Left de -> fail "Invalid bech 32"
      Right x0 -> pure x0

    case Bech32.dataPartToBytes dataPart of
      Nothing -> fail "String not in Bech32 format"
      Just bs -> pure (Bech32.humanReadablePartToText prefix,bs)




parseBech32Type' :: (SerialiseAsBech32 a, MonadFail m) => Text -> AsType a -> ErrorMessage -> m a
parseBech32Type' bs  t msg = case deserialiseFromBech32  t bs of
    Left e ->  fail  msg
    Right v -> pure v

parseBech32OrCBOR :: (FromCBOR  a, SerialiseAsBech32 a, MonadFail m) => Text -> AsType a  -> m a
parseBech32OrCBOR bs t  = case parseHexString bs of
  Just unhexed -> parseCbor unhexed
  Nothing -> parseBech32Type bs t


parseBech32OrCBOR' :: (FromCBOR  a, SerialiseAsBech32 a, MonadFail m) => Text -> AsType a -> ErrorMessage -> m a
parseBech32OrCBOR' bs t msg = case parseHexString bs of
  Just unhexed -> parseCbor' unhexed msg
  Nothing -> parseBech32Type' bs t msg


txinOrUtxoParser  :: (IsTxBuilderEra era) =>  A.Value -> A.Parser (Either TxIn (UTxO era))
txinOrUtxoParser obj@(A.Object o) = do
    txin' ::Maybe A.Value <- o .:? "txIn"
    txin'' ::Maybe A.Value <- o .:? "txin"
    txin<-case txin' of
            Nothing -> case txin'' of
                Nothing -> txInParser obj
                Just any -> txInParser any
            Just any -> txInParser any
    addrM <- o .:? "address"
    valueM <- o.:? "value"
    case (addrM,valueM) of
      (Just addrT , Just valT) -> do
          addr <- parseAddress addrT
          val <-  case valT of
                    A.String s -> parseValueText s
                    A.Number n -> pure $  lovelaceToValue  (round n)
                    _ -> parseJSON valT
          refScript <-  o .:?*  ["referenceScript", "script","referencescript"]
          datumHash <-  o.:? "datumHash"
          inlineDatum <- o .:?* ["inlineDatum", "datum","inlinedatum"]
          dh <- case inlineDatum of
              Nothing -> case datumHash of
                Nothing -> pure TxOutDatumNone
                Just any -> pure $ TxOutDatumHash bAlonzoOnward any
              Just any ->  do
                sd<- scriptDataParser any
                pure $ TxOutDatumInline bBabbageOnward  sd -- proper parse
          let
              rScript =case refScript of
                Nothing -> ReferenceScriptNone
                Just any -> ReferenceScript bBabbageOnward any
          pure $ Right $ UTxO $ Map.singleton txin  (TxOut addr (valueToRequiredEra bCardanoEra val) dh rScript)

      _ -> pure $ Left txin
    where
    valueToRequiredEra:: CardanoEra era -> Value -> TxOutValue era 
    valueToRequiredEra cera val = case cera of
      MaryEra -> TxOutValueShelleyBased ShelleyBasedEraMary (toLedgerValue MaryEraOnwardsMary val)
      AlonzoEra ->  TxOutValueShelleyBased ShelleyBasedEraAlonzo (toLedgerValue MaryEraOnwardsAlonzo val)
      BabbageEra ->  TxOutValueShelleyBased ShelleyBasedEraBabbage (toLedgerValue MaryEraOnwardsBabbage val)
      ConwayEra ->  TxOutValueShelleyBased ShelleyBasedEraConway (toLedgerValue MaryEraOnwardsConway val)
      _ -> error "Unexpected"

    (.:?*) :: FromJSON a => A.KeyMap A.Value  -> [A.Key] -> A.Parser (Maybe a)
    (.:?*) obj = doAccum
      where
        doAccum   [] = pure Nothing
        doAccum   (key:ks)= do
            val <- obj .:? key
            case val of
              Nothing -> doAccum  ks
              Just any -> pure any

txinOrUtxoParser (A.String v) = do
  case parseHexString v of
    Just binary -> parseUtxoCbor binary <&> Right
    Nothing -> parseTxIn v <&> Left

txinOrUtxoParser _ = fail "Expected Utxo Object or cborHex"

parseRawCBorAnyOf ::
        MonadFail m =>[FromSomeType SerialiseAsCBOR b]
      -> ByteString
      -> m b
      -> m b
parseRawCBorAnyOf ((FromSomeType ttoken f):other) bs onErr = case deserialiseFromCBOR ttoken  bs of
      Right v -> pure $ f v
      Left e -> parseRawCBorAnyOf other bs onErr
parseRawCBorAnyOf [] bs onErr = onErr

parseRawTxInAnyEra :: MonadFail m => ByteString -> m (InAnyCardanoEra Tx)
parseRawTxInAnyEra bs = parseRawCBorAnyOf allTxTypes bs (fail "CBOR deserialise of transaction failed")

allTxTypes :: [FromSomeType SerialiseAsCBOR (InAnyCardanoEra Tx)]
allTxTypes  =[
        FromSomeType (AsTx AsBabbageEra) (InAnyCardanoEra BabbageEra)
      , FromSomeType (AsTx AsConwayEra) (InAnyCardanoEra ConwayEra)
      -- , FromSomeType (AsTx AsByronEra)   (InAnyCardanoEra ByronEra)
      , FromSomeType (AsTx AsShelleyEra) (InAnyCardanoEra ShelleyEra)
      , FromSomeType (AsTx AsAllegraEra) (InAnyCardanoEra AllegraEra)
      , FromSomeType (AsTx AsMaryEra)    (InAnyCardanoEra MaryEra)
      , FromSomeType (AsTx AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
  ]
