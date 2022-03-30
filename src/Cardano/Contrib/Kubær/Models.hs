{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use ++" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Contrib.Kubær.Models where

import Cardano.Api
import Cardano.Api.Shelley (TxBody (ShelleyTxBody), toAlonzoData)
import Cardano.Binary (ToCBOR (toCBOR), decodeFull, fromCBOR)
import Cardano.Contrib.Easy.Util (toHexString)
import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.Tx (TxBody (txfee))
import Codec.CBOR.Write (toLazyByteString)
import Data.Aeson (KeyValue ((.=)), encode, object)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), Value (Object, String), (.:), (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Conversions (Base16 (Base16), convertText)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as TSE
import GHC.Generics (Generic)
import Text.Read (readMaybe)

newtype AssetModal = AssetModal AssetId deriving (Show)

newtype AddressModal = AddressModal (AddressInEra AlonzoEra) deriving (Show)

newtype SignKeyModal = SignKeyModal (SigningKey PaymentKey) deriving (Show)

newtype UtxoIdModal = UtxoIdModal (TxId, TxIx) deriving (Show)

newtype WitnessModal = WitnessModal (KeyWitness AlonzoEra) deriving (Show)

newtype TxModal = TxModal (Tx AlonzoEra) deriving (Show)

unAssetModal (AssetModal a) = a

unAddressModal (AddressModal a) = a

unSignKeyModal (SignKeyModal s) = s

unTxModal (TxModal t) = t

unWitnessModal (WitnessModal w) = w

unUtxoIdModal (UtxoIdModal x) = x

parseUtxo :: Data.Aeson.Types.Value -> Parser TxIn
parseUtxo (Object o) = do
  txid <- o .: "hash"
  index <- o .: "index"
  pure $ TxIn txid index
parseUtxo (String v) =
  case T.split (== '#') v of
    [txHash, index] ->
      case deserialiseFromRawBytesHex AsTxId (TSE.encodeUtf8 txHash) of
        Just txid -> case readMaybe (T.unpack index) of
          Just txindex -> pure $ TxIn txid (TxIx txindex)
          Nothing -> fail $ "Failed to parse txIndex in " ++ T.unpack v
        Nothing -> fail $ "Failed to parse value as txHash " ++ T.unpack txHash
    _ -> fail $ "Expected to be of format 'txId#index' got :" ++ T.unpack v
parseUtxo _ = fail "Expected Utxo to be of type Object or String"

data BalanceResponse = BalanceResponse
  { utxos :: UTxO AlonzoEra
  }
  deriving (Generic, Show, ToJSON)

data SubmitTxModal = SubmitTxModal
  { rawTx :: Tx AlonzoEra,
    witness :: Maybe (KeyWitness AlonzoEra)
  }

instance FromJSON SubmitTxModal where
  parseJSON (Object o) =
    do
      SubmitTxModal
      <$> (o .: "tx" <&> unTxModal)
      <*> (o .:? "witness" <&> fmap unWitnessModal)
  parseJSON _ = fail "Expected SubmitTx Object"

instance FromJSON WitnessModal where
  parseJSON (String str) = do
    let cborHexText = case T.stripPrefix "a10081" str of
          Nothing -> str
          Just txt -> T.concat ["8200", txt]
    case convertText cborHexText of
      Nothing -> fail "Witness string is not hex encoded"
      Just (Base16 bs) -> case deserialiseFromCBOR (AsKeyWitness AsAlonzoEra) bs of
        Left e -> fail $ "Witness string: Invalid CBOR format : " ++ show e
        Right witness -> pure $ WitnessModal witness
  parseJSON _ = fail "Expecte Witness Modal cbor hex string"

instance FromJSON TxModal where
  parseJSON (String txStr) = do
    case convertText txStr of
      Nothing -> fail "Tx string is not hex encoded"
      Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsAlonzoEra) bs of
        Left e -> fail $ "Tx string: Invalid CBOR format : " ++ show e
        Right tx -> pure $ TxModal tx
  parseJSON _ = fail "Expected Tx cbor hex string"

data TxResponse = TxResponse
  { txRaw :: Tx AlonzoEra,
    datums :: [ScriptData]
  }
  deriving (Generic, Show)

instance ToJSON TxResponse where
  toJSON (TxResponse tx datums) =
    object
      [ "tx" .= txHex,
        "datums" .= object (map datumPair datums),
        "txHash" .= getTxId (getTxBody tx),
        "fee" .= case getTxBody tx of
          ShelleyTxBody sbe tb scs tbsd m_ad tsv -> txfee tb
      ]
    where
      txHex :: Text
      txHex = toHexString $ serialiseToCBOR tx

      datumPair d = datumHashHexString d .= datumHexString d

      datumHashHexString :: ScriptData -> Text
      datumHashHexString sd = T.pack $ tail $ init $ show $ hashScriptData sd

      datumHexString :: ScriptData -> Text
      datumHexString _data =
        T.decodeUtf8 $ toStrict $ encode $ scriptDataToJson ScriptDataJsonDetailedSchema _data

      dataToBytes :: Data AlonzoEra -> LBS.ByteString
      dataToBytes d = toLazyByteString $ toCBOR d
      toLedger :: ScriptData -> Data AlonzoEra
      toLedger = toAlonzoData

data TxModel = TxModel
  { outputs :: [String]
  }
  deriving (Generic, Show)

instance FromJSON TxModel where
  parseJSON (Object o) =
    do
      TxModel
      <$> (o .: "outputs")
  parseJSON _ = fail "Expected TxModel Object"

data PayToModel = PayToModel
  { sender :: String,
    receiver :: String,
    value :: String
  }
  deriving (Generic, Show)

instance FromJSON PayToModel where
  parseJSON (Object o) =
    PayToModel
      <$> (o .: "sender")
      <*> (o .: "receiver")
      <*> (o .: "value")
  parseJSON _ = fail "Expected TxModel Object"