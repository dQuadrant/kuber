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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Kuber.Data.Models where

import Cardano.Api
import Cardano.Api.Shelley (TxBody (ShelleyTxBody), toAlonzoData, scriptDataFromJsonDetailedSchema, scriptDataToJsonDetailedSchema, ReferenceScript (ReferenceScript, ReferenceScriptNone))
import Cardano.Binary (ToCBOR (toCBOR), decodeFull, fromCBOR)
import Cardano.Ledger.Babbage.Tx (BabbageTxBody (btbTxFee))
import Codec.CBOR.Write (toLazyByteString)
import Data.Aeson (KeyValue ((.=)), encode, object, (.!=))
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), Value (Object, String), (.:), (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Functor ( (<&>) )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Conversions (Base16 (Base16), convertText)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as TSE
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Cardano.Kuber.Utility.Text (toHexString)
import Cardano.Kuber.Data.Parsers (signKeyParser, txinOrUtxoParser)
import Cardano.Slotting.Time (SystemStart(SystemStart))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Cardano.Kuber.Error (FrameworkError)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson as A
import Data.Word (Word64)
import qualified Data.Map as Map
import Data.Vector.Primitive (Vector(Vector))
import qualified Data.Vector as Vector

class Wrapper  m a  where
  unWrap :: m  ->  a

newtype AssetModal = AssetModal AssetId deriving (Show)

newtype AddressModal = AddressModal (AddressInEra BabbageEra) deriving (Show)

newtype SignKeyModal = SignKeyModal (SigningKey PaymentKey) deriving (Show)

newtype UtxoModal = UtxoModal (UTxO BabbageEra) deriving (Show)

newtype UtxoIdModal = UtxoIdModal (TxId, TxIx) deriving (Show)

newtype WitnessModal = WitnessModal (KeyWitness BabbageEra) deriving (Show)

newtype TxModal = TxModal (Tx BabbageEra) deriving (Show)



newtype SystemStartModal = SystemStartModal SystemStart
newtype EraHistoryModal = EraHistoryModal (EraHistory CardanoMode)
newtype GenesisParamModal = GenesisParamModal GenesisParameters


instance Wrapper UtxoModal (UTxO BabbageEra) where
  unWrap (UtxoModal utxos) = utxos

instance Wrapper  (UTxO BabbageEra) (Map.Map TxIn (TxOut CtxUTxO BabbageEra)) where
  unWrap (UTxO utxos) = utxos

instance Wrapper SystemStartModal SystemStart where
  unWrap  (SystemStartModal ss )= ss

instance Wrapper  EraHistoryModal (EraHistory CardanoMode) where
  unWrap (EraHistoryModal eh) = eh

instance Wrapper  GenesisParamModal GenesisParameters where
  unWrap (GenesisParamModal gp) = gp

instance Wrapper TxModal (Tx BabbageEra) where
    unWrap (TxModal v) = v


unAssetModal (AssetModal a) = a

unAddressModal (AddressModal a) = a

unSignKeyModal (SignKeyModal s) = s

unTxModal (TxModal t) = t

unWitnessModal (WitnessModal w) = w

unUtxoIdModal (UtxoIdModal x) = x


data BalanceResponse = BalanceResponse
  { utxos :: UTxO BabbageEra
  }
  deriving (Generic, Show, ToJSON)

data KeyHashResponse = KeyHashResponse
  { keyHash :: String
  }
  deriving (Generic, Show, ToJSON)


data SubmitTxModal = SubmitTxModal
  { rawTx :: Tx BabbageEra,
    witness :: Maybe (KeyWitness BabbageEra)
  }



instance FromJSON SubmitTxModal where
  parseJSON (Object o) =
    do
      SubmitTxModal
      <$> (o .: "tx" <&> unTxModal)
      <*> (o .:? "witness" <&> fmap unWitnessModal)
  parseJSON _ = fail "Expected SubmitTx Object"

instance ToJSON SubmitTxModal where
  toJSON (SubmitTxModal tx witness) = A.object [
      "tx" .= serialiseToTextEnvelope Nothing tx
    ]


instance FromJSON WitnessModal where
  parseJSON (String str) = do
    let cborHexText = case T.stripPrefix "a10081" str of
          Nothing -> str
          Just txt -> T.concat ["8200", txt]
    case convertText cborHexText of
      Nothing -> fail "Witness string is not hex encoded"
      Just (Base16 bs) -> case deserialiseFromCBOR (AsKeyWitness AsBabbageEra) bs of
        Left e -> fail $ "Witness string: Invalid CBOR format : " ++ show e
        Right witness -> pure $ WitnessModal witness
  parseJSON _ = fail "Expecte Witness Modal cbor hex string"

instance FromJSON TxModal where
  parseJSON (String txStr) = do
    case convertText txStr of
      Nothing -> fail "Tx string is not hex encoded"
      Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsBabbageEra) bs of
        Left e -> fail $ "Tx string: Invalid CBOR format : " ++ show e
        Right tx -> pure $ TxModal tx
  parseJSON (Object o) = o .: "cborHex"  >>= parseJSON
  parseJSON _ = fail "Expected Tx cbor hex string"

instance ToJSON TxModal where
  toJSON (TxModal tx) = toJSON (serialiseToTextEnvelope Nothing tx)


instance FromJSON AddressModal where
  parseJSON (String s)=  case deserialiseAddress (AsAddressInEra AsBabbageEra) s of
      Nothing -> fail "Invalid address string. Couldn't be parsed as valid address for babbage era"
      Just aie -> pure $ AddressModal aie
  parseJSON _ = fail "Expected Address to be String"


instance ToJSON AddressModal where
  toJSON (AddressModal addr)= String $ serialiseAddress  addr

instance ToJSON SystemStartModal where
  toJSON (SystemStartModal ss)= case ss of { SystemStart ut -> A.object [
    "systemStart" .= ut
  ] }

instance FromJSON SystemStartModal where
  parseJSON (A.Object o) =  do
    ss <- o A..: "systemStart"
    pure (SystemStartModal $ SystemStart ss)
  parseJSON _ = fail "expected SystemStart Object"

instance ToJSON GenesisParamModal where
  toJSON (GenesisParamModal ss)= case ss of { GenesisParameters ut ni ra n es sl i j x lo pp -> A.object[
        "systemStartTime" .= ut
      , "networkId" .= (case ni of
          Mainnet -> A.String "mainnet"
          Testnet (NetworkMagic nm) -> A.Number (fromInteger$ toInteger nm))
      , "activeSlotsCoefficient" .= ra
      , "security" .= n
      , "epochLength" .=  es
      , "slotLength" .= sl
      , "slotsPerKesPeroid" .= i
      , "maxKesEvolution" .= j
      , "updateQuorum" .= x
      , "maxLovelaceSupply" .= lo
      , "initialUpdateableProtocolParameters" .= pp
  ] }

instance FromJSON GenesisParamModal where
  parseJSON (A.Object o) = GenesisParameters
                <$> o .:  "systemStartTime"
                <*> (o .: "networkId" >>= parseNetwork )
                <*> (o.: "activeSlotsCoefficient")
                <*> (o.: "security")
                <*> (o.: "epochLength")
                <*> (o.: "slotLength")
                <*> (o.: "slotsPerKesPeroid")
                <*> (o.: "maxKesEvolution")
                <*> (o.: "updateQuorum")
                <*> (o.: "maxLovelaceSupply")
                <*> (o.: "initialUpdateableProtocolParameters") <&> GenesisParamModal
    where
      parseNetwork  (A.String str) = case str of
          "mainnet" -> pure Mainnet
          "preview" -> pure (Testnet $ NetworkMagic 1)
          "preprod" -> pure (Testnet $ NetworkMagic 2)
          _         -> fail $ "Unknown network name: " ++ T.unpack str
      parseNetwork (A.Number n) = pure $ Testnet (NetworkMagic $ fromInteger $ round n)
      parseNetwork _ = fail "Expected NetworkId"

  parseJSON _ = fail "Expected GenesisParameters Object"



data QueryTipResponse  = QueryTipResponse{
    blk:: String
  , qtrSlotNo ::  Word64
}

newtype TimeTranslationReq  = TimeTranslationReq POSIXTime
newtype SlotTranslationReq  = SlotTranslationReq SlotNo

data TranslationResponse = TranslationResponse {
    tResSlotNo:: SlotNo,
    tResTimestamp:: POSIXTime
}

instance ToJSON TranslationResponse  where
    toJSON (TranslationResponse slot time)= A.object [
          A.fromString "slot" A..= slot
        , A.fromString "time" A..= time
      ]
instance FromJSON TranslationResponse  where
    parseJSON (A.Object o )  = TranslationResponse
                        <$> (o A..: "slot" <&> SlotNo)
                        <*> (o A..: "time"<&> fromInteger )
    parseJSON _ = fail "Expected Translation Response object"


instance FromJSON  TimeTranslationReq where
  parseJSON (A.Object v) =do
    dateInt <- v A..:  A.fromString "time"
    pure $ TimeTranslationReq  (fromInteger dateInt)
  parseJSON (A.Number n) = pure $ TimeTranslationReq ( fromInteger (round n))
  parseJSON _ = fail "Expected TimeTranslationReq object or number"


instance ToJSON TimeTranslationReq where
  toJSON (TimeTranslationReq time) =  toJSON $   time


instance FromJSON SlotTranslationReq where

  parseJSON (A.Object v) =do
    slotNo <- v A..:  A.fromString "slot"
    pure $ SlotTranslationReq  (fromInteger slotNo)
  parseJSON (A.Number n) = pure $ SlotTranslationReq ( fromInteger (round n))

  parseJSON _ = fail "Expected SlotTranslationReq object or number"

instance ToJSON SlotTranslationReq where
  toJSON (SlotTranslationReq (SlotNo slot)) =  toJSON    slot


newtype ChainPointModal = ChainPointModal ChainPoint

instance Wrapper ChainPointModal ChainPoint where
  unWrap (ChainPointModal cp ) = cp

instance ToJSON ChainPointModal where
  toJSON (ChainPointModal cp) = case cp of
    ChainPointAtGenesis -> A.object [
      A.fromString "slot"  A..= (0::Integer),
      A.fromString "block" A..= ("genesis"::String )
      ]
    ChainPoint (SlotNo slot) ha -> A.object [
      A.fromString "slot"  A..= slot,
      A.fromString "block" A..= serialiseToRawBytesHexText ha
      ]

instance FromJSON ChainPointModal where
  -- parseJSON (A.Object o) =QueryTipResponse <$> o A..: "slot" <*> o A..: "block"
    parseJSON (A.Object o) =do 
        block :: Text <- o  A..: "block"
        case block of 
          "genesis" -> pure (ChainPointModal ChainPointAtGenesis)
          _ -> do
            hHash <- o A..: "block"  
            slot <- o A..: "slot"
            pure $ ChainPointModal $ ChainPoint  (SlotNo slot) hHash
    parseJSON _ = fail "Expected ChainPoint Modal"


instance FromJSON UtxoModal where
  parseJSON v = case v of 
    Object km -> doparse v
    String txt -> doparse v
    A.Array vs -> mconcat $ map doparse  $ Vector.toList  vs
    _ -> fail "parseError : Expected Utxo Modal Object"
    where 
      doparse obj = do 
        result <- txinOrUtxoParser obj
        case result of 
          Left ti -> fail "Parse Utxo Failed, missing address, value "
          Right uto -> pure $ UtxoModal uto

instance ToJSON UtxoModal where
  toJSON (UtxoModal (UTxO utxoMp)) = A.Array $ Vector.fromList  (map (\(tin,TxOut addr val datum sc)->
              let  baseUtxo = [
                      "address" .= addr
                    , "txin" .= tin
                    , "value"   .= val]
              in
                A.object $ (baseUtxo  ++ (case datum of
                 TxOutDatumNone -> []
                 TxOutDatumHash sdsie ha -> [ "datumHash" .= ha]
                 TxOutDatumInline rtisidsie sd -> [ "datum" .= scriptDataToJsonDetailedSchema sd]) 
                )   ++ (case sc of
                ReferenceScript rtisidsie sial -> ["script" .= sial]
                ReferenceScriptNone -> []
                )

    ) $ Map.toList utxoMp)
    where 
    appendNonEmpty :: (Foldable t, KeyValue a1, ToJSON (t a2)) => A.Key -> t a2 -> [a1] -> [a1]
    appendNonEmpty  key  val obj   =  if null val then obj else (key .= val) : obj

    infixl 8 >=
    (>=) a b = appendNonEmpty a b

    infixr 7 <#>
    (<#>)  f  f2  =  f $ f2 []

    infixr 6 <+>
    (<+>) f  v = f v



newtype ExUnitsResponseModal = ExUnitsResponseModal (Map.Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))


instance Wrapper ExUnitsResponseModal (Map.Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits)) where
  unWrap (ExUnitsResponseModal m) =m 

instance ToJSON ExUnitsResponseModal where
  toJSON (ExUnitsResponseModal mp) = let

          mints = Map.filterWithKey (\ k _ -> case k of
            ScriptWitnessIndexMint _ -> True
            _ -> False ) mp
          inputs = Map.filterWithKey (\ k _ -> case k of
            ScriptWitnessIndexTxIn _ -> True
            _ ->  False ) mp
          withdrawals = Map.filterWithKey (\ k _ -> case k of
            ScriptWitnessIndexWithdrawal _ -> True
            _ -> False ) mp
          certs = Map.filterWithKey (\ k _ -> case k of
            ScriptWitnessIndexCertificate _ -> True
            _ -> False ) mp

          mapInt  = Map.mapKeys (\case
            ScriptWitnessIndexTxIn wo -> wo
            ScriptWitnessIndexMint wo -> wo
            ScriptWitnessIndexCertificate wo -> wo
            ScriptWitnessIndexWithdrawal wo -> wo )

          mapErr = \case
            Left fe -> toJSON fe
            Right v -> toJSON v

          updateMap m =  Map.map mapErr $ mapInt m
      in A.object [
          "mints"       A..= updateMap mints
        , "inputs"      A..= updateMap inputs
        , "withdrawals" A..= updateMap withdrawals
        , "certs"       A..= updateMap certs
      ]

instance FromJSON ExUnitsResponseModal where
    parseJSON (A.Object v ) = do
        mints <-        v .:? "mints"     .!= Map.empty
        inputs <-       v .:? "inputs"    .!= Map.empty
        withdrawals <-  v .:? "withdrawals" .!= Map.empty
        certs <-        v .:? "certs"     .!= Map.empty
        mints' <- mapM parseExunits mints
        inputs' <- mapM parseExunits inputs
        withdrawals' <- mapM parseExunits withdrawals
        certs' <- mapM parseExunits certs


        pure $ ExUnitsResponseModal $
                  Map.mapKeys ScriptWitnessIndexMint  mints'
              <>  Map.mapKeys ScriptWitnessIndexTxIn inputs'
              <> Map.mapKeys ScriptWitnessIndexWithdrawal withdrawals'
              <> Map.mapKeys ScriptWitnessIndexCertificate certs'
        where
          parseExunits val@(A.Object o) = do
            msg  <- o .:? "message"
            case msg :: Maybe String of
              Nothing -> A.parseJSON val <&> Right
              Just any -> A.parseJSON val <&> Left
          parseExunits _ = fail "Expecte ExUnits object "

    parseJSON _ = fail "parseError : Expected Object"

