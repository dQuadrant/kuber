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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Kuber.Data.Models where

import Cardano.Api
import Cardano.Api.Shelley (TxBody (ShelleyTxBody), toAlonzoData, scriptDataFromJsonDetailedSchema, scriptDataToJsonDetailedSchema, ReferenceScript (ReferenceScript, ReferenceScriptNone), Proposal, ShelleyLedgerEra, StakeAddress (StakeAddress), StakePoolKey, Hash (unStakePoolKeyHash), toShelleyStakeAddr)
import Cardano.Binary (ToCBOR (toCBOR), decodeFull, fromCBOR)
import Cardano.Ledger.Babbage.Tx (BabbageTxBody (btbTxFee))
import Codec.CBOR.Write (toLazyByteString)
import Data.Aeson (KeyValue ((.=)), encode, object, (.!=), ToJSONKey, FromJSONKey)
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
import Cardano.Kuber.Data.Parsers (signKeyParser, txinOrUtxoParser, parseHexString, parseRawBytes', parseRawBytes, parseBech32OrCBOR', parseRawBech32, parseBech32Type, parseHexString', parseRawBech32')
import Cardano.Slotting.Time (SystemStart(SystemStart))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Cardano.Kuber.Error (FrameworkError)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson as A
import Data.Word (Word64)
import qualified Data.Map as Map
import Data.Vector.Primitive (Vector(Vector))
import qualified Data.Vector as Vector
import Cardano.Api.Ledger (ConwayTxCert(..), ConwayGovCert (..), StrictMaybe (SNothing, SJust), Credential (KeyHashObj, ScriptHashObj), Coin (Coin), StandardCrypto, Url, textToUrl, ShelleyTxCert (ShelleyTxCertDelegCert), ShelleyDelegCert (..), ConwayDelegCert (..), KeyHash (KeyHash), Delegatee (..), KeyRole, hashFromBytes)
import Data.Text.Encoding (encodeUtf8)
import Cardano.Ledger.Api (Constitution (Constitution), Anchor (Anchor), GovAction (..), ProposalProcedure (ProposalProcedure), Crypto (ADDRHASH))
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Core (EraCrypto,DRep(..))
import qualified Cardano.Api.Shelley as CAPI
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Data.Aeson.KeyMap (toHashMapText)
import qualified Data.Map as HM
import qualified Data.Foldable as Foldable
import Cardano.Crypto.Hash (HashAlgorithm, hashToStringAsHex, hashToTextAsHex)
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Ratio ((%))
import Cardano.Ledger.BaseTypes (UnitInterval)
import Data.Default (def)

class Wrapper  m a  where
  unWrap :: m  ->  a

newtype AssetModal = AssetModal AssetId deriving (Show)

newtype AddressModal = AddressModal (AddressInEra ConwayEra) deriving (Show)
newtype RewardAcntModal crypto = RewardAcntModal (Ledger.RewardAcnt crypto) deriving (Show)


newtype SignKeyModal = SignKeyModal (SigningKey PaymentKey) deriving (Show)

newtype UtxoModal = UtxoModal (UTxO ConwayEra) deriving (Show)

newtype UtxoIdModal = UtxoIdModal (TxId, TxIx) deriving (Show)

newtype WitnessModal = WitnessModal (KeyWitness ConwayEra) deriving (Show)

newtype TxModal = TxModal (Tx ConwayEra) deriving (Show)

newtype TxCertificateModal era = TxCertificateModal (Certificate era)

newtype ProposalModal = ProposalModal (Proposal ConwayEra)

newtype GovActionModal era = GovActionModal (GovAction era)

newtype ConstitutionModal era  = ConstitutionModal (Constitution era)

newtype AnchorModal era = AnchorModal (Anchor era)
newtype ProposalProcedureModal  era = ProposalProcedureModal (ProposalProcedure era)

newtype SystemStartModal = SystemStartModal SystemStart
newtype EraHistoryModal = EraHistoryModal (EraHistory CardanoMode)
newtype GenesisParamModal era = GenesisParamModal (GenesisParameters era)
newtype DrepModal era = DrepModal (DRep era)

data CredentialModal (r::KeyRole)  era = CredentialModal (Credential r era) deriving (Show, Eq)

instance Wrapper UtxoModal (UTxO ConwayEra) where
  unWrap (UtxoModal utxos) = utxos

instance Wrapper  (UTxO ConwayEra) (Map.Map TxIn (TxOut CtxUTxO ConwayEra)) where
  unWrap (UTxO utxos) = utxos

instance Wrapper SystemStartModal SystemStart where
  unWrap  (SystemStartModal ss )= ss

instance Wrapper  EraHistoryModal (EraHistory CardanoMode) where
  unWrap (EraHistoryModal eh) = eh

instance Wrapper  (GenesisParamModal era) (GenesisParameters era) where
  unWrap (GenesisParamModal gp) = gp

instance Wrapper TxModal (Tx ConwayEra) where
    unWrap (TxModal v) = v

instance Wrapper (TxCertificateModal era)  (Certificate era) where
    unWrap (TxCertificateModal v) = v

instance Wrapper (ConstitutionModal era) (Constitution era) where
  unWrap (ConstitutionModal v) = v


instance Wrapper ProposalModal (Proposal ConwayEra) where
  unWrap (ProposalModal v) = v

instance Wrapper (AnchorModal era) (Anchor era) where
  unWrap (AnchorModal a) = a

instance Wrapper (GovActionModal era) (GovAction era) where
  unWrap (GovActionModal a) = a

instance Wrapper (ProposalProcedureModal era) (ProposalProcedure era) where
  unWrap (ProposalProcedureModal p) =p

unAssetModal (AssetModal a) = a

unAddressModal (AddressModal a) = a

unSignKeyModal (SignKeyModal s) = s

unTxModal (TxModal t) = t

unWitnessModal (WitnessModal w) = w

unUtxoIdModal (UtxoIdModal x) = x

data BalanceResponse = BalanceResponse
  { utxos :: UTxO ConwayEra
  }
  deriving (Generic, Show, ToJSON)

data KeyHashResponse = KeyHashResponse
  { keyHash :: String
  }
  deriving (Generic, Show, ToJSON)


data SubmitTxModal = SubmitTxModal
  { rawTx :: Tx ConwayEra,
    witness :: Maybe (KeyWitness ConwayEra)
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
      Just (Base16 bs) -> case deserialiseFromCBOR (AsKeyWitness AsConwayEra) bs of
        Left e -> fail $ "Witness string: Invalid CBOR format : " ++ show e
        Right witness -> pure $ WitnessModal witness
  parseJSON _ = fail "Expecte Witness Modal cbor hex string"

instance FromJSON TxModal where
  parseJSON (String txStr) = do
    case convertText txStr of
      Nothing -> fail "Tx string is not hex encoded"
      Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsConwayEra) bs of
        Left e -> fail $ "Tx string: Invalid CBOR format : " ++ show e
        Right tx -> pure $ TxModal tx
  parseJSON (Object o) = o .: "cborHex"  >>= parseJSON
  parseJSON _ = fail "Expected Tx cbor hex string"

instance ToJSON TxModal where
  toJSON (TxModal tx) = toJSON (serialiseToTextEnvelope Nothing tx)


instance FromJSON AddressModal where
  parseJSON (String s)=  case deserialiseAddress (AsAddressInEra AsConwayEra) s of
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

instance ToJSON (GenesisParamModal ShelleyEra) where
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

instance FromJSON (GenesisParamModal ShelleyEra) where
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


-- instance  (StandardCrypto ~ crypto) => FromJSON (RewardAcntModal crypto) where
--   parseJSON (A.Object v) = do  fail "sad"
--   parseJSON (A.String s) = do
--       v <-  case parseHexString s  of
--           Just p -> parseRawBytes'  (AsStakeAddress) p "Hex value is not valid stake address"
--           Nothing ->
--             case parseBech32Type s AsStakeAddress of
--               Just v -> pure $ Left p
--               Nothing -> parseRawBech32' errorMsg <&> Right
--       pure $ RewardAcntModal $ toShelleyStakeAddr s
--     where errorMsg = "Neither encoded bech32 or hex"

--   parseJSON _ = fail "Expected Reward Account  bech32 or cbor or keyhash hex "



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



-- data ProposalProcedure era = ProposalProcedure
--   { pProcDeposit :: !Coin
--   , pProcReturnAddr :: !(RewardAcnt (EraCrypto era))
--   , pProcGovAction :: !(GovAction era)
--   , pProcAnchor :: !(Anchor (EraCrypto era))
--   }

-- data GovAction era
--   = ParameterChange
--       -- | Previous governance action id of `ParameterChange` type, which corresponds to
--       -- `PParamUpdatePurpose`.
--       !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
--       -- | Proposed changes to PParams
--       !(PParamsUpdate era)
--   | HardForkInitiation
--       -- | Previous governance action id of `HardForkInitiation` type, which corresponds
--       -- to `HardForkPurpose`
--       !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
--       -- | Proposed new protocol version
--       !ProtVer
--   | TreasuryWithdrawals
--       -- | Proposed treasury withdrawals
--       !(Map (RewardAcnt (EraCrypto era)) Coin)
--   | NoConfidence
--       -- | Previous governance action id of `NoConfidence` or `NewCommittee` type, which
--       -- corresponds to `CommitteePurpose`
--       !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
--   | NewCommittee
--       -- | Previous governance action id of `NewCommittee` or `NoConfidence` type, which
--       -- corresponds to `CommitteePurpose`
--       !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
--       -- | Old committee
--       !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
--       -- | New Committee
--       !(Committee era)
--   | NewConstitution
--       -- | Previous governance action id of `NewConstitution` type, which corresponds to
--       -- `ConstitutionPurpose`
--       !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
--       !(Constitution era)
--   | InfoAction

-- data Constitution era = Constitution
--   { constitutionAnchor :: !(Anchor (EraCrypto era))
--   , constitutionScript :: !(StrictMaybe (ScriptHash (EraCrypto era)))
--   }


instance Ledger.Crypto era => FromJSON (AnchorModal era) where

  parseJSON (A.Object o) = do
     url <- o .: "url"
     dataHash <- o .:? "dataHash"
     dataHash' <- o.:? "hash"
     hash<-case (dataHash,dataHash') of
       (Just dh,_) -> pure dh
       (_,Just dh) -> pure dh
       _ -> fail "Expected .dataHash or .hash to be present"
     pure $ AnchorModal $ Anchor  url hash

  parseJSON  _ = fail "Expected Anchor Object"


instance  EraCrypto ledgerera ~ StandardCrypto =>  FromJSON  (ConstitutionModal ledgerera) where
  parseJSON (A.Object o) = do
      url <- o .: "url"
      dataHash <- o .: "dataHash"
      scriptHash :: Maybe T.Text <- o .:? "scriptHash"
      pure $ ConstitutionModal $ Constitution (Anchor  url dataHash) SNothing

  parseJSON  _ = fail "Expected Proposal Object"


instance  EraCrypto ledgerera ~ StandardCrypto =>  FromJSON  (GovActionModal ledgerera) where
  parseJSON (A.Object o) = do
    certType <- o .: "type"
    case T.toLower certType of
      "newconstitution" -> do
          (ConstitutionModal constitution) <- o .: "constitution"
          pure $ GovActionModal $ NewConstitution SNothing constitution
      _ -> fail "We only support  \"newconstitution\"|"

  parseJSON  _ = fail "Expected GovActionModal Object"

instance  EraCrypto ledgerera ~ StandardCrypto  =>  FromJSON  (ProposalProcedureModal ledgerera) where
  parseJSON (A.Object o) = do
    deposit :: Integer <- o.:? "deposit" .!= 0
    returnAddress <- o .: "refundAccount"
    (AnchorModal anchor) <- o .: "anchor"
    let chain :: FromJSON v =>  A.Key ->( v ->  Parser a) -> Parser a -> Parser a
        chain key mapper onMissing =do
         result <- o.:? key
         maybe onMissing mapper result

    govAction <-
          chain  "newconstitution"  parseNewConstitution
        $ chain "noconfidence" parseNoConfidence
        $ chain "info" parseInfo
        $ chain "withdraw" parseTreasuryWithdrawal
        $ chain "hardfork" parseHardforkInitiation
        $ chain "updatecommittee" parseUpdateCommittee
        $ pure InfoAction


    pure $ ProposalProcedureModal $
          ProposalProcedure (Coin deposit) returnAddress  govAction  anchor

    where
        parseUpdateCommittee (A.Object  obj)= do
           addMap  <- obj .:? "add" .!= mempty
           remove  <- obj .:? "remove" .!= mempty
           let toAdd = HM.mapKeys (\(CredentialModal c) -> c) $  HM.map fromInteger addMap
           let toRemove = Set.map (\(CredentialModal c) -> c)  remove
           pure $ UpdateCommittee SNothing  toRemove toAdd def -- TODO DO NOT USE THIS DEF


        parseUpdateCommittee _ = fail "Expected updateCommittee Object"

        parseNewConstitution (ConstitutionModal constitution) = pure $ NewConstitution SNothing constitution

        parseNoConfidence (A.Object o) = do
           pure $ NoConfidence SNothing
        parseNoConfidence _ = pure $ NoConfidence SNothing

        parseHardforkInitiation (A.Object protObj) = do
          pVersion <- protObj .: "protocolVersion"
          pure $ HardForkInitiation SNothing pVersion

        parseHardforkInitiation v = do
          pVersion<-parseJSON v
          pure $ HardForkInitiation SNothing pVersion

        parseNoConfidenceProposasl _ = pure $ Ledger.NoConfidence SNothing

        parseTreasuryWithdrawal (A.Object treasuryObj ) = do
          hmap <- o .: "withdraw"
          let
              mapKeysM f hm = mapM (\(k,v)->do
                    k' <- f k
                    pure (k',v)
                ) (HM.toList hm) <&> HM.fromList
          hmap' <- mapKeysM (\x -> do
                  case parseBech32Type x AsStakeAddress of
                      Just p -> pure p
                      Nothing -> case  parseHexString x >>= parseRawBytes AsStakeAddress of
                        Just v-> pure v
                        Nothing -> fail $ "Invalid stake address \"" ++ T.unpack x ++ "\""
                ) hmap
          let hmap'' = HM.mapKeys toShelleyStakeAddr hmap'

          pure $ TreasuryWithdrawals hmap''
        parseTreasuryWithdrawal _ = fail "Expected \"accounts\" to be present for treasuryWithdrawal"

        parseInfo (A.Bool True) = pure InfoAction
        parseInfo _ = fail "Expected \"info\" value to be true"

  parseJSON  _ = fail "Expected GovActionModal Object"

-- data GovAction era
--   = ParameterChange
--       -- | Previous governance action id of `ParameterChange` type, which corresponds to
--       -- `PParamUpdatePurpose`.
--       !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
--       -- | Proposed changes to PParams
--       !(PParamsUpdate era)
--   | HardForkInitiation
--       -- | Previous governance action id of `HardForkInitiation` type, which corresponds
--       -- to `HardForkPurpose`
--       !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
--       -- | Proposed new protocol version
--       !ProtVer
--   | TreasuryWithdrawals
--       -- | Proposed treasury withdrawals
--       !(Map (RewardAcnt (EraCrypto era)) Coin)
--   | NoConfidence
--       -- | Previous governance action id of `NoConfidence` or `NewCommittee` type, which
--       -- corresponds to `CommitteePurpose`
--       !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
--   | NewCommittee
--       -- | Previous governance action id of `NewCommittee` or `NoConfidence` type, which
--       -- corresponds to `CommitteePurpose`
--       !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
--       -- | Old committee
--       !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
--       -- | New Committee
--       !(Committee era)
--   | NewConstitution
--       -- | Previous governance action id of `NewConstitution` type, which corresponds to
--       -- `ConstitutionPurpose`
--       !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
--       !(Constitution era)
--   | InfoAction

-- data ProposalProcedure era = ProposalProcedure
--   { pProcDeposit :: !Coin
--   , pProcReturnAddr :: !(RewardAcnt (EraCrypto era))
--   , pProcGovAction :: !(GovAction era)
--   , pProcAnchor :: !(Anchor (EraCrypto era))
--   }
instance (era ~ ConwayEra)  =>  FromJSON (TxCertificateModal era) where

  parseJSON :: A.Value -> Parser (TxCertificateModal era)
  parseJSON (A.Object o ) = do
    certType <- o .: "type"
    mDeposit <- o .:? "deposit"
    let depositAmount = case mDeposit of
                                  Nothing -> SNothing
                                  Just lovelace -> SJust (Coin lovelace)
    case T.toLower certType of
      "registerstake" -> do
          key <- o .: "key"
          let bytes = encodeUtf8 key
          let stakeKey = deserialiseFromRawBytesHex (AsHash AsStakeKey) (encodeUtf8 key)
          stakeCred <-case stakeKey of
            Left rbhe -> fail "Invalid stake Key"
            Right (CAPI.StakeKeyHash drk) -> pure $  KeyHashObj  drk
          pure $  TxCertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  . ConwayTxCertDeleg
                                  $ ConwayRegCert
                                      stakeCred
                                      depositAmount
      "deregisterstake" -> do
          key <- o .: "key"
          let bytes = encodeUtf8 key
          let stakeKey = deserialiseFromRawBytesHex (AsHash AsStakeKey) (encodeUtf8 key)
          stakeCred <-case stakeKey of
            Left rbhe -> fail "Invalid stake Key"
            Right (CAPI.StakeKeyHash drk) -> pure $  KeyHashObj  drk
          pure $  TxCertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  . ConwayTxCertDeleg
                                  $ ConwayUnRegCert
                                      stakeCred
                                      depositAmount
      "delegate" -> do
          let spoParser :: MonadFail m =>  T.Text -> m (Hash StakePoolKey)
              spoParser val= do
                v <- case parseBech32Type val (AsHash AsStakePoolKey) of
                    Just p -> fail "sad"
                    Nothing -> parseHexString val >>= parseRawBytes (AsHash AsStakePoolKey)
                fail "sad"
          key <- o .: "key"
          mSpo1 <- o .:? "spo"
          mSpo  <- case mSpo1 of
            Nothing ->  o .:? "pool"
            Just txt -> pure $ pure txt
          mDrep <- o.:? "drep"
          delegatee<-case mSpo of
            Nothing -> maybe (fail "expecting either \"spo\" or \"spo\" or both") (\(DrepModal d) -> pure $ DelegVote d) mDrep
            Just spoCred ->do
                pure $ maybe (DelegStake spoCred) (\(DrepModal d)  ->  DelegStakeVote spoCred d) mDrep
          let bytes = encodeUtf8 key
          let stakeKey = deserialiseFromRawBytesHex (AsHash AsStakeKey) (encodeUtf8 key)

          stakeCred <-case stakeKey of
            Left rbhe -> fail "Invalid stake Key"
            Right (CAPI.StakeKeyHash drk) -> pure $  KeyHashObj  drk
          pure $  TxCertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  $ ConwayTxCertDeleg $ ConwayDelegCert
                                      stakeCred
                                      delegatee


-- -- | First type argument is the deposit
-- data Delegatee c
--   = DelegStake !(KeyHash 'StakePool c)
--   | DelegVote !(DRep c)
--   | DelegStakeVote !(KeyHash 'StakePool c) !(DRep c)


      "registerdrep" -> do
          (CredentialModal key) <- o .: "key"
          deposit <- o .: "deposit"
          mAnchor <- o .:? "anchor"
          pure $  TxCertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                        . ConwayTxCertGov
                        $ ConwayRegDRep
                            key
                            (Coin deposit)
                            (case mAnchor of
                              Nothing -> SNothing
                              Just (AnchorModal a) -> SJust a  )

      _ -> fail "Only Drep/Stake registration, drep delegation and stake de-registration certificate are supported"

  parseJSON  _ = fail "Expected Certificate Object"


-- data DRep c
--   = DRepKeyHash !(KeyHash 'DRepRole c)
--   | DRepScriptHash !(ScriptHash c)
--   | DRepAlwaysAbstain
--   | DRepAlwaysNoConfidence

instance ( ledgerera ~ StandardCrypto ,Crypto ledgerera) => FromJSON (CredentialModal r ledgerera) where
  parseJSON :: A.Value -> Parser (CredentialModal r ledgerera)
  parseJSON (A.Object o) = do
    cred <- asum [parser1 o, parser2 o]
    pure $ CredentialModal cred
      where
        parser1 obj = ScriptHashObj <$> (obj .: "scriptHash" <|> obj .: "scripthash" <|> obj .: "scripthash")
        parser2 obj = KeyHashObj <$> (obj .: "keyHash" <|> obj .: "keyhash"  <|> obj .: "key hash")
  parseJSON (A.String txt) = case parseHexString txt of
    Just parsed -> parseKeyHash parsed
    Nothing -> case parseRawBech32'  txt of
      Just parsed ->parseKeyHash parsed
      Nothing -> fail "Neither bech32 encoded credential nor  hex encoded"
    where
      parseKeyHash bytes=  do
                hash <- rawBytesToHash bytes
                pure $ CredentialModal $ KeyHashObj ( KeyHash hash)
  parseJSON  _ = fail "Expected  Credential object or string"

instance ( cre ~ StandardCrypto ,Crypto cre) => FromJSONKey (CredentialModal r cre) where

instance Ord (CredentialModal r cre) where
  compare (CredentialModal c1) (CredentialModal c2) = compare c1 c2

instance Crypto cre => ToJSON  (CredentialModal r cre) where
   toJSON (CredentialModal (KeyHashObj (KeyHash kh))) = A.String $   hashToTextAsHex kh
   toJSON (CredentialModal (ScriptHashObj sh)) = toJSON sh

rawBytesToHash bytes =
  if BS.length bytes ==28
  then case hashFromBytes bytes of
      Nothing -> fail "Credential Hash is not of 28 byte length"
      Just ha -> pure ha
  else fail "Credential Hash is not of 28 byte length"


instance (era ~ StandardCrypto) =>  FromJSON  (DrepModal era) where

  parseJSON :: A.Value -> Parser (DrepModal era)
  parseJSON (A.Object o ) = do
      mKeyHash <- o .:? "keyHash"
      case  mKeyHash of
        Just (keyHash :: T.Text) -> case parseHexString keyHash  of
          Just (unHexed :: BS.ByteString) -> parseFromDrepKeyBytes unHexed
          Nothing -> fail "keyHash is not hex encoded "
        Nothing -> do
              mScriptHash <- o .:? "scriptHash"
              case  mScriptHash of
                Just (keyHash :: T.Text) -> case parseHexString keyHash  of
                        Just (unHexed :: BS.ByteString) -> case parseRawBytes AsScriptHash unHexed   of
                          Just ( CAPI.ScriptHash drepkey) ->  let
                                        scHashObj = ScriptHashObj drepkey
                                      in pure $ DrepModal $  DRepCredential scHashObj
                          Nothing -> fail "scriptHash is not a valid Drep KeyHash"
                        Nothing -> fail "scriptHash is not hex encoded "
                Nothing -> fail "both .keyHash and .scriptHash are missing"

  parseJSON (A.String s) = case T.toLower s of
    "abstain" -> pure  $ DrepModal DRepAlwaysAbstain
    "noconfidence" -> pure  $ DrepModal DRepAlwaysNoConfidence
    bech32 -> do
      case parseHexString bech32 of
        Just (unHexed ::BS.ByteString) -> fromHash unHexed  (fail $  "Invalid encoding of drep : " ++ T.unpack bech32 )
        Nothing -> do
              rawBytes <- parseRawBech32 (Set.fromList $ map T.pack ["drep_", "drep_test", "stake_", "stake_test"]) bech32
              fromHash rawBytes (do
                  case deserialiseFromRawBytes AsStakeAddress rawBytes of
                    Left sarbe -> fail "Invalid drep Key"
                    Right sa ->pure $ DrepModal $  DRepCredential $ case sa of { StakeAddress net cre -> case cre of
                                               ScriptHashObj sh ->  ScriptHashObj sh
                                               KeyHashObj kh -> KeyHashObj  (case kh of { KeyHash ha ->  KeyHash ha } )
                                            }

                )
              let drepKeyHash =  "invalid drep id"
              let drepScriptHash = error "sad"
              fail "sad"
    where
      fromHash unHexed f =
          if BS.length unHexed == 28
            then parseFromDrepKeyBytes unHexed
            else f

  parseJSON  _ = fail "Expected Drep Object"



parseFromDrepKeyBytes unHexed = case parseRawBytes (AsHash AsDRepKey) unHexed   of
    Just ( CAPI.DRepKeyHash drepkey) ->  let
                  keyHashobj = KeyHashObj drepkey
                in pure $ DrepModal $  DRepCredential keyHashobj
    Nothing -> fail "keyHash is not a valid Drep KeyHash"

data AsDrepModal




-- data ConwayGovCert c
--   = ConwayRegDRep !(Credential 'DRepRole c) !Coin !(StrictMaybe (Anchor c))
--   | ConwayUnRegDRep !(Credential 'DRepRole c) !Coin
--   | ConwayUpdateDRep !(Credential 'DRepRole c) !(StrictMaybe (Anchor c))
--   | ConwayAuthCommitteeHotKey !(Credential 'ColdCommitteeRole c) !(Credential 'HotCommitteeRole c)
--   | ConwayResignCommitteeColdKey !(Credential 'ColdCommitteeRole c)

-- data ShelleyTxCert era
--   = ShelleyTxCertDelegCert !(ShelleyDelegCert (EraCrypto era))
--   | ShelleyTxCertPool !(PoolCert (EraCrypto era))
--   | ShelleyTxCertGenesisDeleg !(GenesisDelegCert (EraCrypto era))
--   | ShelleyTxCertMir !(MIRCert (EraCrypto era))

-- data ConwayTxCert era
--   = ConwayTxCertDeleg !(ConwayDelegCert (EraCrypto era))
--   | ConwayTxCertPool !(PoolCert (EraCrypto era))
--   | ConwayTxCertGov !(ConwayGovCert (EraCrypto era))

    -- data Certificate era where
    --  -- Pre-Conway
    --  --   1. Stake registration
    --  --   2. Stake unregistration
    --  --   3. Stake delegation
    --  --   4. Pool retirement
    --  --   5. Pool registration
    --  --   6. Genesis delegation
    --  --   7. MIR certificates
    --  ShelleyRelatedCertificate
    --    :: ShelleyToBabbageEra era
    --    -> Ledger.ShelleyTxCert (ShelleyLedgerEra era)
    --    -> Certificate era

    --  -- Conway onwards
    --  -- TODO: Add comments about the new types of certificates
    --  ConwayCertificate
    --    :: ConwayEraOnwards era
    --    -> Ledger.ConwayTxCert (ShelleyLedgerEra era)
    --    -> Certificate era

toStrictMaybe :: StrictMaybe a -> Maybe a
toStrictMaybe (SJust v) = Just v
toStrictMaybe SNothing = Nothing

mapStrictMaybe :: StrictMaybe a -> (a -> b) -> Maybe b
mapStrictMaybe (SJust v) f = Just (f v)
mapStrictMaybe SNothing _ = Nothing

type PrevGovAction purpose = (StrictMaybe (Ledger.PrevGovActionId purpose StandardCrypto))
type LedgerAnchor = (Ledger.Anchor StandardCrypto)

toGovernanceAction
  ::   EraCrypto ledgerera ~ StandardCrypto
  =>
  PrevGovAction purpose -> LedgerAnchor
  -> Ledger.Constitution ledgerera
toGovernanceAction  prevGovAction anchor =  Constitution anchor SNothing