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
{-# LANGUAGE AllowAmbiguousTypes #-}


module Cardano.Kuber.Data.Models where

import Cardano.Api
import Cardano.Api.Shelley (TxBody (ShelleyTxBody), toAlonzoData, scriptDataFromJsonDetailedSchema, scriptDataToJsonDetailedSchema, ReferenceScript (ReferenceScript, ReferenceScriptNone), Proposal, ShelleyLedgerEra, StakeAddress (StakeAddress), StakePoolKey, Hash (..), toShelleyStakeAddr, toShelleyTxId, fromShelleyTxIn, fromShelleyStakeAddr, LedgerProtocolParameters (LedgerProtocolParameters))
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
import Cardano.Kuber.Data.Parsers (signKeyParser, txinOrUtxoParser, parseHexString, parseRawBytes', parseRawBytes, parseBech32OrCBOR', parseRawBech32, parseBech32Type, parseHexString', parseRawBech32', txInParser, parseRawBech32_)
import Cardano.Slotting.Time (SystemStart(SystemStart))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Cardano.Kuber.Error (FrameworkError)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson as A
import Data.Word (Word64, Word8)
import qualified Data.Map as Map
import Data.Vector.Primitive (Vector(Vector))
import qualified Data.Vector as Vector
import Cardano.Api.Ledger (ConwayTxCert(..), ConwayGovCert (..), StrictMaybe (SNothing, SJust), Credential (KeyHashObj, ScriptHashObj), Coin (Coin), StandardCrypto, Url, textToUrl, ShelleyTxCert (..), ShelleyDelegCert (..), ConwayDelegCert (..), KeyHash (KeyHash), Delegatee (..), KeyRole (DRepRole), hashFromBytes, DRep (..), GovActionId (GovActionId), boundRational, unboundRational, PoolCert (..))
import Data.Text.Encoding (encodeUtf8)
import Cardano.Ledger.Hashes as Hashes
import Cardano.Ledger.Api (Constitution (Constitution), Anchor (Anchor), GovAction (..), ProposalProcedure (ProposalProcedure), Crypto (ADDRHASH), PParamsUpdate, emptyPParamsUpdate, ppuMaxBBSizeL, PrevGovActionId (PrevGovActionId), GovActionIx (GovActionIx), PParams)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Core (EraCrypto, PParamsUpdate (..), EraPParams (..))
import qualified Cardano.Api.Shelley as CAPI
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Data.Aeson.KeyMap (toHashMapText, toHashMap)
import qualified Data.HashMap.Internal as HM
import qualified Data.Foldable as Foldable
import Cardano.Crypto.Hash (HashAlgorithm, hashToStringAsHex, hashToTextAsHex)
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Ratio ((%))
import Cardano.Ledger.BaseTypes (UnitInterval)
import Data.Default (def)
import Data.Bits ((.&.))
import qualified Cardano.Crypto.Hash as Crypto
import Control.Lens ((&), (.~), ASetter, Lens', (^.), Getting)
import Cardano.Ledger.HKD
import GHC.Natural (Natural)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as A
import qualified Cardano.Ledger.Conway.PParams as Ledger
import Data.ByteString (ByteString)
import Codec.CBOR.Magic (intToWord)
import Data.List (intercalate)
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Credential (parseCredential)

class Wrapper  m a  where
  unWrap :: m  ->  a

newtype AssetModal = AssetModal AssetId deriving (Show)

newtype AddressModal = AddressModal (AddressInEra ConwayEra) deriving (Show)
newtype RewardAcntModal crypto = RewardAcntModal (Ledger.RewardAcnt crypto) deriving (Show,Eq)


newtype SignKeyModal = SignKeyModal (SigningKey PaymentKey) deriving (Show)

newtype UtxoModal = UtxoModal (UTxO ConwayEra) deriving (Show)

newtype UtxoIdModal = UtxoIdModal (TxId, TxIx) deriving (Show)

newtype WitnessModal = WitnessModal (KeyWitness ConwayEra) deriving (Show)

newtype TxModal = TxModal (Tx ConwayEra) deriving (Show)

newtype CertificateModal era = CertificateModal (Certificate era)

newtype ProposalModal = ProposalModal (Proposal ConwayEra)

newtype GovActionModal era = GovActionModal (GovAction era)

newtype ConstitutionModal era  = ConstitutionModal (Constitution era)

newtype AnchorModal era = AnchorModal (Anchor era)
newtype ProposalProcedureModal  era = ProposalProcedureModal (ProposalProcedure era)

newtype SystemStartModal = SystemStartModal SystemStart
newtype EraHistoryModal = EraHistoryModal (EraHistory CardanoMode)
newtype GenesisParamModal era = GenesisParamModal (GenesisParameters era)

newtype VotingProcedureModal era = VotingProcedureModal (Ledger.VotingProcedure era)

instance Wrapper (VotingProcedureModal era) (Ledger.VotingProcedure era) where
  unWrap(VotingProcedureModal vp)=vp

newtype VoteModal = VoteModal Ledger.Vote

instance Wrapper VoteModal Ledger.Vote where
  unWrap (VoteModal vote) = vote

instance   ToJSON (LedgerProtocolParameters ConwayEra)where
  toJSON (LedgerProtocolParameters param)=toJSON param

instance   FromJSON (LedgerProtocolParameters ConwayEra)where
  parseJSON (A.Object obj)= 
    let hmap=  A.fromHashMapText $  HM.mapKeys (T.toLower . A.toText ) $ toHashMap obj
        pparams=Ledger.emptyPParams
        paramParser :: (FromJSON a , EraCrypto ledgerera ~ StandardCrypto,Ledger.ConwayEraPParams ledgerera,
                EraPParams ledgerera) => T.Text
            -> Lens' (PParams ledgerera) ( b)
            -> (a -> b)
            -> PParams ledgerera
            -> Parser (PParams ledgerera)
        paramParser k f f2 pUpdate   = do
          minSize <- hmap .:? A.fromText (T.toLower k)
          pure $ case minSize of
            Nothing ->  pUpdate
            Just val ->   pUpdate  & f .~  f2 val
    in  (   paramParser  "maxblocksize" Ledger.ppMaxBBSizeL id pparams
        >>= paramParser  "minFeeA"   Ledger.ppMinFeeAL id
        >>= paramParser "MaxBBSize" Ledger.ppMaxBBSizeL id
        >>= paramParser "MaxTxSize" Ledger.ppMaxTxSizeL id
        >>= paramParser "MaxBHSize" Ledger.ppMaxBHSizeL id
        >>= paramParser "KeyDeposit" Ledger.ppKeyDepositL id
        >>= paramParser "PoolDeposit" Ledger.ppPoolDepositL id
        >>= paramParser "EMax" Ledger.ppEMaxL id
        >>= paramParser "NOpt" Ledger.ppNOptL id
        >>= paramParser "A0" Ledger.ppA0L id
        >>= paramParser "Rho" Ledger.ppRhoL id
        >>= paramParser "Tau" Ledger.ppTauL id
        >>= paramParser "ProtocolVersion" Ledger.ppProtocolVersionL id
        >>= paramParser "MinPoolCost" Ledger.ppMinPoolCostL id
        >>= paramParser "CoinsPerUTxOByte" Ledger.ppCoinsPerUTxOByteL id
        >>= paramParser "CostModels" Ledger.ppCostModelsL id
        >>= paramParser "Prices" Ledger.ppPricesL id
        >>= paramParser "MaxTxExUnits" Ledger.ppMaxTxExUnitsL id
        >>= paramParser "MaxBlockExUnits" Ledger.ppMaxBlockExUnitsL id
        >>= paramParser "MaxValSize" Ledger.ppMaxValSizeL id
        >>= paramParser "CollateralPercentage" Ledger.ppCollateralPercentageL id
        >>= paramParser "MaxCollateralInputs" Ledger.ppMaxCollateralInputsL id
        >>= paramParser "PoolVotingThresholds" Ledger.ppPoolVotingThresholdsL id
        >>= paramParser "DRepVotingThresholds" Ledger.ppDRepVotingThresholdsL id
        >>= paramParser "CommitteeMinSize" Ledger.ppCommitteeMinSizeL id
        >>= paramParser "CommitteeMaxTermLength" Ledger.ppCommitteeMaxTermLengthL id
        >>= paramParser "GovActionLifetime" Ledger.ppGovActionLifetimeL id
        >>= paramParser "GovActionDeposit" Ledger.ppGovActionDepositL id
        >>= paramParser "DRepDeposit" Ledger.ppDRepDepositL id
        >>= paramParser "DRepActivity" Ledger.ppDRepActivityL id
      ) <&> LedgerProtocolParameters
  parseJSON _= fail "Expected pParams object" 
-- newtype VotingProcedures era = VotingProcedures
--   { unVotingProcedures ::
--       Map (Voter (EraCrypto era)) (Map (GovActionId (EraCrypto era)) (VotingProcedure era))
--   }
-- CAPI
-- newtype VotingProcedures era = VotingProcedures
  -- { unVotingProcedures  :: L.VotingProcedures (ShelleyLedgerEra era)
  -- }
data  TxVoteL (ledgerera) = TxVoteL
                        (Ledger.GovActionId (EraCrypto ledgerera))
                        (Ledger.VotingProcedure  ledgerera)
                        (Ledger.Voter (EraCrypto ledgerera))
                      deriving (Show,Eq)

newtype (TxVote era) = TxVote (TxVoteL (ShelleyLedgerEra era)) deriving (Show,Eq)




newtype DrepModal era = DrepModal (DRep era)

instance Wrapper (DrepModal era) (DRep era) where
  unWrap(DrepModal drep) = drep

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

instance Wrapper (CertificateModal era)  (Certificate era) where
    unWrap (CertificateModal v) = v

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

instance Wrapper (RewardAcntModal era) (Ledger.RewardAcnt era) where
  unWrap (RewardAcntModal ra) =ra

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
      "tx" .= serialiseTxLedgerCddl cardanoEra tx
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
  toJSON (TxModal tx) = toJSON (serialiseTxLedgerCddl  cardanoEra tx)


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
          "sancho" -> pure (Testnet $ NetworkMagic 4)
          "sanchonet" -> pure (Testnet $ NetworkMagic 4)
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


instance  (StandardCrypto ~ crypto) => FromJSON (RewardAcntModal crypto) where
  parseJSON val@(A.Object o) = parseJSON  val <&> RewardAcntModal

  parseJSON (A.String txt) = parseRewardAcntTxt txt

    where errorMsg = "Neither encoded bech32 or hex"

  parseJSON _ = fail "Expected stake address  bech32 or cbor or keyhash hex "

instance (StandardCrypto ~crypto) => ToJSON (RewardAcntModal crypto) where
    toJSON (RewardAcntModal ra) = A.String $ serialiseAddress $ fromShelleyStakeAddr ra

parseRewardAcntTxt txt =do
  stakeAddr <-  case parseHexString txt  of
          Just p -> parseRawBytes'  AsStakeAddress p "Hex value is not valid stake address"
          Nothing ->
            case parseBech32Type txt AsStakeAddress of
              Just v -> pure  v
              Nothing -> fail "Expected Hex or bech32 encoding of stake address "
  pure $ RewardAcntModal $ toShelleyStakeAddr stakeAddr

instance ( era ~ StandardCrypto ) => FromJSONKey (RewardAcntModal era) where
  fromJSONKey  = A.FromJSONKeyTextParser  parseRewardAcntTxt


-- instance FromJSONKey LT.Text where
--     fromJSONKey = FromJSONKeyText LT.fromStrict

instance ( era ~ StandardCrypto ) => ToJSONKey (RewardAcntModal era) where
  toJSONKey  =A.toJSONKeyText anctToJsonKey
    where
      anctToJsonKey (RewardAcntModal acnt) =  serialiseAddress $ fromShelleyStakeAddr acnt


instance Ord (RewardAcntModal era) where
  compare (RewardAcntModal c1) (RewardAcntModal c2) = compare c1 c2

instance ToJSON TimeTranslationReq where
  toJSON (TimeTranslationReq time) =  toJSON  time

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

instance (Crypto era) => ToJSON (AnchorModal era) where
  toJSON (AnchorModal (Anchor url hash)) = A.object [
        "url" .= url
      , "hash" .= hash
    ]

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

instance  (EraCrypto ledgerera ~ StandardCrypto, Ledger.EraPParams ledgerera,Ledger.ConwayEraPParams ledgerera)  =>  FromJSON  (ProposalProcedureModal ledgerera) where
  parseJSON (A.Object o) = do
    deposit :: Integer <- o.:? "deposit" .!= 0
    (RewardAcntModal returnAddress) <- o .: "refundAccount"
    (AnchorModal anchor) <- o .: "anchor"
    mUtxoIdModal <- o .:? "prevGovAction"

    let chain :: FromJSON v =>  A.Key ->( v ->  Parser a) -> Parser a -> Parser a
        chain key mapper onMissing =do
          result <- o.:? key
          maybe onMissing mapper  result

        prevGovActionId :: StrictMaybe (PrevGovActionId purpose StandardCrypto)
        prevGovActionId =case mUtxoIdModal of
            Nothing -> SNothing
            Just (UtxoIdModal ( prevGovTxId, TxIx prevGovTxIx)) -> SJust
                $  PrevGovActionId $ GovActionId (toShelleyTxId prevGovTxId) (GovActionIx $ fromIntegral prevGovTxIx)
        knownKeys = [
          "refundaccount","deposit","anchor","prevgovaction"
          ,"newconstitution","noconfidence","info","withdraw","hardfork","updatecommittee","parameterupdate"
          ]
        extraKeys= Set.difference  (Set.map ( T.unpack . T.toLower) $  Map.keysSet $  A.toMapText o) (Set.fromList knownKeys)
    if null extraKeys then pure ()
    else fail $ "Invalid key: " ++ intercalate ", " (Set.toList extraKeys)
    govAction <-
          chain  "newconstitution"  (parseNewConstitution prevGovActionId)
        $ chain "noconfidence" (parseNoConfidence prevGovActionId)
        $ chain "info" parseInfo
        $ chain "withdraw" parseTreasuryWithdrawal
        $ chain "hardfork" (parseHardforkInitiation prevGovActionId)
        $ chain "updatecommittee" (parseUpdateCommittee prevGovActionId)
        $ chain "parameterupdate" (parseParamUpdate prevGovActionId)
        $ pure InfoAction


    pure $ ProposalProcedureModal $
          ProposalProcedure (Coin deposit) returnAddress  govAction  anchor

    where
        parseParamUpdate :: (purpose ~ 'Ledger.PParamUpdatePurpose,EraPParams ledgerera,Ledger.ConwayEraPParams ledgerera) => StrictMaybe (PrevGovActionId purpose StandardCrypto) -> A.Value -> A.Parser (GovAction ledgerera)
        parseParamUpdate prevGovAction v@(A.Object obj)= do
          let hmap=  A.fromHashMapText $  HM.mapKeys (T.toLower . A.toText ) $ toHashMap obj
          -- pUpdate<- parseJSON v
          let pParamUpdate=emptyPParamsUpdate
              paramParser :: (FromJSON a , EraCrypto ledgerera ~ StandardCrypto,Ledger.ConwayEraPParams ledgerera,
                     EraPParams ledgerera) => T.Text
                  -> Lens' (PParamsUpdate ledgerera) (StrictMaybe b)
                  -> (a -> b)
                  -> PParamsUpdate ledgerera
                  -> Parser (PParamsUpdate ledgerera)
              paramParser k f f2 pUpdate   = do
                minSize <- hmap .:? A.fromText (T.toLower k)
                pure $ case minSize of
                  Nothing ->  pUpdate
                  Just val ->   pUpdate  & f .~ SJust  (f2 val)

          param <-    paramParser  "maxblocksize" ppuMaxBBSizeL id pParamUpdate
                  >>= paramParser  "minFeeA"   Ledger.ppuMinFeeAL id
                  >>= paramParser "MaxBBSize" Ledger.ppuMaxBBSizeL id
                  >>= paramParser "MaxTxSize" Ledger.ppuMaxTxSizeL id
                  >>= paramParser "MaxBHSize" Ledger.ppuMaxBHSizeL id
                  >>= paramParser "KeyDeposit" Ledger.ppuKeyDepositL id
                  >>= paramParser "PoolDeposit" Ledger.ppuPoolDepositL id
                  >>= paramParser "EMax" Ledger.ppuEMaxL id
                  >>= paramParser "NOpt" Ledger.ppuNOptL id
                  >>= paramParser "A0" Ledger.ppuA0L id
                  >>= paramParser "Rho" Ledger.ppuRhoL id
                  >>= paramParser "Tau" Ledger.ppuTauL id
                  -- >>= paramParser "ProtocolVersion" Ledger.ppuProtocolVersionL id
                  >>= paramParser "MinPoolCost" Ledger.ppuMinPoolCostL id
                  >>= paramParser "CoinsPerUTxOByte" Ledger.ppuCoinsPerUTxOByteL id
                  >>= paramParser "CostModels" Ledger.ppuCostModelsL id
                  >>= paramParser "Prices" Ledger.ppuPricesL id
                  >>= paramParser "MaxTxExUnits" Ledger.ppuMaxTxExUnitsL id
                  >>= paramParser "MaxBlockExUnits" Ledger.ppuMaxBlockExUnitsL id
                  >>= paramParser "MaxValSize" Ledger.ppuMaxValSizeL id
                  >>= paramParser "CollateralPercentage" Ledger.ppuCollateralPercentageL id
                  >>= paramParser "MaxCollateralInputs" Ledger.ppuMaxCollateralInputsL id
                  >>= paramParser "PoolVotingThresholds" Ledger.ppuPoolVotingThresholdsL id
                  >>= paramParser "DRepVotingThresholds" Ledger.ppuDRepVotingThresholdsL id
                  >>= paramParser "CommitteeMinSize" Ledger.ppuCommitteeMinSizeL id
                  >>= paramParser "CommitteeMaxTermLength" Ledger.ppuCommitteeMaxTermLengthL id
                  >>= paramParser "GovActionLifetime" Ledger.ppuGovActionLifetimeL id
                  >>= paramParser "GovActionDeposit" Ledger.ppuGovActionDepositL id
                  >>= paramParser "DRepDeposit" Ledger.ppuDRepDepositL id
                  >>= paramParser "DRepActivity" Ledger.ppuDRepActivityL id

          pure $ ParameterChange  prevGovAction  param

        parseParamUpdate _  _ = fail "Expected Protocol Parameter Update Object"

        parseUpdateCommittee prevAction (A.Object  obj)= do
           addMap  <- obj .:? "add" .!= mempty
           remove  <- obj .:? "remove" .!= mempty
           qourum <- obj .: "qourum"
           let toAdd = Map.mapKeys (\(CredentialModal c) -> c) $  Map.map fromInteger addMap
           let toRemove = Set.map (\(CredentialModal c) -> c)  remove
           pure $ UpdateCommittee prevAction  toRemove toAdd (  unJust $ boundRational qourum)
        parseUpdateCommittee _ _ = fail "Expected updateCommittee Object"

        unJust (Just a) =a
        unJust _ = error "asd"


        parseNewConstitution prevAction (ConstitutionModal constitution) = pure $ NewConstitution prevAction constitution

        parseNoConfidence prevAction (A.Object o) = do
           pure $ NoConfidence prevAction
        parseNoConfidence _ _ = pure $ NoConfidence SNothing

        parseHardforkInitiation  prevAction(A.Object protObj) = do
          pVersion <- protObj .: "protocolVersion"
          pure $ HardForkInitiation prevAction pVersion

        parseHardforkInitiation prevAction v = do
          pVersion<-parseJSON v
          pure $ HardForkInitiation SNothing pVersion

        parseNoConfidenceProposasl prevAction _ = pure $ Ledger.NoConfidence prevAction

        parseTreasuryWithdrawal  (A.Object treasuryObj ) = do
          hmap :: Map (RewardAcntModal ledgerera') Coin <- o .: "withdraw"
          let
              mapKeysM f hm = mapM (\(k,v)->do
                    k' <- f k
                    pure (k',v)
                ) (Map.toList hm) <&> Map.fromList

          let hmap'' = Map.mapKeys unWrap hmap

          pure $ TreasuryWithdrawals hmap''
        parseTreasuryWithdrawal  _ = fail "Expected \"accounts\" to be present for treasuryWithdrawal"

        parseInfo (A.Bool True) = pure InfoAction
        parseInfo _ = fail "Expected \"info\" value to be true"

  parseJSON  _ = fail "Expected GovActionModal Object"


instance (Crypto (EraCrypto era),EraCrypto era ~ StandardCrypto, Ledger.Era era, EraPParams era,Ledger.ConwayEraPParams era) => ToJSON (ProposalProcedureModal era) where
  toJSON (ProposalProcedureModal (ProposalProcedure (Coin co) refundAccnt ga an))
    = A.object $ ["anchor" .= an, "deposit" .= co , "refundAccount" .= RewardAcntModal refundAccnt ] ++  (case ga of
   ParameterChange sm ppu -> addPrevGovAction sm [A.fromString "parameterUpdate" .= parameterUpdateJson ppu ]
   HardForkInitiation sm pv -> addPrevGovAction sm ["hardFork" .= A.object ["protocolVersion" .= pv] ]
   TreasuryWithdrawals map ->  ["withdraw" .= Map.mapKeys RewardAcntModal map ]
   NoConfidence sm -> addPrevGovAction sm ["noConfidence" .= True ]
   UpdateCommittee sm toRemove toAdd quorum -> addPrevGovAction sm  ["updateCommittee" .= A.object (
                  [ "quorum" .= unboundRational quorum]
              ++  (["remove" .= Set.map CredentialModal toRemove | not (null toRemove)])
              ++ (["add" .= Map.mapKeys CredentialModal toAdd | not (null toAdd)])
            )]
   NewConstitution sm con -> addPrevGovAction sm  ["newConstitution" .= con ]
   InfoAction -> ["info" .= True ])
    where
      addPrevGovAction :: StrictMaybe (PrevGovActionId purpose (EraCrypto era))-> [A.Pair] -> [A.Pair]
      addPrevGovAction (SJust (PrevGovActionId (GovActionId txid (GovActionIx index)))) v = ("prevGovAction"  .=  renderTxIn ( fromShelleyTxIn  $ Ledger.TxIn txid ( Ledger.TxIx $ fromIntegral index))) : v
      addPrevGovAction SNothing v = v

      parameterUpdateJson ppu = let
                paramUpdateField :: (ToJSON a) => A.Key-> Getting (StrictMaybe v) (PParamsUpdate era) (StrictMaybe v)-> (v -> a)-> [A.Pair]
                paramUpdateField k getter f = case ppu ^. getter  of
                  SJust v -> [ k .= toJSON (f v)]
                  SNothing -> []
            in A.object $ 
             paramUpdateField "Maxblocksize" Ledger.ppuMaxBBSizeL id
          <> paramUpdateField "MinFeeA"   Ledger.ppuMinFeeAL id
          <> paramUpdateField "MaxBBSize" Ledger.ppuMaxBBSizeL id
          <> paramUpdateField "MaxTxSize" Ledger.ppuMaxTxSizeL id
          <> paramUpdateField "MaxBHSize" Ledger.ppuMaxBHSizeL id
          <> paramUpdateField "KeyDeposit" Ledger.ppuKeyDepositL id
          <> paramUpdateField "PoolDeposit" Ledger.ppuPoolDepositL id
          <> paramUpdateField "EMax" Ledger.ppuEMaxL id
          <> paramUpdateField "NOpt" Ledger.ppuNOptL id
          <> paramUpdateField "A0" Ledger.ppuA0L id
          <> paramUpdateField "Rho" Ledger.ppuRhoL id
          <> paramUpdateField "Tau" Ledger.ppuTauL id
          <> paramUpdateField "MinPoolCost" Ledger.ppuMinPoolCostL id
          <> paramUpdateField "CoinsPerUTxOByte" Ledger.ppuCoinsPerUTxOByteL id
          <> paramUpdateField "CostModels" Ledger.ppuCostModelsL id
          <> paramUpdateField "Prices" Ledger.ppuPricesL id
          <> paramUpdateField "MaxTxExUnits" Ledger.ppuMaxTxExUnitsL id
          <> paramUpdateField "MaxBlockExUnits" Ledger.ppuMaxBlockExUnitsL id
          <> paramUpdateField "MaxValSize" Ledger.ppuMaxValSizeL id
          <> paramUpdateField "CollateralPercentage" Ledger.ppuCollateralPercentageL id
          <> paramUpdateField "MaxCollateralInputs" Ledger.ppuMaxCollateralInputsL id
          <> paramUpdateField "PoolVotingThresholds" Ledger.ppuPoolVotingThresholdsL id
          <> paramUpdateField "DRepVotingThresholds" Ledger.ppuDRepVotingThresholdsL id
          <> paramUpdateField "CommitteeMinSize" Ledger.ppuCommitteeMinSizeL id
          <> paramUpdateField "CommitteeMaxTermLength" Ledger.ppuCommitteeMaxTermLengthL id
          <> paramUpdateField "GovActionLifetime" Ledger.ppuGovActionLifetimeL id
          <> paramUpdateField "GovActionDeposit" Ledger.ppuGovActionDepositL id
          <> paramUpdateField "DRepDeposit" Ledger.ppuDRepDepositL id
          <> paramUpdateField "DRepActivity" Ledger.ppuDRepActivityL id

instance (era ~ ConwayEra)  =>  FromJSON (CertificateModal era) where
  parseJSON :: A.Value -> Parser (CertificateModal era)
  parseJSON (A.Object o ) = do
    certType <- o .: "type"
    mDeposit <- o .:? "deposit"
    mAnchor  <- o .:? "anchor"
    let smAnchor =  (case mAnchor of
                              Nothing -> SNothing
                              Just (AnchorModal a) -> SJust a  )

    let depositAmount = case mDeposit of
                                  Nothing -> SNothing
                                  Just lovelace -> SJust (Coin lovelace)
    case T.toLower certType of
      "registerstake" -> do
          (CredentialModal stakeCred) <- o .: "key"
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  . ConwayTxCertDeleg
                                  $ ConwayRegCert
                                      stakeCred
                                      depositAmount
      "deregisterstake" -> do
          (CredentialModal stakeCred) <- o .: "key"
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  . ConwayTxCertDeleg
                                  $ ConwayUnRegCert
                                      stakeCred
                                      depositAmount
      "delegate" -> do
          let spoParser :: MonadFail m =>  T.Text -> m (Hash StakePoolKey)
              spoParser val= do
                case parseBech32Type val (AsHash AsStakePoolKey) of
                    Just p -> pure p
                    Nothing -> parseHexString val >>= parseRawBytes (AsHash AsStakePoolKey)
          (CredentialModal stakeCred) <- o .: "key"
          mSpo1 <- o .:? "spo" -- TODO support pool bech32 type
          mSpo  <- case mSpo1 of
            Nothing ->  o .:? "pool"
            Just txt -> pure $ pure txt
          mDrep <- o.:? "drep"
          delegatee<-case mSpo of
            Nothing -> maybe (fail "expecting either \"spo\" or \"drep\" or both") (\(DrepModal d) -> pure $ DelegVote d) mDrep
            Just spoCred ->do
                pure $ maybe (DelegStake spoCred) (\(DrepModal d)  ->  DelegStakeVote spoCred d) mDrep
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                                  $ ConwayTxCertDeleg $ ConwayDelegCert
                                      stakeCred
                                      delegatee
      "registerdrep" -> do
          (CredentialModal key) <- o .: "key"
          deposit <- o .: "deposit"
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                        . ConwayTxCertGov
                        $ ConwayRegDRep
                            key
                            (Coin deposit)
                            smAnchor
      "deregisterdrep" -> do
          (CredentialModal key) <- o .: "key"
          deposit <- o .: "deposit"
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                        . ConwayTxCertGov
                        $ ConwayUnRegDRep
                            key
                            (Coin deposit)
      "updatedrep" -> do
          (CredentialModal key) <- o .: "key"
          pure $  CertificateModal $  ConwayCertificate ConwayEraOnwardsConway
                        . ConwayTxCertGov
                        $ ConwayUpdateDRep
                            key
                            smAnchor
      _ -> fail "Only Drep/Stake registration, drep delegation and stake de-registration certificate are supported"

  parseJSON  _ = fail "Expected Certificate Object"


instance ( ConwayEra ~ era) => ToJSON (CertificateModal era ) where
  toJSON (CertificateModal cert) = A.object  $case cert of
    ShelleyRelatedCertificate stbe stc -> case stc of
      ShelleyTxCertDelegCert sdc -> []
      ShelleyTxCertPool pc -> []
      ShelleyTxCertGenesisDeleg gdc -> []
      ShelleyTxCertMir mc -> []
    ConwayCertificate ceo ctc -> case ctc of
      ConwayTxCertDeleg cdc -> case cdc of
        ConwayRegCert cre mDeposit -> [ keyJson cre ,"type" .= A.String "registerstake"] ++ withDeposit mDeposit
        ConwayUnRegCert cre mDeposit -> ["type" .= A.String "deregisterstake",keyJson cre] ++ withDeposit mDeposit
        ConwayDelegCert cre del -> ["type" .= A.String "delegate",keyJson cre] ++ delegateeToJson del
        ConwayRegDelegCert cre del co ->    ["type" .= A.String "register+delegate" ,keyJson cre, "deposit" .= co]
                                        ++  delegateeToJson del
      ConwayTxCertPool pc -> case pc of
        RegPool pp -> ["type" .=  A.String "registerpool","params" .= pp]
        RetirePool kh en -> ["type" .=  A.String "retirepool","epoch" .= en]
      ConwayTxCertGov cgc -> case cgc of
        ConwayRegDRep cre co sm -> ["type" .= A.String  "registerdrep",keyJson cre,"deposit" .= co ] ++ anchorToKeyValue sm
        ConwayUnRegDRep cre co -> ["type" .= A.String "deregisterdrep",keyJson cre,"deposit" .= co ]
        ConwayUpdateDRep cre sm -> ["type" .= A.String "updatedrep",keyJson cre] ++ anchorToKeyValue sm
        ConwayAuthCommitteeHotKey cre_cold cre_hot -> ["type" .= A.String "authcommittee", keyJson cre_cold, "hotkey" .= CredentialModal cre_hot]
        ConwayResignCommitteeColdKey cre sm -> ("type" .= A.String"resigncommittee") : anchorToKeyValue sm

    where
      keyJson cre = "key" .= CredentialModal cre
      delegateeToJson del =  case del of
          DelegStake kh -> ["pool" .= kh]
          DelegVote c -> ["drep" .= DrepModal c]
          DelegStakeVote kh dr -> ["pool" .= kh,"drep" .= DrepModal dr ]

      withDeposit md = addKeyIfJust "deposit" md



instance ( ledgerera ~ StandardCrypto ,Crypto ledgerera) => FromJSON (CredentialModal r ledgerera) where
  parseJSON :: A.Value -> Parser (CredentialModal r ledgerera)
  parseJSON (A.Object o) = do
    cred <- asum [parser1 o, parser2 o]
    pure $ CredentialModal cred
      where
        parser1 obj = ScriptHashObj <$> (obj .: "scriptHash" <|> obj .: "scripthash" <|> obj .: "script hash")
        parser2 obj = KeyHashObj <$> (obj .: "keyHash" <|> obj .: "keyhash"  <|> obj .: "key hash")
  parseJSON (A.String txt) = parseCredentialText txt
  parseJSON  _ = fail "Expected  Credential object or string"

parseCredentialText txt = case parseHexString txt of
    Just unHexed -> parseRawBytes unHexed True
    Nothing -> case parseRawBech32'  txt of
      Just parsed ->parseRawBytes parsed True
      Nothing -> (parseCredential txt <&> CredentialModal) <|> fail "Neither bech32 encoded credential nor  hex encoded"
    where
      parseRawBytes bytes stripPrefix=  do
                hash <- rawBytesToCred bytes stripPrefix
                pure $ CredentialModal  hash

instance Ord (CredentialModal r cre) where
  compare (CredentialModal c1) (CredentialModal c2) = compare c1 c2



instance ( era ~ StandardCrypto, Typeable r ) => ToJSONKey (CredentialModal r era) where
  toJSONKey  =A.toJSONKeyText anctToJsonKey
    where
      anctToJsonKey (CredentialModal cre) = case cre of
        ScriptHashObj sh -> Ledger.credToText cre
        KeyHashObj kh ->T.drop 4 $ serializeAsHexText kh --TODO do this properly

instance FromJSON VoteModal where
  parseJSON (A.String txt) = case T.toLower txt of
    "abstain" -> pure $ VoteModal Ledger.Abstain
    "yes" -> pure $ VoteModal Ledger.VoteYes
    "no"  -> pure $ VoteModal Ledger.VoteNo
    _ ->  fail  $ "Expected  \"yes\", \"no\" or \"abstain\" " ++"got: \"" ++ T.unpack txt ++ "\""
  parseJSON (A.Bool b) = pure $ VoteModal $ if b then  Ledger.VoteYes else Ledger.VoteNo
  parseJSON (A.Number n) = pure $ VoteModal if round n ==0 then Ledger.VoteNo else Ledger.VoteYes
  parseJSON _ = fail "Expected vote to be boolean or \"yes\", \"no\" or \"abstain\""

instance ToJSON VoteModal where
  toJSON (VoteModal vote) = case vote of
    Ledger.VoteNo -> A.Bool False
    Ledger.VoteYes -> A.Bool True
    Ledger.Abstain -> "abstain"

instance (Crypto (EraCrypto era)) => FromJSON (VotingProcedureModal era) where
  parseJSON (A.Object o) = do
     mAnchor <- o .:? "anchor"
     (VoteModal vote) <- o .: "vote"
     pure $ VotingProcedureModal (Ledger.VotingProcedure vote (toStrictMaybe mAnchor))
  parseJSON _ = fail "Expected VotingProcedure Object"

instance   FromJSON (TxVote ConwayEra) where
  parseJSON (A.Object o) = do
      govAction <- o .: "govAction" <|> o .: "proposal"
      mAnchor <- o.:? "anchor"
      (VoteModal vote) <- o .: "vote"
      voter <-  o.: "voter" 
      let
          mkTxVote f = do
            pure $ TxVote $ TxVoteL (govActionId govAction) (Ledger.VotingProcedure vote (toStrictMaybe mAnchor)) f
          getCred credBytes= rawBytesToCred credBytes  True <|> fail ".voter bech32 is not valid credential"
      case parseRawBech32_ voter  of
        Just (header,credBytes) -> do
            case header of
              "drep" ->  getCred credBytes >>= (mkTxVote . Ledger.DRepVoter)
              "cc_hot" -> getCred credBytes >>= (mkTxVote . Ledger.CommitteeVoter)
              "pool" -> case hashFromBytes credBytes of
                  Nothing -> fail $  "Invalid key :" ++ T.unpack voter
                  Just ha -> mkTxVote $ Ledger.StakePoolVoter (KeyHash ha)
              _ -> fail $ "Invalid credential type : " ++ T.unpack header

        Nothing  -> do
          role <- o.: "role" <|> fail ".role is required when voter is not bech32 credential"
          credBytes <- parseHexString' voter ".voter cannot parse as bech32 or hex "
          if role == "drep"
            then  getCred credBytes >>= (mkTxVote . Ledger.DRepVoter)
          else if role == "cc" || role == "committee"  || role == "cc_hot"
            then getCred credBytes >>= (mkTxVote . Ledger.CommitteeVoter)
          else if role == "pool" || role =="stakepool" || role=="stakePool"
            then case hashFromBytes credBytes of
                  Nothing -> fail $  "Invalid key :" ++ T.unpack voter
                  Just ha -> mkTxVote $ Ledger.StakePoolVoter (KeyHash ha)
          else fail $ ".role expected: drep, pool or cc but got: " ++ T.unpack role
    where
        govActionId
            (UtxoIdModal ( prevGovTxId, TxIx prevGovTxIx))
                =  GovActionId (toShelleyTxId prevGovTxId) (GovActionIx $ fromIntegral prevGovTxIx)
  parseJSON _ = fail "Expected Vote Object"

instance   ToJSON (TxVote ConwayEra) where
  toJSON (TxVote ( TxVoteL (GovActionId txid (GovActionIx index)) (Ledger.VotingProcedure vote mAnchor) voter) ) = A.object $ [
          "vote" .= VoteModal vote
        , "proposal" .= renderTxIn ( fromShelleyTxIn  $ Ledger.TxIn txid ( Ledger.TxIx $ fromIntegral index))
    ] ++ anchorToKeyValue mAnchor 
      ++  (case voter of
          Ledger.CommitteeVoter cre -> ["voter" .=CredentialModal cre, "role" .= A.String "committee"]
          Ledger.DRepVoter cre -> ["voter" .=CredentialModal cre,"role" .= A.String "drep"]
          Ledger.StakePoolVoter kh -> ["voter" .=CredentialModal (KeyHashObj kh) , "role" .= A.String "stakePool"])

instance ( ledgerera ~ StandardCrypto ,Crypto ledgerera) => FromJSONKey (CredentialModal r ledgerera) where
  fromJSONKey  = A.FromJSONKeyTextParser  parseCredentialText

instance (Crypto ledgerera, Typeable r) => ToJSON (CredentialModal r ledgerera) where
  toJSON (CredentialModal cre) = case cre of
    ScriptHashObj sh -> A.object ["scriptHash" .= sh ]
    KeyHashObj kh -> A.String $ T.drop 4 $ serializeAsHexText kh


-- Serialization format  header for stake address
-- -- ┏━━━━━━━━━━━━━━━━┳━┯━┯━┯━┯━┯━┯━┯━┓
-- -- ┃ Reward Account ┃1┊1┊1┊x┊0┊0┊0┊x┃
-- -- ┗━━━━━━━━━━━━━━━━╋━┿━┿━┿━┿━┿━┿━┿━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
-- --                  ┃1┊1┊1┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingKey    ┃
-- --                  ┃1┊1┊1┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingKey    ┃
-- --                  ┃1┊1┊1┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingKey    ┃
-- --                  ┃1┊1┊1┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingKey    ┃
-- --                  ┗━┷━┷━┷━┷━┷━┷━┷━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-- --                          \       \
-- --                           \       `Is Mainnet Address
-- --                            `Account Credential is a Script

rawBytesToCred :: (HashAlgorithm (ADDRHASH c), MonadFail f) => Crypto.ByteString -> Bool -> f (Credential kr c)
rawBytesToCred bytes stripPrefix
  | BS.length bytes == 28 =  doConvert bytes <&> KeyHashObj. KeyHash
  | BS.length bytes ==29 && stripPrefix = let
              headerByte = BS.index bytes 0
              isPublicKeyHash = (headerByte .&. 0x10 ::Word8) ==0
              hash = BS.drop 1 bytes
         in  if isPublicKeyHash
                then doConvert hash <&> KeyHashObj . KeyHash
                else doConvert hash <&> ScriptHashObj . Hashes.ScriptHash
  | otherwise = fail "Credential Hash is not of 28 byte length "
  where
    doConvert :: (HashAlgorithm h, MonadFail m) => BS8.ByteString -> m (Crypto.Hash h a)
    doConvert bytes
      = case hashFromBytes bytes of
              Nothing
                -> fail "Credential Hash .fromBytes, invalid credentialHash"
              Just ha -> pure ha

instance (era ~ StandardCrypto) =>  FromJSON  (DrepModal era) where

  parseJSON :: A.Value -> Parser (DrepModal era)
  parseJSON (A.Object o ) = do
        cred <- asum [parser1 o, parser2 o]
        pure $ DrepModal . DRepCredential $ cred
          where
            parser1 obj = ScriptHashObj <$> (obj .: "scriptHash" <|> obj .: "scripthash" <|> obj .: "script hash")
            parser2 obj = KeyHashObj <$> (obj .: "keyHash" <|> obj .: "keyhash"  <|> obj .: "key hash")
  parseJSON (A.String s) = case T.toLower s of
    "abstain" -> pure  $ DrepModal DRepAlwaysAbstain
    "noconfidence" -> pure  $ DrepModal DRepAlwaysNoConfidence
    txt -> do
      case parseHexString txt of
        Just (unHexed ::BS.ByteString) -> drepFromBytes unHexed
        Nothing -> do
              rawBytes <- parseRawBech32 (Set.fromList $ map T.pack ["drep", "drep_test", "stake", "stake_test"]) txt
              drepFromBytes rawBytes
    where
      drepFromBytes bytes =  DrepModal . DRepCredential   <$>  rawBytesToCred  bytes True

  parseJSON  _ = fail "Expected Drep Object"

instance (era ~ StandardCrypto) => ToJSON (DrepModal era) where
  toJSON (DrepModal drep)= case drep of
      DRepCredential cre -> toJSON (CredentialModal cre)
      DRepAlwaysAbstain -> A.String "abstain"
      DRepAlwaysNoConfidence -> A.String "noConfidence"

parseFromDrepKeyBytes unHexed = case parseRawBytes (AsHash AsDRepKey) unHexed   of
    Just ( CAPI.DRepKeyHash drepkey) ->  let
                  keyHashobj = KeyHashObj drepkey
                in pure $ DrepModal $  DRepCredential keyHashobj
    Nothing -> fail "keyHash is not a valid Drep KeyHash"

data AsDrepModal

instance FromJSON UtxoIdModal where
  parseJSON v = txInParser v <&> (\(TxIn txid ix)-> UtxoIdModal (txid,ix))

toStrictMaybe (Just v) = SJust v
toStrictMaybe Nothing =SNothing

fromStrictMaybe :: StrictMaybe a -> Maybe a
fromStrictMaybe (SJust v) = Just v
fromStrictMaybe SNothing = Nothing

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


anchorToKeyValue  ma = addKeyIfJust "anchor" ma

addKeyIfJust  k = \case
  SJust val ->  [  k .= val]
  SNothing -> []