{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Kuber.Core.TxBuilder

where


import Cardano.Api hiding(txMetadata, txFee,txCertificates)
import Cardano.Api.Shelley hiding (txMetadata, txFee,txCertificates)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Map (Map)
import Control.Exception
import Data.Either
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import PlutusLedgerApi.V2 (PubKeyHash(PubKeyHash), CurrencySymbol)
import qualified PlutusLedgerApi.V2 as Plutus hiding (TxOut)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as A.Object
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import Debug.Trace (trace, traceM)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString(fromString))
import qualified Debug.Trace as Debug
import qualified Data.Aeson as Aeson
import Data.Word (Word64)
import qualified Data.HashMap.Internal.Strict as H
import Data.Bifunctor
import GHC.Generics (Generic)
import Data.Time.Clock.POSIX
import Foreign.C (CTime)
import Cardano.Api.Ledger (StandardCrypto, EraCrypto, GovActionId)
import qualified Cardano.Ledger.Api.Era as Ledger
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Api (Babbage)
import qualified Cardano.Ledger.Api as Ledger


newtype TxSimpleScript = TxSimpleScript SimpleScript
    deriving(Show)

data TxScript = TxScriptSimple TxSimpleScript
        |   TxScriptPlutus   TxPlutusScript
        deriving (Show)


data  TxVoteL (ledgerera) = TxVoteL
                        (Ledger.GovActionId (EraCrypto ledgerera))
                        (Ledger.VotingProcedure  ledgerera)
                        (Ledger.Voter (EraCrypto ledgerera))
                      deriving (Show,Eq)

newtype (TxVote era) = TxVote (TxVoteL (ShelleyLedgerEra era)) deriving (Show,Eq)




data  TxPlutusScript  =
    TxPlutusScriptV1 (PlutusScript  PlutusScriptV1)
  | TxPlutusScriptV2 (PlutusScript  PlutusScriptV2)
                          deriving (Show)


data TxInputResolved_ era = TxInputUtxo (UTxO era)
              | TxInputScriptUtxo TxPlutusScript (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) (TxIn,TxOut CtxUTxO era)
              | TxInputReferenceScriptUtxo TxIn (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) (TxIn,TxOut CtxUTxO era)

              deriving (Show)


data TxInputUnResolved_ era = TxInputTxin TxIn
              | TxInputSkey (SigningKey PaymentKey)
              | TxInputAddr (AddressInEra era)
              | TxInputScriptTxin TxPlutusScript (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) TxIn
              | TxInputReferenceScriptTxin TxIn (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) TxIn

              deriving (Show)

data TxInput era = TxInputResolved (TxInputResolved_ era) | TxInputUnResolved (TxInputUnResolved_ era) deriving (Show)

data TxInputReference era = TxInputReferenceTxin TxIn
    |  TxInputReferenceUtxo (UTxO era)
    deriving (Show)

data TxOutputContent era =
     TxOutPkh PubKeyHash Value
  |  TxOutScript TxPlutusScript Value  (Hash ScriptData)
  |  TxOutScriptInline TxPlutusScript Value (Hash ScriptData)
  |  TxOutScriptWithScript TxPlutusScript Value (Hash ScriptData) TxScript
  |  TxOutScriptWithData TxPlutusScript Value HashableScriptData
  |  TxOutScriptWithDataAndScript TxPlutusScript Value HashableScriptData TxScript
  |  TxOutScriptWithDataAndReference TxPlutusScript Value HashableScriptData
  |  TxOutNative (TxOut  CtxTx era)
   deriving (Show)

data TxOutput content =TxOutput {
  content :: content,
  deductFee :: Bool,
  addChange :: Bool,
  onMinAda :: InsufficientUtxoAdaAction
} deriving (Show)

transfrormOutput :: TxOutput content1 -> content2 -> TxOutput content2
transfrormOutput (TxOutput con fee change minAda) v =TxOutput v fee change minAda

data InsufficientUtxoAdaAction = DropOnUtxoInsufficientUtxoAda | IncreaseOnUtxoInsufficientUtxoAda|ErrorOnInsufficientUtxoAda | OnInsufficientUtxoAdaUnset deriving (Show,Eq,Generic, ToJSON )


data TxCollateral era =  TxCollateralTxin TxIn
                  |  TxCollateralUtxo (UTxO era) deriving (Show)

data TxSignature era =  TxSignatureAddr (AddressInEra era)
                  | TxSignaturePkh PubKeyHash
                  | TxSignatureSkey (SigningKey PaymentKey)
                  deriving (Show)



data TxChangeAddr era = TxChangeAddrUnset
                  | TxChangeAddr (AddressInEra era) deriving (Show)


data TxInputSelection era = TxSelectableAddresses [Ledger.Addr StandardCrypto]
                  | TxSelectableUtxos  (UTxO era)
                  | TxSelectableTxIn [TxIn]
                  | TxSelectableSkey [SigningKey PaymentKey]
                  deriving(Show)


data TxMintingScriptSource =
            TxMintingPlutusScript TxPlutusScript  (Maybe ExecutionUnits ) HashableScriptData
          | TxMintingReferenceScript TxIn (Maybe ExecutionUnits ) (Maybe HashableScriptData)
          | TxMintingSimpleScript TxSimpleScript
            deriving  (Show)

data TxMintData  s = TxMintData  s  [(AssetName ,Quantity)]  (Map Word64 (Map AssetName Aeson.Value)) deriving (Show)

data ValidityTimestamp = NoValidityTime
  | ValidityPosixTime POSIXTime
  | ValiditySlot  SlotNo deriving (Show,Eq)

instance Semigroup ValidityTimestamp where
  (<>) = maxValidity

instance Monoid ValidityTimestamp where
  mempty = NoValidityTime

minValidity :: ValidityTimestamp -> ValidityTimestamp -> ValidityTimestamp
minValidity NoValidityTime v2 = v2
minValidity v1 NoValidityTime = v1
minValidity (ValidityPosixTime t1) (ValidityPosixTime t2) = ValidityPosixTime (min t1 t2)
minValidity (ValiditySlot s1) (ValiditySlot s2) = ValiditySlot (min s1 s2)
minValidity v1@(ValiditySlot _) _ = v1
minValidity _ v2 = v2


maxValidity :: ValidityTimestamp -> ValidityTimestamp -> ValidityTimestamp
maxValidity NoValidityTime v2 = v2
maxValidity v1 NoValidityTime = v1
maxValidity (ValidityPosixTime t1) (ValidityPosixTime t2) = ValidityPosixTime (max t1 t2)
maxValidity (ValiditySlot s1) (ValiditySlot s2) = ValiditySlot (max s1 s2)
maxValidity v1@(ValiditySlot _) _ = v1
maxValidity _ v2 = v2

-- |  
-- `TxBuilder` is not to be directly used but, parts of it are constructed using helper functions.
-- Multiple builder parts can be combined to construct full transaction specification

type TxBuilder =  (TxBuilder_ ConwayEra)

data TxBuilder_  era  =TxBuilder_{
    txSelections :: [TxInputSelection era],
    txInputs:: [TxInput era],
    txInputReferences:: [TxInputReference era],
    txOutputs :: [TxOutput (TxOutputContent era) ],
    txCollaterals :: [TxCollateral era],  -- collateral for the transaction
    txValidityStart :: ValidityTimestamp,
    txValidityEnd :: ValidityTimestamp,
    txMintData :: [TxMintData TxMintingScriptSource],
    txSignatures :: [TxSignature era],
    txProposals :: [Proposal era],
    txVotes :: [TxVote  era],
    txCertificates :: [Certificate era],
    txFee :: Maybe Integer,
    txDefaultChangeAddr :: Maybe (AddressInEra era),
    txMetadata' :: Map Word64 Aeson.Value
  } deriving (Show)


class IsShelleyBasedEra era => IsTxBuilderEra era where
  bMaryOnward :: MaryEraOnwards era
  bAlonzoOnward :: AlonzoEraOnwards era
  bBabbageOnward :: BabbageEraOnwards era
  bConwayOnward :: Maybe (ConwayEraOnwards era)
  bShelleyBasedEra :: ShelleyBasedEra era
  bCardanoEra :: CardanoEra era
  bAsEra :: AsType era


instance IsTxBuilderEra ConwayEra where
  bMaryOnward = MaryEraOnwardsConway
  bAlonzoOnward = AlonzoEraOnwardsConway
  bBabbageOnward = BabbageEraOnwardsConway
  bConwayOnward = Just ConwayEraOnwardsConway
  bShelleyBasedEra = ShelleyBasedEraConway
  bCardanoEra = ConwayEra
  bAsEra = AsConwayEra

instance IsTxBuilderEra BabbageEra where
  bMaryOnward = MaryEraOnwardsBabbage
  bAlonzoOnward = AlonzoEraOnwardsBabbage
  bBabbageOnward = BabbageEraOnwardsBabbage
  bConwayOnward = Nothing
  bShelleyBasedEra = ShelleyBasedEraBabbage
  bCardanoEra =  BabbageEra
  bAsEra = AsBabbageEra


instance Monoid (TxBuilder_ era) where
  mempty = TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty

instance Semigroup (TxBuilder_ era) where
  (<>)  txb1 txb2 =TxBuilder_{
    txSelections = txSelections txb1 ++ txSelections txb2,
    txInputs = txInputs txb1 ++ txInputs txb2,
    txInputReferences = txInputReferences txb1 ++ txInputReferences txb2,
    txOutputs = txOutputs txb1 ++ txOutputs txb2,
    txCollaterals  = txCollaterals txb1 ++ txCollaterals txb2,  -- collateral for the transaction
    txValidityStart = minValidity (txValidityStart txb1) (txValidityStart txb2),
    txValidityEnd = maxValidity (txValidityEnd txb1) (txValidityEnd txb2),
    txMintData = txMintData txb1 <> txMintData txb2,
    txSignatures = txSignatures txb1 ++ txSignatures txb2,
    txProposals = txProposals txb1 ++ txProposals txb2,
    txVotes = txVotes txb1 ++ txVotes txb2,
    txCertificates = txCertificates txb1 ++ txCertificates txb2,
    txFee  = case txFee txb1 of
      Just f -> case txFee txb2 of
        Just f2 -> Just $ max f f2
        _ -> Just f
      Nothing -> txFee txb2,
    txDefaultChangeAddr = case txDefaultChangeAddr txb1 of
      Just addr -> Just addr
      _ -> txDefaultChangeAddr txb2,
    txMetadata' = txMetadata' txb1 <> txMetadata' txb2
  }


txSelection :: TxInputSelection ConwayEra -> TxBuilder
txSelection v = TxBuilder_  [v] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty

txInput :: TxInput ConwayEra -> TxBuilder
txInput v = TxBuilder_  [] [v] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty

txInputReference :: TxInputReference ConwayEra -> TxBuilder
txInputReference v = TxBuilder_  [] [] [v] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty


txMints :: [TxMintData TxMintingScriptSource] -> TxBuilder
txMints md= TxBuilder_  [] [] [] [] [] mempty mempty md [] [] [] [] Nothing Nothing Map.empty


txOutput :: TxOutput (TxOutputContent ConwayEra) -> TxBuilder
txOutput v =  TxBuilder_  [] [] [] [v] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty

txCollateral' :: (TxCollateral ConwayEra) -> TxBuilder
txCollateral' v =  TxBuilder_  [] [] [] [] [v] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty

txSignature :: (TxSignature ConwayEra) -> TxBuilder
txSignature v =  TxBuilder_  [] [] [] [] [] mempty mempty [] [v] [] [] [] Nothing Nothing Map.empty



-- Transaction validity

-- Set validity Start and end time in posix seconds
txValidPosixTimeRange :: POSIXTime  -> POSIXTime -> TxBuilder
txValidPosixTimeRange start end = TxBuilder_  [] [] [] [] [] (ValidityPosixTime start ) (ValidityPosixTime end) [] [] [] [] [] Nothing Nothing Map.empty

-- set  validity statart time in posix seconds
txValidFromPosixTime:: POSIXTime -> TxBuilder
txValidFromPosixTime start =  TxBuilder_  [] [] [] [] [] (ValidityPosixTime start) mempty [] [] [] [] [] Nothing Nothing Map.empty

-- set transaction validity end time in posix seconds
txValidUntilPosixTime :: POSIXTime -> TxBuilder
txValidUntilPosixTime end =  TxBuilder_  [] [] [] [] [] mempty (ValidityPosixTime  end) [] [] [] [] [] Nothing Nothing Map.empty

-- Set validity Start and end slot
txValidSlotRange :: SlotNo  -> SlotNo -> TxBuilder
txValidSlotRange start end = TxBuilder_  [] [] [] [] [] (ValiditySlot  start ) (ValiditySlot end) [] [] [] [] [] Nothing Nothing Map.empty

-- set  validity statart time in posix seconds
txValidFromSlot:: SlotNo -> TxBuilder
txValidFromSlot start =  TxBuilder_  [] [] [] [] [] (ValiditySlot start) mempty [] [] [] [] [] Nothing Nothing Map.empty

-- set transaction validity end time in posix seconds
txValidUntilSlot :: SlotNo  -> TxBuilder
txValidUntilSlot end =  TxBuilder_  [] [] [] [] [] mempty (ValiditySlot  end) [] [] [] [] [] Nothing Nothing Map.empty


-- governanceProposals
txProposal :: Proposal ConwayEra -> TxBuilder
txProposal p =TxBuilder_  [] [] [] [] [] mempty mempty [] [] [p] [] [] Nothing Nothing Map.empty

-- voting
txVote :: TxVote ConwayEra -> TxBuilder
txVote v =TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [v] [] Nothing Nothing Map.empty

-- voting
txCertificate :: Certificate ConwayEra -> TxBuilder
txCertificate v =TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [] [v] Nothing Nothing Map.empty


--- minting
_txMint  v = txMints [v]

-- | Mint token with plutus v1 or v2 script
txMintPlutusScript :: IsPlutusScript script =>script  ->  HashableScriptData -> [(AssetName,Quantity)] -> TxBuilder
txMintPlutusScript script sData amounts = _txMint $ TxMintData (TxMintingPlutusScript  (toTxPlutusScript script) Nothing sData) amounts Map.empty

-- | Mint token with simple script
txMintSimpleScript :: IsSimpleScript script =>script  -> [(AssetName,Quantity)] -> TxBuilder
txMintSimpleScript script amounts = _txMint $ TxMintData (TxMintingSimpleScript  (toTxSimpleScript script)) amounts Map.empty


-- txMintWithMetadata :: IsMintingScript script =>script  ->   [(AssetName,Integer)] -> Map Word64 (Map AssetName Aeson.Value)  -> TxBuilder
-- txMintWithMetadata script amounts mp = _txMint $ TxMintData (TxMintingScriptCode $ toTxMintingScript script) amounts mp
--  witness (valueFromList  $ map (bimap (AssetId policyId) Quantity )  amounts )
--   where
--     witness=   SimpleScriptWitness SimpleScriptV2InConway SimpleScriptV2 (SScript simpleScript)
--     script = SimpleScript SimpleScriptV2 simpleScript
--     policyId = scriptPolicyId script

-- | Pay to this address in transaction
txPayTo:: AddressInEra ConwayEra ->Value ->TxBuilder
txPayTo addr v=  txOutput $  TxOutput (TxOutNative $TxOut addr  (TxOutValue MaryEraOnwardsConway v) TxOutDatumNone ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset

-- | Pay to address  and inline the script in resulting utxo.
txPayToWithReferenceScript:: AddressInEra ConwayEra ->Value -> TxScript  ->TxBuilder
txPayToWithReferenceScript  addr v pScript=  txOutput $  TxOutput (TxOutNative $TxOut addr  (TxOutValue MaryEraOnwardsConway v) TxOutDatumNone (ReferenceScript BabbageEraOnwardsConway (txScriptToScriptAny pScript) ))False False OnInsufficientUtxoAdaUnset

-- | Pay to the enterprise address of this PublicKeyHash
txPayToPkh:: PubKeyHash  ->Value ->TxBuilder
txPayToPkh pkh v= txOutput $  TxOutput ( TxOutPkh  pkh  v ) False False OnInsufficientUtxoAdaUnset

-- | Pay to script address with datumHash
txPayToScript :: AddressInEra ConwayEra -> Value -> Hash ScriptData -> TxBuilder
txPayToScript addr v d = txOutput $TxOutput (TxOutNative $TxOut addr  (TxOutValue MaryEraOnwardsConway v) (TxOutDatumHash AlonzoEraOnwardsConway d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset

-- | Pay to script address and inline the datum in utxo
txPayToScriptWithData :: AddressInEra ConwayEra -> Value -> HashableScriptData -> TxBuilder
txPayToScriptWithData addr v d  = txOutput $ TxOutput  (TxOutNative $ TxOut  addr (TxOutValue MaryEraOnwardsConway v)  (TxOutDatumInline BabbageEraOnwardsConway d) ReferenceScriptNone ) False False OnInsufficientUtxoAdaUnset

-- | Pay to the script and inline it in the utxo. Script enterprise address is derrived from script hash
txPayToScriptWithReference :: IsPlutusScript sc => sc -> Value -> Hash ScriptData -> TxBuilder
txPayToScriptWithReference pScript v d = txOutput $ TxOutput (TxOutScript (toTxPlutusScript pScript) v d) False False OnInsufficientUtxoAdaUnset

-- | Pay to script  with inline both datum and inline it in datum. Script enterprise address is derrived from script hash
txPayToScriptWithDataAndReference :: IsPlutusScript sc => sc -> Value -> HashableScriptData -> TxBuilder
txPayToScriptWithDataAndReference pScript v d  =
  txOutput $ TxOutput (TxOutScriptWithData (toTxPlutusScript  pScript) v d) False False OnInsufficientUtxoAdaUnset

-- input consmptions

-- use Utxo as input in the transaction
txConsumeUtxos :: UTxO ConwayEra -> TxBuilder
txConsumeUtxos utxo =  txInput $ TxInputResolved $  TxInputUtxo  utxo

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn  v = txInput $ TxInputUnResolved $ TxInputTxin v

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txReferenceTxIn :: TxIn -> TxBuilder
txReferenceTxIn  v = txInputReference $ TxInputReferenceTxin v

txReferenctUtxo :: TxIn -> TxOut CtxUTxO ConwayEra -> TxBuilder
txReferenctUtxo tin tout = txInputReference$ TxInputReferenceUtxo (UTxO $ Map.singleton tin tout)

-- use txIn as input in the transaction
-- Since TxOut is also given the txIn is not queried from the node.
txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> TxBuilder
txConsumeUtxo tin v =txConsumeUtxos $ UTxO $ Map.singleton tin  v

-- |Mark this address as txExtraKeyWitness in the transaction object.
txSignBy :: AddressInEra ConwayEra -> TxBuilder
txSignBy  a = txSignature (TxSignatureAddr a)

-- |Mark this PublicKeyhash as txExtraKeyWitness in the transaction object.
txSignByPkh :: PubKeyHash  -> TxBuilder
txSignByPkh p = txSignature $ TxSignaturePkh p

-- Mark this signingKey's vKey as txExtraKey Witness in the transaction object.
-- When validating `txSignedBy` in plutus, this can be used to add the
txSign :: SigningKey PaymentKey -> TxBuilder
txSign p = txSignature $ TxSignatureSkey p


-- | explicitly set Fee for the transaction
txSetFee :: Integer -> TxBuilder
txSetFee v = TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [] [] (Just v) Nothing Map.empty

txMetadata :: Map Word64 Aeson.Value -> TxBuilder
txMetadata  =  TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing


class IsPlutusVersion v where
  toTxPlutusScriptInstance :: PlutusScript v -> TxPlutusScript

instance IsPlutusVersion PlutusScriptV1  where
   toTxPlutusScriptInstance = TxPlutusScriptV1

instance IsPlutusVersion PlutusScriptV2  where
   toTxPlutusScriptInstance = TxPlutusScriptV2


class IsPlutusScript sc where
  toTxPlutusScript :: sc -> TxPlutusScript

class IsSimpleScript sc where
  toTxSimpleScript :: sc -> TxSimpleScript

instance IsPlutusScript TxPlutusScript where
  toTxPlutusScript = id

instance IsSimpleScript TxSimpleScript where
  toTxSimpleScript = id

instance IsSimpleScript SimpleScript where
  toTxSimpleScript = TxSimpleScript

hashPlutusScript :: TxPlutusScript -> ScriptHash
hashPlutusScript sc = case sc of
  TxPlutusScriptV1 ps -> hashScript (PlutusScript  PlutusScriptV1 ps)
  TxPlutusScriptV2 ps -> hashScript (PlutusScript  PlutusScriptV2 ps)

plutusScriptAddr :: IsShelleyBasedEra era => TxPlutusScript -> NetworkId -> AddressInEra era
plutusScriptAddr sc networkId =
    let payCred = PaymentCredentialByScript (hashPlutusScript sc)
        addr = makeShelleyAddress networkId payCred NoStakeAddress
        addrInEra = AddressInEra (ShelleyAddressInEra shelleyBasedEra) addr
    in addrInEra

plutusScriptToScriptAny :: TxPlutusScript -> ScriptInAnyLang
plutusScriptToScriptAny sc = case sc of
    TxPlutusScriptV1 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV1) (PlutusScript PlutusScriptV1 ps)
    TxPlutusScriptV2 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV2) (PlutusScript PlutusScriptV2 ps)

instance  IsPlutusVersion ver => IsPlutusScript (PlutusScript ver) where
  toTxPlutusScript  = toTxPlutusScriptInstance

-- instance  IsPlutusVersion ver => IsPlutusScript (PlutusScript ver) where
--   toTxPlutusScript  = toTxPlutusScriptInstance

instance   IsPlutusVersion ver => IsPlutusScript (Script ver) where
  toTxPlutusScript  (PlutusScript psv ps) = toTxPlutusScript ps
  toTxPlutusScript _ = error "Impossible"


class IsMintingScript sc where
  toTxMintingScript:: sc -> TxScript

txScriptPolicyId :: TxScript -> PolicyId
txScriptPolicyId sc = PolicyId (hashTxScript sc)


txScriptAddress :: IsShelleyBasedEra era => TxScript -> NetworkId -> StakeAddressReference -> AddressInEra era
txScriptAddress sc net = makeShelleyAddressInEra  shelleyBasedEra net   (PaymentCredentialByScript $ txScriptHash sc)

txScriptHash :: TxScript -> ScriptHash
txScriptHash = hashTxScript

hashTxScript :: TxScript -> ScriptHash
hashTxScript sc = case sc of
  TxScriptSimple (TxSimpleScript ss) -> hashScript (SimpleScript ss)
  TxScriptPlutus tps -> hashPlutusScript tps


txScriptToScriptAny :: TxScript -> ScriptInAnyLang
txScriptToScriptAny sc = case sc of
  TxScriptSimple (TxSimpleScript ss) ->  ScriptInAnyLang SimpleScriptLanguage (SimpleScript ss)
  TxScriptPlutus tps -> plutusScriptToScriptAny tps

txScriptFromScriptAny:: ScriptInAnyLang -> TxScript 
txScriptFromScriptAny = \case ScriptInAnyLang sl sc -> case sc of 
                                SimpleScript ss -> TxScriptSimple $ toTxSimpleScript  ss
                                PlutusScript psv ps -> case psv of
                                  PlutusScriptV1 -> TxScriptPlutus$  toTxPlutusScript ps
                                  PlutusScriptV2 -> TxScriptPlutus$  toTxPlutusScript ps
                                  _ -> error "Cardano.Kuber.Core.Txbuilder.txScriptFromScriptAny TODO: for pv3"

class IsScriptVersion v where
  translationFunc :: Script v -> TxScript

instance IsScriptVersion PlutusScriptV1  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps

instance IsScriptVersion PlutusScriptV2  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps


instance (IsPlutusVersion ver) => IsMintingScript (PlutusScript ver) where
  toTxMintingScript  v = TxScriptPlutus (toTxPlutusScript v)

-- | Add a  script utxo containing datum-hash to  transaction input . Script code, datum matching datumHash and redeemer should be  passed for building transaction.
txRedeemUtxoWithDatum :: IsPlutusScript sc =>  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra ->   sc   -> HashableScriptData  -> HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithDatum  txin txout sc _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo  (toTxPlutusScript sc)  (Just _data) _redeemer  exUnitsM  ( txin,  txout)

-- | Add a  script utxo containing inline-datum  to  transaction input. Script code and redeemer should be  passed for building transaction.
txRedeemUtxo :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> sc  -> HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxo txin txout script _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo  (toTxPlutusScript script)  Nothing _redeemer  exUnitsM  ( txin,  txout)

-- | Add a script utxo-reference containing inline-datum to transaction input.  Script code and Reedemer  should be passed.
txRedeemTxin :: IsPlutusScript sc => TxIn  -> sc  -> HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxin txin  script _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputScriptTxin  (toTxPlutusScript script)  Nothing _redeemer  exUnitsM  txin

type ScriptReferenceTxIn = TxIn

-- | Add a script utxo txin containing datum-hash to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemUtxoWithDatumAndReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra  -> HashableScriptData ->  HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn (Just _data) _redeemer exUnitsM ( txin,  txout)

-- | Add a script utxo containing datum-hash to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemTxinWithDatumAndReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra  -> HashableScriptData ->  HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxinWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn (Just _data) _redeemer exUnitsM txin

-- | Add a script utxo containing inline-datum to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemUtxoWithReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra  -> HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithReferenceScript scRefTxIn txin txout  _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn Nothing _redeemer exUnitsM ( txin,  txout)

-- | Add a script txIn containing inline-datum to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemTxinWithReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra  -> HashableScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxinWithReferenceScript scRefTxIn txin txout  _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn Nothing _redeemer exUnitsM txin


 -- wallet addresses, from which utxos can be spent for balancing the transaction
txWalletAddresses :: [AddressInEra ConwayEra] -> TxBuilder
txWalletAddresses v = txSelection $ TxSelectableAddresses  (map toShelleyAddr v)


-- wallet address, from which utxos can be spent  for balancing the transaction
txWalletAddress :: AddressInEra ConwayEra -> TxBuilder
txWalletAddress v = txWalletAddresses [v]

-- wallet utxos, that can be spent  for balancing the transaction
txWalletUtxos :: UTxO ConwayEra -> TxBuilder
txWalletUtxos v =  txSelection $  TxSelectableUtxos v

-- wallet utxo, that can be spent  for balancing the transaction
txWalletUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> TxBuilder
txWalletUtxo tin tout = txWalletUtxos $  UTxO $ Map.singleton tin  tout

-- | add signkey to the selection. All the utxos from the enterprise address of the signkey will be automatically
-- added to selection too. The `TxBuilder` containing signkeys when built to transaction, transaction will contain signatures if their utxo is used.
-- 
-- In order to automatically sign tx for address other than enterprise address, both address and signkey must be present in the builder.
txWalletSignKey :: SigningKey PaymentKey -> TxBuilder
txWalletSignKey s= txWalletSignKeys [s]

txWalletSignKeys :: [SigningKey PaymentKey] -> TxBuilder
txWalletSignKeys s= txSelection $ TxSelectableSkey s

txCollateral :: TxIn -> TxBuilder
txCollateral colTxIn = txCollateral' $ TxCollateralTxin colTxIn

txCollateralUtxo :: TxIn -> TxOut CtxUTxO ConwayEra -> TxBuilder
txCollateralUtxo tin tout =  txCollateral' $ TxCollateralUtxo  $ UTxO $ Map.singleton  tin tout

txChangeAddress :: AddressInEra ConwayEra -> TxBuilder
txChangeAddress addr = TxBuilder_  [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing (Just addr) Map.empty
