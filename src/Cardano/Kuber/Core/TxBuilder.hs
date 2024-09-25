{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Kuber.Core.TxBuilder where

import Cardano.Api hiding (txCertificates, txFee, txMetadata,txAuxScripts)
import Cardano.Api.Ledger (EraCrypto, StandardCrypto)
import Cardano.Api.Shelley hiding (txCertificates, txFee, txMetadata,txAuxScripts)
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Api as Ledger
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX ( POSIXTime )
import Data.Word (Word64)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (PubKeyHash)
import qualified Cardano.Ledger.Api as L
import Cardano.Kuber.Core.TxScript
import Cardano.Ledger.Api (Babbage)
import Data.Either (fromRight)

throwEraError source = error (source ++ ": Can only Support Babbage and Conway Era")

getEra :: CardanoEra era1 -> ShelleyBasedEra era1
getEra era = case era of
  ShelleyEra -> ShelleyBasedEraShelley
  AllegraEra -> ShelleyBasedEraAllegra
  MaryEra -> ShelleyBasedEraMary
  AlonzoEra -> ShelleyBasedEraAlonzo
  BabbageEra -> ShelleyBasedEraBabbage
  ConwayEra -> ShelleyBasedEraConway
  _ -> error "Unexpected era"

data TxVoteL ledgerera
  = TxVoteL
      (Ledger.GovActionId (EraCrypto ledgerera))
      (Ledger.VotingProcedure ledgerera)
      (Ledger.Voter (EraCrypto ledgerera))
  deriving (Show, Eq)

newtype TxVote era = TxVote (TxVoteL (ShelleyLedgerEra era)) deriving (Show, Eq)


data TxInputResolved_ era
  = TxInputUtxo (UTxO era)
  | TxInputScriptUtxo TxPlutusScript (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) (TxIn, TxOut CtxUTxO era)
  | TxInputReferenceScriptUtxo TxIn (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) (TxIn, TxOut CtxUTxO era)
  deriving (Show)

data TxInputUnResolved_ era
  = TxInputTxin TxIn
  | TxInputSkey (SigningKey PaymentKey)
  | TxInputAddr (AddressInEra era)
  | TxInputScriptTxin TxPlutusScript (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) TxIn
  | TxInputReferenceScriptTxin TxIn (Maybe HashableScriptData) HashableScriptData (Maybe ExecutionUnits) TxIn
  deriving (Show)

data TxInput era = TxInputResolved (TxInputResolved_ era) | TxInputUnResolved (TxInputUnResolved_ era) deriving (Show)

data TxInputReference era
  = TxInputReferenceTxin TxIn
  | TxInputReferenceUtxo (UTxO era)
  deriving (Show)

data TxOutputContent era
  = TxOutPkh PubKeyHash Value
  | TxOutScript TxPlutusScript Value (Hash ScriptData)
  | TxOutScriptInline TxPlutusScript Value (Hash ScriptData)
  | TxOutScriptWithScript TxPlutusScript Value (Hash ScriptData) TxScript
  | TxOutScriptWithData TxPlutusScript Value HashableScriptData
  | TxOutScriptWithDataAndScript TxPlutusScript Value HashableScriptData TxScript
  | TxOutScriptWithDataAndReference TxPlutusScript Value HashableScriptData
  | TxOutNative (TxOut CtxTx era)
  deriving (Show)

data TxOutput content = TxOutput
  { content :: content,
    deductFee :: Bool,
    addChange :: Bool,
    onMinAda :: InsufficientUtxoAdaAction
  }
  deriving (Show)

transfrormOutput :: TxOutput content1 -> content2 -> TxOutput content2
transfrormOutput (TxOutput con fee change minAda) v = TxOutput v fee change minAda

data InsufficientUtxoAdaAction = DropOnUtxoInsufficientUtxoAda | IncreaseOnUtxoInsufficientUtxoAda | ErrorOnInsufficientUtxoAda | OnInsufficientUtxoAdaUnset deriving (Show, Eq, Generic, ToJSON)

data TxCollateral era
  = TxCollateralTxin TxIn
  | TxCollateralUtxo (UTxO era)
  deriving (Show)

data TxSignature era
  = TxSignatureAddr (AddressInEra era)
  | TxSignaturePkh PubKeyHash
  | TxSignatureSkey (SigningKey PaymentKey)
  deriving (Show)

data TxChangeAddr era
  = TxChangeAddrUnset
  | TxChangeAddr (AddressInEra era)
  deriving (Show)

data TxInputSelection era
  = TxSelectableAddresses [Ledger.Addr StandardCrypto]
  | TxSelectableUtxos (UTxO era)
  | TxSelectableTxIn [TxIn]
  | TxSelectableSkey [SigningKey PaymentKey]
  deriving (Show)

data TxMintingScriptSource
  = TxMintingPlutusScript TxPlutusScript (Maybe ExecutionUnits) HashableScriptData
  | TxMintingReferenceScript TxIn (Maybe ExecutionUnits) (Maybe HashableScriptData)
  | TxMintingSimpleScript SimpleScript
  deriving (Show)

data TxMintData s = TxMintData s [(AssetName, Quantity)] (Map Word64 (Map AssetName Aeson.Value)) deriving (Show)

data ValidityTimestamp
  = NoValidityTime
  | ValidityPosixTime POSIXTime
  | ValiditySlot SlotNo
  deriving (Show, Eq)

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
type TxBuilder = (TxBuilder_ ConwayEra)

newtype (L.EraPParams (ShelleyLedgerEra era)) =>  ProposalProcedureModal  era = 
    ProposalProcedureModal (L.ProposalProcedure  (ShelleyLedgerEra era))

data TxProposal era = TxProposal  (ProposalProcedureModal era)
      | TxProposalScript (ProposalProcedureModal era) (Maybe ExecutionUnits) TxPlutusScript
      | TxProposalScriptReference (ProposalProcedureModal era) (Maybe ExecutionUnits) TxIn

data TxBuilder_ era = TxBuilder_
  { txSelections :: [TxInputSelection era],
    txInputs :: [TxInput era],
    txInputReferences :: [TxInputReference era],
    txOutputs :: [TxOutput (TxOutputContent era)],
    txCollaterals :: [TxCollateral era], -- collateral for the transaction
    txValidityStart :: ValidityTimestamp,
    txValidityEnd :: ValidityTimestamp,
    txMintData :: [TxMintData TxMintingScriptSource],
    txSignatures :: [TxSignature era],
    txProposals :: [TxProposal era],
    txVotes :: [TxVote era],
    txCertificates :: [Certificate era],
    txFee :: Maybe Integer,
    txDefaultChangeAddr :: Maybe (AddressInEra era),
    txMetadata' :: Map Word64 Aeson.Value,
    txAuxScripts :: [TxScript]
  } 


class IsShelleyBasedEra era => IsTxBuilderEra era where
  bMaryOnward :: MaryEraOnwards era
  bAlonzoOnward :: AlonzoEraOnwards era
  bBabbageOnward :: BabbageEraOnwards era
  bAllegraOnward :: AllegraEraOnwards era
  bConwayOnward :: Maybe (ConwayEraOnwards era)
  bShelleyBasedEra :: ShelleyBasedEra era
  bCardanoEra :: CardanoEra era
  bAsEra :: AsType era

instance IsTxBuilderEra ConwayEra where
  bMaryOnward = MaryEraOnwardsConway
  bAlonzoOnward = AlonzoEraOnwardsConway
  bAllegraOnward = AllegraEraOnwardsConway
  bBabbageOnward = BabbageEraOnwardsConway
  bConwayOnward = Just ConwayEraOnwardsConway
  bShelleyBasedEra = ShelleyBasedEraConway
  bCardanoEra = ConwayEra
  bAsEra = AsConwayEra


instance IsTxBuilderEra BabbageEra where
  bMaryOnward = MaryEraOnwardsBabbage
  bAlonzoOnward = AlonzoEraOnwardsBabbage
  bAllegraOnward = AllegraEraOnwardsBabbage
  bBabbageOnward = BabbageEraOnwardsBabbage
  bConwayOnward = Nothing
  bShelleyBasedEra = ShelleyBasedEraBabbage
  bCardanoEra = BabbageEra
  bAsEra = AsBabbageEra

instance Monoid (TxBuilder_ era) where
  mempty = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

instance Semigroup (TxBuilder_ era) where
  (<>) txb1 txb2 =
    TxBuilder_
      { txSelections = txSelections txb1 ++ txSelections txb2,
        txInputs = txInputs txb1 ++ txInputs txb2,
        txInputReferences = txInputReferences txb1 ++ txInputReferences txb2,
        txOutputs = txOutputs txb1 ++ txOutputs txb2,
        txCollaterals = txCollaterals txb1 ++ txCollaterals txb2, -- collateral for the transaction
        txValidityStart = minValidity (txValidityStart txb1) (txValidityStart txb2),
        txValidityEnd = maxValidity (txValidityEnd txb1) (txValidityEnd txb2),
        txMintData = txMintData txb1 <> txMintData txb2,
        txSignatures = txSignatures txb1 ++ txSignatures txb2,
        txProposals = txProposals txb1 ++ txProposals txb2,
        txVotes = txVotes txb1 ++ txVotes txb2,
        txCertificates = txCertificates txb1 ++ txCertificates txb2,
        txFee = case txFee txb1 of
          Just f -> case txFee txb2 of
            Just f2 -> Just $ max f f2
            _ -> Just f
          Nothing -> txFee txb2,
        txDefaultChangeAddr = case txDefaultChangeAddr txb1 of
          Just addr -> Just addr
          _ -> txDefaultChangeAddr txb2,
        txMetadata' = txMetadata' txb1 <> txMetadata' txb2,
        txAuxScripts = txAuxScripts txb1 <> txAuxScripts txb2
      }


txSelection :: TxInputSelection era -> TxBuilder_ era
txSelection v = TxBuilder_ [v] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

txInput :: TxInput era -> TxBuilder_ era
txInput v = TxBuilder_ [] [v] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

txInputReference :: TxInputReference era -> TxBuilder_ era
txInputReference v = TxBuilder_ [] [] [v] [] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

txMints :: [TxMintData TxMintingScriptSource] -> TxBuilder_ era
txMints md = TxBuilder_ [] [] [] [] [] mempty mempty md [] [] [] [] Nothing Nothing Map.empty []

txOutput :: TxOutput (TxOutputContent era) -> TxBuilder_ era
txOutput v = TxBuilder_ [] [] [] [v] [] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

txCollateral' :: TxCollateral era -> TxBuilder_ era
txCollateral' v = TxBuilder_ [] [] [] [] [v] mempty mempty [] [] [] [] [] Nothing Nothing Map.empty []

txSignature :: TxSignature era -> TxBuilder_ era
txSignature v = TxBuilder_ [] [] [] [] [] mempty mempty [] [v] [] [] [] Nothing Nothing Map.empty []

txReplacePoposalsNCert :: TxBuilder_ era -> [TxProposal era] -> [Certificate era] -> TxBuilder_ era
txReplacePoposalsNCert (TxBuilder_ a b c d e f g h i _ k _ m n o p) ps cs = TxBuilder_ a b c d e f g h i ps k cs m n o p

-- Transaction validity

-- Set validity Start and end time in posix seconds
txValidPosixTimeRange :: POSIXTime -> POSIXTime -> TxBuilder
txValidPosixTimeRange start end = txValidPosixTimeRange_ start end

txValidPosixTimeRange_ :: POSIXTime -> POSIXTime -> TxBuilder_ era
txValidPosixTimeRange_ start end = TxBuilder_ [] [] [] [] [] (ValidityPosixTime start) (ValidityPosixTime end) [] [] [] [] [] Nothing Nothing Map.empty []

-- set  validity statart time in posix seconds
txValidFromPosixTime :: POSIXTime -> TxBuilder
txValidFromPosixTime start = txValidFromPosixTime_ start 

txValidFromPosixTime_ :: POSIXTime -> TxBuilder_ era
txValidFromPosixTime_ start = TxBuilder_ [] [] [] [] [] (ValidityPosixTime start) mempty [] [] [] [] [] Nothing Nothing Map.empty []

-- set transaction validity end time in posix seconds
txValidUntilPosixTime :: POSIXTime -> TxBuilder
txValidUntilPosixTime end = txValidUntilPosixTime_ end 

txValidUntilPosixTime_ :: POSIXTime -> TxBuilder_ era
txValidUntilPosixTime_ end = TxBuilder_ [] [] [] [] [] mempty (ValidityPosixTime end) [] [] [] [] [] Nothing Nothing Map.empty []

-- Set validity Start and end slot
txValidSlotRange :: SlotNo -> SlotNo -> TxBuilder
txValidSlotRange start end = txValidSlotRange_ start end

txValidSlotRange_ :: SlotNo -> SlotNo -> TxBuilder_ era
txValidSlotRange_ start end = TxBuilder_ [] [] [] [] [] (ValiditySlot start) (ValiditySlot end) [] [] [] [] [] Nothing Nothing Map.empty []

-- set  validity statart time in posix seconds
txValidFromSlot :: SlotNo -> TxBuilder
txValidFromSlot start = txValidFromSlot_ start

txValidFromSlot_ :: SlotNo -> TxBuilder_ era
txValidFromSlot_ start = TxBuilder_ [] [] [] [] [] (ValiditySlot start) mempty [] [] [] [] [] Nothing Nothing Map.empty []

-- set transaction validity end time in posix seconds
txValidUntilSlot :: SlotNo -> TxBuilder
txValidUntilSlot end = txValidUntilSlot_ end

txValidUntilSlot_ :: SlotNo -> TxBuilder_ era
txValidUntilSlot_ end = TxBuilder_ [] [] [] [] [] mempty (ValiditySlot end) [] [] [] [] [] Nothing Nothing Map.empty []

-- governanceProposals
-- txProposal ::
--   Proposal ConwayEra ->
--   TxBuilder
-- txProposal p = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [p] [] [] Nothing Nothing Map.empty

-- voting
txVote :: TxVote ConwayEra -> TxBuilder
txVote v = txVote_ v 

txVote_ :: TxVote era -> TxBuilder_ era
txVote_ v = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [v] [] Nothing Nothing Map.empty []

-- certifying
txCertificate :: Certificate ConwayEra -> TxBuilder
txCertificate v = txCertificate_ v 

txCertificate_ :: Certificate era -> TxBuilder_ era
txCertificate_ v = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [] [v] Nothing Nothing Map.empty []

--- minting
_txMint :: TxMintData TxMintingScriptSource -> TxBuilder_ era
_txMint v = txMints [v]

-- | Mint token with plutus v1 or v2 script
txMintPlutusScript :: IsPlutusScript script => script -> HashableScriptData -> [(AssetName, Quantity)] -> TxBuilder
txMintPlutusScript script sData amounts = txMintPlutusScript_ script sData amounts 

txMintPlutusScript_ :: IsPlutusScript script => script -> HashableScriptData -> [(AssetName, Quantity)] -> TxBuilder_ era
txMintPlutusScript_ script sData amounts = _txMint $ TxMintData (TxMintingPlutusScript (toTxPlutusScript script) Nothing sData) amounts Map.empty

-- | Mint token with simple script
txMintSimpleScript ::  SimpleScript -> [(AssetName, Quantity)] -> TxBuilder
txMintSimpleScript script amounts = txMintSimpleScript_ script amounts

txMintSimpleScript_ ::  SimpleScript -> [(AssetName, Quantity)] -> TxBuilder_ era 
txMintSimpleScript_ script amounts = _txMint $ TxMintData (TxMintingSimpleScript ( script)) amounts Map.empty

-- txMintWithMetadata :: IsMintingScript script =>script  ->   [(AssetName,Integer)] -> Map Word64 (Map AssetName Aeson.Value)  -> TxBuilder
-- txMintWithMetadata script amounts mp = _txMint $ TxMintData (TxMintingScriptCode $ toTxMintingScript script) amounts mp
--  witness (valueFromList  $ map (bimap (AssetId policyId) Quantity )  amounts )
--   where
--     witness=   SimpleScriptWitness SimpleScriptV2InConway SimpleScriptV2 (SScript simpleScript)
--     script = SimpleScript SimpleScriptV2 simpleScript
--     policyId = scriptPolicyId script

-- | Pay to this address in transaction

txPayTo :: AddressInEra ConwayEra -> Value -> TxBuilder
txPayTo addr v = txPayTo_ addr v

txPayTo_ :: (IsTxBuilderEra era ) =>  AddressInEra era -> Value -> TxBuilder_ era
txPayTo_  addr v =  txPayTo_' (bCardanoEra) addr v

txPayTo_' :: CardanoEra era -> AddressInEra era -> Value -> TxBuilder_ era
txPayTo_' era addr v =
  case era of 
    BabbageEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraBabbage (toMaryValue v)) TxOutDatumNone ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
    ConwayEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraConway (toMaryValue v)) TxOutDatumNone ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
    _ -> throwEraError "txPayTo" 

-- | Pay to address  and inline the script in resulting utxo.
txPayToWithReferenceScript :: AddressInEra ConwayEra -> Value -> TxScript -> TxBuilder
txPayToWithReferenceScript addr v pScript = txPayToWithReferenceScript_ addr v pScript

txPayToWithReferenceScript_ :: (IsTxBuilderEra era ) => AddressInEra era -> Value -> TxScript -> TxBuilder_ era
txPayToWithReferenceScript_ addr v pScript = txPayToWithReferenceScript_' bCardanoEra addr v pScript

txPayToWithReferenceScript_' :: CardanoEra era -> AddressInEra era -> Value -> TxScript -> TxBuilder_ era
txPayToWithReferenceScript_' era addr v pScript = 
  case era of 
    BabbageEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraBabbage (toMaryValue v)) TxOutDatumNone (ReferenceScript BabbageEraOnwardsBabbage (txScriptToScriptAny pScript))) False False OnInsufficientUtxoAdaUnset
    ConwayEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraConway (toMaryValue v)) TxOutDatumNone (ReferenceScript BabbageEraOnwardsConway (txScriptToScriptAny pScript))) False False OnInsufficientUtxoAdaUnset
    _ -> throwEraError "txPayToWithReferenceScript" 

-- | Pay to the enterprise address of this PublicKeyHash
txPayToPkh :: PubKeyHash -> Value -> TxBuilder
txPayToPkh pkh v = txPayToPkh_ pkh v

txPayToPkh_ :: PubKeyHash -> Value -> TxBuilder_ era
txPayToPkh_ pkh v = txOutput $ TxOutput (TxOutPkh pkh v) False False OnInsufficientUtxoAdaUnset

txPayToScriptWithDataInTx :: AddressInEra ConwayEra -> Value -> HashableScriptData -> TxBuilder
txPayToScriptWithDataInTx addr v d = txPayToScriptWithDataInTx_ addr v d

txPayToScriptWithDataInTx_ :: (IsTxBuilderEra era ) => AddressInEra era -> Value -> HashableScriptData -> TxBuilder_ era
txPayToScriptWithDataInTx_ addr v d = txPayToScriptWithDataInTx_' bCardanoEra addr v d 

txPayToScriptWithDataInTx_' :: CardanoEra era -> AddressInEra era -> Value -> HashableScriptData -> TxBuilder_ era
txPayToScriptWithDataInTx_' era addr v d = case era of 
  BabbageEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraBabbage (toMaryValue v)) (TxOutDatumInTx AlonzoEraOnwardsBabbage d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  ConwayEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraConway (toMaryValue v)) (TxOutDatumInTx AlonzoEraOnwardsConway d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  _ -> throwEraError "txPayToWithReferenceScript" 

-- | Pay to script address with datumHash
txPayToScript :: AddressInEra ConwayEra -> Value -> Hash ScriptData -> TxBuilder
txPayToScript addr v d = txPayToScript_ addr v d

txPayToScript_ :: (IsTxBuilderEra era ) => AddressInEra era -> Value -> Hash ScriptData -> TxBuilder_ era
txPayToScript_ addr v d = txPayToScript_' bCardanoEra addr v d

txPayToScript_' :: CardanoEra era -> AddressInEra era -> Value -> Hash ScriptData -> TxBuilder_ era
txPayToScript_' era addr v d = case era of 
  BabbageEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraBabbage (toMaryValue v)) (TxOutDatumHash AlonzoEraOnwardsBabbage d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  ConwayEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraConway (toMaryValue v)) (TxOutDatumHash AlonzoEraOnwardsConway d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  _ -> throwEraError "txPayToScript" 

-- | Pay to script address and inline the datum in utxo
txPayToScriptWithData :: AddressInEra ConwayEra -> Value -> HashableScriptData -> TxBuilder
txPayToScriptWithData addr v d = txPayToScriptWithData_ addr v d

txPayToScriptWithData_ :: (IsTxBuilderEra era ) => AddressInEra era -> Value -> HashableScriptData -> TxBuilder_ era
txPayToScriptWithData_ addr v d = txPayToScriptWithData_' bCardanoEra addr v d

txPayToScriptWithData_' :: CardanoEra era -> AddressInEra era -> Value -> HashableScriptData -> TxBuilder_ era
txPayToScriptWithData_' era addr v d = case era of 
  BabbageEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraBabbage (toMaryValue v)) (TxOutDatumInline BabbageEraOnwardsBabbage d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  ConwayEra -> txOutput $ TxOutput (TxOutNative $ TxOut addr (TxOutValueShelleyBased ShelleyBasedEraConway (toMaryValue v)) (TxOutDatumInline BabbageEraOnwardsConway d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset
  _ -> throwEraError "txPayToScriptWithData"

-- | Pay to the script and inline it in the utxo. Script enterprise address is derrived from script hash
txPayToScriptWithReference :: IsPlutusScript sc => sc -> Value -> Hash ScriptData -> TxBuilder
txPayToScriptWithReference pScript v d = txPayToScriptWithReference_ pScript v d 

txPayToScriptWithReference_ :: IsPlutusScript sc => sc -> Value -> Hash ScriptData -> TxBuilder_ era
txPayToScriptWithReference_ pScript v d = txOutput $ TxOutput (TxOutScript (toTxPlutusScript pScript) v d) False False OnInsufficientUtxoAdaUnset

-- | Pay to script  with inline both datum and inline it in datum. Script enterprise address is derrived from script hash
txPayToScriptWithDataAndReference :: IsPlutusScript sc => sc -> Value -> HashableScriptData -> TxBuilder
txPayToScriptWithDataAndReference pScript v d = txPayToScriptWithDataAndReference_ pScript v d

txPayToScriptWithDataAndReference_ :: IsPlutusScript sc => sc -> Value -> HashableScriptData -> TxBuilder_ era
txPayToScriptWithDataAndReference_ pScript v d =
  txOutput $ TxOutput (TxOutScriptWithData (toTxPlutusScript pScript) v d) False False OnInsufficientUtxoAdaUnset

-- input consmptions

-- use Utxo as input in the transaction
txConsumeUtxos :: UTxO ConwayEra -> TxBuilder
txConsumeUtxos utxo = txConsumeUtxos_ utxo

txConsumeUtxos_ :: UTxO era -> TxBuilder_ era
txConsumeUtxos_ utxo = txInput $ TxInputResolved $ TxInputUtxo utxo

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn v = txConsumeTxIn_ v

txConsumeTxIn_ :: TxIn -> TxBuilder_ era
txConsumeTxIn_ v = txInput $ TxInputUnResolved $ TxInputTxin v

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txReferenceTxIn :: TxIn -> TxBuilder
txReferenceTxIn v = txReferenceTxIn_ v

txReferenceTxIn_ :: TxIn -> TxBuilder_ era
txReferenceTxIn_ v = txInputReference $ TxInputReferenceTxin v


txReferenctUtxo :: TxIn -> TxOut CtxUTxO ConwayEra -> TxBuilder
txReferenctUtxo tin tout = txReferenctUtxo_ tin tout

txReferenctUtxo_ :: TxIn -> TxOut CtxUTxO era -> TxBuilder_ era
txReferenctUtxo_ tin tout = txInputReference $ TxInputReferenceUtxo (UTxO $ Map.singleton tin tout)

-- use txIn as input in the transaction
-- Since TxOut is also given the txIn is not queried from the node.
txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> TxBuilder
txConsumeUtxo tin v = txConsumeUtxo_ tin v

txConsumeUtxo_ :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> TxBuilder_ era
txConsumeUtxo_ tin v = txConsumeUtxos_ $ UTxO $ Map.singleton tin v

-- | Mark this address as txExtraKeyWitness in the transaction object.
txSignBy :: AddressInEra ConwayEra -> TxBuilder
txSignBy a = txSignBy_ a 

txSignBy_ :: AddressInEra era -> TxBuilder_ era
txSignBy_ a = txSignature (TxSignatureAddr a)

-- | Mark this PublicKeyhash as txExtraKeyWitness in the transaction object.
txSignByPkh :: PubKeyHash -> TxBuilder
txSignByPkh p = txSignByPkh_ p

txSignByPkh_ :: PubKeyHash -> TxBuilder_ era 
txSignByPkh_ p = txSignature $ TxSignaturePkh p

-- Mark this signingKey's vKey as txExtraKey Witness in the transaction object.
-- When validating `txSignedBy` in plutus, this can be used to add the
txSign :: SigningKey PaymentKey -> TxBuilder
txSign p = txSign_ p

txSign_ :: SigningKey PaymentKey -> TxBuilder_ era
txSign_ p = txSignature $ TxSignatureSkey p

-- | explicitly set Fee for the transaction
txSetFee :: Integer -> TxBuilder
txSetFee v = txSetFee_ v

txSetFee_ :: Integer -> TxBuilder_ era 
txSetFee_ v = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [] [] (Just v) Nothing Map.empty []

txMetadata :: Map Word64 Aeson.Value -> TxBuilder
txMetadata md = txMetadata_ md 

txMetadata_ :: Map Word64 Aeson.Value -> TxBuilder_ era
txMetadata_ md = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing Nothing md []

-- | Add a  script utxo containing datum-hash to  transaction input . Script code, datum matching datumHash and redeemer should be  passed for building transaction.
txRedeemUtxoWithDatum :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> sc -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemUtxoWithDatum txin txout sc _data _redeemer exUnitsM = txRedeemUtxoWithDatum_ txin txout sc _data _redeemer exUnitsM 

txRedeemUtxoWithDatum_ :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> sc -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemUtxoWithDatum_ txin txout sc _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo (toTxPlutusScript sc) (Just _data) _redeemer exUnitsM (txin, txout)

-- | Add a  script utxo containing inline-datum  to  transaction input. Script code and redeemer should be  passed for building transaction.
txRedeemUtxo :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> sc -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemUtxo txin txout script _redeemer exUnitsM = txRedeemUtxo_ txin txout script _redeemer exUnitsM

txRedeemUtxo_ :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> sc -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemUtxo_ txin txout script _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo (toTxPlutusScript script) Nothing _redeemer exUnitsM (txin, txout)

-- | Add a script utxo-reference containing inline-datum to transaction input.  Script code and Reedemer  should be passed.
txRedeemTxin :: IsPlutusScript sc => TxIn -> sc -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemTxin txin script _redeemer exUnitsM = txRedeemTxin_ txin script _redeemer exUnitsM

txRedeemTxin_ :: IsPlutusScript sc => TxIn -> sc -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemTxin_ txin script _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputScriptTxin (toTxPlutusScript script) Nothing _redeemer exUnitsM txin

type ScriptReferenceTxIn = TxIn

-- | Add a script utxo txin containing datum-hash to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemUtxoWithDatumAndReferenceScript :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemUtxoWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txRedeemUtxoWithDatumAndReferenceScript_ scRefTxIn txin txout _data _redeemer exUnitsM

txRedeemUtxoWithDatumAndReferenceScript_ :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemUtxoWithDatumAndReferenceScript_ scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn (Just _data) _redeemer exUnitsM (txin, txout)

-- | Add a script utxo containing datum-hash to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemTxinWithDatumAndReferenceScript :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemTxinWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txRedeemTxinWithDatumAndReferenceScript_ scRefTxIn txin txout _data _redeemer exUnitsM

txRedeemTxinWithDatumAndReferenceScript_ :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> HashableScriptData -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemTxinWithDatumAndReferenceScript_ scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn (Just _data) _redeemer exUnitsM txin

-- | Add a script utxo containing inline-datum to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemUtxoWithReferenceScript :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemUtxoWithReferenceScript scRefTxIn txin txout _redeemer exUnitsM = txRedeemUtxoWithReferenceScript_ scRefTxIn txin txout _redeemer exUnitsM 

txRedeemUtxoWithReferenceScript_ :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemUtxoWithReferenceScript_ scRefTxIn txin txout _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn Nothing _redeemer exUnitsM (txin, txout)

-- | Add a script txIn containing inline-datum to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
txRedeemTxinWithReferenceScript :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder
txRedeemTxinWithReferenceScript scRefTxIn txin txout _redeemer exUnitsM = txRedeemTxinWithReferenceScript_ scRefTxIn txin txout _redeemer exUnitsM

txRedeemTxinWithReferenceScript_ :: ScriptReferenceTxIn -> TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> HashableScriptData -> Maybe ExecutionUnits -> TxBuilder_ era
txRedeemTxinWithReferenceScript_ scRefTxIn txin txout _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn Nothing _redeemer exUnitsM txin

-- wallet addresses, from which utxos can be spent for balancing the transaction
txWalletAddresses :: [AddressInEra ConwayEra] -> TxBuilder
txWalletAddresses v = txWalletAddresses_ v

txWalletAddresses_ :: [AddressInEra era] -> TxBuilder_ era 
txWalletAddresses_ v = txSelection $ TxSelectableAddresses (map toShelleyAddr v)

-- wallet address, from which utxos can be spent  for balancing the transaction
txWalletAddress :: AddressInEra ConwayEra -> TxBuilder
txWalletAddress v = txWalletAddresses [v]

txWalletAddress_ :: AddressInEra era -> TxBuilder_ era
txWalletAddress_ v = txWalletAddresses_ [v]

-- wallet utxos, that can be spent  for balancing the transaction
txWalletUtxos :: UTxO ConwayEra -> TxBuilder
txWalletUtxos v = txWalletUtxos_ v

txWalletUtxos_ :: UTxO era -> TxBuilder_ era
txWalletUtxos_ v = txSelection $ TxSelectableUtxos v

-- wallet utxo, that can be spent  for balancing the transaction
txWalletUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO ConwayEra -> TxBuilder
txWalletUtxo tin tout = txWalletUtxos $ UTxO $ Map.singleton tin tout

txWalletUtxo_ :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO era -> TxBuilder_ era
txWalletUtxo_ tin tout = txWalletUtxos_ $ UTxO $ Map.singleton tin tout

-- | add signkey to the selection. All the utxos from the enterprise address of the signkey will be automatically
-- added to selection too. The `TxBuilder` containing signkeys when built to transaction, transaction will contain signatures if their utxo is used.
--
-- In order to automatically sign tx for address other than enterprise address, both address and signkey must be present in the builder.
txWalletSignKey :: SigningKey PaymentKey -> TxBuilder
txWalletSignKey s = txWalletSignKeys [s]

txWalletSignKey_ :: SigningKey PaymentKey -> TxBuilder_ era
txWalletSignKey_ s = txWalletSignKeys_ [s]

txWalletSignKeys :: [SigningKey PaymentKey] -> TxBuilder
txWalletSignKeys s = txWalletSignKeys_ s

txWalletSignKeys_ :: [SigningKey PaymentKey] -> TxBuilder_ era
txWalletSignKeys_ s = txSelection $ TxSelectableSkey s

txCollateral :: TxIn -> TxBuilder
txCollateral colTxIn = txCollateral_ colTxIn

txCollateral_ :: TxIn -> TxBuilder_ era
txCollateral_ colTxIn = txCollateral' $ TxCollateralTxin colTxIn

txCollateralUtxo :: TxIn -> TxOut CtxUTxO ConwayEra -> TxBuilder
txCollateralUtxo tin tout = txCollateralUtxo_ tin tout

txCollateralUtxo_ :: TxIn -> TxOut CtxUTxO era -> TxBuilder_ era 
txCollateralUtxo_ tin tout = txCollateral' $ TxCollateralUtxo $ UTxO $ Map.singleton tin tout

txChangeAddress :: AddressInEra ConwayEra -> TxBuilder
txChangeAddress addr = txChangeAddress_ addr 

txChangeAddress_ :: AddressInEra era -> TxBuilder_ era 
txChangeAddress_ addr = TxBuilder_ [] [] [] [] [] mempty mempty [] [] [] [] [] Nothing (Just addr) Map.empty []
