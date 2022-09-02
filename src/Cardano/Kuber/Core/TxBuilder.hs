{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Kuber.Core.TxBuilder

where


import Cardano.Api hiding(txMetadata, txFee)
import Cardano.Api.Shelley hiding (txMetadata, txFee)
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
import Cardano.Kuber.Util
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V2.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, CurrencySymbol, MintingPolicy)
import qualified Plutus.V2.Ledger.Api as Plutus hiding (TxOut)
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
import Cardano.Kuber.Utility.ScriptUtil ( fromPlutusV2Script)
import GHC.Generics (Generic)
import Data.Time.Clock.POSIX
import Foreign.C (CTime)


data TxSimpleScript = TxSimpleScriptV1 (SimpleScript  SimpleScriptV1 )
              |        TxSimpleScriptV2 (SimpleScript  SimpleScriptV2 )
                            deriving(Show)

data TxScript = TxScriptSimple TxSimpleScript
        |   TxScriptPlutus   TxPlutusScript
        deriving (Show)


data  TxPlutusScript  =
    TxPlutusScriptV1 (PlutusScript  PlutusScriptV1)
  | TxPlutusScriptV2 (PlutusScript  PlutusScriptV2)
                          deriving (Show)


data TxInputResolved_ = TxInputUtxo (UTxO BabbageEra)
              | TxInputScriptUtxo TxPlutusScript (Maybe ScriptData) ScriptData (Maybe ExecutionUnits) (UTxO BabbageEra)
              | TxInputReferenceScriptUtxo TxIn (Maybe ScriptData) ScriptData (Maybe ExecutionUnits) (UTxO BabbageEra)

              deriving (Show)


data TxInputUnResolved_ = TxInputTxin TxIn
              | TxInputSkey (SigningKey PaymentKey)
              | TxInputAddr (AddressInEra BabbageEra)
              | TxInputScriptTxin TxPlutusScript (Maybe ScriptData) ScriptData (Maybe ExecutionUnits) TxIn
              | TxInputReferenceScriptTxin TxIn (Maybe ScriptData) ScriptData (Maybe ExecutionUnits) TxIn

              deriving (Show)

data TxInput  = TxInputResolved TxInputResolved_ | TxInputUnResolved TxInputUnResolved_ deriving (Show)

newtype TxInputReference  = TxInputReference TxIn deriving (Show)

data TxOutputContent =
     TxOutPkh PubKeyHash Value
  |  TxOutScript TxPlutusScript Value  (Hash ScriptData)
  |  TxOutScriptInline TxPlutusScript Value (Hash ScriptData)
  |  TxOutScriptWithScript TxPlutusScript Value (Hash ScriptData) TxScript
  |  TxOutScriptWithData TxPlutusScript Value ScriptData
  |  TxOutScriptWithDataAndScript TxPlutusScript Value ScriptData TxScript
  |  TxOutScriptWithDataAndReference TxPlutusScript Value ScriptData
  |  TxOutNative (TxOut  CtxTx BabbageEra)
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


data TxCollateral =  TxCollateralTxin TxIn
                  |  TxCollateralUtxo (UTxO BabbageEra) deriving (Show)

data TxSignature =  TxSignatureAddr (AddressInEra BabbageEra)
                  | TxSignaturePkh PubKeyHash
                  | TxSignatureSkey (SigningKey PaymentKey)
                  deriving (Show)



data TxChangeAddr = TxChangeAddrUnset
                  | TxChangeAddr (AddressInEra BabbageEra) deriving (Show)

data TxInputSelection = TxSelectableAddresses [AddressInEra BabbageEra]
                  | TxSelectableUtxos  (UTxO BabbageEra)
                  | TxSelectableTxIn [TxIn]
                  | TxSelectableSkey [SigningKey PaymentKey]
                  deriving(Show)


data TxMintingScriptSource =
            TxMintingPlutusScript TxPlutusScript  (Maybe ExecutionUnits ) ScriptData
          | TxMintingReferenceScript TxIn (Maybe ExecutionUnits ) (Maybe ScriptData)
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

-- TxBuilder object
-- It is a semigroup and monoid instance, so it can be constructed using helper function
-- and merged to construct a transaction specification
data TxBuilder=TxBuilder{
    txSelections :: [TxInputSelection],
    txInputs:: [TxInput],
    txInputReferences:: [TxInputReference],
    txOutputs :: [TxOutput TxOutputContent],
    txCollaterals :: [TxCollateral],  -- collateral for the transaction
    txValidityStart :: ValidityTimestamp,
    txValidityEnd :: ValidityTimestamp,
    txMintData :: [TxMintData TxMintingScriptSource],
    txSignatures :: [TxSignature],
    txFee :: Maybe Integer,
    txDefaultChangeAddr :: Maybe (AddressInEra BabbageEra),
    txMetadata :: Map Word64 Aeson.Value
  } deriving (Show)

instance Monoid TxBuilder where
  mempty = TxBuilder  [] [] [] [] [] mempty mempty [] [] Nothing Nothing Map.empty

instance Semigroup TxBuilder where
  (<>)  txb1 txb2 =TxBuilder{
    txSelections = txSelections txb1 ++ txSelections txb2,
    txInputs = txInputs txb1 ++ txInputs txb2,
    txInputReferences = txInputReferences txb1 ++ txInputReferences txb2,
    txOutputs = txOutputs txb1 ++ txOutputs txb2,
    txCollaterals  = txCollaterals txb1 ++ txCollaterals txb2,  -- collateral for the transaction
    txValidityStart = minValidity (txValidityStart txb1) (txValidityStart txb2),
    txValidityEnd = maxValidity (txValidityStart txb1) (txValidityStart txb2),
    txMintData = txMintData txb1 <> txMintData txb2,
    txSignatures = txSignatures txb1 ++ txSignatures txb2,
    txFee  = case txFee txb1 of
      Just f -> case txFee txb2 of
        Just f2 -> Just $ max f f2
        _ -> Just f
      Nothing -> txFee txb2,
    txDefaultChangeAddr = case txDefaultChangeAddr txb1 of
      Just addr -> Just addr
      _ -> txDefaultChangeAddr txb2,
    txMetadata = txMetadata txb1 <> txMetadata txb2
  }

data TxContext = TxContext {
  ctxAvailableUtxo :: UTxO BabbageEra,
  ctxBuiler :: [TxBuilder]
}

txSelection :: TxInputSelection -> TxBuilder
txSelection v = TxBuilder  [v] [] [] [] [] mempty mempty [] [] Nothing Nothing Map.empty

txInput :: TxInput -> TxBuilder
txInput v = TxBuilder  [] [v] [] [] [] mempty mempty [] [] Nothing Nothing Map.empty

txInputReference :: TxInputReference -> TxBuilder
txInputReference v = TxBuilder  [] [] [v] [] [] mempty mempty [] [] Nothing Nothing Map.empty


txMints :: [TxMintData TxMintingScriptSource] -> TxBuilder
txMints md= TxBuilder  [] [] [] [] [] mempty mempty md [] Nothing Nothing Map.empty


txOutput :: TxOutput TxOutputContent -> TxBuilder
txOutput v =  TxBuilder  [] [] [] [v] [] mempty mempty [] [] Nothing Nothing Map.empty

txCollateral' :: TxCollateral -> TxBuilder
txCollateral' v =  TxBuilder  [] [] [] [] [v] mempty mempty [] [] Nothing Nothing Map.empty

txSignature :: TxSignature -> TxBuilder
txSignature v =  TxBuilder  [] [] [] [] [] mempty mempty [] [v] Nothing Nothing Map.empty



-- Transaction validity

-- Set validity Start and end time in posix seconds
txValidPosixTimeRange :: POSIXTime  -> POSIXTime -> TxBuilder
txValidPosixTimeRange start end = TxBuilder  [] [] [] [] [] (ValidityPosixTime start ) (ValidityPosixTime end) [] [] Nothing Nothing Map.empty

-- set  validity statart time in posix seconds
txValidFromPosix:: POSIXTime -> TxBuilder
txValidFromPosix start =  TxBuilder  [] [] [] [] [] (ValidityPosixTime start) mempty [] [] Nothing Nothing Map.empty

-- set transaction validity end time in posix seconds
txValidUntilPosix :: POSIXTime -> TxBuilder
txValidUntilPosix end =  TxBuilder  [] [] [] [] [] mempty (ValidityPosixTime  end) [] [] Nothing Nothing Map.empty

-- Set validity Start and end slot
txValidSlotRange :: SlotNo  -> SlotNo -> TxBuilder
txValidSlotRange start end = TxBuilder  [] [] [] [] [] (ValiditySlot  start ) (ValiditySlot end) [] [] Nothing Nothing Map.empty

-- set  validity statart time in posix seconds
txValidFromSlot:: SlotNo -> TxBuilder
txValidFromSlot start =  TxBuilder  [] [] [] [] [] (ValiditySlot start) mempty [] [] Nothing Nothing Map.empty

-- set transaction validity end time in posix seconds
txValidUntilSlot :: SlotNo  -> TxBuilder
txValidUntilSlot end =  TxBuilder  [] [] [] [] [] mempty (ValiditySlot  end) [] [] Nothing Nothing Map.empty

--- minting
_txMint  v = txMints [v]

-- mint Simple Script
txMintPlutusScript :: IsPlutusScript script =>script  ->  ScriptData -> [(AssetName,Quantity)] -> TxBuilder
txMintPlutusScript script sData amounts = _txMint $ TxMintData (TxMintingPlutusScript  (toTxPlutusScript script) Nothing sData) amounts Map.empty

txMintSimpleScript :: IsSimpleScript script =>script  -> [(AssetName,Quantity)] -> TxBuilder
txMintSimpleScript script amounts = _txMint $ TxMintData (TxMintingSimpleScript  (toTxSimpleScript script)) amounts Map.empty


-- txMintWithMetadata :: IsMintingScript script =>script  ->   [(AssetName,Integer)] -> Map Word64 (Map AssetName Aeson.Value)  -> TxBuilder
-- txMintWithMetadata script amounts mp = _txMint $ TxMintData (TxMintingScriptCode $ toTxMintingScript script) amounts mp
--  witness (valueFromList  $ map (bimap (AssetId policyId) Quantity )  amounts )
--   where
--     witness=   SimpleScriptWitness SimpleScriptV2InBabbage SimpleScriptV2 (SScript simpleScript)
--     script = SimpleScript SimpleScriptV2 simpleScript
--     policyId = scriptPolicyId script

-- pay to an Address
txPayTo:: AddressInEra BabbageEra ->Value ->TxBuilder
txPayTo addr v=  txOutput $  TxOutput (TxOutNative $TxOut addr  (TxOutValue MultiAssetInBabbageEra v) TxOutDatumNone ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset

txPayToWithReference:: AddressInEra BabbageEra ->Value -> Plutus.Script  ->TxBuilder
txPayToWithReference  addr v pScript=  txOutput $  TxOutput (TxOutNative $TxOut addr  (TxOutValue MultiAssetInBabbageEra v) TxOutDatumNone (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV2) $ fromPlutusV2Script  pScript) ))False False OnInsufficientUtxoAdaUnset

-- pay to an Address by pubKeyHash. Note that the resulting address will be an enterprise address
txPayToPkh:: PubKeyHash  ->Value ->TxBuilder
txPayToPkh pkh v= txOutput $  TxOutput ( TxOutPkh  pkh  v ) False False OnInsufficientUtxoAdaUnset

-- pay to Script address
txPayToScript :: AddressInEra BabbageEra -> Value -> Hash ScriptData -> TxBuilder
txPayToScript addr v d = txOutput $TxOutput (TxOutNative $TxOut addr  (TxOutValue MultiAssetInBabbageEra v) (TxOutDatumHash ScriptDataInBabbageEra d) ReferenceScriptNone) False False OnInsufficientUtxoAdaUnset

--Babbage era functions
-- pay to script Address with datum added to the transaction
txPayToScriptWithData :: AddressInEra BabbageEra -> Value -> ScriptData -> TxBuilder
txPayToScriptWithData addr v d  = txOutput $ TxOutput  (TxOutNative $ TxOut  addr (TxOutValue MultiAssetInBabbageEra v)  (TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra d) ReferenceScriptNone ) False False OnInsufficientUtxoAdaUnset

-- pay to script with reference script attached to the output
txPayToScriptWithReference :: Plutus.Script -> Value -> Hash ScriptData -> TxBuilder
txPayToScriptWithReference pScript v d = txOutput $ TxOutput (TxOutScript (toTxPlutusScript (fromPlutusV2Script pScript)) v d) False False OnInsufficientUtxoAdaUnset

-- pay to script with reference script attached to the output and datum inlined
txPayToScriptWithDataAndReference :: Plutus.Script -> Value -> ScriptData -> TxBuilder
txPayToScriptWithDataAndReference pScript v d  =
  txOutput $ TxOutput (TxOutScriptWithData (toTxPlutusScript $ fromPlutusV2Script pScript) v d) False False OnInsufficientUtxoAdaUnset

-- input consmptions

-- use Utxo as input in the transaction
txConsumeUtxos :: UTxO BabbageEra -> TxBuilder
txConsumeUtxos utxo =  txInput $ TxInputResolved $  TxInputUtxo  utxo

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn  v = txInput $ TxInputUnResolved $ TxInputTxin v

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txReferenceTxIn :: TxIn -> TxBuilder
txReferenceTxIn  v = txInputReference $ TxInputReference v


-- use txIn as input in the transaction
-- Since TxOut is also given the txIn is not queried from the node.
txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> TxBuilder
txConsumeUtxo tin v =txConsumeUtxos $ UTxO $ Map.singleton tin  v

-- Mark this address as txExtraKeyWitness in the transaction object.
txSignBy :: AddressInEra BabbageEra -> TxBuilder
txSignBy  a = txSignature (TxSignatureAddr a)

-- Mark this PublicKeyhash as txExtraKeyWitness in the transaction object.
txSignByPkh :: PubKeyHash  -> TxBuilder
txSignByPkh p = txSignature $ TxSignaturePkh p

-- Mark this signingKey's vKey as txExtraKey Witness in the transaction object.
-- When validating `txSignedBy` in plutus, this can be used to add the
txSign :: SigningKey PaymentKey -> TxBuilder
txSign p = txSignature $ TxSignatureSkey p
-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.


-- Redeem from a Script. The script address and value in the TxIn is determined automatically by querying the utxo from cardano node
-- txRedeemTxin:: TxIn ->  ->ScriptData -> ScriptData  -> TxBuilder
-- txRedeemTxin txin script _data _redeemer = txInput $ TxInputUnResolved $ TxInputScriptTxin  ( TxValidatorScript $ script)  (Just  _data)  _redeemer  Nothing txin

-- Redeem from Script Address.
-- TxOut is provided so the address and value need not be queried from the caradno-node

class IsPlutusVersion v where
  toTxPlutusScriptInstance :: PlutusScript v -> TxPlutusScript

instance IsPlutusVersion PlutusScriptV1  where
   toTxPlutusScriptInstance = TxPlutusScriptV1

instance IsPlutusVersion PlutusScriptV2  where
   toTxPlutusScriptInstance = TxPlutusScriptV2

class IsSScriptVersion v where
  toTxSimpleScriptInstance :: SimpleScript v -> TxSimpleScript

instance IsSScriptVersion SimpleScriptV1  where
   toTxSimpleScriptInstance = TxSimpleScriptV1

instance IsSScriptVersion SimpleScriptV2  where
   toTxSimpleScriptInstance = TxSimpleScriptV2

class IsPlutusScript sc where
  toTxPlutusScript :: sc -> TxPlutusScript

class IsSimpleScript sc where
  toTxSimpleScript :: sc -> TxSimpleScript

instance IsPlutusScript TxPlutusScript where
  toTxPlutusScript = id

instance IsSimpleScript TxSimpleScript where
  toTxSimpleScript = id

instance (IsSScriptVersion ver =>  IsSimpleScript (SimpleScript ver)) where
  toTxSimpleScript  = toTxSimpleScriptInstance

instance (IsSScriptVersion ver =>  IsSimpleScript (Script ver)) where
  toTxSimpleScript  (SimpleScript _ sc) = toTxSimpleScriptInstance sc
  toTxSimpleScript _ = error "Impossible"


hashPlutusScript :: TxPlutusScript -> ScriptHash
hashPlutusScript sc = case sc of
  TxPlutusScriptV1 ps -> hashScript (PlutusScript  PlutusScriptV1 ps)
  TxPlutusScriptV2 ps -> hashScript (PlutusScript  PlutusScriptV2 ps)

plutusScriptAddr :: TxPlutusScript -> NetworkId -> AddressInEra BabbageEra
plutusScriptAddr sc networkId =
    let payCred = PaymentCredentialByScript (hashPlutusScript sc)
        addr = makeShelleyAddress networkId payCred NoStakeAddress
        addrInEra = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) addr
    in addrInEra

plutusScriptToScriptAny :: TxPlutusScript -> ScriptInAnyLang
plutusScriptToScriptAny sc = case sc of
    TxPlutusScriptV1 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV1) (PlutusScript PlutusScriptV1 ps)
    TxPlutusScriptV2 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV2) (PlutusScript PlutusScriptV2 ps)

instance (IsPlutusVersion ver =>  IsPlutusScript (PlutusScript ver)) where
  toTxPlutusScript  = toTxPlutusScriptInstance

instance (IsPlutusVersion ver =>  IsPlutusScript (Script ver)) where
  toTxPlutusScript  (PlutusScript psv ps) = toTxPlutusScript ps
  toTxPlutusScript _ = error "Impossible"


class IsSimpleScriptVersion v where
  toTxMintingScriptInstance :: SimpleScript v -> TxScript

class IsMintingScript sc where
  toTxMintingScript:: sc -> TxScript

txScriptPolicyId :: TxScript -> PolicyId
txScriptPolicyId sc = PolicyId (hashTxScript sc)


hashTxScript :: TxScript -> ScriptHash
hashTxScript sc = case sc of
  TxScriptSimple tss -> case tss of
      TxSimpleScriptV1 ss -> hashScript $ SimpleScript SimpleScriptV1 ss
      TxSimpleScriptV2 ss -> hashScript $ SimpleScript SimpleScriptV2 ss
  TxScriptPlutus tps -> hashPlutusScript tps



txScriptToScriptAny :: TxScript -> ScriptInAnyLang
txScriptToScriptAny sc = case sc of
  TxScriptSimple tss ->  case tss of
    TxSimpleScriptV1 ss -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1) (SimpleScript SimpleScriptV1 ss)
    TxSimpleScriptV2 ss -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2) (SimpleScript SimpleScriptV2 ss)
  TxScriptPlutus tps -> plutusScriptToScriptAny tps

 -- TxPlutusMintingScript tps -> plutusScriptToScriptAny tps


class IsScriptVersion v where
  translationFunc :: Script v -> TxScript

instance IsScriptVersion PlutusScriptV1  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps

instance IsScriptVersion PlutusScriptV2  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps

instance IsScriptVersion SimpleScriptV1   where
  translationFunc (SimpleScript psv ps)=  toTxMintingScript  ps

instance IsScriptVersion SimpleScriptV2   where
  translationFunc (SimpleScript psv ps)=  toTxMintingScript  ps

instance IsScriptVersion v  => IsMintingScript (Script v) where
  toTxMintingScript sc = translationFunc sc

instance IsSimpleScriptVersion SimpleScriptV1  where
  toTxMintingScriptInstance  i = TxScriptSimple $  TxSimpleScriptV1  i

instance IsSimpleScriptVersion SimpleScriptV2  where
  toTxMintingScriptInstance i = TxScriptSimple $  TxSimpleScriptV2  i


instance (IsSimpleScriptVersion ver) =>  IsMintingScript (SimpleScript ver)  where
  toTxMintingScript = toTxMintingScriptInstance

instance (IsPlutusVersion ver =>  IsMintingScript (PlutusScript ver)) where
  toTxMintingScript  v = TxScriptPlutus (toTxPlutusScript v)


txRedeemUtxoWithDatum :: IsPlutusScript sc =>  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra ->   sc   -> ScriptData  -> ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithDatum  txin txout sc _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo  (toTxPlutusScript sc)  (Just _data) _redeemer  exUnitsM $ UTxO $ Map.singleton txin  txout

txRedeemUtxo :: IsPlutusScript sc => TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> sc  -> ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxo txin txout script _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputScriptUtxo  (toTxPlutusScript script)  Nothing _redeemer  exUnitsM $ UTxO $ Map.singleton txin  txout

txRedeemTxin :: IsPlutusScript sc => TxIn  -> sc  -> ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxin txin  script _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputScriptTxin  (toTxPlutusScript script)  Nothing _redeemer  exUnitsM  txin

type ScriptReferenceTxIn = TxIn

txRedeemUtxoWithDatumAndReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra  -> ScriptData ->  ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn (Just _data) _redeemer exUnitsM (UTxO $ Map.singleton txin  txout)

txRedeemTxinWithDatumAndReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra  -> ScriptData ->  ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxinWithDatumAndReferenceScript scRefTxIn txin txout _data _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn (Just _data) _redeemer exUnitsM txin

txRedeemUtxoWithReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra  -> ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemUtxoWithReferenceScript scRefTxIn txin txout  _redeemer exUnitsM = txInput $ TxInputResolved $ TxInputReferenceScriptUtxo scRefTxIn Nothing _redeemer exUnitsM (UTxO $ Map.singleton txin  txout)

txRedeemTxinWithReferenceScript :: ScriptReferenceTxIn ->  TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra  -> ScriptData -> Maybe ExecutionUnits ->TxBuilder
txRedeemTxinWithReferenceScript scRefTxIn txin txout  _redeemer exUnitsM = txInput $ TxInputUnResolved $ TxInputReferenceScriptTxin scRefTxIn Nothing _redeemer exUnitsM txin


 -- wallet addresses, from which utxos can be spent for balancing the transaction
txWalletAddresses :: [AddressInEra BabbageEra] -> TxBuilder
txWalletAddresses v = txSelection $ TxSelectableAddresses  v

-- wallet address, from which utxos can be spent  for balancing the transaction
txWalletAddress :: AddressInEra BabbageEra -> TxBuilder
txWalletAddress v = txWalletAddresses [v]

-- wallet utxos, that can be spent  for balancing the transaction
txWalletUtxos :: UTxO BabbageEra -> TxBuilder
txWalletUtxos v =  txSelection $  TxSelectableUtxos v

-- wallet utxo, that can be spent  for balancing the transaction
txWalletUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> TxBuilder
txWalletUtxo tin tout = txWalletUtxos $  UTxO $ Map.singleton tin  tout

txWalletSignKey :: SigningKey PaymentKey -> TxBuilder
txWalletSignKey s= txWalletSignKeys [s]

txWalletSignKeys :: [SigningKey PaymentKey] -> TxBuilder
txWalletSignKeys s= txSelection $ TxSelectableSkey s

txCollateral :: TxIn -> TxBuilder
txCollateral colTxIn = txCollateral' $ TxCollateralTxin colTxIn

txCollateralUtxo :: TxIn -> TxOut CtxUTxO BabbageEra -> TxBuilder
txCollateralUtxo tin tout =  txCollateral' $ TxCollateralUtxo  $ UTxO $ Map.singleton  tin tout

txChangeAddress :: AddressInEra BabbageEra -> TxBuilder
txChangeAddress addr = TxBuilder  [] [] [] [] [] mempty mempty [] [] Nothing (Just addr) Map.empty
