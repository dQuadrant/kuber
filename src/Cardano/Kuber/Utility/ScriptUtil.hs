{-# LANGUAGE TypeFamilies #-}
module Cardano.Kuber.Utility.ScriptUtil where
import Cardano.Api
import Cardano.Kuber.Error
import Cardano.Api.Shelley (fromPlutusData, PlutusScriptOrReferenceInput (PScript, PReferenceScript), SimpleScriptOrReferenceInput (..), PlutusScript (PlutusScriptSerialised))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2
import qualified PlutusLedgerApi.V1
import PlutusTx (CompiledCode, BuiltinData)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Cardano.Kuber.Core.TxBuilder (TxPlutusScript (..), TxScript (..), TxSimpleScript (TxSimpleScript), IsTxBuilderEra)
import qualified Debug.Trace as Debug
import Data.Functor ((<&>))
type TxScriptParsed witctx era =  Either  
          (  ScriptDatum witctx
                         -> ScriptRedeemer
                         -> ExecutionUnits
                         -> ScriptWitness witctx era)
          (ScriptWitness witctx era)

createTxInScriptWitness :: ScriptInAnyLang -> Maybe HashableScriptData -> HashableScriptData -> ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxTxIn ConwayEra)
createTxInScriptWitness anyScript mDatum redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' ShelleyBasedEraConway anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) datumForTxin redeemer exUnits
    SimpleScript sscript ->Left $ FrameworkError  WrongScriptType "Simple Script used in Txin"
  where
    datumForTxin = maybe InlineScriptDatum ScriptDatumForTxIn mDatum


createTxInReferenceScriptWitness :: TxIn -> Maybe ScriptHash -> Maybe HashableScriptData -> ScriptRedeemer -> ExecutionUnits -> Either FrameworkError (ScriptWitness WitCtxTxIn ConwayEra)
createTxInReferenceScriptWitness scTxIn mScriptHash mDatum redeemer exUnits = pure $ PlutusScriptWitness PlutusScriptV2InConway PlutusScriptV2 (PReferenceScript scTxIn mScriptHash) datumForTxin redeemer exUnits
  where
    datumForTxin = maybe InlineScriptDatum ScriptDatumForTxIn mDatum

createPlutusMintingWitness :: ScriptInAnyLang ->HashableScriptData ->ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxMint ConwayEra)
createPlutusMintingWitness anyScript redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' ShelleyBasedEraConway anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) NoScriptDatumForMint redeemer exUnits
    SimpleScript sscript -> Left $ FrameworkError WrongScriptType "Simple script not supported on creating plutus script witness."

createSimpleMintingWitness :: ScriptInAnyLang -> Either FrameworkError (ScriptWitness WitCtxMint ConwayEra)
createSimpleMintingWitness anyScript = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' ShelleyBasedEraConway anyScript
  case script' of
    PlutusScript version pscript -> Left $ FrameworkError  WrongScriptType "Plutus script not supported on creating simple script witness"
    SimpleScript sscript -> pure $ SimpleScriptWitness langInEra (SScript sscript)


validateScriptSupportedInEra' :: ShelleyBasedEra era -> ScriptInAnyLang -> Either FrameworkError (ScriptInEra era)
validateScriptSupportedInEra' era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ FrameworkError WrongScriptType   (show lang ++ " not supported in " ++ show era ++ " era")
    Just script' -> pure script'

makeTxPlutusScriptWitness :: 
    ShelleyBasedEra era -> 
    TxPlutusScript -> 
    Maybe TxIn -> 
    Either FrameworkError (ScriptDatum witctx  -> ScriptRedeemer -> ExecutionUnits -> ScriptWitness witctx era)
makeTxPlutusScriptWitness era script mtxIn = case script of
    TxPlutusScriptV1 ps -> do
      langInEra <- validatePv1
      pure $  PlutusScriptWitness langInEra PlutusScriptV1 (toWitnessPScript ps )
    TxPlutusScriptV2 ps -> do
      langInEra <- validatePv2
      pure $   PlutusScriptWitness langInEra PlutusScriptV2 (toWitnessPScript ps )
    TxPlutusScriptV3 ps -> do
      langInEra <- validatePv3
      pure $   PlutusScriptWitness langInEra PlutusScriptV3 (toWitnessPScript ps )  
  where
      toWitnessPScript :: PlutusScript lang-> PlutusScriptOrReferenceInput lang
      toWitnessPScript ps = case mtxIn of
        Nothing -> PScript ps
        Just ti -> PReferenceScript ti Nothing
      validatePv1 = validateLang  era (PlutusScriptLanguage PlutusScriptV1) $ "PlutusScriptV1 not supported in " ++ show era
      validatePv2 = validateLang era (PlutusScriptLanguage PlutusScriptV2) $ "PlutusScriptV2 not supported in " ++ show era
      validatePv3 = validateLang era (PlutusScriptLanguage PlutusScriptV3) $ "PlutusScriptV3 not supported in " ++ show era

makeTxSimpleScriptWitness :: ShelleyBasedEra era ->  TxSimpleScript -> Maybe TxIn -> Either FrameworkError (ScriptWitness witctx era)
makeTxSimpleScriptWitness cera sc mtxIn = do
    lang <- validateSimpleScript
    let simpleSc = case sc of { TxSimpleScript ss -> ss } 
    pure $ SimpleScriptWitness lang (case mtxIn of
      Nothing -> SScript simpleSc
      Just ti ->  SReferenceScript ti Nothing)

  where
    validateSimpleScript = validateLang cera SimpleScriptLanguage ("Simple Script not supported for this era" ++ show cera)


makeTxScriptWitness ::  ShelleyBasedEra era ->  TxScript -> Maybe TxIn -> Either FrameworkError (TxScriptParsed witctx era)
makeTxScriptWitness era script mtxIn = case script of
  TxScriptSimple (TxSimpleScript tss) -> do
    lang <- validateSimpleScript
    pure$ Right $ SimpleScriptWitness lang (case mtxIn of
      Nothing -> SScript tss
      Just ti ->  SReferenceScript ti Nothing)
    -- pure $ ScriptInEra lang (SimpleScript tss)
  TxScriptPlutus tps -> makeTxPlutusScriptWitness era tps mtxIn <&> Left
  where
      toWitnessPScript :: PlutusScript lang-> PlutusScriptOrReferenceInput lang
      toWitnessPScript ps = case mtxIn of
        Nothing -> PScript ps
        Just ti -> PReferenceScript ti Nothing
      validateSimpleScript = validateLang era SimpleScriptLanguage ("Simple script not supported for this era" ++ show era)


validateLang ::  ShelleyBasedEra era2 ->   ScriptLanguage lang-> String-> Either FrameworkError (ScriptLanguageInEra lang era2)
validateLang  era lang msg = case scriptLanguageSupportedInEra era lang  of
      Nothing -> Left $ FrameworkError FeatureNotSupported msg
      Just scInEra -> pure scInEra

fromPlutusV3Script :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Script PlutusScriptV3
fromPlutusV3Script plutusScript = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $  serialiseCompiledCode plutusScript

fromPlutusV2Script :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Script PlutusScriptV2
fromPlutusV2Script plutusScript = PlutusScript PlutusScriptV2 $ PlutusScriptSerialised $  serialiseCompiledCode plutusScript

fromPlutusV1Script :: CompiledCode (BuiltinData -> BuiltinData  -> BuiltinData-> ()) -> Script PlutusScriptV1
fromPlutusV1Script plutusScript = PlutusScript PlutusScriptV1 $ PlutusScriptSerialised $ serialiseCompiledCode plutusScript
