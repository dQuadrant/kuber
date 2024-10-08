{-# LANGUAGE TypeFamilies #-}
module Cardano.Kuber.Utility.ScriptUtil where
import Cardano.Api
import Cardano.Kuber.Error
import Cardano.Api.Shelley (PlutusScriptOrReferenceInput (PScript, PReferenceScript), SimpleScriptOrReferenceInput (..), PlutusScript (PlutusScriptSerialised))
import PlutusTx (CompiledCode, BuiltinData)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Cardano.Kuber.Core.TxScript
import Data.Functor ((<&>))
import PlutusTx.Prelude (BuiltinUnit)


type TxScriptParsed witctx era =  Either
          (  ScriptDatum witctx
                         -> ScriptRedeemer
                         -> ExecutionUnits
                         -> ScriptWitness witctx era)
          (ScriptWitness witctx era)
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

makeTxSimpleScriptWitness :: ShelleyBasedEra era ->  SimpleScript -> Maybe TxIn -> Either FrameworkError (ScriptWitness witctx era)
makeTxSimpleScriptWitness cera simpleSc mtxIn = do
    lang <- validateSimpleScript
    pure $ SimpleScriptWitness lang (case mtxIn of
      Nothing -> SScript simpleSc
      Just ti ->  SReferenceScript ti Nothing)

  where
    validateSimpleScript = validateLang cera SimpleScriptLanguage ("Simple Script not supported for this era" ++ show cera)


makeTxScriptWitness ::  ShelleyBasedEra era ->  TxScript -> Maybe TxIn -> Either FrameworkError (TxScriptParsed witctx era)
makeTxScriptWitness era script mtxIn = case script of
  TxScriptSimple tss -> do
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

fromPlutusV3Script :: CompiledCode (BuiltinData -> BuiltinUnit) -> Script PlutusScriptV3
fromPlutusV3Script plutusScript = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $  serialiseCompiledCode plutusScript

fromPlutusV2Script :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit) -> Script PlutusScriptV2
fromPlutusV2Script plutusScript = PlutusScript PlutusScriptV2 $ PlutusScriptSerialised $  serialiseCompiledCode plutusScript

fromPlutusV1Script :: CompiledCode (BuiltinData -> BuiltinData  -> BuiltinData-> BuiltinUnit) -> Script PlutusScriptV1
fromPlutusV1Script plutusScript = PlutusScript PlutusScriptV1 $ PlutusScriptSerialised $ serialiseCompiledCode plutusScript
