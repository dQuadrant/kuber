{-# LANGUAGE TypeFamilies #-}
module Cardano.Kuber.Utility.ScriptUtil where
import Cardano.Api
import Cardano.Kuber.Error
import Cardano.Api.Shelley (fromPlutusData, PlutusScriptOrReferenceInput (PScript, PReferenceScript), SimpleScriptOrReferenceInput (SScript), PlutusScript (PlutusScriptSerialised))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2
import qualified PlutusLedgerApi.V1
import PlutusTx (CompiledCode, BuiltinData)
import PlutusLedgerApi.Common (serialiseCompiledCode)

createTxInScriptWitness :: ScriptInAnyLang -> Maybe HashableScriptData -> HashableScriptData -> ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxTxIn BabbageEra)
createTxInScriptWitness anyScript mDatum redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) datumForTxin redeemer exUnits
    SimpleScript sscript ->Left $ FrameworkError  WrongScriptType "Simple Script used in Txin"
  where
    datumForTxin = maybe InlineScriptDatum ScriptDatumForTxIn mDatum


createTxInReferenceScriptWitness :: TxIn -> Maybe ScriptHash -> Maybe HashableScriptData -> ScriptRedeemer -> ExecutionUnits -> Either FrameworkError (ScriptWitness WitCtxTxIn BabbageEra)
createTxInReferenceScriptWitness scTxIn mScriptHash mDatum redeemer exUnits = pure $ PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2 (PReferenceScript scTxIn mScriptHash) datumForTxin redeemer exUnits
  where
    datumForTxin = maybe InlineScriptDatum ScriptDatumForTxIn mDatum


createPlutusMintingWitness :: ScriptInAnyLang ->HashableScriptData ->ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxMint BabbageEra)
createPlutusMintingWitness anyScript redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) NoScriptDatumForMint redeemer exUnits
    SimpleScript sscript -> Left $ FrameworkError WrongScriptType "Simple script not supported on creating plutus script witness."

createSimpleMintingWitness :: ScriptInAnyLang -> Either FrameworkError (ScriptWitness WitCtxMint BabbageEra)
createSimpleMintingWitness anyScript = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript -> Left $ FrameworkError  WrongScriptType "Plutus script not supported on creating simple script witness"
    SimpleScript sscript -> pure $ SimpleScriptWitness langInEra (SScript sscript)


validateScriptSupportedInEra' ::  CardanoEra era -> ScriptInAnyLang -> Either FrameworkError (ScriptInEra era)
validateScriptSupportedInEra' era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ FrameworkError WrongScriptType   (show lang ++ " not supported in " ++ show era ++ " era")
    Just script' -> pure script'

fromPlutusV2Script :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Script PlutusScriptV2
fromPlutusV2Script plutusScript = PlutusScript PlutusScriptV2 $ PlutusScriptSerialised $  serialiseCompiledCode plutusScript

fromPlutusV1Script :: CompiledCode (BuiltinData -> BuiltinData  -> BuiltinData-> ()) -> Script PlutusScriptV1
fromPlutusV1Script plutusScript = PlutusScript PlutusScriptV1 $ PlutusScriptSerialised $ serialiseCompiledCode plutusScript
