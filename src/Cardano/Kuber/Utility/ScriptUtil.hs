module Cardano.Kuber.Utility.ScriptUtil where
import Cardano.Api
import Cardano.Kuber.Error
import Cardano.Api.Shelley (fromPlutusData, PlutusScriptOrReferenceInput (PScript, PReferenceScript), SimpleScriptOrReferenceInput (SScript), PlutusScript (PlutusScriptSerialised))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import qualified Plutus.V1.Ledger.Api as Plutus

createTxInScriptWitness :: ScriptInAnyLang -> ScriptData -> ScriptData -> ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxTxIn BabbageEra)
createTxInScriptWitness anyScript datum redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) (ScriptDatumForTxIn  datum) redeemer exUnits
    SimpleScript version sscript ->Left $ FrameworkError  WrongScriptType "Simple Script used in Txin"

createTxInScriptWitnessInlineDatum :: ScriptInAnyLang -> ScriptData -> ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxTxIn BabbageEra)
createTxInScriptWitnessInlineDatum anyScript redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) InlineScriptDatum redeemer exUnits
    SimpleScript version sscript ->Left $ FrameworkError  WrongScriptType "Simple Script used in Txin"

createTxInScriptWitnessInlineDatumWithReference :: TxIn -> ScriptData -> ExecutionUnits -> Either FrameworkError (ScriptWitness WitCtxTxIn BabbageEra)
createTxInScriptWitnessInlineDatumWithReference scTxIn redeemer exUnits = pure $ PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2 (PReferenceScript scTxIn) InlineScriptDatum redeemer exUnits


createPlutusMintingWitness :: ScriptInAnyLang ->ScriptData ->ExecutionUnits -> Either FrameworkError  (ScriptWitness WitCtxMint BabbageEra)
createPlutusMintingWitness anyScript redeemer exUnits = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript ->
      pure $ PlutusScriptWitness langInEra version (PScript pscript) NoScriptDatumForMint redeemer exUnits
    SimpleScript version sscript -> Left $ FrameworkError WrongScriptType "Simple script not supported on creating plutus script witness."

createSimpleMintingWitness :: ScriptInAnyLang -> Either FrameworkError (ScriptWitness WitCtxMint BabbageEra)
createSimpleMintingWitness anyScript = do
  ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anyScript
  case script' of
    PlutusScript version pscript -> Left $ FrameworkError  WrongScriptType "Plutus script not supported on creating simple script witness"
    SimpleScript version sscript -> pure $ SimpleScriptWitness langInEra version (SScript sscript)


validateScriptSupportedInEra' ::  CardanoEra era -> ScriptInAnyLang -> Either FrameworkError (ScriptInEra era)
validateScriptSupportedInEra' era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ FrameworkError WrongScriptType   (show lang ++ " not supported in " ++ show era ++ " era")
    Just script' -> pure script'


plutusScriptToScriptAny :: Plutus.Script -> ScriptInAnyLang
plutusScriptToScriptAny plutusScript = ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) $ PlutusScript PlutusScriptV2 $ PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise plutusScript
