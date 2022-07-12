##### Migration required to make older transaction builder code work that utlizes cardano-api library:

1. Changes of Era if `AlonzoEra` is explicitly used then it needs to be updated to `BabbageEra` like 
```haskell
UTxO AlonzoEra -> UTxO BabbageEra
```
2. Changes of `EraInCardanoMode BabbageEraInCardanoMode` like
```haskell
AlonozoEraInCardanoMode -> BabbageEraInCardanoMode
```
3. Add either `ReferenceScript` or `ReferenceScriptNone` additoinal field on TxOut like
```haskell
TxOut address txOutValue datum ReferenceScriptNone
-- or
TxOut address txOutValue datum (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra script)
```
4. Changes to `ScriptWitness` strucutre now the Script section is replaced by `PlutusScriptOrReferenceInput` for plutus while for simple script it is `SimpleScriptOrReferenceInput`. So accoriding to usages if you need script witness with Reference Script or you intend to pass the script itself like

For plutus script witness
```haskell
PlutusScriptWitness langInEra version (PScript pscript) datumForTxin redeemer exUnits
```
For simple script witness
```haskell
SimpleScriptWitness langInEra version (SScript sscript)
```
