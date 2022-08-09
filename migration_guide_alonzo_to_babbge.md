# Migration: Alonzo -> Babbage 
The following are the required changes to make Alonzo transaction builder code to work on Babbage Era. These changes are significant if you're using `cardano-api` library.

## 1. Change Era 
If `AlonzoEra` is explicitly used then it needs to be updated to `AlonzoEra` like 
```haskell
UTxO AlonzoEra -> UTxO AlonzoEra
```

## 2. Change `EraInCardanoMode`
Change `EraInCardanoMode AlonzoEraInCardanoMode` like
```haskell
AlonozoEraInCardanoMode -> AlonzoEraInCardanoMode
```

## 3. Add either `ReferenceScript` or `ReferenceScriptNone` additional field on TxOut
```haskell
TxOut address txOutValue datum ReferenceScriptNone
-- or
TxOut address txOutValue datum (ReferenceScript ReferenceTxInsScriptsInlineDatumsInAlonzoEra script)
```

## 4. Change `ScriptWitness` structure
The Script section is replaced by `PlutusScriptOrReferenceInput` for plutus scripts and for simple script it is `SimpleScriptOrReferenceInput`. So according to usages if you need script witness with Reference Script or you intend to pass the script itself.

For plutus script witness
```haskell
PlutusScriptWitness langInEra version (PScript pscript) datumForTxin redeemer exUnits
```
For simple script witness
```haskell
SimpleScriptWitness langInEra version (SScript sscript)
```

## 5. Function for parsing  
`deserialiseFromRawBytesHex` now returns `Either` instead of `Just`
