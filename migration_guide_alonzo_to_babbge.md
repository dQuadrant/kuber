# Migration from the Alonzo era to the Babbage era
Here we outline the changes needed to make the Alonzo transaction builder code work correctly in the Babbage era. These changes are significant if you are using the `cardano-api` library.

## 1. Change Era 
If `AlonzoEra` is explicitly used, then it needs to be updated to `BabbageEra`, as follows:
```haskell
UTxO AlonzoEra -> UTxO BabbageEra
```

## 2. Change `EraInCardanoMode`
Change `EraInCardanoMode AlonzoEraInCardanoMode` as follows:
```haskell
AlonozoEraInCardanoMode -> BabbageEraInCardanoMode
```

## 3. Add either `ReferenceScript` or `ReferenceScriptNone` additional field on TxOut
```haskell
TxOut address txOutValue datum ReferenceScriptNone
-- or
TxOut address txOutValue datum (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra script)
```

## 4. Change `ScriptWitness` structure
The Script section should be replaced by `PlutusScriptOrReferenceInput` for Plutus scripts and  `SimpleScriptOrReferenceInput` for simple script. So, depending on your use case, you can either pass the script itself or use script witness with Reference Script. The snippets for both are shown below.

For plutus script witness, use:
```haskell
PlutusScriptWitness langInEra version (PScript pscript) datumForTxin redeemer exUnits
```
For simple script witness, use:
```haskell
SimpleScriptWitness langInEra version (SScript sscript)
```

## 5. Function for parsing  
`deserialiseFromRawBytesHex` now returns `Either` instead of `Just`

These points summarize the required changes on Kuber as a library applied on NFT marketplace use case. For your use case if you required more changes, please share in the comments below.
