export const SimpleContractCode = `{-# LANGUAGE NoImplicitPrelude  #-} 
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DataKinds #-}

module Contract
where

import PlutusTx.Prelude
import PlutusTx hiding( txOutDatum)
import Plutus.V1.Ledger.Api

{-# INLINABLE mkValidator #-}
mkValidator ::  ()  -> () -> ScriptContext  -> Bool
mkValidator  _ _ _  =True

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator  d r c = check $ mkValidator  (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: Validator
validator  = mkValidatorScript  $$(PlutusTx.compile [|| mkWrappedValidator ||])`;

export const DefaultHaskellCode = `{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DataKinds #-}

module Contract
where

import PlutusTx.Prelude
import PlutusTx hiding( txOutDatum)
import Plutus.V1.Ledger.Api

{-# INLINABLE mkValidator #-}
mkValidator ::  ()  -> () -> ScriptContext  -> Bool
mkValidator  _ _ _  =True

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator  d r c = check $ mkValidator  (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: Validator
validator  = mkValidatorScript  $$(PlutusTx.compile [|| mkWrappedValidator ||])`;

export const ExampleMetadataCode = `{
  "selections":["addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz"],
  "outputs": [{
      "address": "addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
      "value": "5A"
  }],
  "metadata": {
      "0": {
          "library": "Kuber",
          "version": "2.0.0"
      },
      "1": "Send 5Ada to same address to create collateral"
  }
}`;

export const DefaultKuberCode = `{
  "selections":["addr_test1qz7j6lf6tdw8cd2l93fa26y4ty578hjwz5ga0q4c5lw4c2wxcq3twjkadkn5jvyxrn66traalhyx5kw58kmkas64e94qwqyx4c"],
  "outputs": [{
      "address": "addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
      "value": "10A"
  }]
}`;

export const KuberReadme = `Transaction builder object specifies the spec for the transaction that is to be composed. transaction can be composed by posting a
TransactionBuilder JSON object as POST request to localhost:8081/api/v1/tx

1. selections : List of utxos/addresses that can be used for balancing transaction

2. inputs : List inputs in transactions

3. referenceInputs : Reference Inputs

4. outputs : List Output utxos in the transaction

5. collaterals : [optional] List of collaterals in the transaction (It is automatically selected if missing)

6. validityStart : [Integer: PosixTimestamp seconds] (convinence field for usage instead of validityStartSlot) Transaction validFrom

7. validityStartSlot : [Integer: Slot Number] Transaction validFrom

8. validityEnd : [Integer : PosixTimestamp seconds] (convinence field for usage instead of validityEndSlot) Transaction validUntil

9. validityEndSlot : [Integer : Slot Numbers] Transaction validUntil

10. mint : Minting Scripts and value in the transaction

11. signatures

12. fee : [Integer : Lovelace] Fee is calculated automatically, but setting this will set transaction fee explicitly.

13. changeAddress [Optional ] : Default change address. If it's missing, it's selected from one of the selection address. Setting addChange in any one output will disable this option

14. metadata : Transaction metadata`;

export const HaskellReadme = `Plutus Compiler
The module name must be Contract.`;

export const KuberCodes = {
  "kuber.json": DefaultKuberCode,
  exampleMetadata: ExampleMetadataCode,
  readme: KuberReadme,
};

export const HaskellCodes = {
  "contract.hs": DefaultHaskellCode,
  simpleContract: SimpleContractCode,
  readme: HaskellReadme,
};

export const DefaultComment =
  "// Auto completion is a testing feature, It maynot always work.";

export const NetworkUrls = {
  "preview testnet": "https://preview.kuberide.com",
  "preprod testnet": "https://preprod.kuberide.com",
  mainnet: "https://mainnet.kuberide.com",
  localhost: "http://localhost:8081",
};

export const HaskellResults = {
  "contract.hs":{"code":0,"error":"","script":{"cborHex":"49480100002221200101","description":"","type":"PlutusScriptV2"},"success":true}
}