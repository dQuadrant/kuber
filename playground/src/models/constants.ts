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

export const ExampleTransfer = `{
    "selections":["addr_test1qz7j6lf6tdw8cd2l93fa26y4ty578hjwz5ga0q4c5lw4c2wxcq3twjkadkn5jvyxrn66traalhyx5kw58kmkas64e94qwqyx4c"],
    "outputs": [{
        "address": "addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
        "value": "10A"
    }]
}`;

export const DefaultComment =
  "// Auto completion is a testing feature, It maynot always work.";

export const NetworkUrls = {
  "Preview Testnet": "https://preview.cnftregistry.io/kuber",
  "Preprod Testnet": "https://preprod.cnftregistry.io/kuber",
  Mainnet: "https://cnftregistry.io/kuber",
  "Legacy Testnet": "https://testnet.cnftregistry.io/kuber",
  Localhost: "http://localhost:8081",
};
