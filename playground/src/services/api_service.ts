export default class APIService {
  static contractCode = `
  SAMPLE_CONTRACT_CODE = """
  {-# LANGUAGE NoImplicitPrelude  #-}
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
  validator  = mkValidatorScript  $$(PlutusTx.compile [|| mkWrappedValidator ||])
  `;

  static async compileCode() {
    const response = await fetch(import.meta.env.VITE_COMPILER_API, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        // accept: "application/json",
      },
      body: JSON.stringify({ code: APIService.contractCode }),
    });
    console.log(response);
    console.log(await response.json());
  }
}
