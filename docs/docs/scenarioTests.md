---
sidebar_position: 2
---

# Scenario Tests

We have tested the following scenarios in Hydra:

## 1. Mint a token and close hydra head

- A token was minted using a plutusV3 always pass script
- The snapshot was confirmed for minting token
- Head was closed
- Error during fanout

```json
"postTxError": {
        "failureReason": "TxValidationErrorInCardanoMode (ShelleyTxValidationError ShelleyBasedEraConway (ApplyTxError (ConwayUtxowFailure (UtxoFailure (ValueNotConservedUTxO (MaryValue (Coin 325709888) (MultiAsset (fromList [(PolicyID {policyID = ScriptHash \"29c699a1d8dc832504e4ec37a41286176820c9221c5505f7005bae68\"},fromList [(\"4879647261486561645631\",1),(\"e696fc821063f9b7311bb350539e67c8fad1bd571605e75b5a353eab\",1),(\"fce240ccfcb839aa37e5b04206a84530e027b0d3bfb596e7d0685f6a\",1)])]))) (MaryValue (Coin 325709888) (MultiAsset (fromList [(PolicyID {policyID = ScriptHash \"29c699a1d8dc832504e4ec37a41286176820c9221c5505f7005bae68\"},fromList [(\"4879647261486561645631\",1),(\"e696fc821063f9b7311bb350539e67c8fad1bd571605e75b5a353eab\",1),(\"fce240ccfcb839aa37e5b04206a84530e027b0d3bfb596e7d0685f6a\",1)]),(PolicyID {policyID = ScriptHash \"3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712\"},fromList [(\"68796472612d6b75626572\",1)])]))))) :| [])))",
        "tag": "FailedToPostTx"
    }
```

## 2. Mint a token, burn it and close hydra head
- A token was minted using a plutusV3 always pass script.  
- The snapshot was confirmed for minting token
- Token was burnt
- The snapshot was confirmed for burning token
- Head was closed
- Fanout Successful

## 3. Pay to script and close hydra head
- Paid **10 ₳** to script address `addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla`
- Snapshot was confirmed for this transaction
- Head was closed
- Fanout Successful: [21f398e9a5a7661c326036d5e9577b64f28554da9e26387e780a032fdb77e99a](https://preview.cexplorer.io/tx/21f398e9a5a7661c326036d5e9577b64f28554da9e26387e780a032fdb77e99a)

## 4. Pay to 500 addresses and close hydra head
- Transactions for 500 addresses were done within the hydra head
- Snapshot was confirmed for each transaction
- Head was closed
- Error during fanout

```json
{
   "postTxError": {
        "failureReason": "ValidationFailure (WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 0, exUnitsSteps' = 0}}) (CekError An error has occurred:\nThe machine terminated part way through evaluation due to overspending the budget.\nThe budget when the machine terminated was:\n({cpu: 6396337807\n| mem: -2582})\nNegative numbers indicate the overspent budget; note that this only indicates the budget that was needed for the next step, not to run the program to completion.) [] ..."
     }
}
```