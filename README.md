Kuber 
===========

Haskell library and API server for composing balanced cardano transaction

## 1. Building locally
Kuber depends on `cardano-api` library. The system packages and dependencies required for building this project is same as that of `cardano-node`

To prepare your system for building kuber from sources, you can follow these instructions:

 [building-cardano-node](https://developers.cardano.org/docs/get-started/installing-cardano-node/)

 Once everything is installed and is ready, kuber is ready to run
 ```
cabal update
cabal run kuber
 ```

 This will start kuber on port `8081`


 ## 2. Composing transactions with api
 transaction can be composed by posting a TransactionBuilder object as POST request to  /api/v1/tx

 
 ### 2.1 TransactionBuilder Object
 Transaction builder object specifies the spec for the transaction that is to be composed.

#### 2.1.1 `selections` : [ TxHash#index | Address ] Array of utxos that can be used to balance the transaction . 
 
 Selection is  generally the wallet from which payment is being made.  Selection can be in one of following format

 - "3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1" : Transaction output 
 - "addr_test1qrmntnd29t3kpnn...qtpcsrj" : PublicKey  Address i.e. Script address cannot be used for selection
 - ""00f7...f99531306750d3d460d88" : PubkicKey Address in CBOR hex format

 When address is used for selection, any of the utxos of the address may be used if required.  

eg: 

```json
    {
        "selections": [
            "3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1",
            "00a4dcec5ae1b9882304d6597f7a3763f0372c4f06939edaa4720ee29a927020a2ac787f4fa218a59ee1ecab4eaf117b8fab9a79850534e41f",
            "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj"
        ] 
    }
```

#### 2.1.2 `inputs` : [ TxIx#index | Address | Object ] : Inputs (Utxos being spent) in the transaction

input can have following fields  depending on the context in which it's being used.

- "utxo" \[required] : Utxo being consumed "TxHaxh#index" 
- "script" [optional]: The script code in json format if it's script input
- "datum"  [optional]: if it's script utxo, datum of the utxo. Datum is in json format with object structure

eg:
```json
{
    "inputs": [
        "3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1",
        {
            "utxo": "8277e7ca93315b49fd0bc7b4c9bebf1e553bce7c967cd5e8ed061dae736a567d#0",
            "script": {
                "type": "PlutusScriptV1",
                "description": "",
                "cborHex": "5916de5916db010000332332232332232323233322232..048202b7881120c096b1022222123333001005004003002200101",
                    "datum": {
                    "constructor": 0,
                    "fields": [
                        {
                            "bytes": "faf109a75f7eff663c15a36954f6b27aed5d461932a5651dc2966448"
                        },
                        {
                            "list": [
                                {
                                    "fields": [
                                        {
                                            "bytes": "f735cdaa2ae360ce67e27cd2f6038a44c77bf3ede94bd144674d34e6"
                                        },
                                        {
                                            "int": 33330000
                                        }
                                    ],
                                    "constructor": 0
                                }
                            ]
                        },
                        {
                            "int": 76234100
                        },
                        {
                            "fields": [],
                            "constructor": 0
                        }
                    ]
                }
            }
        }
    ]
}
```

### 2.1.3 `outputs` : Object : Outputs of the transaction.

Output can have following fields depending on the context in which it's being used

- address : Receiving address
- value : Amount to be sent. In case of native tokens, they can be joined by `+` like in cardano-cli response
- script : Plutus script in jsonenvelope. Address is determined from the scipt if it's not provided
- datumHash : In case of script address, datum hash should be provided
- datum : Datum can directly be provided instead of dataumHash. In that case, datumHas is calculated and used for the output
- deductFee :  Deduct fee value from this output
- addChange :  add Change in balancing to this output


e.g:
```json
{
    "outputs": [
        {
            "address": "addr_test1vzlea6jj330c8549xynn25stqxteetrket668hp65w73ussmzu06p",
            "value": "200A",
            "deductFee": true
        },
        {
            "address": "addr_test1wp04uqjsfjeaqgrzq60y8aajuqhcaa3ar4jp227k5w2v8hs4p9my9",
            "value": "2A + 366757b2f1bf64ea60760dced8ebfacbbbc8a5f110abbdc5e2f5e2a6.UltraKingKongNFT014",
            "datum":{
              "fields":[{"bytes":"bf9eea528c5f83d2a5312735520b01979cac76caf5a3dc3aa3bd1e42"},{"list":[]},{"bytes":""},{"bytes":""},{"int":20000000},{"fields":[],"constructor":0}],
              "constructor":0
            }
        }
    ]
}
```
