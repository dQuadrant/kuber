
Kuber Json Api Reference
=====================
 Transaction builder object specifies the spec for the transaction that is to be composed.
  transaction can be composed by posting a TransactionBuilder JSON object as POST request to  localhost:8081/api/v1/tx


- [**selections**](#1-selections---string--object--array-of-utxos-that-can-be-used-to-balance-the-transaction) : List of utxos/addresses that can be used for balancing transaction
- [**inputs**](#2-inputs---string--object---inputs-utxos-being-spent-in-the-transaction) : List inputs in transactions
- [**referenceInputs**](#3-referenceinputs--string--referenceinputs-transction-field) : Reference Inputs
- [**outputs**](#4-outputs--object--outputs-created-in--the-transaction) : List Output utxos in the transaction
- [**collaterals**](#5-collaterals-string-optional--collateral-inputs-in-the-transaction) : [optional] List of collaterals in the transaction (It is automatically selected if missing) 
- **validityStart** : [Integer: UnixTimestamp millisecond] Transaction validFrom
- **validityEnd** : [Integer : UnixTimestamp millisecond] Transaction validUntil 
- [**mint**](#6-mint--object--minting-script-and-amount-in-the-transaction) : Minting Scripts and value in the transaction
- [**signatures**](#7 -)
- **fee** : [Integer : Lovelace]  Fee  is calculated automatically, but setting this will set  transaction fee explicitly.
- **changeAddress** [Optional ] : Default change address. If it's missing, it's selected from one of the selection address. Setting `addChange`  in any one output will disable this option
- [**metadata**](#7-metadata--object--transaction-metadata) : Transaction metadata

### 1. `selections` : [ string | object ] Array of utxos that can be used to balance the transaction
 
 Selection is the wallet from which payment is being made. Selection utxos are used if required during balancing.
  Selection utxos can only be the output to PublicKey addresses and not to Script addresses. 
  The  Selection can be in one of following format.

- TxIn Object : TxIn in object format.  Kuber will determine the utxo value by querying with node and use it if required for balancing
  
      { 
         "hash" | "txid" | "txId" : "String" transaction hash,
         "index"                  : [integer] transaction output index
       }

    ___eg___:&#160;{ "hash": "3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8", "index": 1}
   

- TxIn [ TxHash#index ]\
   TxIn in readable format. Kuber will determine the utxo value by querying with node and use it if required for balancing\
   ___eg___:&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1"
 

- Utxo CborHex\
   Utxo( transaction output ) in cbor format. Utxo cbor contains output address, value.  Kuber directly uses the value  and address by decoding it so, querying the node is not necessary.\
   ___eg___:&#160;"828258202ff238f64b773435d6f626aafe56073f251b52281c50a3872951905fbc597e560082583901ab1d01c80b7ef656194c4af4a682f2d55d714379bde1afe72dc5d348f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b1a0037e4d2"  


- Address bech32\
   Wallet address in bech32 format. when address is used, all the utxos in the address is queried from the node and then the utxos are used in the transaction if required.\
   ___eg___:&#160;"addr_test1vzzc9nx2lxmu9r7gyd8cyd0dcx0ynh729rpv4c553exs7kgyu9cxl"
            

- Address CborHex\
   Wallet address in cbor format. when address is used, all the utxos in the address is queried from the node and then the utxos are used if required for balancing.\
   ___eg___:&#160;"6136e0cf1e52e05ef92e52c7bc2a04493d6bae481b8acbab12ec4300d7"\
   ___eg___:&#160;"0136e0cf1e52e05ef92e52c7bc2a04493d6bae481b8acbab12ec4300d7f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b"

### 2. `inputs` : [ string | Object ] : Inputs (Utxos being spent) in the transaction

input can have following fields  depending on the context in which it's being used.

#### PubKeyUtxos as input

- TxIn Object : TxIn in object format.  Kuber will determine the utxo value by querying with node and use it if required for balancing

      { 
         "hash" | "txid" | "txId" : "String" transaction hash,
         "index"                  : [integer] transaction output index
      }
    __eg__:&#160;{&#160;"hash":&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8",&#160;"index":&#160;1}
                    

- TxIn [ TxHash#index ]
   TxIn in readable format. Kuber will use this utxo in the transaction\
   ___eg___:&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1"


- Address bech32\
   Wallet address in bech32 format. when address is used, all the utxos in the address will be used as input and spent in the transaction.\
   __eg__:&#160;"addr_test1vzzc9nx2lxmu9r7gyd8cyd0dcx0ynh729rpv4c553exs7kgyu9cxl"
                            
  
- Address CborHex\
   Wallet address in cbor format. When address is used, all the utxos in the address will be used as input and spent in the transaction.\
   ___eg___:&#160;"6136e0cf1e52e05ef92e52c7bc2a04493d6bae481b8acbab12ec4300d7"\
   ___eg___:&#160;"0136e0cf1e52e05ef92e52c7bc2a04493d6bae481b8acbab12ec4300d7f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b"
          
  
- Utxo CborHex\
Utxo( transaction output ) in cbor format. Utxo cbor contains output address, value , (datum hash in case of script output).  Kuber directly uses the value  and address by decoding it so, querying the node is not necesasry.\
   ___eg___:&#160;"828258202ff238f64b773435d6f626aafe56073f251b52281c50a3872951905fbc597e560082583901ab1d01c80b7ef656194c4af4a682f2d55d714379bde1afe72dc5d348f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b1a0037e4d2"
   

#### Script Utxos as input

When spending/redeeming form script utxo, Input value should be an object with all of the following fields.
 - `utxo` : TxIn [ TxHash#index ]
    TxIn in readable format. Kuber will use this utxo in the transaction 

    ___eg___:&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1"   

- `script` : TxIn [TxHash#index]   
    Utxo where the reference script is present. The utxo will be added to referenceInputs automatically.      

 - `script` [Object] :  Serialized script wrapped in text evelope. `script` object must have the following fields

       {
          "type": "String" Script type [ "PlutuScrpitV1" | "PlutusScriptV2" ]
          "description": "String"  Description for the script. Can be set to empty
          "cborHex": "String" Scrialized cbor hex representation of the script.
       }
                         

 - `redeemer` [Object] : Redeemer datum in json format.

     ___eg___: `{ "constructor": 0, "fields": []}`
                                                   

 - `datum` [Object] [Optional] : Datum matching the datumHash in the utxo. If Inline Datum is present in utxo, datum is not required and is directly fetched from the datum. Datum 

    ___eg___: `{"constructor": 0, "fields": [{"bytes":"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8"}]}`
                

 - `exUnits` | `executionUnits` [Object] [Optional]: \
Specify Execution units values. If not provided, Kuber automatically calculates the execution units and includes it in the transaction
 
       {
           "steps": "String" Execution units steps
           "memory": "String" Execution units memory
       }

### 3. `referenceInputs` : [String] : ReferenceInputs transction field

Reference inputs are used for referenceData or referenceScripts feature. When referenceInput contains datum, it can be accesed from plutus contract, and when referenceInput contains script, the script need not be provided in the input. When referenceScript is provided in the inputs, the utxo will be automatically added to referenceInputs

 -`Txid#index` \"String" : Utxo to be used as reference input in the transaction.
 - _Utxo CborHex_ \"HexString": Serialized utxo in the format used by cip30 wallets to be added in referenceInputs

### 4. `outputs` : [Object] : Outputs created in  the transaction

##### Output object can have following common fields

- `address` \"String" [Optional if script is present] :  Receiver address. Receiver address may be script or public key address. Address may be in bech32 or cborHex format. If `address` is not provided and script is present, address is automatically calculated.

    ___e.g___: "addr_test1vzzc9nx2lxmu9r7gyd8cyd0dcx0ynh729rpv4c553exs7kgyu9cxl"

    ___e.g___: "6136e0cf1e52e05ef92e52c7bc2a04493d6bae481b8acbab12ec4300d7" 

- `value` [String | Integer] : \
Amount to be sent. In case of native tokens, they can be joined by `+` like in cardano-cli response\
    ___e.g___: _ada amount_ :  "3A", "3.2Ada", "3.3a", "3ada"\
    ___e.g___: _lovelace amount_ : 3000000 or "3000000"\
    ___e.g___: _with single Nft_ : "3a + b14005d41c24863c570edc85e180cde5eda45bff6c9117ea70856b04.546f6b656e2331"\
    ___e.g___: _with native token_ : "3000000 + 3 21666f85344ad0f92b47ad3b359d91edc369e51031cb80e649a43434d058bd6a.546f6b656e2332"

- `deductFee`  [_Optional_] :  If set to true, Fee will be deducted from this output.


- `addChange` [_Optional_] : If set to true, Change is added to this output. This can be used to make sure that at least `value` amount  is sent to this output.

- `insuffientUtxoAda` [_String_] [_Optional_] : Specify action to perform when output doesn't have min output ada required, as per the protocol parameters. Possible values are
   - "`error`" [Default] : throw error when utxo doesn't have sufficient ada.
   - "`increase`" : Increase the ada amount in output to meet the minAda requirement
   - "`drop`" : Don't include the output in transaction if it doesn't meet the minAda requirement
                       

##### Following fields can be present **if the Output address is scriptAddress**

- `script` |  : [Object] : Serialized script wrapped in text evelope. If address is not provided, it is used to compute the outputAddress.
    {
        "type": "String" Script type [ "PlutuScrpitV1" | "PlutusScriptV2" ]
        "description": "String"  Description for the script. Can be set to empty
        "cborHex": "String" Scrialized cbor hex representation of the script.
    }
 - `inlineScript` : [Object] : Script to be inlined in the utxo. The output utxo can be used as referenceScript in future transactions.

- `datumHash` \"String" \[Optional when is provided]  : DatumHash in cbor format.

- `datum` \"String" \[Optional]  : Datum of the script output. The datum is by default inlined, It can also be used to calculate datumHash only
- `inlineDatum` Boolean [Default : True] : when datum is provided, instead of datumHash, but it should not be inlined, then `inlineDatum: false` can be set. 

### 5 `collaterals` "String" [Optional] : Collateral inputs in the transaction.
Collaterals are selected automatically by Kuber using one of the utxos in the `selections` .
If required, collaterals list can be set explicitly .

Each Item in the list can be in one of the following form.

- _TxIn Object_ : TxIn in object format.  

      {
          "hash" | "txid" | "txId" : "String" transaction hash,
          "index"                  : [integer] transaction output index
      }

  ___eg___:&#160;{&#160;"hash":&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8",&#160;"index":&#160;1}
           

- _TxIn_ [ TxHash#index ] :  TxIn in readable format.

  ___eg___:&#160;"3500e13b06e8b5c628064cba7bb4637520d2b59acfeee216961362b3919e1ca8#1"        c


- _Address bech32_
  Wallet address in bech32 format. when address is used, all the utxos int the address will be used as an input and spent in the transaction.

  ___eg___:&#160;"addr_test1vzzc9nx2lxmu9r7gyd8cyd0dcx0ynh729rpv4c553exs7kgyu9cxl"
                        

- _Utxo CborHex_
   Utxo( transaction output ) in cbor format. Utxo cbor contains output address, value .

   ___eg___:&#160;"828258202ff238f64b773435d6f626aafe56073f251b52281c50a3872951905fbc597e560082583901ab1d01c80b7ef656194c4af4a682f2d55d714379bde1afe72dc5d348f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b1a0037e4d2"
       

### 6 mint : [Object] : Minting script and amount in the transaction
Each object in the mint list must have following keys

- `amount` {Object} : in amount object, keys represent the tokenName in hexFromat and value is the amount of that token to be minted. 

    e.g. `{ "546f6b656e2331": 1, "546f6b656e2332": 1}`

#### For Plutus script
- `script` {Object} :  Serialized script wrapped in text evelope. `script` object must have following fields

      {
           "type": "String" Script type [ "PlutuScrpitV1" | "PlutusScriptV2" ]
           "cborHex": "String" Scrialized cbor hex representation of the script.
      }
 
- `executionUnits` {Object} [Optional] : Specify Execution units values. If not provided, Kuber automatically calculates the executionunits and include it in the transaction

      {
        "steps": (integer) Execution units steps
        "memory": (integer) Execution units memory
      }

#### For Simple script 
 - `script` {Object} : Simple script json format object

        {
            "type": "String" Script type ["all" | "any"| "atleast" | "sig" | "after" | "before" ]
            "..." : Other keys are based on the type of simple script.
        },

    **BasicScripts**

        SignatureScript : {
            "type": "sig" ,
            "keyHash": "String" PublicKeyHash hex string. This public key can only mint the token
        }

        TimeBeforeScript : {
            "type" : "before"
            "slot" : (number) the slot no  before which this script cannot be minted.  
        }

        TimeAfterScript : {
            "type" : "after"
            "slot" : (number) the slot no after which this script cannot be minted.  
        }

    **MultiScripts:**

        AnyScript{
            "type": "any",
            "scripts" : [ BasicScript | MultiScript ] : If any one of these script condition is met, token can be minted
        }
        AllScript{
            "type": "all"
            "scripts": [ BasicScript | MultiScript ] : If condition of all of these scripts is 
        }
        AnyMScript{
            "type": "atLeast"
            "required": (number) minimun no of scripts for which condition should be met. 
            "scripts": [ BasicScript | MultiScript ] : when required number of script condition is met, token can be minted.
        }


### 7. metadata : Object : Transaction Metadata
Transaction metadata must be a json object with top level integer key label.

Keys in the json shouldn't be longer than 64 bytes length. If the string value in the metadata is longer than 64 bytes length, Kuber will split the string and replace it with array of smaller chunks of the string.

Metadata object example:
```json
{
    "420": "content here",
    "421": {
        "key": "value",
        "key": ["value1","value2"]
    }
}
```
