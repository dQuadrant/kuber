## Transaction Metadata with kuber

### 1. Demo using metadata.
```json
{
    "selections":"addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
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
}
```
[View Resulting transaction](https://testnet.cardanoscan.io/transaction/4e2930a0433e3aeed51a21525f0f389620b51f71c1d3fdaa610ba8da083b6ea1)


### 2. Minting an NFT with metadata

```json
{
  "selections": ["addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj"],
  "mint": [
    {
      "script":{
        "type": "sig",
        "keyHash": "f735cdaa2ae360ce67e27cd2f6038a44c77bf3ede94bd144674d34e6"
      },
      "amount": {
        "test1": 1,
        "test2": 2
      }
    }
    ],
  "metadata": {
      "721": {
          "a6d263a3c017823752bcd42685c78fcb11f7a5759d9c956f51b64318": {
              "test1": {
                  "name": "Test1 NFT",
                  "image": "https://bit.ly/3xpjbJ9",
                  "artist": "Eternal",
                  "mediaType": "image/jpeg"
              }
          }
      },
      "20": {
         "a6d263a3c017823752bcd42685c78fcb11f7a5759d9c956f51b64318": {
              "test2": {
                  "name": "Test2 Fungible Token",
                  "image": "https://bit.ly/3xpjbJ9",
                  "artist": "Eternal",
                  "mediaType": "image/jpeg"
              }
              
        }
      }
  }
}
```

[View Resulting transaction](https://testnet.cardanoscan.io/transaction/5dff57ac386fcb45e5f3b884164b80d85ce196174755978a072a1966225d8b02)