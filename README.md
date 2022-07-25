Kuber
===========

Haskell library and API server for composing balanced cardano transaction

You can test kuber on cardano testnet via this playground interface :  [Kuber-Playground](https://dquadrant.github.io/kuber/)

## Docs
 - [Json ApiReference](./docs/js3on-api-reference.md)
 - [Examples](./docs/)


# Example (JSON API)
 
To pay ada to address, you can use following transaction in playground interface. 

Remove the `selections` key and using `Add Wallet UTxOs in selection` option for simplicity

```
{
    "selections":["<YOUR_WALLET_ADDRESS>"],
    "outputs": [
        {
            "address": "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj",
            "value": "10A"
        },
        {
            "address": "addr1q8ht7426x486kve8pahevnvzn0r0mc6k63qjxx87l0kw748ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsklq4x3",
            "value": "20A"
        }
    ]
}


```
# Building locally
Kuber uses  `cardano-api` library. The system packages and dependencies required for building this project is same as that of `cardano-node`

To prepare your system for building kuber from sources, you can follow these instructions:

    The steps can be summarized as 
    - install system dependencies for ghc and cardano-node
    - install ghc compiler and tools with ghcup
    - install iokhk patch of libsodium on the system

The steps are described in detailed in the documentation of [building-cardano-node-from-soruces](https://developers.cardano.org/docs/get-started/installing-cardano-node/)

 Once everything is installed and is ready, kuber is ready to run
 ```
cabal update
CARDANO_NODE_SOCKET_PATH=/home/user/.cardano/testnet/node.socket cabal run exe:kuber
 ```

 This will start kuber on port `8081`.