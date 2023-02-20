Kuber
===========

Haskell library and API server for composing balanced Cardano transactions.

You can test Kuber on Cardano testnet via this playground interface :  [Kuber-Playground](https://dquadrant.github.io/kuber/). No installation required.



## Docs
 - [JSON API Reference](./docs/json-api-reference.md)
 - [Examples](./docs/)



# An example (JSON API)
 
To pay Ada to one or more addresses, you can use compose the following transaction in the [playground](https://dquadrant.github.io/kuber/) and execute them using Wallet extension like Nami and Eternl. The UTXOs will be selected using your browser wallet.

```JSON
{
    "outputs": [
        {
            "address": "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj",
            "value": "10A"
        },
        {
            "address": "addr_test1qpg8r3qvm7qm75p5zjq0c6hj5n2qfw3jp4ppluf7caygyfxv3a6l0p0x9q2e6hjeu62z0g5slxzr4hzjr9x3emnxd5qq65n7xz",
            "value": "20A"
        }
    ]
}
```

If you're using Kuber as an API, you'll need to add `selections` field to indicate the UTXOs to use while composing the transactions. See relevant examples [here](./docs/example-transfer.md).


For more examples, check [here](./docs/).


## Example Project using Kuber
[cardano-marketplace](https://github.com/dQuadrant/cardano-marketplace)

# Run Kuber server with docker-compose

Kuber can be stared easily with [docker-compose.yml](./docker-compose.yml) file. But you will have to wait for cardano-node to sync to latest block

```bash
git clone https://github.com/dquadrant/kuber.git
git checkout 2.3.1
docker-compose up -d
```


If you want to build docker image locally, you can use the helper script
```bash
$ ./.ci/build
```


# Building locally
The system packages and dependencies required for building Kuber is the same as that of [cardano-node](https://github.com/input-output-hk/cardano-node).
The steps are described in detail in the documentation of [building-cardano-node-from-sources](https://developers.cardano.org/docs/get-started/installing-cardano-node/). 


In summary, to prepare your system for building kuber from sources, you have to:
    - install system dependencies for ghc and cardano-node
    - install ghc compiler and tools with ghcup
    - install iokhk patch of libsodium on the system

Once everything is installed and ready, kuber will also be ready to run
 ```
cabal update
CARDANO_NODE_SOCKET_PATH=/home/user/.cardano/preprod/node.socket NETWORK=preprod cabal run exe:kuber
 ```

This will start kuber on port `8081`.
