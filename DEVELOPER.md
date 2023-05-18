# Building locally
The system packages and dependencies required for building Kuber is the same as that of [cardano-node](https://github.com/input-output-hk/cardano-node).
The steps are described in detail in the documentation of [building-cardano-node-from-sources](https://developers.cardano.org/docs/get-started/installing-cardano-node/). 


In summary, to prepare your system for building kuber from sources, you have to:
- install system dependencies for ghc and cardano-node
- install ghc compiler and tools with ghcup
- install iokhk patch of libsodium on the system
- install lib-secp256 with extensions on the system
    
Once everything is installed and ready, kuber will also be ready to run
 ```
cabal update
CARDANO_NODE_SOCKET_PATH=/home/user/.cardano/preprod/node.socket NETWORK=preprod cabal run kuber-server
 ```

This will start kuber on port `8081`.
