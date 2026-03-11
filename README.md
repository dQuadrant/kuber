Kuber
===========

Haskell library and API server to query Crdano Node, Transaction balancing with deterministic Utxo selection

## Key Features

- **Transaction Composition**
  - JSON-driven transaction builder with automatic fee balancing
  - Support for complex transaction scenarios including multi-input, multi-output, and multi-signature transactions
  - Plutus script integration with collateral management
  - Support for native tokens and NFTs
  - Integrated datum and redeemer handling
- **Chain Queries**
  - UTXO queries and filtering by address, TxId.
  - Protocol parameter queries and network detection
  - Time/Slot query and conversion Apis

- **Layer 2 Integration**
  - Full Hydra transaction building and lifecycle support
  - Simultaneous L1 and L2 operations through unified interface
  - Head initialization, commits, decommits, and state close operations
  - Contest and fanout mechanisms for security
- **Developer Experience**
  - Unified API for both Layer 1 (Cardano) and Layer 2 (Hydra) operations
  - REST and WebSocket endpoints for flexibility
  - Type-safe Haskell library for complex applications
  - Interactive IDE with Plutus compilation support

## Getting Started
 Kuber client provides same interface for cardano network and layer2 hydra network.
 - [Kuber API Reference](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/kuber-api-reference).
 - [Transaction Bulder and Ide reference](https://kuberide.com/kuber/docs/intro)
 - [kuber-client library](https://www.npmjs.com/package/kuber-client/v/3.4.0)


### Run Kuber-Server with docker-compose

Kuber can be stared easily with [docker-compose.yml](./docker-compose.yml) file. But you will have to wait for cardano-node to sync to latest block

```bash
git clone https://github.com/dquadrant/kuber.git
git checkout v4.0.0
docker-compose up -d
```


If you want to build docker image locally, you can use the helper script
```bash
$ ./.ci/build
```

## Hydra
Kuber fully supports hydra transaction building and lifecycle. With `kuber-hydra`, you can interact simultaneously with the layer1 in addition to hydra.
 - Get Started: [Hydra docs](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/getting-started)
-  Source code: [kuber-hydra](./kuber-hydra)

## IDE
 Official IDE with Plutus compilation support is available at [kuberide.com/ide](https://kuberide.com/kuber/login/?state=\%2fide)



## Developer guide
- Example Project : [cardano-marketplace](https://github.com/dQuadrant/cardano-marketplace)
 - [Kuber Haskel Library](https://dquadrant.github.io/kuber/haskell_library/)


Instructions for local development of kuber is available in [DEVELOPER.md](DEVELOPER.md) 
