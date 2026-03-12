Kuber
===========

Haskell library and API server to query Crdano Node, Transaction balancing with deterministic Utxo selection

## Key Features

- **Chain Queries** — UTxO filtering by address/TxId, protocol & genesis parameters, time/slot conversion
- **Transaction Composition** — JSON-driven builder with automatic fee balancing, multi-sig, Plutus scripts, collateral management, native tokens & NFTs
- **Transaction Submit** — Cardano submit api compatible endpoint `POST /api/submit/tx` 
- **Layer 2 Integration** — Full Hydra lifecycle (init, commit, decommit, close, contest, fanout) with simultaneous L1/L2 operations
- **Developer Experience** — Unified REST & WebSocket API via [kuber-client](https://www.npmjs.com/package/kuber-client/v/3.4.0), type-safe Haskell library, interactive IDE with Plutus compilation

## Getting Started
 Kuber client provides same interface for cardano network and layer2 hydra network.
 - [Kuber API Reference](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/kuber-api-reference).
 - [Transaction Bulder and Ide reference](https://kuberide.com/kuber/docs/intro)
 - [kuber-client library](https://www.npmjs.com/package/kuber-client/v/3.4.0)


### Run Kuber-Server with docker-compose

Kuber can be started easily with the [docker-compose.yml](./docker-compose.yml) file. The official Docker images are available on GitHub Container Registry (GHCR): `ghcr.io/dquadrant/kuber`.

Note: You will have to wait for the cardano-node to sync to the latest block.

```bash
git clone https://github.com/dquadrant/kuber.git
# Use the latest stable version
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
