# Copilot Instructions

## Big picture
- Kuber is a Haskell library + API server for composing balanced Cardano and Hydra transactions (see README.md).
- Core services: kuber-server (Cardano L1 JSON API) and kuber-hydra (Hydra relay + tx builder over WebSocket).
- Transaction flow: JSON TxBuilder input -> formatter/builder -> unsigned tx -> signing -> submit over WebSocket to Hydra node (docs/docs/architecture.md).

## Key components & boundaries
- Haskell code lives under src/ and kuber-server/, kuber-hydra/ (cabal projects: kuber.cabal, kuber-hydra/kuber-hydra.cabal).
- Hydra relay and devnet tooling live in kuber-hydra/devnet (shell scripts + docker compose).
- JS/TS client and tests live in kuber-client-js/; docs site lives in docs/ (Docusaurus).

## Developer workflows
- Build/run Haskell server locally (requires Cardano node deps):
  - cabal update
  - CARDANO_NODE_SOCKET_PATH=... NETWORK=preprod cabal run kuber-server (DEVELOPER.md)
- Hydra devnet bootstrap (from kuber-hydra/devnet):
  - bash setup-devnet.sh
  - docker compose up -d cardano-node
  - bash generate-credentials.sh
  - bash seed-devnet.sh
  - docker compose up -d hydra-node-{1,2,3}
- Run kuber-hydra (cabal):
  - export CARDANO_NODE_SOCKET_PATH=...; export NETWORK=preview
  - cabal run kuber-hydra -- --hydra-url ws://HOST:PORT --port 8081 (kuber-hydra/README.md)
- Docs site: yarn && yarn start (docs/README.md).

## API surface examples
- L1 JSON API endpoints include /api/v3/utxo, /api/v3/protocol-params, /api/v1/tx (docs/docs/kuber-api-reference.md).
- Hydra relay exposes REST endpoints like initializeHead/commit/decommit/close/contest/abort/fanout (docs/docs/architecture.md).

## Project-specific conventions
- Tx building is JSON-driven (TxBuilder) with automatic fee balancing; Hydra txs must be built using Hydra protocol parameters (docs/docs/architecture.md).
- WebSocket integration with Hydra nodes is central; keep relay -> WS connector -> Hydra node flow intact when changing request/response shapes.
