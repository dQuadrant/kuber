# Local devnet

This guide walks through the steps to set up a devnet and run a Hydra Head using the `kuber` relay server. The devnet runs as a single-node Cardano cluster and is ideal for local testing with three Hydra nodes: **Alice**, **Bob**, and **Carol**.

## Devnet setup

Here we prepare the devnet configuration for bootstrapping a local Cardano node. This is the simplified variant of the Cardano node that doesn't require any stake pools.

:::note
If you don't want to use a devnet and want to set up a testnet/mainnet, follow this link -> [Testnet/Mainnet](./testnet_or_mainnet.md)
:::

### Quickstart (cluster reset script)
```bash
git clone git@github.com:dQuadrant/kuber.git
cd kuber-hydra/devnet
./reset-cluster.sh # creates fresh cluster
docker compose ps
```

The `reset-cluster.sh` script creates a new cluster and resets the cluster to a fresh state if one is already running. You can now [start developing](./installation.md).

### Explanation (step-by-step)

#### 1. Prepare cluster and start node
**Navigate to the `kuber-hydra/devnet` directory:**

```bash
cd kuber-hydra/devnet
```

Run the devnet setup script:

```bash
bash setup-devnet.sh
```

`setup-devnet.sh` cleans the runtime directory and puts the current time in `genesis-shelley` and `genesis-byron`.

Start the Cardano node:

```bash
docker compose up -d cardano-node
```

Verify that the cardano-node is running:

```bash
docker compose logs cardano-node -f
```

Generate credentials for Alice, Bob, and Carol (Hydra and Cardano keys):

```bash
bash generate-credentials.sh
```

Fund Alice, Bob, and Carol with UTxOs for commits and fees:

```bash
bash seed-devnet.sh
```

This uses the `cardano-cli` inside the running cardano-node container and also produces protocol parameters and publishes reference scripts.

#### 2. Start Hydra nodes

```bash
docker compose up -d hydra-node-{1,2,3}
```

Verify the nodes are listening:

```bash
docker ps
docker compose logs hydra-node-1
```

## Kuber-Hydra relay

After hydra nodes and the cardano node are up and running, start kuber-hydra.

### With Docker

Start the services:

```bash
docker compose up -d kuber-hydra-{1,2,3}
```

Verify services are running:

```bash
docker compose ps
```

Access the Kuber-Hydra Relay API:

- `http://localhost:8082` (alice)
- `http://localhost:8083` (bob)
- `http://localhost:8084` (carol)

Example test endpoint: `http://localhost:8082/hydra/query/head`.

For full API references follow this link: [kuber-hydra-apis](/docs/kuber-hydra-api-reference.md)

### With cabal

To run `kuber-hydra` with cabal, set the required environment variables and run the application:

```bash
export CARDANO_NODE_SOCKET_PATH=/path/to/cardano-node/preview/node.socket
export NETWORK=preview
cd kuber-hydra
cabal run kuber-hydra -- --hydra-url ws://172.16.238.10:4001 --port 8081
```

- `CARDANO_NODE_SOCKET_PATH`: Path to your Cardano node socket.
- `NETWORK`: The Cardano network ID (for devnet, `42` or a `network_magic` number). This is required.
- `--hydra-url`: WebSocket URL of your Hydra node. This is required.
- `--port`: Port for the Kuber-Hydra relay server (defaults to `8081`).
