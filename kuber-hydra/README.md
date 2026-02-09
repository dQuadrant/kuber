# Kuber Hydra

Kuber greatly simplifis interaction with hydra. Transaction building Apis share same interface for both layer1 and layer2

- [Getting Started with Kuber-Hydra](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/getting-started)

## Tests:
 - [✅ Passing](https://dquadrant.github.io/kuber/test-reports/hydra/) on v4.0.0


## Quickstart with devnet

Here we prepare the devnet configuration for bootstrapping a local cardano node.
Devnet runs as a single node cluster and greatly simplifies local testing

### Use cluster setup script
```bash 
 cd devnet
 ./reset-cluster.sh   # creates new cluster, resets cluster to fresh state if already running
 docker compose ps
```
 You can now [start developing](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/installation)


### Explaination
#### 1. Prepare cluster and start node
**Navigate to the `kuber-hydra` directory:**

```bash
    cd kuber-hydra/devnet
```
After you are in /kuber-hydra/devnet

```bash
 bash setup-devnet.sh
```
setup-devnet.sh cleans the runtime directory and puts the current time in genesis-shelly and genesis-byron.

Next getting up the cardano node running.
```bash 
docker compose up -d cardano-node
```

To verify the cardano node is running 
```bash 
docker compose logs cardano-node -f
```

After that we need to generate the credentials for each participant.This contains hydras signing key , verification keys and cardano verification and signing keys.

```bash
bash generate-credentials.sh
```


Next we need to fund alice,bob,carol some utxos for committing and for paying fees.
```bash 
bash seed-devnet.sh
```
It used the cardano-cli that is within the already running cardano-node container.

### 2. Start Hydra Nodes

```bash
docker compose up -d hydra-node-{1,2,3}
```
See logs or docker ps for seeing if their ports are active and listening.
```bash
docker ps
docker compose logs hydra-node-1
```

### 3.Start Kuber-Hydra

### 3.1 With Docker

After hydra-nodes and cardano node are up and runnig next step is to start kuber-hydra.

  **Start the services:**
    ```bash
    docker-compose up -d kuber-hydra-{1,2,3}
    ```
  **Verify services are running:**
    ```bash
    docker-compose ps
    ```
    Ensure all services are up and healthy.

  **Access the Kuber-Hydra Relay API:**
    The API will be accessible at `http://localhost:8081`.
    Test : `http://localhost:8082/hydra/query/head` .


### 3.2 With cabal

To run `kuber-hydra` with cabal, you need to set the required environment variables and then run the application.

```bash
export CARDANO_NODE_SOCKET_PATH=/path/to/cardano-node/preview/node.socket
export NETWORK=preview
cd kuber-hydra
cabal run kuber-hydra -- --hydra-url ws://172.16.238.10:4001 --port 8081
```
-   `CARDANO_NODE_SOCKET_PATH`: The path to your Cardano node socket.
-   `NETWORK`: The Cardano network ID (e.g., `mainnet`, `preview`, `preprod`, or a `network_magic` number). This is a **required** environment variable.
-   `--hydra-url`: The WebSocket URL of your Hydra node. This is a **required** command-line argument.
-   `--port`: The port for the Kuber-Hydra relay server. If not specified, it defaults to `8081`.

