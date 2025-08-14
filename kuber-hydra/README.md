# Kuber Hydra

- [Project Scope and Planning](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/milestones)
- [Architecture](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/architecture)
- [Getting Started with Kuber-Hydra](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/getting-started)


### Tests:
 - [✅ Passing](https://dquadrant.github.io/kuber/test-reports/hydra/) on v4.0.0

## Running Kuber-Hydra

### With cabal

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

### With Docker

For a quick setup, you can use the provided `docker-compose.yml` to run `kuber-hydra` along with a Cardano node and Hydra node.

1.  **Navigate to the `kuber-hydra` directory:**
    ```bash
    cd kuber-hydra
    ```
2.  **Start the services:**
    Update the hydra-node configuration in docker-compose.yml and run the stack.
    ```bash
    docker-compose up -d
    ```
    This will start `cardano-node`, `hydra-node`, and `kuber-hydra` in detached mode.

3.  **Verify services are running:**
    ```bash
    docker-compose ps
    ```
    Ensure all services are up and healthy.

4.  **Access the Kuber-Hydra Relay API:**
    The API will be accessible at `http://localhost:8081`.
