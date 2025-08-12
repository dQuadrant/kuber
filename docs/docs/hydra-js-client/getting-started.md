# Setting Up

This guide walks through the steps to set up and run a Hydra Head using the `kuber-hydra` relay server. We’ll demonstrate the process using two Hydra nodes—**Alice** and **Bob**—on the Cardano testnet.

## **1. Hydra Node Setup**

To set up a Hydra Head on the testnet, follow the official Hydra protocol tutorial:
👉 [Hydra Head Protocol Documentation](https://hydra.family/head-protocol/docs/tutorial)

In our example setup:

- **Alice's Hydra Node** runs on `172.16.238.10:4001`
- **Bob's Hydra Node** runs on `172.16.238.20:4002`

## **2. Kuber-Hydra Relay Server**

### **Repository**

- GitHub: [kuber](https://github.com/dquadrant/kuber)

### **Configuration**

You can run `kuber-hydra` either directly or using Docker.

#### **With cabal**

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

> The Kuber-Hydra relay API will be accessible at `http://localhost:8081`.

#### **With Docker**

For a quick setup, you can use the provided `docker-compose.yml` to run `kuber-hydra` along with a Cardano node and Hydra node.

1.  **Navigate to the `kuber-hydra` directory:**
    ```bash
    cd kuber-hydra
    ```
2.  **Start the services:**
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

## **3. Kuber Client**

Example repository: [kuber-client-example](https://github.com/sireto/kuber-client-js/tree/master/hydra-example)

Test file: [hydra.test.ts](https://github.com/sireto/kuber-client-js/blob/master/__tests__/hydra.test.ts)

### Hydra Service Initialization

Assuming that the hydra node is running and kuber-hdra server is started on localhost:8081, we can pass the host url to this class constructor to create the service:

```ts
import { KuberHydraService } from "kuber-client";

const hydraService = new KuberHydraService("http://localhost:8081");
