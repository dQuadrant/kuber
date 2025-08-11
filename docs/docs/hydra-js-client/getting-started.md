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

#### **Option 1: Running Natively**

To run `kuber-hydra` natively, you need to set the required environment variables and then run the application using `cabal`.

1.  **Set Environment Variables (Required):**
    Before running, you must set the following environment variables:
    ```bash
    export CARDANO_NODE_SOCKET_PATH=/path/to/cardano-node/preview/node.socket
    export NETWORK=2
    ```
    -   `CARDANO_NODE_SOCKET_PATH`: The path to your Cardano node socket.
    -   `NETWORK`: The Cardano network ID (e.g., `2` for Preview testnet). This is a **required** environment variable.

2.  **Run `kuber-hydra`:**
    Navigate to the `kuber-hydra` directory and run the application using `cabal`:
    ```bash
    cd kuber-hydra
    cabal run kuber-hydra -- --hydra-url ws://172.16.238.10:4001 --port 8081
    ```
    -   `--hydra-url`: The WebSocket URL of your Hydra node. This is a **required** command-line argument.
    -   `--port`: The port for the Kuber-Hydra relay server. If not specified, it defaults to `8081`.

> The Kuber-Hydra relay API will be accessible at `http://localhost:8081`.

#### **Option 2: Running with Docker (Recommended for quick setup)**

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

Example repository: [kuber-client-example](https://github.com/cardanoapi/kuber-client-example)

### Hydra Service Initialization

Assuming that the hydra node is running and kuber-hdra server is started on localhost:8081, we can pass the host url to this class constructor to create the service:

```ts
import { KuberHydraService } from "kuber-client/service/kuberHydraService";

const hydraService = new KuberHydraService("http://localhost:8081");
