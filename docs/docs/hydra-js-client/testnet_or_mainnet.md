# testnet/mainnet

This guide walks through the steps to set up cardano node and run a Hydra Head using the `kuber` relay server. 


## **Hydra Node Setup**

To set up a Hydra Head on the testnet, follow the official Hydra protocol tutorial:
👉 [Hydra Head Protocol Documentation](https://hydra.family/head-protocol/docs/tutorial)

In our example setup:

- **Alice's Hydra Node** runs on `172.16.238.10:4001`
- **Bob's Hydra Node** runs on `172.16.238.20:4002`
- **Carol's Hydra Node** runs on `172.16.238.30:4003`

## **kuber relay server Setup**
After the hydra clusters are setup we now setup kuber-hydra.


#### **With Docker**

Here with docker we run `kuber server` along with a Cardano node and Hydra node.

1.  **Start the services:**
 
        ```bash
        docker run -d --name kuber-hydra-1 --platform linux/amd64 \
        -p 8081:8081 \
        -v node-ipc:/ipc \
        -e HYDRA_URL=<your hydra node url> \
        -e CARDANO_NODE_SOCKET_PATH=<node socket path> \
        -e NETWORK=1 \
        --restart no \
        dquadrant/kuber-hydra:latest
        ```
        Follow the similar format for other hydra nodes.

2.  **Verify services are running:**
    ```bash
    docker ps
    ```
    Ensure all services are up and healthy.

3.  **Access the Kuber-Hydra Relay API:**
    The API will be accessible at `http://localhost:8081`.


#### **With cabal**

To run `kuber` with cabal, you need to set the required environment variables and then run the application.

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

> The Kuber relay API will be accessible at `http://localhost:8081`.



For full api references follow this link: [kuber-hydra-apis](/docs/kuber-hydra-api-reference.md)