# Getting Started

This tutorial walks through the steps to initialize and interact with a Hydra Head using the `kuber-hydra` relay server and client libraries. We’ll demonstrate the process using two Hydra nodes—**Alice** and **Bob**—on the Cardano testnet.

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

Create a `.env` file with the following variables (example for Alice):

```env
HYDRA_IP=172.16.238.10
HYDRA_PORT=4001
SERVER_PORT=8081
CARDANO_NODE_SOCKET_PATH=/path/to/cardano-node/preview/node.socket
NETWORK=2
```

> The Kuber-Hydra relay API will be accessible at `http://localhost:8081`.

## **3. Kuber Client**

Example repository: [kuber-client-example](https://github.com/cardanoapi/kuber-client-example)

### Hydra Service Initialization

Assuming that the hydra node is running and kuber-hdra server is started on localhost:8081, we can pass the host url to this class constructor to create the service:

```ts
import { KuberHydraService } from "kuber-client/service/kuberHydraService";

const hydraService = new KuberHydraService("http://localhost:8081");
```
