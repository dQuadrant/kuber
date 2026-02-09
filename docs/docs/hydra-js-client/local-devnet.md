# Local devnet

This guide walks through the steps to set up a devnet and run a Hydra Head using the `kuber` relay server. We'll demonstrate the process using three Hydra nodes - **Alice**, **Bob** and **Carol** - on the Cardano devnet.

## **1. Devnet Setup**

Here we prepare the devnet configuration for bootstrapping a local Cardano node. This is the simplified variant of the Cardano node that doesn't require any stake pools.

:::note
If you don't want to use a devnet and want to set up a testnet/mainnet, follow this link -> [Testnet/Mainnet](./testnet_or_mainnet.md)
:::

### All bash commands (quickstart)
```bash
cd kuber-hydra/devnet
bash setup-devnet.sh
docker compose up -d cardano-node
bash generate-credentials.sh
bash seed-devnet.sh
docker compose up -d
docker ps
```

Step-by-step implementation:
1. **Navigate to the `kuber-hydra/devnet` directory:**
   ```bash
   cd kuber-hydra/devnet
   ```
2. **Run setup-devnet script:**
   ```bash
   bash setup-devnet.sh
   ```
   This script cleans the runtime directory and puts the current time in genesis-shelley and genesis-byron.

3. **Starting the Cardano node:**
   ```bash
   docker compose up -d cardano-node
   ```
   Verify that the cardano-node is running:
   ```bash
   docker compose logs cardano-node -f
   ```

4. **Generate credentials for Alice, Bob and Carol:**
   ```bash
   bash generate-credentials.sh
   ```
   This script will generate all the required keys for each participant. See the credentials folder.

5. **Fund Alice, Bob and Carol:**
   ```bash
   bash seed-devnet.sh
   ```
   This script will fund all participants.

   alice         -> 30 ADA
   bob           -> 30 ADA
   carol         -> 30 ADA
   alice-funds   -> 100 ADA
   bob-funds     -> 50 ADA
   carol-funds   -> 25 ADA

   This script also produces protocol parameters and publishes reference scripts.

## **2. Hydra Node Setup**

To set up hydra nodes simply do:

```bash
docker compose up hydra-node-{1,2,3}
```

Verify hydra nodes are up and running:
```bash
docker ps | grep hydra-node
```

## **3. Kuber Relay Server**

### **Repository**

- GitHub: [kuber](https://github.com/dquadrant/kuber)

### **Configuration**
After the hydra node setup we now setup kuber server.

You can run `kuber` using Docker.

#### **With Docker**

For a quick setup, you can use the provided `docker-compose.yml` to run `kuber` along with a Cardano node and Hydra node.

1. **Navigate to the `kuber-hydra/devnet` directory:**
   ```bash
   cd kuber-hydra/devnet
   ```
2. **Start the services:**
   ```bash
   docker compose up -d kuber-hydra-{1,2,3}
   ```
   This will start `kuber-hydra` in detached mode.

3. **Verify services are running:**
   ```bash
   docker compose ps
   ```
   Ensure all services are up and healthy.

4. **Access the Kuber-Hydra Relay API:**
   The API will be accessible at:

- `http://localhost:8082` for alice
- `http://localhost:8083` for bob
- `http://localhost:8084` for carol

For full api references follow this link: [kuber-hydra-apis](/docs/kuber-hydra-api-reference.md)
