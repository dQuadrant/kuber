---
sidebar_position: 1
sidebar_label: Working with Wallets
---

# Working with Wallets

This guide demonstrates how to interact with wallets to query UTxOs from both the Layer 1 (L1) Cardano chain and the Hydra Head using the `KuberHydraApiProvider`. We will use same cip30 interface  for htis example

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed.
- Access to a running Hydra node and its credentials (e.g., `node.addr`, `funds.sk`).
- An active Hydra Head (can be in any state, but "Open" is ideal for demonstrating Hydra UTxOs).

## Example: Querying UTxOs from L1 and Hydra

This example sets up a wallet and then queries its UTxOs on both the L1 chain and within the Hydra Head.

```typescript
import { readFileSync } from "fs";
import { loadCrypto, Ed25519Key } from "libcardano";
import { ShelleyWallet, Cip30ShelleyWallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client"; // Adjust path as needed

async function runWalletQueryExample() {
  // Initialize Hydra API Provider
  const hydra = new KuberHydraApiProvider("http://172.31.6.1:8082"); // Replace with your Hydra node URL

  // Load test wallet signing key
  const testWalletSigningKey = await Ed25519Key.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );

  // Setup libcardano crypto and Shelley wallet
  await loadCrypto();
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  console.log("Base Shelley Wallet:", shelleyWallet.toJSON());

  // Create a wallet instance for Hydra operations
  const hydraWallet = new Cip30ShelleyWallet(hydra, hydra, shelleyWallet, 0);
  const hydraWalletAddress = (await hydraWallet.getChangeAddress()).toBech32();
  console.log("Hydra Wallet Address:", hydraWalletAddress);

  // Create a wallet instance for Layer 1 (L1) operations
  // Pass hydra.l1Api as both L1 and Hydra API provider for this wallet,
  // as it only interacts with L1.
  const layer1Wallet = new Cip30ShelleyWallet(hydra.l1Api, hydra.l1Api, shelleyWallet, 0);
  const layer1WalletAddress = (await layer1Wallet.getChangeAddress()).toBech32();
  console.log("Layer 1 Wallet Address:", layer1WalletAddress);



  console.log("\n--- Querying Balance from Layer 1 (L1) Chain using layer1Wallet ---");
  // Use the layer1Wallet's getBalance() method (CIP-30)
  const l1Balance = await layer1Wallet.getBalance();
  console.log(`L1 Balance for address ${layer1WalletAddress}: ${l1Balance.lovelace} lovelace`);

  console.log("\n--- Querying Balance from Hydra Head using hydraWallet ---");
  // Use the hydraWallet's getBalance() method (CIP-30)
  const hydraBalance = await hydraWallet.getBalance();
  console.log(`Hydra Head Balance for address ${hydraWalletAddress}: ${hydraBalance.lovelace} lovelace`);

}

runWalletQueryExample();
```

This example demonstrates how to use the `KuberHydraApiProvider` to query the balance of wallets from both the underlying L1 chain (via `hydra.l1Api`) and directly from the Hydra Head. This is crucial for understanding the state of funds available to your wallet in both environments.
