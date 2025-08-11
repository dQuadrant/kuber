---
sidebar_position: 3
sidebar_label: Building and Submitting TXs
---

#  Building and Submitting TXs

This guide demonstrates how to build and submit transactions within an open Hydra Head using the cip30 interface and `KuberHydraApiProvider`.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed.
- An active Hydra Head (in "Open" state).
- A configured `KuberHydraApiProvider` instance.

This example demonstrates how to build and submit a simple transaction (sending Lovelace to an address) within an open Hydra Head using the `KuberHydraApiProvider` and a CIP-30 compatible wallet.

```typescript
import { KuberHydraApiProvider } from "kuber-client"; // Adjust path as needed
import { Value,Ed25519Key ,loadCrypto } from "libcardano";
import { ShelleyWallet, Cip30ShelleyWallet } from "libcardano-wallet";
import { readFileSync } from "fs";

async function runBuildAndSubmitTransactionExample() {
  // Initialize Hydra API Provider (replace with your Hydra node URL)
  const hydra = new KuberHydraApiProvider("http://172.31.6.1:8082");

  // Load test wallet signing key
  const testWalletSigningKey = await Ed25519Key.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );

  // Setup libcardano crypto and Shelley wallet
  await loadCrypto();
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  const cip30Wallet = new Cip30ShelleyWallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();

  console.log("Wallet Address:", walletAddress);

  // Ensure head is in 'Open' state
  const headState = await hydra.queryHeadState();
  if (headState.state !== "Open") {
    console.log("Head is not in 'Open' state. Please ensure it's in 'Open' state before running this example.");
    return;
  }
  console.log("Hydra Head is Open. Proceeding with transaction.");

  // Define the transaction outputs
  const transaction = {
    outputs: [{ address: walletAddress, value: { lovelace: "3000000" } }], // Sending 3 ADA
    changeAddress: walletAddress,
  };

  try {
    // Use the buildAndSubmitWithWallet function from KuberProvider
    const submitResult = await hydra.buildAndSubmitWithWallet(cip30Wallet, transaction);
    console.log("Transaction submitted to Hydra Head. Result:", submitResult);    

  } catch (error: unknown) {
    if (error instanceof Error) {
      console.error("Error building or submitting transaction:", error.message);
    } else {
      console.error("An unknown error occurred:", error);
    }
  }
}

runBuildAndSubmitTransactionExample();
```
