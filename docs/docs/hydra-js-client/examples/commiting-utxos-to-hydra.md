---
sidebar_position: 2
sidebar_label: Commiting - Using cip-30
---

# Using CIP-30 Interface

The Kuber Hydra Client can be integrated with a CIP-30 compatible wallet to sign transactions. This guide demonstrates how to set up and use a `SimpleCip30Wallet` with the `KuberHydraApiProvider`.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed.
- Access to a running Hydra node and its credentials (e.g., `node.addr`, `funds.sk`).

This example demonstrates how to set up a `SimpleCip30Wallet` and use it to sign and submit a transaction to a Hydra Head.

```typescript
import { readFileSync } from "fs";
import { CardanoKeyAsync, Value } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client"; // Adjust path as needed
import { UTxO } from "libcardano/serialization";

async function runCip30CommitExample() {
  // Initialize Hydra API Provider
  const hydra = new KuberHydraApiProvider("http://172.31.6.1:8082"); // Replace with your Hydra node URL

  // Load node address and test wallet signing key
  const node_addr_path = process.env.HOME + "/.cardano/preview/hydra-0/credentials/node.addr";
  const nodeAddr = readFileSync(node_addr_path).toString("utf-8").trim();

  // Setup Shelley wallet
  const testWalletSigningKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  console.log("Wallet", shelleyWallet.toJSON());

  // Create SimpleCip30Wallet instance
  // The first two arguments are for the L1 API provider and the Hydra API provider, respectively.
  // In this case, KuberHydraApiProvider implements both interfaces.
  const cip30Wallet = new SimpleCip30Wallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();

  console.log("Wallet Address:", walletAddress);

  // Ensure head is in 'Initial' state before committing
  const headState = await hydra.queryHeadState();
  if (headState.state !== "Initial") {
    console.log("Head is not in 'Initial' state. Skipping commit example.");
    return;
  }

  // Query UTxOs from the L1 chain using the wallet address
  const l1Utxos = await hydra.l1Api.queryUTxOByAddress(walletAddress);
  if (l1Utxos.length === 0) {
    throw new Error(`No balance on ${walletAddress} in L1 chain`);
  }

  // Select UTxOs to commit (e.g., the first one with a value greater than 4 ADA)
  const selectedUtxos = l1Utxos.filter((x: UTxO) => x.txOut.value.greaterThan(Value.fromString("4A")));
  if (selectedUtxos.length === 0) {
    throw new Error(`Not enough balance on ${walletAddress} in L1 chain for commit example`);
  }

  const txIn = selectedUtxos[0].txIn;
  const utxoToCommit = [`${txIn.txHash.toString("hex")}#${txIn.index}`];

  // Build the commit transaction using Hydra API
  const commitResult = await hydra.commit({ utxos: utxoToCommit });
  console.log("Transaction to be signed:", commitResult.hash);

  // Sign the transaction using the CIP-30 wallet
  const signResult = await cip30Wallet.signTx(commitResult.cborHex);

  // Submit the signed transaction to the L1 chain
  await hydra.l1Api.submitTx(signResult.transaction.toBytes().toString("hex"));
  console.log("Submitted Commit transaction hash:", commitResult.hash);

  // Wait for the transaction to be confirmed and head state to change
  await hydra.l1Api.waitForUtxoConsumption(selectedUtxos[0].txIn, 280000);
  console.log("Commit transaction confirmed.");

}

runCip30CommitExample();
```
