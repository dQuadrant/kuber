---
sidebar_position: 7
sidebar_label: waitForUtxoConsumption
---

# waitForUtxoConsumption

`waitForUtxoConsumption` waits for a specific transaction output (UTxO) to be consumed (spent) on the blockchain. It will not return if the `txHash#index` is available on chain, and perform background poll, unless it is is consumed. 

## Usage
- You build a transaction that consumes an input.
- You submit the transacition on-chain.
- Now now you `waitForUtxoConsumption` on the input that you spent.

## Function Signature

```typescript
async waitForUtxoConsumption(
  txin: TxInput,
  timeoutMs: number = 80000,
  logPoll: boolean = false,
  pollIntervalMs: number = 5000,
): Promise<number>
```

## Parameters

- `txin`: The `TxInput` object representing the UTxO to wait for.
- `timeoutMs`: An optional `number` representing the maximum time in milliseconds to wait. Defaults to `80000` (80 seconds).
- `logPoll`: An optional `boolean` indicating whether to log the polling status. Defaults to `false`.
- `pollIntervalMs`: An optional `number` representing the interval in milliseconds to poll for the UTxO. Defaults to `5000` (5 seconds).

## Returns

A `Promise` that resolves with the total time spent waiting in milliseconds when the UTxO vanishes.

## Example

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { loadCrypto, Ed25519Key, Value } from "libcardano";
import { ShelleyWallet, Cip30ShelleyWallet } from "libcardano-wallet";
import { readFileSync } from "fs";
import { TxInput } from "libcardano/cardano/serialization/txinout";

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  // Load test wallet signing key
  const testWalletSigningKey = await Ed25519Key.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );

  // Setup libcardano crypto and Shelley wallet
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  const cip30Wallet = new Cip30ShelleyWallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();

  console.log("Wallet Address:", walletAddress);

  // Query UTxOs from the wallet
  const utxos = await hydra.queryUTxOByAddress(walletAddress);
  if (utxos.length === 0) {
    console.log("No UTxOs found for the wallet address. Please ensure the wallet has funds.");
    return;
  }

  // Select a UTxO to spend
  const utxoToSpend = utxos[0];
  const txInputToWatch: TxInput = {
    txHash: utxoToSpend.txIn.txHash,
    index: utxoToSpend.txIn.index,
  };

  // Define a simple transaction to spend the UTxO
  const transaction = {
    inputs: [{ txIn: `${txInputToWatch.txHash.toString('hex')}#${txInputToWatch.index}` }],
    outputs: [{ address: walletAddress, value: "1_000_000" }], // Send 1 ADA back to self
  };

  try {
    console.log("Building, signing, and submitting transaction to consume UTxO...");
    const txHash = await hydra.buildAndSubmitWithWallet(cip30Wallet, transaction);
    console.log("Transaction submitted. Hash:", txHash);

    console.log(`Waiting for UTxO ${txInputToWatch.txHash.toString('hex')}#${txInputToWatch.index} to be consumed...`);
    const timeWaited = await hydra.waitForUtxoConsumption(txInputToWatch, 180000, true); // Wait up to 3 minutes, log progress
    console.log(`UTxO consumed after ${timeWaited} ms.`);
  } catch (error) {
    console.error("Error in transaction or waiting for UTxO consumption:", error);
  }
}

main();
