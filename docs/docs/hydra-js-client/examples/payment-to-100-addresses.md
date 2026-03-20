---
sidebar_position: 9
sidebar_label: Payment to 100+ Addresses
---

# Payment to 100+ New Addresses (Minimum 2 ADA Each)

This guide demonstrates how to build high-fanout payment transactions in Hydra where each new recipient gets at least 2 ADA.

## Goal

- Pay to more than 100 recipient addresses.
- Enforce at least `2_000_000` lovelace per output.
- Submit in safe chunks to avoid transaction size limits.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed
- An active Hydra Head in `Open` state
- Source wallet funded for total transfer + fees

## Example strategy

1. Build a recipient list (100+ addresses).
2. Split into chunks (example: 40 outputs per tx).
3. Submit each chunk sequentially.

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { readFileSync } from "fs";

const MIN_PER_OUTPUT = 2_000_000;
const CHUNK_SIZE = 40;

function chunk<T>(items: T[], size: number): T[][] {
  const chunks: T[][] = [];
  for (let i = 0; i < items.length; i += size) {
    chunks.push(items.slice(i, i + size));
  }
  return chunks;
}

async function runBulkPaymentExample() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(
      readFileSync(
        process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk",
        "utf-8",
      ),
    ),
  );

  const shelleyWallet = new ShelleyWallet(signingKey);
  const wallet = new SimpleCip30Wallet(hydra, hydra, shelleyWallet, 0);
  const changeAddress = (await wallet.getChangeAddress()).toBech32();

  const headState = await hydra.queryHeadState();
  if (headState.state !== "Open") {
    throw new Error(`Hydra head is ${headState.state}. Expected Open.`);
  }

  // Replace with real generated/imported recipient addresses.
  const recipients = Array.from({ length: 105 }).map(
    (_, i) => `addr_test1...recipient_${i}`,
  );

  const batches = chunk(recipients, CHUNK_SIZE);

  for (let batchIndex = 0; batchIndex < batches.length; batchIndex += 1) {
    const outputs = batches[batchIndex].map((address) => ({
      address,
      value: String(MIN_PER_OUTPUT),
    }));

    const txBuilder = {
      outputs,
      changeAddress,
    };

    const txHash = await hydra.buildAndSubmitWithWallet(wallet, txBuilder);
    console.log(`Batch ${batchIndex + 1}/${batches.length} submitted:`, txHash);
  }
}

runBulkPaymentExample().catch((err) => {
  console.error("Bulk payment flow failed:", err);
});
```

## Validation checklist

1. All intended recipient addresses were included.
2. Every output value is at least 2 ADA.
3. All chunks were accepted by the Hydra head.
4. Source wallet balance decreased by expected total.
