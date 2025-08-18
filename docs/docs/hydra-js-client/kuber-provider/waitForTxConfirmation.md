---
sidebar_position: 8
sidebar_label: waitForTxConfirmation
---

# waitForTxConfirmation

`waitForTxConfirmation` is an asynchronous function that waits for a transaction to be confirmed on the blockchain by checking its UTxO status.

## Function Signature

```typescript
async waitForTxConfirmation(
  txHash: string,
  timeoutMs: number = 80000,
  logPoll: boolean = false,
  pollIntervalMs: number = 5000,
): Promise<number>
```

## Parameters

- `txHash`: A `string` representing the transaction hash (without the `#0`).
- `timeoutMs`: An optional `number` representing the maximum time in milliseconds to wait. Defaults to `80000` (80 seconds).
- `logPoll`: An optional `boolean` indicating whether to log the polling status. Defaults to `false`.
- `pollIntervalMs`: An optional `number` representing the polling interval in milliseconds. Defaults to `5000` (5 seconds).

## Returns

A `Promise` that resolves with the total time spent waiting in milliseconds if confirmed, or rejects on timeout.

## Example

```typescript
import { KuberHydraApiProvider } from "kuber-client";

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL
  const exampleTxHash = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2"; // Replace with a real transaction hash

  try {
    console.log(`Waiting for transaction ${exampleTxHash} to be confirmed...`);
    const timeWaited = await hydra.waitForTxConfirmation(exampleTxHash, 120000, true); // Wait up to 2 minutes, log progress
    console.log(`Transaction confirmed after ${timeWaited} ms.`);
  } catch (error) {
    console.error("Error waiting for transaction confirmation:", error);
  }
}

main();
