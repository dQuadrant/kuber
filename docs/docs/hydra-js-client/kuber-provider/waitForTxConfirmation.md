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
import { readFileSync } from "fs";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client";

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");
  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync("../../kuber-hydra/devnet/credentials/alice-funds.sk", "utf-8")),
  );
  const wallet = new SimpleCip30Wallet(hydra, hydra, new ShelleyWallet(signingKey), 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  try {
    const result = await hydra.buildAndSubmitWithWallet(wallet, {
      outputs: [{ address: walletAddress, value: "1_000_000" }],
      changeAddress: walletAddress,
    });
    const txHash = result.transaction.hash().toString("hex");

    console.log(`Waiting for transaction ${txHash} to be confirmed...`);
    const timeWaited = await hydra.waitForTxConfirmation(txHash, 120000, true);
    console.log(`Transaction confirmed after ${timeWaited} ms.`);
  } catch (error) {
    console.error("Error waiting for transaction confirmation:", error);
  }
}

main();
```
