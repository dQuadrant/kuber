---
sidebar_position: 6
sidebar_label: buildAndSubmitWithWallet
---

# buildAndSubmitWithWallet

`buildAndSubmitWithWallet` is an asynchronous function that automatically addes utxos from wallet to the transaction, signs, and submits the transaction to the chain using the provided CIP-30 compatible wallet.

## Function Signature

```typescript
async buildAndSubmitWithWallet(
  cip30OrProvider: Cip30 | Cip30Provider,
  buildRequest: Record<string, any>,
  autoAddCollateral = false,
  estimatedSpending?: number | bigint,
): Promise<HexString>
```

## Parameters

- `cip30OrProvider`: A CIP-30 compatible wallet instance or provider.
- `buildRequest`: An object following Kuber's transaction builder JSON specification. For a comprehensive reference on transaction builder fields and their usage, please refer to the [KuberIDE TxBuilder Object Reference](https://kuberide.com/kuber/docs/tx-builder-reference).
- `autoAddCollateral`: An optional `boolean` indicating whether to automatically add collateral from the provider. Defaults to `false`.
- `estimatedSpending`: An optional `number` or `bigint` representing the estimated amount of Lovelace to be spent.

## Returns

A `Promise` that resolves to a `HexString` representing the submitted transaction's hash.

## Example

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { loadCrypto, Ed25519Key } from "libcardano";
import { ShelleyWallet, Cip30ShelleyWallet } from "libcardano-wallet";
import { readFileSync } from "fs";

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

  const transaction = {
    outputs: [{ address: walletAddress, value: "1_000_000" }], // Sending 1 ADA
    changeAddress: walletAddress,
  };

  try {
    console.log("Building, signing, and submitting transaction with wallet...");
    const txHash = await hydra.buildAndSubmitWithWallet(cip30Wallet, transaction);
    console.log("Transaction submitted. Hash:", txHash);
  } catch (error) {
    console.error("Error building, signing, and submitting transaction with wallet:", error);
  }
}

main();
