---
sidebar_position: 4
sidebar_label: buildWithWallet
---

# buildWithWallet

`buildWithWallet` is an asynchronous function that builds a transaction by automatically adding available UTxOs from the wallet to the available UTxO selection.

## Function Signature

```typescript
async buildWithWallet(
  cip30OrProvider: Cip30 | Cip30Provider,
  buildRequest: Record<string, any>,
  autoAddCollateral = false,
  estimatedSpending?: number | bigint,
): Promise<CommonTxObject>
```

## Parameters

- `cip30OrProvider`: A CIP-30 compatible wallet instance or provider.
- `buildRequest`: An object following Kuber's transaction builder JSON specification. For a comprehensive reference on transaction builder fields and their usage, please refer to the [KuberIDE TxBuilder Object Reference](https://kuberide.com/kuber/docs/tx-builder-reference).
- `autoAddCollateral`: An optional `boolean` indicating whether to automatically add collateral from the provider. Defaults to `false`.
- `estimatedSpending`: An optional `number` or `bigint` representing the estimated amount of Lovelace to be spent.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the built transaction.

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
    console.log("Building transaction with wallet...");
    const builtTx = await hydra.buildWithWallet(cip30Wallet, transaction);
    console.log("Built transaction:", builtTx);
  } catch (error) {
    console.error("Error building transaction with wallet:", error);
  }
}

main();
