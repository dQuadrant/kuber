---
sidebar_position: 5
sidebar_label: buildAndSignWithWallet
---

# buildAndSignWithWallet

`buildAndSignWithWallet` is an asynchronous function that builds a transaction, automatically adds UTxOs from the wallet, and then signs the transaction using the provided CIP-30 compatible wallet.

## Function Signature

```typescript
async buildAndSignWithWallet(
  cip30OrProvider: Cip30,
  buildRequest: Record<string, any>,
  autoAddCollateral = false,
  estimatedSpending?: number | bigint,
): Promise<SignTxResult>
```

## Parameters

- `cip30OrProvider`: A CIP-30 compatible wallet instance or provider.
- `buildRequest`: An object following Kuber's transaction builder JSON specification. For a comprehensive reference on transaction builder fields and their usage, please refer to the [KuberIDE TxBuilder Object Reference](https://kuberide.com/kuber/docs/tx-builder-reference).
- `autoAddCollateral`: An optional `boolean` indicating whether to automatically add collateral from the provider. Defaults to `false`.
- `estimatedSpending`: An optional `number` or `bigint` representing the estimated amount of Lovelace to be spent.

## Returns

A `Promise` that resolves to a `SignTxResult` object, containing the signed transaction.

## Example

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { readFileSync } from "fs";

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  // Load test wallet signing key
  const testWalletSigningKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );

  // Setup libcardano crypto and Shelley wallet
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  const cip30Wallet = new SimpleCip30Wallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();

  const transaction = {
    outputs: [{ address: walletAddress, value: "1_000_000" }], // Sending 1 ADA
    changeAddress: walletAddress,
  };

  try {
    console.log("Building and signing transaction with wallet...");
    const signedTx = await hydra.buildAndSignWithWallet(cip30Wallet, transaction);
    console.log("Signed transaction:", signedTx);
    console.log("Signed CBOR Hex:", signedTx.transaction.toBytes().toString('hex'));
  } catch (error) {
    console.error("Error building and signing transaction with wallet:", error);
  }
}

main();
