# buildTx

`buildTx` is an asynchronous function that constructs a transaction within the Hydra head. This function allows you to define outputs and other transaction parameters for off-chain transactions.

## Function Signature

```typescript
async buildTx(tx: any, submit: boolean = false): Promise<CommonTxObject>
```

## Parameters

- `tx`: An object representing the transaction to be built. This object should conform to the expected transaction structure for Hydra. For a comprehensive reference on transaction builder fields and their usage, please refer to the [KuberIDE TxBuilder Object Reference](https://kuberide.com/kuber/docs/tx-builder-reference).
- `submit`: An optional `boolean` indicating whether to submit the transaction to the head. Defaults to `false`.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the built transaction.

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

  const transaction = {
    selections: [walletAddress],
    outputs: [{ address: walletAddress, value: "1_000_000" }],
    changeAddress: walletAddress,
  };

  try {
    console.log("Building transaction...");
    const builtTx = await hydra.buildTx(transaction, true); // Submit the transaction
    console.log("Transaction hash:", builtTx.hash);
  } catch (error) {
    console.error("Error building transaction:", error);
  }
}

main();
```
