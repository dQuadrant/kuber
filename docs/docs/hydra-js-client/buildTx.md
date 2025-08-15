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

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  const transaction = {
    outputs: [
      {
        address: "addr_test1qr...", // Recipient address
        value: {
          lovelace: "500000", // 0.5 ADA
        },
      },
    ],
    // Add other transaction parameters as needed, e.g., inputs, metadata
  };

  try {
    console.log("Building transaction...");
    const builtTx = await hydra.buildTx(transaction, true); // Submit the transaction
    console.log("Built transaction:", builtTx);
  } catch (error) {
    console.error("Error building transaction:", error);
  }
}

main();
