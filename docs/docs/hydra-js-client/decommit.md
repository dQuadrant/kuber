# decommit

`decommit` is an asynchronous function that decommits UTxOs from an active Hydra head. This action moves funds from the Hydra head back to the main chain.

## Function Signature

```typescript
async decommit(utxos: Commit, wait: boolean = false, submit: boolean = false): Promise<any>
```

## Parameters

- `utxos`: A `Commit` object specifying the UTxOs to be decommitted.
- `wait`: An optional `boolean` indicating whether to wait for the decommit to complete. Defaults to `false`.
- `submit`: An optional `boolean` indicating whether to submit the transaction to the chain. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the decommit process.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  const utxosToDecommit = {
    utxos: [
      {
        txIn: "yourTxHash#0", // Replace with a valid UTxO from the head
        value: {
          lovelace: "500000", // 0.5 ADA
        },
      },
    ],
  };

  try {
    console.log("Decommitting UTxOs...");
    const result = await hydra.decommit(utxosToDecommit, true, true); // Wait and submit
    console.log("Decommit result:", result);
  } catch (error) {
    console.error("Error decommitting UTxOs:", error);
  }
}

main();
```
