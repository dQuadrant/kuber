# commit

`commit` is an asynchronous function that commits UTxOs to an initializing Hydra head. This action moves funds from the main chain into the Hydra head, making them available for off-chain transactions.

## Function Signature

```typescript
async commit(utxos: Commit, submit: boolean = false): Promise<CommonTxObject>
```

## Parameters

- `utxos`: A `Commit` object specifying the UTxOs to be committed.
- `submit`: An optional `boolean` indicating whether to submit the transaction to the chain. Defaults to `false`.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the commitment transaction.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  const utxosToCommit = {
    utxos: [
      {
        txIn: "yourTxHash#0", // Replace with a valid UTxO
        value: {
          lovelace: "1000000", // 1 ADA
        },
      },
    ],
  };

  try {
    console.log("Committing UTxOs...");
    const result = await hydra.commit(utxosToCommit, true); // Submit the transaction
    console.log("Commit transaction:", result);
  } catch (error) {
    console.error("Error committing UTxOs:", error);
  }
}

main();
