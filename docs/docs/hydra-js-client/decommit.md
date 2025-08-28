# decommit

`decommit` is an asynchronous function that decommits UTxOs from an active Hydra head. This action moves funds from the Hydra head back to the main chain.

## Function Signature

```typescript
async decommit(tx: HexString, wait: boolean = false): Promise<DecommitResult>
```

## Parameters

- `tx`: A `HexString` representing the CBOR-encoded transaction to be decommitted.
- `wait`: An optional `boolean` indicating whether to wait for the decommit to complete. Defaults to `false`.

## Returns

A `Promise` that resolves to a `DecommitResult` object containing information about the decommit process.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  // First, create the decommit transaction
  const utxoToDecommit = "yourTxHash#0"; // Replace with a valid UTxO from the head
  const decommitTx = await hydra.createDecommitTx(utxoToDecommit);

  // Then, sign the transaction (assuming you have a wallet setup)
  // const signedTx = await wallet.signTx(decommitTx.cborHex); // Replace 'wallet' with your actual wallet instance
  const signedTxCbor = decommitTx.cborHex; // For demonstration, using unsigned CBOR

  try {
    console.log("Decommitting UTxOs...");
    const result = await hydra.decommit(signedTxCbor, true); // Pass the signed transaction CBOR and wait
    console.log("Decommit result:", result);
    console.log("Decommit transaction hash:", result.decommitTx.txHash);
  } catch (error) {
    console.error("Error decommitting UTxOs:", error);
  }
}

main();
```
