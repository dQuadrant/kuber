# submitTx

`submitTx` is an asynchronous function that submits a signed transaction (in CBOR format) to the Cardano blockchain via the Kuber API. This function is typically used after a transaction has been built and signed.

## Function Signature

```typescript
async submitTx(cborString: HexString): Promise<CommonTxObject>
```

## Parameters

- `cborString`: A `HexString` representing the CBOR-encoded, signed transaction.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the submitted transaction.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  // This is a placeholder. In a real application, you would build and sign a transaction
  // to get a valid CBOR string.
  const signedCborTx = "840081825820..."; // Replace with your signed CBOR transaction string

  try {
    console.log("Submitting transaction...");
    const result = await hydra.submitTx(signedCborTx);
    console.log("Transaction submitted. Hash:", result.hash);
    console.log("CBOR Hex:", result.cborHex);
  } catch (error) {
    console.error("Error submitting transaction:", error);
  }
}

main();
