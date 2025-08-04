# queryProtocolParameters

`queryProtocolParameters` is an asynchronous function that retrieves the current protocol parameters from the Hydra head. These parameters are essential for building and validating transactions on the Cardano blockchain.

## Function Signature

```typescript
async queryProtocolParameters(): Promise<CommonProtocolParameters>
```

## Parameters

None.

## Returns

A `Promise` that resolves to a `CommonProtocolParameters` object, containing various network parameters such as `minFeeA`, `minFeeB`, `maxTxSize`, etc.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  try {
    const protocolParams = await hydra.queryProtocolParameters();
    console.log("Protocol Parameters:", protocolParams);
  } catch (error) {
    console.error("Error querying protocol parameters:", error);
  }
}

main();
