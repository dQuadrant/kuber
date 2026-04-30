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
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  try {
    const protocolParams = await hydra.queryProtocolParameters();
    console.table({
      protocolVersion: `${protocolParams.protocolVersion.major}.${protocolParams.protocolVersion.minor}`,
      txFeeFixed: protocolParams.txFeeFixed,
      txFeePerByte: protocolParams.txFeePerByte,
      maxTxSize: protocolParams.maxTxSize,
      maxValueSize: protocolParams.maxValueSize,
      maxCollateralInputs: protocolParams.maxCollateralInputs,
      utxoCostPerByte: protocolParams.utxoCostPerByte,
    });
  } catch (error) {
    console.error("Error querying protocol parameters:", error);
  }
}

main();
```
