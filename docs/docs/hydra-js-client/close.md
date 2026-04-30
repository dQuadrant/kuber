# close

`close` is an asynchronous function that closes an active Hydra head. This action finalizes the off-chain transactions and moves the head to a `Closed` state, eventually allowing funds to be fanned out to the main chain.

## Function Signature

```typescript
async close(wait: boolean = false): Promise<any>
```

## Parameters

- `wait`: An optional `boolean` indicating whether to wait for the head to be closed. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the close process.

## Example

```javascript
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  try {
    console.log("Closing Hydra head...");
    await hydra.close(true); // Wait for closure to complete
    const headState = await hydra.queryHeadState();
    console.log("Hydra head state:", headState.state);
  } catch (error) {
    console.error("Error closing Hydra head:", error);
  }
}

main();
```
