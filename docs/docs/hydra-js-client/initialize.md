# initialize

`initialize` is an asynchronous function that initiates a Hydra head. This is the first step to open a new Hydra head for off-chain transactions.

## Function Signature

```typescript
async initialize(wait: boolean = false): Promise<any>
```

## Parameters

- `wait`: An optional `boolean` indicating whether to wait for the head to be initialized. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the initialization process.

## Example

```javascript
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  try {
    console.log("Initializing Hydra head...");
    await hydra.initialize(true); // Wait for initialization to complete
    const headState = await hydra.queryHeadState();
    console.log("Hydra head state:", headState.state);
  } catch (error) {
    console.error("Error initializing Hydra head:", error);
  }
}

main();
```
