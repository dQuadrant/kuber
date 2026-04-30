# abort

`abort` is an asynchronous function that aborts the initialization of a Hydra head. This action can be used if the head initialization process is stuck or needs to be cancelled before it reaches the `Open` state.

## Function Signature

```typescript
async abort(wait: boolean = false): Promise<any>;
```

## Parameters

- `wait`: An optional `boolean` indicating whether to wait for the abort to complete. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the abort process.

## Example

```javascript
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  try {
    console.log("Aborting Hydra head initialization...");
    await hydra.abort(true); // Wait for abort to complete
    const headState = await hydra.queryHeadState();
    console.log("Hydra head state:", headState.state);
  } catch (error) {
    console.error("Error aborting Hydra head:", error);
  }
}

main();
```
