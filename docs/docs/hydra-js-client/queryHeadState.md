# queryHeadState

`queryHeadState` is an asynchronous function that retrieves the current state of the Hydra head. This function is useful for monitoring the lifecycle of the head (e.g., `Idle`, `Initializing`, `Open`, `Closed`, `Fanout`).

## Function Signature

```typescript
async queryHeadState(): Promise<{ state: string }>
```

## Parameters

None.

## Returns

A `Promise` that resolves to an object with a `state` property, which is a string indicating the current state of the Hydra head.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  try {
    const headState = await hydra.queryHeadState();
    console.log("Current Head State:", headState.state);
  } catch (error) {
    console.error("Error querying head state:", error);
  }
}

main();
```