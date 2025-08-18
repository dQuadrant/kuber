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
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  try {
    console.log("Initializing Hydra head...");
    const result = await hydra.initialize(true); // Wait for initialization to complete
    console.log("Hydra head initialized:", result);
  } catch (error) {
    console.error("Error initializing Hydra head:", error);
  }
}

main();
```
