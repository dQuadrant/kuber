# fanout

`fanout` is an asynchronous function that initiates the fanout process for a closed Hydra head. This action distributes the final UTxOs from the Hydra head back to the main Cardano chain.

## Function Signature

```typescript
async fanout(wait: boolean = false): Promise<any>
```

## Parameters

- `wait`: An optional `boolean` indicating whether to wait for the fanout to complete. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the fanout process.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  try {
    console.log("Initiating fanout...");
    const result = await hydra.fanout(true); // Wait for fanout to complete
    console.log("Fanout completed:", result);
  } catch (error) {
    console.error("Error during fanout:", error);
  }
}

main();
```