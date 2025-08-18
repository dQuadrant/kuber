# contest

`contest` is an asynchronous function that allows a participant to contest a transaction in a Hydra head. This is typically used during the `Closed` state if there's a disagreement about the final state of the head.

## Function Signature

```typescript
async contest(wait: boolean = false): Promise<any>
```

## Parameters

- `wait`: An optional `boolean` indicating whether to wait for the contest to complete. Defaults to `false`.

## Returns

A `Promise` that resolves to an object containing information about the contest process.

## Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  try {
    console.log("Contesting Hydra head...");
    const result = await hydra.contest(true); // Wait for contest to complete
    console.log("Hydra head contested:", result);
  } catch (error) {
    console.error("Error contesting Hydra head:", error);
  }
}

main();
```