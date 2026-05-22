# Start developing


## **Kuber Client**

Example repository: [kuber-client-example](https://github.com/sireto/kuber-client-js/tree/master/hydra-example)

Test file: [hydra.test.ts](https://github.com/sireto/kuber-client-js/blob/master/__tests__/hydra.test.ts)

## Installation

To install the `kuber-client` package, you can use npm or yarn:

```bash
npm install kuber-client
# or
yarn add kuber-client
```

## Quick Start

Here's a quick example of how to use `KuberHydraApiProvider` to interact with a Hydra head:

```javascript
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  const headState = await hydra.queryHeadState();
  console.log("Head state:", headState.state);

  // Example: Close the head
  await hydra.close(true);
}

main();
```
