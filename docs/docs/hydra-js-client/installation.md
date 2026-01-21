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


### Hydra Service Initialization

Assuming that the hydra node is running and kuber-hydra server is started on localhost:8081, we can pass the host url to this class constructor to create the service:

```ts
import { KuberHydraService } from "kuber-client";

const hydraService = new KuberHydraService("http://localhost:8081");
```


## Quick Start

Here's a quick example of how to use `KuberHydraApiProvider` to interact with a Hydra head:

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL

  console.log("Head state:", await hydra.queryHeadState());

  // Example: Close the head
  await hydra.close(true);
}

main();
```