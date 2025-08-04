# Installation and Quick Start

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
