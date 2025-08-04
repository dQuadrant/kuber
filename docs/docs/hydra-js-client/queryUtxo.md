# Query UTxOs

This section describes functions for querying Unspent Transaction Outputs (UTxOs) within the Hydra head.

## queryUTxOByAddress

`queryUTxOByAddress` is an asynchronous function that queries the UTxOs associated with a given address within the Hydra head.

### Function Signature

```typescript
async queryUTxOByAddress(address: string): Promise<UTxO[]>
```

### Parameters

- `address`: A `string` representing the Bech32 encoded address for which to query UTxOs.

### Returns

A `Promise` that resolves to an array of `UTxO` objects. Each `UTxO` object contains details about an unspent transaction output, including its transaction hash, index, and value.

### Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL
  const walletAddress = "addr_test1qr..."; // Replace with your wallet address

  try {
    const utxos = await hydra.queryUTxOByAddress(walletAddress);
    console.log("UTxOs for address:", utxos);
  } catch (error) {
    console.error("Error querying UTxOs:", error);
  }
}

main();
```

---

## queryUTxOByTxIn

`queryUTxOByTxIn` is an asynchronous function that queries the UTxOs associated with a specific transaction input within the Hydra head.

### Function Signature

```typescript
async queryUTxOByTxIn(txIn: string): Promise<UTxO[]>
```

### Parameters

- `txIn`: A `string` representing the transaction input (e.g., "txHash#index") for which to query UTxOs.

### Returns

A `Promise` that resolves to an array of `UTxO` objects. Each `UTxO` object contains details about an unspent transaction output, including its transaction hash, index, and value.

### Example

```javascript
const { loadCrypto } = require("libcardano");
const { KuberHydraApiProvider } = require("kuber-client");

async function main() {
  await loadCrypto();

  const hydra = new KuberHydraApiProvider("http://localhost:8081"); // Replace with your Hydra API URL
  const transactionInput = "yourTxHash#0"; // Replace with a valid transaction input

  try {
    const utxos = await hydra.queryUTxOByTxIn(transactionInput);
    console.log("UTxOs for transaction input:", utxos);
  } catch (error) {
    console.error("Error querying UTxOs by TxIn:", error);
  }
}

main();
