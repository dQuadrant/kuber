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

```typescript
import { readFileSync } from "fs";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client";

function formatAssets(assets: Record<string, Record<string, bigint>> = {}) {
  return Object.fromEntries(
    Object.entries(assets).map(([policyId, tokens]) => [
      policyId,
      Object.fromEntries(Object.entries(tokens).map(([assetName, quantity]) => [assetName, quantity.toString()])),
    ]),
  );
}

function formatValue(value) {
  return {
    lovelace: value.lovelace.toString(),
    assets: formatAssets(value.multiAssetsUtf8 ? value.multiAssetsUtf8() : value.multiassets),
  };
}

function formatUtxo(utxo) {
  return {
    txIn: `${utxo.txIn.txHash.toString("hex")}#${utxo.txIn.index}`,
    address: utxo.txOut.address.toBech32(),
    value: formatValue(utxo.txOut.value),
    datum: utxo.txOut.datum ? "inline" : utxo.txOut.datumHash?.toString("hex"),
    referenceScript: Boolean(utxo.txOut.referenceScript),
  };
}

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");
  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync("../../kuber-hydra/devnet/credentials/alice-funds.sk", "utf-8")),
  );
  const wallet = new SimpleCip30Wallet(hydra, hydra, new ShelleyWallet(signingKey), 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  try {
    const utxos = await hydra.queryUTxOByAddress(walletAddress);
    console.table(utxos.map(formatUtxo));
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

```typescript
import { readFileSync } from "fs";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client";

function formatAssets(assets: Record<string, Record<string, bigint>> = {}) {
  return Object.fromEntries(
    Object.entries(assets).map(([policyId, tokens]) => [
      policyId,
      Object.fromEntries(Object.entries(tokens).map(([assetName, quantity]) => [assetName, quantity.toString()])),
    ]),
  );
}

function formatValue(value) {
  return {
    lovelace: value.lovelace.toString(),
    assets: formatAssets(value.multiAssetsUtf8 ? value.multiAssetsUtf8() : value.multiassets),
  };
}

function formatUtxo(utxo) {
  return {
    txIn: `${utxo.txIn.txHash.toString("hex")}#${utxo.txIn.index}`,
    address: utxo.txOut.address.toBech32(),
    value: formatValue(utxo.txOut.value),
    datum: utxo.txOut.datum ? "inline" : utxo.txOut.datumHash?.toString("hex"),
    referenceScript: Boolean(utxo.txOut.referenceScript),
  };
}

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");
  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync("../../kuber-hydra/devnet/credentials/alice-funds.sk", "utf-8")),
  );
  const wallet = new SimpleCip30Wallet(hydra, hydra, new ShelleyWallet(signingKey), 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  try {
    const walletUtxos = await hydra.queryUTxOByAddress(walletAddress);
    if (walletUtxos.length === 0) {
      console.log(`No UTxOs found for ${walletAddress}`);
      return;
    }

    const transactionInput = `${walletUtxos[0].txIn.txHash.toString("hex")}#${walletUtxos[0].txIn.index}`;
    const utxos = await hydra.queryUTxOByTxIn(transactionInput);
    console.table(utxos.map(formatUtxo));
  } catch (error) {
    console.error("Error querying UTxOs by TxIn:", error);
  }
}

main();
```
