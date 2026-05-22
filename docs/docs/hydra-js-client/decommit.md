# decommit

`decommit` is an asynchronous function that decommits UTxOs from an active Hydra head. This action moves funds from the Hydra head back to the main chain.

## Function Signature

```typescript
async decommit(tx: HexString, wait: boolean = false): Promise<DecommitResult>
```

## Parameters

- `tx`: A `HexString` representing the CBOR-encoded transaction to be decommitted.
- `wait`: An optional `boolean` indicating whether to wait for the decommit to complete. Defaults to `false`.

## Returns

A `Promise` that resolves to a `DecommitResult` object containing information about the decommit process.

## Example

```typescript
import { readFileSync } from "fs";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client";

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");
  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync("../../kuber-hydra/devnet/credentials/alice-funds.sk", "utf-8")),
  );
  const wallet = new SimpleCip30Wallet(hydra, hydra, new ShelleyWallet(signingKey), 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  const headUtxos = await hydra.queryUTxOByAddress(walletAddress);
  if (headUtxos.length === 0) {
    throw new Error(`Alice has no UTxO in the Hydra head at ${walletAddress}`);
  }

  const txIn = `${headUtxos[0].txIn.txHash.toString("hex")}#${headUtxos[0].txIn.index}`;
  const decommitTx = await hydra.createDecommitTx(txIn);
  const signedTx = await wallet.signTx(decommitTx.cborHex);

  try {
    console.log("Decommitting UTxOs...");
    const result = await hydra.decommit(signedTx.transaction.toBytes().toString("hex"), true);
    console.log("Decommit transaction hash:", result.decommitTx.hash);
  } catch (error) {
    console.error("Error decommitting UTxOs:", error);
  }
}

main();
```
