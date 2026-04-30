# commit

`commit` is an asynchronous function that commits UTxOs to an initializing Hydra head. This action moves funds from the main chain into the Hydra head, making them available for off-chain transactions.

## Function Signature

```typescript
async commit(utxos: Commit, submit: boolean = false): Promise<CommonTxObject>
```

## Parameters

- `utxos`: A `Commit` object specifying the UTxOs to be committed.
- `submit`: An optional `boolean` indicating whether to submit the transaction to the chain. Defaults to `false`.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the commitment transaction.

## Example

```typescript
import { readFileSync } from "fs";
import { CardanoKeyAsync, Value } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { UTxO } from "libcardano/serialization";
import { KuberHydraApiProvider } from "kuber-client";

async function main() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");
  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync("../../kuber-hydra/devnet/credentials/alice-funds.sk", "utf-8")),
  );
  const wallet = new SimpleCip30Wallet(hydra, hydra, new ShelleyWallet(signingKey), 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  const l1Utxos = await hydra.l1Api.queryUTxOByAddress(walletAddress);
  const selectedUtxo = l1Utxos.find((utxo: UTxO) => utxo.txOut.value.greaterThan(Value.fromString("4A")));
  if (!selectedUtxo) {
    throw new Error(`Alice has no L1 UTxO larger than 4 ADA at ${walletAddress}`);
  }

  const txIn = `${selectedUtxo.txIn.txHash.toString("hex")}#${selectedUtxo.txIn.index}`;

  try {
    console.log("Committing UTxOs...");
    const commitTx = await hydra.commit({ utxos: [txIn] });
    const signedTx = await wallet.signTx(commitTx.cborHex);
    await hydra.l1Api.submitTx(signedTx.transaction.toBytes().toString("hex"));
    console.log("Commit transaction hash:", commitTx.hash);
  } catch (error) {
    console.error("Error committing UTxOs:", error);
  }
}

main();
```
