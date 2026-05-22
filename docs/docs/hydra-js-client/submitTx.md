# submitTx

`submitTx` is an asynchronous function that submits a signed transaction (in CBOR format) to the Cardano blockchain via the Kuber API. This function is typically used after a transaction has been built and signed.

## Function Signature

```typescript
async submitTx(cborString: HexString): Promise<CommonTxObject>
```

## Parameters

- `cborString`: A `HexString` representing the CBOR-encoded, signed transaction.

## Returns

A `Promise` that resolves to a `CommonTxObject` representing the submitted transaction.

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

  try {
    const builtTx = await hydra.buildWithWallet(wallet, {
      outputs: [{ address: walletAddress, value: "1_000_000" }],
      changeAddress: walletAddress,
    });
    const signedTx = await wallet.signTx(builtTx.cborHex, true);

    console.log("Submitting transaction...");
    const result = await hydra.submitTx(signedTx.transaction.toBytes().toString("hex"));
    console.log("Transaction hash:", result.hash);
  } catch (error) {
    console.error("Error submitting transaction:", error);
  }
}

main();
```
