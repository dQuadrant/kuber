
# Building Transactions in Hydra

**1. Create a Sample Transaction JSON**

```ts
const createSampleOutputTx = (
  selectionAddress: string,
  outputAddress: string
) => ({
  selections: [selectionAddress],
  outputs: [
    {
      address: outputAddress,
      value: 2_000_000,
      datum: { constructor: 1, fields: [] },
    },
    {
      address: outputAddress,
      value: 2_000_000,
      datum: { constructor: 2, fields: [] },
    },
  ],
});
```

**2. Generate Address from Key**

```ts
import { setup } from "libcardano/lib/cardano/crypto";
import { Ed25519Key } from "libcardano/cardano/primitives/keys";
import { ShelleyWallet } from "libcardano/cardano/primitives/address";

await setup(); //this is necessary to run Ed25519Key funcitons

const testWalletSigningKey = await Ed25519Key.fromCardanoCliFile(
  path.join("src", "example.sk")
);
const testWalletAddress = new ShelleyWallet(testWalletSigningKey).addressBech32(
  0
); // 0 = testnet
```

**3. Create a Hydra Wallet**

```ts
async function createHydraWallet(
  service: KuberHydraService,
  ed25519Key: Ed25519Key,
  network: 0 | 1
): Promise<HydraWallet> {
  try {
    const shelleyWallet = new ShelleyWallet(ed25519Key);
    const hydraWallet = new HydraWallet(service, shelleyWallet, network);
    return hydraWallet;
  } catch (error: any) {
    return respondWithError(error);
  }
}

const myWallet = await createHydraWallet(hydraService, testWalletSigningKey, 0);
```

> The `HydraWallet` supports CIP-30 functions such as `getUTxO`, `signData`, `signTx`, `submitTx`, etc.

**4. Build the Transaction**

```ts
// For the sake of simplicity, we can fund our own address since the protocol fees are `0`.
const txBody = createSampleOutputTx(testWalletAddress, testWalletAddress);
const buildTxResponse = await hydraService.buildTx(txBody, false); // false = don't submit as we have not signed the transaction yet
```

The response from this function will provide the transaction details in the following format:

```json
{
  "cborHex": "84a400d90102818258204051dd270c1a51da8645b6c91d23053273547f1f853929fbec5519527e18266d0d0183a300581d60182aeee511419facd4bf4eab7538187288a55a633f579be0cf36897b011a001e8480028201d81843d87b80a300581d60182aeee511419facd4bf4eab7538187288a55a633f579be0cf36897b011a001e8480028201d81843d87b8082581d60182aeee511419facd4bf4eab7538187288a55a633f579be0cf36897b1a00f4240002000ed9010281581c182aeee511419facd4bf4eab7538187288a55a633f579be0cf36897ba0f5f6",
  "description": "Ledger Cddl Format",
  "hash": "ff6de11af9d0998a85c3eb5333f4afd173a904164630fc0717be8ee819900d4f",
  "type": "Unwitnessed Tx ConwayEra"
}
```

**5. Sign and Submit**

```ts
const txCborHex = buildTxResponse.cborHex;
const signature = await myWallet.signTx(txCborHex);
```

The wallet's signTx function returns a witness set in cbor hex format. To add this witness set to the transaction, we need to merge the witnesses.

```ts
const signedTxHex = txWithMergedSignature(txCborHex, signature);
```

Now, we have the signed transaction that we can submit to the hydra head.

```ts
const submissionResult = await myWallet.submitTx(signedTxHex);
console.log("Tx Submitted: ", submissionResult);
```

The response from the wallet is the signed transction cbor hex upon successful submission.