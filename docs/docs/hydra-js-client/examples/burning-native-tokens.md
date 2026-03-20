---
sidebar_position: 7
sidebar_label: Burning Native Tokens
---

# Burning Native Tokens

This guide demonstrates how to burn native tokens in an open Hydra Head using a CIP-30 wallet and `KuberHydraApiProvider`.

## When to use this flow

Use token burn when you need to reduce total circulating supply for a policy, invalidate temporary in-app assets, or clean up test assets after scenario runs.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed
- An active Hydra Head in `Open` state
- A wallet that currently holds the token you want to burn
- The same mint policy used when the token was created

## Example: Burn 1 `Token1`

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { readFileSync } from "fs";

async function runBurnNativeTokensExample() {
  const hydra = new KuberHydraApiProvider("http://localhost:8082");

  const signingKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(
      readFileSync(
        process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk",
        "utf-8",
      ),
    ),
  );

  const shelleyWallet = new ShelleyWallet(signingKey);
  const wallet = new SimpleCip30Wallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await wallet.getChangeAddress()).toBech32();

  const headState = await hydra.queryHeadState();
  if (headState.state !== "Open") {
    throw new Error(`Hydra head is ${headState.state}. Expected Open.`);
  }

  // Negative amount burns assets under the policy script.
  const burnTx = {
    mint: [
      {
        script: {
          type: "sig",
          keyHash: shelleyWallet.paymentKey.pkh.toString("hex"),
        },
        amount: {
          Token1: -1,
        },
      },
    ],
    changeAddress: walletAddress,
  };

  const result = await hydra.buildAndSubmitWithWallet(wallet, burnTx);
  console.log("Burn transaction submitted:", result.transaction.toBytes().toString("hex"));
}

runBurnNativeTokensExample().catch((err) => {
  console.error("Burn flow failed:", err);
});
```

## Verify the burn

1. Query wallet UTxOs before and after the burn.
2. Confirm token quantity decreased by the expected amount.
3. Confirm no policy mismatch error occurred.

## Common issues

- `ValueNotConservedUTxO`: burn amount is larger than wallet token balance.
- Script witness failure: policy script/key hash does not match minted asset policy.
- Head not open: call this flow only after commit + head open.
