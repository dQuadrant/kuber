---
sidebar_position: 8
sidebar_label: Paying to a Script
---

# Paying to a Script

This guide demonstrates how to create a transaction output locked by a script address (pay-to-script) from within an open Hydra Head.

## Why this matters

Pay-to-script is the base workflow for contract-driven interactions: escrow, conditional settlement, and state-machine style protocols.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed
- An active Hydra Head in `Open` state
- Script address available for your target validator
- Datum format agreed by your application

## Example: Lock 5 ADA at script address

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { CardanoKeyAsync } from "libcardano";
import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { readFileSync } from "fs";

async function runPayToScriptExample() {
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

  // Replace this with your real validator address.
  const scriptAddress = "addr_test1wq...your_script_address";

  const txBuilder = {
    outputs: [
      {
        address: scriptAddress,
        value: "5000000",
        datum: {
          constructor: 0,
          fields: [
            { int: 42 },
            { bytes: "68656c6c6f" },
          ],
        },
      },
    ],
    changeAddress: walletAddress,
  };

  const txHash = await hydra.buildAndSubmitWithWallet(wallet, txBuilder);
  console.log("Pay-to-script tx submitted:", txHash);
}

runPayToScriptExample().catch((err) => {
  console.error("Pay-to-script flow failed:", err);
});
```

## Validate output exists

1. Query Hydra UTxO set and filter by script address.
2. Confirm datum is present in the output.
3. Use your script spending flow in a follow-up transaction.

## Notes

- Datum shape must match validator expectation.
- Keep datum minimal where possible to reduce transaction size.
- If spending from script later, include required redeemer and script witness.
