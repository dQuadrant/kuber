---
sidebar_position: 4
sidebar_label: Minting Native Tokens
---

# Minting Native Tokens

This guide demonstrates how to mint native tokens within an open Hydra Head using the cip30 interface and `KuberHydraApiProvider`.

## Prerequisites

- Node.js environment
- `libcardano` and `libcardano-wallet` installed.
- An active Hydra Head (in "Open" state).
- A configured `KuberHydraApiProvider` instance.
- A minting policy script and its corresponding key hash.

This example demonstrates how to build and submit a transaction that mints native tokens within an open Hydra Head.

```typescript
import { KuberHydraApiProvider } from "kuber-client";
import { Value, CardanoKeyAsync } from "libcardano";
import { parseTransaction } from 'libcardano/serialization';

import { ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import { readFileSync } from "fs";

async function runMintNativeTokensExample() {
  // Initialize Hydra API Provider (replace with your Hydra node URL)
  const hydra = new KuberHydraApiProvider("http://172.31.6.1:8082");

  // Load test wallet signing key (used for signing the transaction)
  // Setup Shelley wallet
  const testWalletSigningKey = await CardanoKeyAsync.fromCardanoCliJson(
    JSON.parse(readFileSync(process.env.HOME + "/.cardano/preview/hydra-0/credentials/funds.sk", "utf-8")),
  );
  const shelleyWallet = new ShelleyWallet(testWalletSigningKey);
  const cip30Wallet = new SimpleCip30Wallet(hydra, hydra, shelleyWallet, 0);
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();

  console.log("Wallet Address:", walletAddress);

  // Ensure head is in 'Open' state
  const headState = await hydra.queryHeadState();
  if (headState.state !== "Open") {
    console.log("Head is not in 'Open' state. Please ensure it's in 'Open' state before running this example.");
    return;
  }
  console.log("Hydra Head is Open. Proceeding with minting transaction.");

  // Define the minting transaction
  // For a comprehensive reference on transaction builder fields, refer to:
  // https://kuberide.com/kuber/docs/tx-builder-reference
  const mintingTransaction = {
    mint: [
      {
        script: {
          type: "sig",
          keyHash: shelleyWallet.paymentKey.pkh.toString('hex'), 
        },
        amount: {
          Token1: 2,
        },
      },
    ],
    changeAddress: walletAddress,
  };

  try {
    // Use the buildAndSubmitWithWallet function from KuberProvider to mint tokens
    const mintResult = await hydra.buildAndSubmitWithWallet(cip30Wallet, mintingTransaction);
    console.log("Minting transaction submitted to Hydra Head");
    console.log("CBOR Hex:", mintResult.transaction.toBytes().toString('hex'));
  } catch (error: unknown) {
    if (error instanceof Error) {
      console.error("Error building or submitting minting transaction:", error.message);
    } else {
      console.error("An unknown error occurred:", error);
    }
  }
}

runMintNativeTokensExample();
