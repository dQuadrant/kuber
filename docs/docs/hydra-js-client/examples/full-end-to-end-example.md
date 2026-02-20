---
sidebar_position: 5
sidebar_label: Full End-to-End Example
---

# Full End-to-End Hydra Flow (Devnet + kuber-hydra)

This guide is for developers who want to try Cardano Hydra Layer 2 quickly, without spending days wiring node APIs, keys, and transaction flow manually.

It is based on:

- `kuber-hydra/hydra-example/hydra-e2e.ts`
- `kuber-hydra/devnet/reset-cluster.sh`

## Why use Hydra L2?

Hydra gives you a fast off-chain execution environment (a Hydra Head) while still settling final results back to Cardano L1.

Typical use-cases:

- **High-frequency transactions**: many transfers between participants with lower latency.
- **Interactive apps**: games, micro-payments, and collaborative workflows where quick finality matters.
- **Batching L1 interaction**: run activity in Hydra, settle net result to L1 at close/fanout.

In simple terms: Hydra is useful when your users need speed and responsiveness, but you still want L1 settlement guarantees.

## Why use `kuber-hydra`?

Directly integrating with Hydra + Cardano can be operationally heavy. `kuber-hydra` makes this easier by exposing a familiar API for both L1 and Hydra workflows.

With `kuber-hydra`, developers can:

- Reuse a consistent transaction-building model.
- Use simple HTTP relay endpoints for Alice/Bob/Carol.
- Build, sign, submit, and query with less low-level boilerplate.
- Start from a working devnet and iterate quickly.

## What this full example demonstrates

The script runs the complete lifecycle end-to-end:

1. Initialize/open head
2. Fund participants if needed
3. Commit UTxOs from Alice, Bob, Carol
4. Submit in-head transactions
5. Close head and fanout
6. Verify L1 settlement

This gives you a realistic baseline for production-oriented flows: open -> transact -> close -> settle.

## Prerequisites (minimal)

- Docker running locally
- Node.js 18+ (or compatible)
- `kuber` repository cloned locally
- Devnet ports available:
	- `8082` (Alice relay)
	- `8083` (Bob relay)
	- `8084` (Carol relay)

## Quick start (easy path)

### 1) Start a clean devnet

From `kuber-hydra/devnet`:

```bash
./reset-cluster.sh
docker compose ps
```

You do not need to run each setup script manually. `reset-cluster.sh` already does it for you.

Behind the scenes it:

- Recreates runtime and credentials
- Starts cardano-node
- Seeds participant wallets
- Publishes Hydra scripts
- Starts all services (`cardano-node`, `hydra-node-1..3`, `kuber-hydra-1..3`)

### 2) Install example dependencies

From `kuber-hydra/hydra-example`:

```bash
pnpm install
```

The example uses `tsx` and `kuber-client` from `hydra-example/package.json`.

### 3) Run the full E2E flow

```bash
cd kuber-hydra/hydra-example
npx tsx hydra-e2e.ts
```

## What you should expect

On success, you will see logs showing:

- Head reached `Open`
- Transactions submitted inside Hydra
- Head moved to `Closed`
- `fanout` completed
- Final L1 balances printed for Alice/Bob/Carol

This confirms the full L2-to-L1 cycle worked.

## Script behavior (high level)

### Network and parties

The script targets three relays:

- Alice: `http://localhost:8082`
- Bob: `http://localhost:8083`
- Carol: `http://localhost:8084`

Signing keys are loaded from:

- `kuber-hydra/devnet/credentials/alice-hydra.sk`
- `kuber-hydra/devnet/credentials/bob-hydra.sk`
- `kuber-hydra/devnet/credentials/carol-hydra.sk`

### Deterministic start

At flow start, `ensureFreshDevnetHead()` executes:

```bash
cd kuber-hydra/devnet && ./reset-cluster.sh
```

Then it waits until all three relays respond to `queryHeadState`.

### Head preparation

`ensureHeadOpenAndCommitted()` handles different initial states:

- **Open**: proceeds directly
- **Closed**: waits for contestation deadline, tries `fanout`, verifies settlement, exits as recovered flow
- **Idle/other**: initializes head and commits missing parties

If a party does not have enough L1 balance, the script funds from faucet before commit.

### In-head transactions

After head is open, the script performs:

- Alice -> Bob: `1A`
- Bob -> Carol: `1A`
- Carol -> Alice: `1A`

It retries on transient Hydra submission timing issues (snapshot timing / cluster timing mismatches).

### Close and fanout

The script:

1. Ensures close collateral exists on L1
2. Attempts close via Alice, then Bob, then Carol relay
3. Waits for `Closed`
4. Waits until contestation deadline
5. Calls `fanout`

### Settlement verification

Finally, it queries each party L1 address and logs:

- UTxO count
- Total ADA settled after fanout

## How this helps your own project

Use this example as a template for:

- Opening a head for a known participant set
- Committing initial user liquidity
- Running your app logic as Hydra transactions
- Settling final state back to L1

A practical approach is to keep your domain logic separate, and call `kuber-hydra` only for lifecycle + transaction operations.

## Troubleshooting

- If relays are not reachable, run `docker compose ps` in `kuber-hydra/devnet` and ensure ports `8082-8084` are up.
- If close fails due to collateral, re-run and allow faucet funding to complete.
- If a previous head is stuck in `Closed`, the script tries automatic recovery/fanout; if recovery fails, run `./reset-cluster.sh` and retry.


## Full E2E example code
```ts
import { readFileSync } from "fs";
import { execSync } from "child_process";
import { loadCrypto, Ed25519Key, Value } from "libcardano";
import { ShelleyWallet, Cip30ShelleyWallet } from "libcardano-wallet";
import { KuberHydraApiProvider } from "kuber-client"; // Adjust path as needed
import { UTxO } from "libcardano/cardano/serialization";

// Set your work directory and paths according to your local setup. 
// This example assumes a specific structure for the devnet, so adjust as necessary for your environment. 
const WORK_DIR = process.env.HOME + "/work/kuber/kuber-hydra/devnet";

const hydraAlice = new KuberHydraApiProvider("http://localhost:8082");
const hydraBob = new KuberHydraApiProvider("http://localhost:8083");
const hydraCarol = new KuberHydraApiProvider("http://localhost:8084");

type PartyConfig = {
  name: string;
  skPath: string;
  hydra: KuberHydraApiProvider;
};

type PartyRuntime = PartyConfig & {
  wallet: Cip30ShelleyWallet;
  address: string;
  pubKeyHex?: string;
};

const PARTIES: PartyConfig[] = [
  { name: "Alice", skPath: WORK_DIR + "/credentials/alice-hydra.sk", hydra: hydraAlice },
  { name: "Bob", skPath: WORK_DIR + "/credentials/bob-hydra.sk", hydra: hydraBob },
  { name: "Carol", skPath: WORK_DIR + "/credentials/carol-hydra.sk", hydra: hydraCarol },
];

const DEVNET_RELAYS = [8082, 8083, 8084];

async function waitForRelayHealth(timeoutMs: number = 120000): Promise<void> {
  const start = Date.now();

  while (Date.now() - start < timeoutMs) {
    const checks = await Promise.all(
      DEVNET_RELAYS.map(async (port) => {
        try {
          const state = await new KuberHydraApiProvider(`http://localhost:${port}`).queryHeadState();
          return typeof state?.state === "string";
        } catch {
          return false;
        }
      }),
    );

    if (checks.every(Boolean)) {
      return;
    }

    await new Promise((resolve) => setTimeout(resolve, 1000));
  }

  throw new Error(`Timed out waiting for relays ${DEVNET_RELAYS.join(", ")} to become healthy.`);
}

async function ensureFreshDevnetHead(): Promise<void> {
  console.log("Resetting devnet at flow start for deterministic E2E execution...");
  execSync(`cd ${WORK_DIR} && ./reset-cluster.sh`, { stdio: "inherit" });
  await waitForRelayHealth();
}

async function queryUtxosByAddress(address: string): Promise<UTxO[]> {
  const utxos = await hydraAlice.l1Api.queryUTxOByAddress(address);
  console.log(`\nUTxOs for address ${address}:`);
  if (utxos.length === 0) {
    console.log("No UTxOs found for this address.");
    return [];
  }
  utxos.forEach((utxo) => {
    const txIn = utxo.txIn;
    const balance = Number(utxo.txOut.value.lovelace) / 1_000_000; // Convert from lovelace to ADA
    console.log(`${txIn.txHash.toString("hex")}#${txIn.index} - ${balance} ADA`);
  });
  return utxos;
}

async function getTotalAdaBalanceByAddress(address: string): Promise<number> {
  const utxos = await hydraAlice.l1Api.queryUTxOByAddress(address);
  let totalBalance = 0;
  utxos.forEach((utxo) => {
    totalBalance += Number(utxo.txOut.value.lovelace);
  });
  console.log(`\nTotal balance for address ${address}: ${totalBalance / 1_000_000} ADA`);
  return totalBalance / 1_000_000;
}

async function getCip30WalletFromSkFile(skFilePath: string, hydra: KuberHydraApiProvider): Promise<Cip30ShelleyWallet> {
  const skFile = readFileSync(skFilePath, "utf-8");
  const signingKey = await Ed25519Key.fromCardanoCliJson(
    JSON.parse(skFile),
  );
  const shelleyWallet = new ShelleyWallet(signingKey);
  return new Cip30ShelleyWallet(hydra, hydra, shelleyWallet, 0);
}

async function waitForBalance(address: string, minimumBalance: number, timeout: number = 120000): Promise<void> {
  const startTime = Date.now();
  while (Date.now() - startTime < timeout) {
    const balance = await getTotalAdaBalanceByAddress(address);
    if (balance >= minimumBalance) {
      console.log(`Balance of ${balance} ADA reached for address ${address}`);
      return;
    }
    console.log(`Current balance ${balance} ADA is less than required ${minimumBalance} ADA. Retrying in 1 second...`);
    await new Promise((resolve) => setTimeout(resolve, 1000)); // Wait for 1 second before retrying
  }
  throw new Error(`Timeout: Balance did not reach ${minimumBalance} ADA for address ${address} within ${timeout / 60000} minutes.`);
}

async function fundWalletWithFaucet(walletAddress: string, minimumBalanceAda: number = 10, fundingAda: number = 10) {

  const faucetSkPath = WORK_DIR + "/runtime/cardano-node/faucet.sk";
  const faucetCip30Wallet = await getCip30WalletFromSkFile(faucetSkPath, hydraAlice);
  const faucetAddress = (await faucetCip30Wallet.getChangeAddress()).toBech32();
  console.log("Faucet Address:", faucetAddress);
  await queryUtxosByAddress(faucetAddress);

  // Get total balance of wallet before funding
  const balance = await getTotalAdaBalanceByAddress(walletAddress);

  if (balance >= minimumBalanceAda) {
    console.log(`Wallet already has ${balance} ADA, skipping funding.`);
    return;
  }

  const txBuilder = {
    selections: [
      faucetAddress, // Funding from faucet address
    ],
    outputs: [{ address: walletAddress, value: `${fundingAda}A` }],
    changeAddress: faucetAddress,
  };
  const buildResult = await hydraAlice.l1Api.buildTx(txBuilder);
  const signResult = await faucetCip30Wallet.signTx(buildResult.cborHex);
  await hydraAlice.l1Api.submitTx(signResult.updatedTxBytes.toString("hex"));
  console.log("Submitted funding transaction hash:", buildResult.hash);

  // // // Wait for the transaction to be confirmed
  await waitForBalance(walletAddress, minimumBalanceAda, 120000);

}

async function getMaxUtxoAdaByAddress(address: string): Promise<number> {
  const utxos = await hydraAlice.l1Api.queryUTxOByAddress(address);
  if (utxos.length === 0) return 0;
  const maxLovelace = utxos.reduce((max, utxo) => {
    const value = Number(utxo.txOut.value.lovelace);
    return value > max ? value : max;
  }, 0);
  return maxLovelace / 1_000_000;
}

async function ensureCloseCollateral(parties: PartyRuntime[]): Promise<void> {
  const minimumLargestUtxoAda = 8;

  for (const party of parties) {
    const largestUtxoAda = await getMaxUtxoAdaByAddress(party.address);
    if (largestUtxoAda >= minimumLargestUtxoAda) {
      continue;
    }

    console.log(
      `${party.name} largest L1 UTxO is ${largestUtxoAda} ADA (< ${minimumLargestUtxoAda} ADA). Funding collateral buffer...`,
    );
    await fundWalletWithFaucet(party.address, 12, 12);

    const updatedLargestUtxoAda = await getMaxUtxoAdaByAddress(party.address);
    if (updatedLargestUtxoAda < minimumLargestUtxoAda) {
      throw new Error(
        `${party.name} still lacks a large-enough collateral UTxO after funding (largest=${updatedLargestUtxoAda} ADA).`,
      );
    }
  }
}

async function commitToHydraHead(cip30Wallet: Cip30ShelleyWallet, hydra: KuberHydraApiProvider, partyName: string) {
  // Select UTxOs to commit (e.g., the first one with a value greater than 4 ADA)
  const walletAddress = (await cip30Wallet.getChangeAddress()).toBech32();
  const l1Utxos = await queryUtxosByAddress(walletAddress);
  const selectedUtxos = l1Utxos.filter((x) => x.txOut.value.greaterThan(Value.fromString("4A")));
  if (selectedUtxos.length === 0) {
    throw new Error(`Not enough balance on ${walletAddress} in L1 chain for commit example`);
  }
  console.log(`\nSelected UTxOs for commit (${partyName}):`);
  selectedUtxos.forEach((utxo) => {
    const txIn = utxo.txIn;
    const balance = Number(utxo.txOut.value.lovelace) / 1_000_000; // Convert from lovelace to ADA
    console.log(`${txIn.txHash.toString("hex")}#${txIn.index} - ${balance} ADA`);
  });

  const txIn = selectedUtxos[0].txIn;
  const utxoToCommit = [`${txIn.txHash.toString("hex")}#${txIn.index}`];

  // Build the commit transaction using Hydra API
  const commitResult = await hydra.commit({ utxos: utxoToCommit });
  console.log(`Transaction to be signed (${partyName}):`, commitResult.hash);

  // Sign the transaction using the CIP-30 wallet
  const signResult = await cip30Wallet.signTx(commitResult.cborHex);

  // Submit the signed transaction to the L1 chain
  await hydra.l1Api.submitTx(signResult.updatedTxBytes.toString("hex"));
  console.log(`Submitted Commit transaction hash (${partyName}):`, commitResult.hash);

  // // Wait for the transaction to be confirmed and head state to change
  await hydra.l1Api.waitForUtxoConsumption(selectedUtxos[0].txIn, 280000);
  console.log("Commit transaction confirmed.");

}

async function buildPartyRuntime(): Promise<PartyRuntime[]> {
  const runtime: PartyRuntime[] = [];
  for (const party of PARTIES) {
    const wallet = await getCip30WalletFromSkFile(party.skPath, party.hydra);
    const address = (await wallet.getChangeAddress()).toBech32();
    const pubKeyHex = wallet?.shelleyWallet?.paymentKey?.public?.toString("hex");
    runtime.push({ ...party, wallet, address, pubKeyHex });
  }
  return runtime;
}

async function prepareHeadForTestFlow(parties: PartyRuntime[]): Promise<void> {
  const initialHeadState = await hydraAlice.queryHeadState();
  console.log("Current head state:", initialHeadState.state);

  if (initialHeadState.state !== "Idle") {
    throw new Error(`Expected Idle after reset, got ${initialHeadState.state}`);
  }

  await hydraAlice.initialize(true);
  console.log("Hydra head initialized from Idle.");

  for (const party of parties) {
    console.log(`${party.name} Address:`, party.address);
    await fundWalletWithFaucet(party.address);
    await commitToHydraHead(party.wallet, party.hydra, party.name);
  }

  const openWaitMs = await hydraAlice.waitForHeadState("Open", 300000, true, 3000);
  console.log(`Hydra head is OPEN (waited ${openWaitMs} ms).`);
}

async function submitHydraTransactions(parties: PartyRuntime[]): Promise<void> {
  const byName = Object.fromEntries(parties.map((x) => [x.name, x])) as Record<string, PartyRuntime>;

  const txPlan = [
    { from: "Alice", to: "Bob", value: "1A" },
    { from: "Bob", to: "Carol", value: "1A" },
    { from: "Carol", to: "Alice", value: "1A" },
  ];

  console.log("Submitting Hydra head transactions between all parties...");
  for (const tx of txPlan) {
    const sender = byName[tx.from];
    const receiver = byName[tx.to];
    const maxRetries = 8;
    let submittedHash = "";

    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      try {
        const builtTx = await hydraAlice.buildWithWallet(sender.wallet, {
          selections: [sender.address],
          outputs: [{ address: receiver.address, value: tx.value }],
          changeAddress: sender.address,
        });
        const signedTx = await sender.wallet.signTx(builtTx.cborHex, true);

        try {
          await hydraAlice.submitTx(signedTx.updatedTxBytes.toString("hex"));
        } catch (error: any) {
          const type = error?.data?.type;
          const message = String(error?.data?.message ?? error?.message ?? "");
          const isAcceptedButReportedAsError =
            type === "TxSubmissionError" && message.includes("Request created");
          if (!isAcceptedButReportedAsError) {
            throw error;
          }
          console.log(`Hydra submit accepted by relay with async response (${tx.from} -> ${tx.to}).`);
        }

        submittedHash = builtTx.hash ?? "";
        console.log(`Hydra tx submitted: ${tx.from} -> ${tx.to} (${tx.value}), hash=${submittedHash}`);
        break;
      } catch (error: any) {
        const type = error?.data?.type;
        const message = String(error?.data?.message ?? error?.message ?? "");
        const isSnapshotTimingError =
          type === "TxSubmissionError" && (message.includes("SnapshotConfirmed") || message.includes("NetworkClusterIDMismatch"));

        if (!isSnapshotTimingError || attempt === maxRetries) {
          throw error;
        }

        console.log(`Hydra tx retry (${attempt}/${maxRetries}) for ${tx.from} -> ${tx.to} due to snapshot timing. Waiting 4s...`);
        await new Promise((resolve) => setTimeout(resolve, 4000));
      }
    }

    if (!submittedHash) {
      throw new Error(`Failed to submit Hydra tx from ${tx.from} to ${tx.to}`);
    }

    await new Promise((resolve) => setTimeout(resolve, 2000));
  }

  const headUtxos = await hydraAlice.queryUtxos();
  console.log(`Hydra head UTxO count after transfers: ${headUtxos.length}`);
}

async function closeAndFanoutHead(parties: PartyRuntime[]): Promise<void> {
  const beforeClose = await hydraAlice.queryHeadState();
  if (beforeClose.state !== "Open") {
    throw new Error(`Expected head state Open before close, got ${beforeClose.state}`);
  }

  await ensureCloseCollateral(parties);

  console.log("Closing Hydra head...");
  const closeProviders = [
    { name: "Alice relay", hydra: hydraAlice },
    { name: "Bob relay", hydra: hydraBob },
    { name: "Carol relay", hydra: hydraCarol },
  ];

  let closeSucceeded = false;
  let closeErrors: string[] = [];

  for (const provider of closeProviders) {
    try {
      console.log(`Attempting close via ${provider.name}...`);
      await provider.hydra.close(true);
      closeSucceeded = true;
      console.log(`Close accepted via ${provider.name}.`);
      break;
    } catch (error: any) {
      console.log(error);
      const message = String(error?.message ?? "Unknown close error");
      closeErrors.push(`${provider.name}: ${message}`);

      const currentState = await hydraAlice.queryHeadState();
      if (currentState.state === "Closed") {
        closeSucceeded = true;
        console.log(`Head reached Closed after close attempt on ${provider.name}.`);
        break;
      }

      console.log(`Close failed via ${provider.name}.`);
    }
  }

  if (!closeSucceeded) {
    throw new Error(`Unable to close head. Errors: ${closeErrors.join(" | ")}`);
  }

  const closedWaitMs = await hydraAlice.waitForHeadState("Closed", 300000, true, 3000);
  const afterClose = await hydraAlice.queryHeadState();
  console.log(`Hydra head state after close: ${afterClose.state} (waited ${closedWaitMs} ms).`);
  if (afterClose.state !== "Closed") {
    throw new Error(`Close failed. Expected Closed, got ${afterClose.state}`);
  }

  // Wait until the contestation deadline has passed before attempting fanout.
  // The head may expose contestationDeadline in different fields; check both.
  const headInfo: any = await hydraAlice.queryHead();
  const contestationIso = headInfo?.contents?.contestationDeadline ?? headInfo?.postChainTx?.contestationDeadline ?? null;
  if (contestationIso) {
    const contestationDate = new Date(contestationIso);
    const now = new Date();
    if (contestationDate > now) {
      const waitMs = contestationDate.getTime() - now.getTime() + 1000; // add small buffer
      console.log(`Waiting ${Math.ceil(waitMs / 1000)}s for contestation deadline to pass before fanout...`);
      await new Promise((r) => setTimeout(r, waitMs));
    }
  } else {
    // fallback short wait if contestation deadline not available
    console.log("No contestationDeadline found on head — sleeping 5s before fanout as fallback.");
    await new Promise((r) => setTimeout(r, 5000));
  }

  console.log("Running fanout...");
  await hydraAlice.fanout(true);
  console.log("Fanout request completed.");
}

async function verifyL1Settlement(parties: PartyRuntime[]): Promise<void> {
  console.log("Verifying L1 UTxOs after fanout...");
  for (const party of parties) {
    const utxos = await hydraAlice.l1Api.queryUTxOByAddress(party.address);
    const totalLovelace = utxos.reduce((acc, utxo) => acc + Number(utxo.txOut.value.lovelace), 0);
    const totalAda = totalLovelace / 1_000_000;
    console.log(`${party.name} L1 settlement: ${utxos.length} UTxO(s), ${totalAda} ADA at ${party.address}`);
  }
}

async function runHydraE2EFlow() {
  await loadCrypto();
  await ensureFreshDevnetHead();

  const parties = await buildPartyRuntime();
  await prepareHeadForTestFlow(parties);

  await submitHydraTransactions(parties);
  await closeAndFanoutHead(parties);
  await verifyL1Settlement(parties);

  const finalHeadState = await hydraAlice.queryHeadState();
  console.log("Final head state:", finalHeadState.state);
}

runHydraE2EFlow();

```

### Example Package.json


```ts
{
  "dependencies": {
    "@types/node": "^25.2.3",
    "kuber-client": "3.3.1"
  },
  "devDependencies": {
    "tsx": "^4.20.3",
    "typescript": "^5.8.3",
    "libcardano": "2.2.8",
    "libcardano-wallet": "2.2.8"
  },
  "scripts": {
    "start": "node example.cjs",
    "start:esm": "node example.mjs"
  },
  "name": "hydra-example",
  "version": "1.0.0",
  "main": "example.cjs",
  "license": "MIT",
  "type": "commonjs"
}

```

