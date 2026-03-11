---
sidebar_position: 6
sidebar_label: Devnet Cluster
---

# Devnet cluster

This tutorial shows how to use `HydraTestCluster` from `kuber-client` to control a local Hydra devnet with multiple participants.

## Prerequisites

- Local devnet is running (`cardano-node`, `hydra-node-{1,2,3}`, and `kuber-hydra-{1,2,3}`).
- Devnet credentials exist in `kuber-hydra/devnet/credentials`.
- You can reach relay endpoints:
  - `http://localhost:8082` (alice)
  - `http://localhost:8083` (bob)
  - `http://localhost:8084` (carol)

## 1) Configure cluster participants

### Option A: Scan standard devnet folder

```javascript
const path = require("path");
const { HydraTestCluster } = require("kuber-client");

const devnetPath = path.join(process.cwd(), "../kuber-hydra/devnet");
const participantConfigs = HydraTestCluster.scanDevnetFolder(devnetPath, "localhost", 8082);
const cluster = new HydraTestCluster({ participants: participantConfigs });
```

### Option B: Add participants manually

```javascript
const { HydraTestCluster } = require("kuber-client");

const cluster = new HydraTestCluster();

cluster.addParticipantConfig(
  "http://localhost:8082",
  "../kuber-hydra/devnet/credentials/alice-funds.sk",
  "../kuber-hydra/devnet/credentials/alice-hydra.sk"
);

cluster.addParticipantConfig(
  "http://localhost:8083",
  "../kuber-hydra/devnet/credentials/bob-funds.sk",
  "../kuber-hydra/devnet/credentials/bob-hydra.sk"
);

cluster.addParticipantConfig(
  "http://localhost:8084",
  "../kuber-hydra/devnet/credentials/carol-funds.sk",
  "../kuber-hydra/devnet/credentials/carol-hydra.sk"
);
```

## 2) Perform commands on the devnet cluster

This flow is aligned with `kuber-client-js` cluster tests:

```javascript
async function runClusterFlow(cluster) {
  const hasFunds = await cluster.checkAllParticipantsHaveFunds(1_000_000n);
  if (!hasFunds) {
    throw new Error("One or more participants are missing required funds");
  }

  await cluster.resetClusterToOpenState();

  const alice = cluster.getParticipant(0);
  const bob = cluster.getParticipant(1);
  const carol = cluster.getParticipant(2);
  if (!alice || !bob || !carol) {
    throw new Error("Expected 3 participants");
  }

  const aliceHydra = alice.getKuberHydraApiProvider();
  const bobHydra = bob.getKuberHydraApiProvider();
  const carolHydra = carol.getKuberHydraApiProvider();

  console.log("Alice head:", await aliceHydra.queryHeadState());
  console.log("Bob head:", await bobHydra.queryHeadState());
  console.log("Carol head:", await carolHydra.queryHeadState());

  await cluster.resetClusterToClosedState({ fanoutReady: true });
  await aliceHydra.fanout(true);
  await aliceHydra.waitForHeadState("Idle", 180000, true);
}
```

## 3) Useful participant operations

- `participant.getCip30Wallet()` for sign/build/submit style flows.
- `participant.getKuberHydraApiProvider()` for Hydra and L1 relay methods.
- `cluster.resetCluster("Open")` at test start for deterministic state.
- `cluster.resetCluster("Initial")` or `cluster.resetCluster("Closed")` for state-specific test scenarios.

## Reference

- Public exports from `kuber-client`: `HydraTestCluster`, `HydraTestParticipant`.
- Main package import: `const { HydraTestCluster, HydraTestParticipant } = require("kuber-client")`.
- For implementation details, use the published package sources in your installed `kuber-client` version.
