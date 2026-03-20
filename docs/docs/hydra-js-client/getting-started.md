# Getting Started with Kuber-Hydra

Use this page as your launchpad for real Hydra development with Kuber. You will find setup paths, API references, and practical end-to-end examples you can run immediately.

## What Kuber-Hydra gives you

Kuber-Hydra provides a developer-friendly relay API around Hydra nodes so you can:

- Build and submit in-head transactions with a familiar TxBuilder flow.
- Query head state, UTxOs, and protocol parameters from one place.
- Drive full head lifecycle operations (`initialize`, `commit`, `close`, `fanout`) without wiring low-level Hydra protocol code yourself.

In short: you write app transaction logic, and Kuber-Hydra handles most of the operational integration work.

## What you will run

- A Cardano node (local devnet or testnet/mainnet)
- 3 Hydra nodes for Alice, Bob, and Carol
- 3 Kuber-Hydra relay servers for Alice, Bob, and Carol

## Recommended prerequisites

Before running examples, make sure you have:

- Node.js 18+
- A package manager (`pnpm`, `yarn`, or `npm`)
- Docker Desktop (required for local devnet flow)
- Wallet/signing key files for participants
- Enough funds for commit/collateral scenarios

If you are just starting, use local devnet first. It is faster, deterministic, and easier to debug.

## Choose your setup path

### Quick start: [Local devnet](./local-devnet.md)

Best for fast iteration and repeatable testing. You can run full head lifecycle and transaction scenarios without waiting for public-network confirmations.

### 🌐 Real network: [Testnet/Mainnet](./testnet_or_mainnet.md)

Best for realistic integration conditions. Use this path when validating wallet behavior, infrastructure setup, and production-like transaction flow.

## First run checklist using Quick Start
See more for API details - [Kuber-Hydra API reference](../kuber-hydra-api-reference.md)

1. Start local cluster and verify relays are reachable.
2. Query `/query/head` from Alice relay to see current state.
3. Initialize head.
4. Commit at least one UTxO per participant.
5. Wait for `Open`.
6. Submit one simple transfer transaction. (See - [Submitting Hydra transactions](./examples/submitting-hydra-transactions.md)
)
7. Close and fanout.
8. Verify final L1 balances.

See more concrete examples below:

## Practical examples


- [Working with wallets](./examples/working-with-wallets.md)
- [Committing UTxOs to Hydra](./examples/commiting-utxos-to-hydra.md)
- [Submitting Hydra transactions](./examples/submitting-hydra-transactions.md)
- [Minting native tokens](./examples/minting-native-tokens.md)
- [Burning native tokens](./examples/burning-native-tokens.md)
- [Paying to a script](./examples/paying-to-script.md)
- [Payment to 100+ new addresses (minimum 2 ADA)](./examples/payment-to-100-addresses.md)
- [Full end-to-end Hydra flow](./examples/full-end-to-end-example.md)
- [Devnet cluster workflow](./examples/devnet-cluster.md)

## Core API references

- [Kuber-Hydra API reference](../kuber-hydra-api-reference.md)
- [Transaction builder API (`buildTx`)](./buildTx.md)
- [Hydra head status query API (`queryHeadState`)](./queryHeadState.md)
- [UTxO query API](./queryUtxo.md)
- [Protocol parameters query API](./queryProtocolParameters.md)

## Common pitfalls

- Head not `Open` when submitting tx: wait for state transition and retry.
- Commit fails due to low balance/collateral: fund wallet and select larger UTxO.
- High-fanout outputs fail: chunk outputs into multiple transactions.
- Inconsistent local runs: reset devnet and rerun from a known state.

## Suggested route by use-case

- Wallet integration focus: wallets -> commit -> submit tx.
- Token lifecycle focus: mint -> burn -> verify balances.
- Contract flow focus: pay-to-script -> follow-up spend flow.
- Throughput focus: 100+ recipient output batching.

## Recommended learning path

1. Start with [Local devnet](./local-devnet.md).
2. Run [Working with wallets](./examples/working-with-wallets.md) and [Submitting Hydra transactions](./examples/submitting-hydra-transactions.md).
3. Run practical advanced examples: mint, burn, pay-to-script, and high-fanout payments.
4. Finish with the [Full end-to-end example](./examples/full-end-to-end-example.md).

