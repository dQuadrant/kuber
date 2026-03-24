# Hydra Escrow App — Tutorial

A 3-party Layer 2 escrow application built on Cardano using Hydra Head Protocol. This tutorial demonstrates a complete escrow workflow using Kuber Hydra's devnet for local development and testing.

## Demo Video
[Watch Demo](https://drive.google.com/file/d/1Umr9kbSVCq8T7yvPWc9kygNKVT8VQrGm/preview)

## Source Code
[GitHub — hydra-settlement](https://github.com/prabinpkrl/hydra-settlement)

## Prerequisites

This application requires:
- **Kuber Hydra devnet** running locally (3 Hydra nodes + cardano-node)
- See [Local Devnet Setup](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/local-devnet) for devnet configuration
- Node.js 18+ and npm/pnpm

## What it demonstrates
- Opening a Hydra Head (3 parties commit ADA to L2)
- Instant zero-fee transactions on Layer 2
- Escrow flow: lock → release → dispute → mediator resolves
- Close head + fanout → funds settle back to Cardano L1

## Flow
1. Buyer creates payment room — all parties commit ADA to L2
2. Buyer sends protected payment — instant, 0 fees on L2
3. Seller confirms delivery
4. Buyer releases or raises dispute
5. Mediator resolves if disputed
6. Close + fanout — settled on Cardano L1