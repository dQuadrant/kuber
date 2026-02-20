import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
  // By default, Docusaurus generates a sidebar from the docs folder structure
  architecture: [
    {
      type: "doc",
      id: "hydra_feature_analysis", // Document ID
      label: "🔍 Hydra Analysis",
    },
    {
      type: "doc",
      id: "milestones", // Document ID
      label: "🗓️ Planning",
    },
    {
      type: "doc",
      id: "architecture", // Document ID
      label: "🏗️ Architecture",
    },
    {
      type: "category",
      label: "📊 Detailed Sequence Diagrams", // Category label
      collapsed: false, // This makes the category open by default

      items: [
        {
          type: "doc",
          id: "sequenceDiagrams/initialize_head", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/commit_utxos", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/decommit_utxos", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/abort_head", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/close_head", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/query_protocol_params", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/query_utxos", // Document ID for a sub-doc
        },
        {
          type: "doc",
          id: "sequenceDiagrams/fanout_head", // Document ID for a sub-doc
        },
      ],
    },
  ],
  hydraJsClient: [
    {
      type: "category",
      label: "🚀 Getting Started",
      collapsed: false,
      items: [
        "hydra-js-client/getting-started",
        "hydra-js-client/local-devnet",
        "hydra-js-client/testnet_or_mainnet",
      ],
      
    },
    {
      type: "doc",
      id: "hydra-js-client/installation",
      label: "📦 Start Developing",
    },
    {
      type: "category",
      label: "📚 Full Examples",
      collapsed: false,
      items: [
        "hydra-js-client/examples/working-with-wallets",
        "hydra-js-client/examples/commiting-utxos-to-hydra",
        "hydra-js-client/examples/submitting-hydra-transactions",
        "hydra-js-client/examples/minting-native-tokens",
        "hydra-js-client/examples/full-end-to-end-example",
      ],
    },
    {
      type: "category",
      label: "🛠️ Hydra Provider Reference",
      collapsed: false,
      items: [
        {
          type: "category",
          label: "Query APIs",
          collapsed: true,
          items: [
            "hydra-js-client/queryUtxo",
            "hydra-js-client/queryProtocolParameters",
            "hydra-js-client/queryHeadState",
          ],
        },
        {
          type: "category",
          label: "Command APIs",
          collapsed: true,
          items: [
            "hydra-js-client/initialize",
            "hydra-js-client/close",
            "hydra-js-client/fanout",
            "hydra-js-client/abort",
            "hydra-js-client/contest",
            "hydra-js-client/commit",
            "hydra-js-client/decommit",
          ],
        },
        {
          type: "category",
          label: "Transaction APIs",
          collapsed: true,
          items: [
            "hydra-js-client/buildTx",
            "hydra-js-client/submitTx",
          ],
        },
        {
          type: "category",
          label: "Common Kuber functions",
          collapsed: true,
          items: [
            "hydra-js-client/kuber-provider/buildWithWallet",
            "hydra-js-client/kuber-provider/buildAndSignWithWallet",
            "hydra-js-client/kuber-provider/buildAndSubmitWithWallet",
            "hydra-js-client/kuber-provider/waitForUtxoConsumption",
            "hydra-js-client/kuber-provider/waitForTxConfirmation",
          ],
        },
      ],
    },
  ],
  apiReference: [
        {
      type: "doc",
      id: "kuber-api-reference",
      label: "⚙️🐭 Kuber API Reference",
    },
    {
      type: "doc",
      id: "kuber-hydra-api-reference",
      label: "⚙️🐲 Kuber-Hydra API Reference",
    }
  ],
};

export default sidebars;
