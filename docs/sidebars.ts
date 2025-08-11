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
      type: "doc",
      id: "hydra-js-client/installation",
      label: "🚀 Installation & Quick Start",
    },
    {
      type: "category",
      label: "📚 Full Examples",
      collapsed: false,
      items: [
        "hydra-js-client/examples/working-with-wallets",
        "hydra-js-client/examples/commiting-utxos-to-hydra",
        "hydra-js-client/examples/submitting-hydra-transactions",
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
          collapsed: false,
          items: [
            "hydra-js-client/queryUtxo",
            "hydra-js-client/queryProtocolParameters",
            "hydra-js-client/queryHeadState",
          ],
        },
        {
          type: "category",
          label: "Command APIs",
          collapsed: false,
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
          collapsed: false,
          items: [
            "hydra-js-client/buildTx",
            "hydra-js-client/submitTx",
          ],
        },
      ],
    },
    {
      type: "doc",
      id: "kuber-hydra-api-reference",
      label: "⚙️🐲 Kuber-Hydra API Reference",
    },
    {
      type: "doc",
      id: "kuber-api-reference",
      label: "⚙️🐭 Kuber API Reference",
    },
  ],
};

export default sidebars;
