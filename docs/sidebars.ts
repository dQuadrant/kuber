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
      id: "architecture", // Document ID
      label: "Architecture",
    },
    {
      type: "category",
      label: "Detailed Sequence Diagrams", // Category label
      collapsed: false,  // This makes the category open by default

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


        // {
        //   type: "doc",
        //   id: "sequenceDiagramB", // Document ID for another sub-doc
        //   label: "Sequence Diagram B",
        // },
        // {
        //   type: "doc",
        //   id: "sequenceDiagramC", // Document ID for another sub-doc
        //   label: "Sequence Diagram C",
        // },
      ],
    }
  ],
  scenarioTests: [
    {
      type: "doc",
      id: "hydra_feature_analysis", // Document ID
      label: "Hydra Analysis"
    },
  ],
  milestones: [
    {
      type: "doc",
      id: "milestones", // Document ID
      label: "Milestones",
    },
  ],
};

export default sidebars;
