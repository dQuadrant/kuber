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
      type: "doc",
      id: "sequenceDiagrams", // Document ID
      label: "Sequence Diagrams",
    },
  ],
  scenarioTests: [
    {
      type: "doc",
      id: "scenarioTests", // Document ID
      label: "Scenario Tests",
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
