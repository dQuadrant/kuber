import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";

const config: Config = {
  title: "Kuber-Hydra",
  tagline: "Streamline your Layer 2 transactions",
  favicon: "img/favicon.ico",

  // Set the production url of your site here
  url: "https://cadanoapi.github.io",
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: "/kuber/kuber_hydra_docusaurus/",

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: "dQuadrant", // Usually your GitHub org/user name.
  projectName: "kuber", // Usually your repo name.

  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },
  themes: ["@docusaurus/theme-mermaid"],
  presets: [
    [
      "classic",
      {
        docs: {
          sidebarPath: "./sidebars.ts",

          editUrl: "https://github.com/dQuadrant/kuber/tree/feat/hydra",
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: "img/docusaurus-social-card.jpg",
    navbar: {
      title: "Kuber-Hydra",
      logo: {
        alt: "My Site Logo",
        src: "img/logo.png",
      },
      items: [
        {
          type: "docSidebar",
          sidebarId: "architecture",
          position: "left",
          label: "Architecture",
        },
        {
          type: "docSidebar",
          sidebarId: "scenarioTests",
          position: "left",
          label: "Scenario Tests",
        },
        {
          type: "docSidebar",
          sidebarId: "milestones",
          position: "left",
          label: "Milestones",
        },
        {
          href: "https://github.com/dQuadrant/kuber/tree/feat/hydra",
          label: "GitHub",
          position: "right",
        },
      ],
    },
    footer: {
      style: "dark",
      copyright: `Copyright © ${new Date().getFullYear()} Kuber Hydra`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
