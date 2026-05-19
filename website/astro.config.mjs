// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import mermaid from 'astro-mermaid';
import tailwindcss from '@tailwindcss/vite';
import { readFileSync, existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Auto-generated module sidebar lives here. The Haddock ingester writes it
// each time it runs; we fall back to an empty list so the site still builds
// even when the API has not been ingested yet (e.g. in CI without GHC).
const sidebarPath = resolve(__dirname, 'src/content/generated/sidebar.json');
let apiSidebar = [];
if (existsSync(sidebarPath)) {
  try {
    apiSidebar = JSON.parse(readFileSync(sidebarPath, 'utf8'));
  } catch {
    apiSidebar = [];
  }
}

export default defineConfig({
  site: 'https://iand675.github.io',
  base: '/wireform-',
  trailingSlash: 'ignore',
  vite: {
    plugins: [tailwindcss()],
  },
  integrations: [
    mermaid({
      theme: 'neutral',
      autoTheme: true,
      mermaidConfig: {
        flowchart: { curve: 'basis' },
      },
    }),
    starlight({
      title: 'wireform',
      description:
        'One Haskell ecosystem for serialization, codegen, streaming, messaging, and analytics.',
      social: [
        {
          icon: 'github',
          label: 'GitHub',
          href: 'https://github.com/iand675/wireform-',
        },
      ],
      customCss: ['./src/styles/fumadocs.css'],
      components: {
        Hero: './src/components/Hero.astro',
      },
      sidebar: [
        {
          label: 'Overview',
          items: [
            { label: 'Getting started', slug: 'guides/getting-started' },
            { label: 'Format catalogue', slug: 'guides/formats' },
            { label: 'Columnar roadmap', slug: 'guides/columnar-roadmap' },
          ],
        },
        {
          label: 'Concepts',
          autogenerate: { directory: 'concepts' },
        },
        {
          label: 'Kafka Streams',
          items: [
            { label: 'Overview', slug: 'kafka-streams' },
            {
              label: 'Get started',
              items: [
                { label: 'Quickstart', slug: 'kafka-streams/get-started/quickstart' },
                { label: '1. What is Kafka Streams?', slug: 'kafka-streams/get-started/what-is-kafka-streams' },
                { label: '2. Your first topology', slug: 'kafka-streams/get-started/your-first-topology' },
                { label: '3. Stateful processing', slug: 'kafka-streams/get-started/stateful-processing' },
                { label: '4. Joins and tables', slug: 'kafka-streams/get-started/joins-and-tables' },
                { label: '5. Going to production', slug: 'kafka-streams/get-started/going-to-production' },
              ],
            },
            { label: 'Riffle: Flink-class extensions', slug: 'kafka-streams/riffle' },
            {
              label: 'Operations',
              items: [
                { label: 'Topology evolution', slug: 'kafka-streams/operating/topology-evolution' },
                { label: 'Scaling and rebalancing', slug: 'kafka-streams/operating/scaling' },
                { label: 'Exactly-once across systems', slug: 'kafka-streams/operating/exactly-once' },
                { label: 'Observability', slug: 'kafka-streams/operating/observability' },
                { label: 'Visibility versus ACID', slug: 'kafka-streams/operating/visibility' },
                { label: 'Runbooks', slug: 'kafka-streams/operating/runbooks' },
              ],
            },
            {
              label: 'Concepts',
              items: [
                { label: 'Topology optimization', slug: 'kafka-streams/concepts/topology-optimization' },
                { label: 'Dynamic topology changes', slug: 'kafka-streams/concepts/dynamic-topology' },
                { label: 'Railway-oriented programming', slug: 'kafka-streams/concepts/railway-oriented-programming' },
              ],
            },
            {
              label: 'Guides',
              items: [
                { label: 'Enrichment via external systems', slug: 'kafka-streams/guides/enrichment' },
              ],
            },
            { label: 'Glossary', slug: 'kafka-streams/glossary' },
          ],
        },
        {
          label: 'API reference',
          collapsed: false,
          items: apiSidebar.length
            ? apiSidebar
            : [
                {
                  label: 'Not yet ingested',
                  slug: 'api',
                },
              ],
        },
      ],
      expressiveCode: {
        themes: ['github-dark-default', 'github-light'],
        shiki: {
          langs: [
            JSON.parse(readFileSync(resolve(__dirname, 'src/grammars/cabal.tmLanguage.json'), 'utf8')),
          ],
        },
        styleOverrides: {
          borderRadius: '4px',
          codeFontFamily:
            "'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace",
        },
      },
    }),
  ],
});
