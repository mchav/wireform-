// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
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
  integrations: [
    starlight({
      title: 'wireform',
      description:
        'One Haskell library for serialization, schema parsing, code generation, streaming, RPC framing, container I/O, and analytics metadata.',
      social: [
        {
          icon: 'github',
          label: 'GitHub',
          href: 'https://github.com/iand675/wireform-',
        },
      ],
      customCss: ['./src/styles/fumadocs.css'],
      sidebar: [
        {
          label: 'Overview',
          items: [
            { label: 'Introduction', slug: 'index' },
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
          label: 'API reference',
          collapsed: false,
          items: apiSidebar.length
            ? apiSidebar
            : [
                {
                  label: 'Not yet ingested',
                  slug: 'api/index',
                },
              ],
        },
      ],
      expressiveCode: {
        themes: ['github-dark-default', 'github-light'],
        // `cabal` isn't in the default Shiki bundle; alias it to text so we
        // don't lose the build to language warnings until/unless we ship a
        // proper cabal grammar.
        shiki: {
          langAlias: { cabal: 'text' },
        },
        styleOverrides: {
          borderRadius: '0.5rem',
          codeFontFamily:
            "'JetBrains Mono', 'Fira Code', ui-monospace, SFMono-Regular, Menlo, monospace",
        },
      },
    }),
  ],
});
