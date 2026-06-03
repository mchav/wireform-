# wireform docs site

A fumadocs-styled [Astro Starlight](https://starlight.astro.build) site for
wireform, plus a Haddock-HTML ingester that re-renders generated API docs
into clean, navigable MDX pages.

## What's here

- `src/content/docs/` — guides, concepts, and the home page.
- `src/content/docs/api/` — auto-generated MDX from Haddock (gitignored).
- `src/content/generated/sidebar.json` — sidebar JSON the ingester writes;
  consumed by `astro.config.mjs`.
- `scripts/ingest-haddock.mjs` — cheerio-based parser that reads Haddock
  HTML and emits the MDX pages above.
- `src/styles/fumadocs.css` — theme overlay (typography, sidebar, decl
  blocks, "Source" links).

## Develop

```bash
cd website
npm install
npm run dev          # http://localhost:4321/wireform-/
```

The dev/build pipeline runs `npm run prebuild` (= the ingester with
`--if-present --fallback-fixture`) automatically, so the site builds even
when no real Haddock output is present — it falls back to the small fixture
under `../haddock-fixture/` so the layout is browsable end-to-end.

## Dead-link checking

`npm run build` validates every internal link and in-page anchor via the
[`starlight-links-validator`](https://github.com/HiDeoo/starlight-links-validator)
plugin (configured in `astro.config.mjs`). A typo'd slug, a renamed page,
or a missing heading anchor fails the build instead of shipping a 404, so
running `npm run build` locally is the quickest way to confirm the docs
still cross-link correctly.

Notes:

- Use **relative** links between docs (e.g. `../operating/scaling/`) rather
  than root-absolute ones (`/kafka-streams/operating/scaling/`). The site is
  served under a `base` of `/wireform-` on GitHub Pages but `/` on Vercel;
  relative links resolve correctly under both, absolute ones do not.
- The auto-generated Haddock API pages under `api/` are excluded from the
  link graph, since they only exist after the ingester runs.

CI runs this check on every pull request that touches `website/`, `docs/`,
or the Haddock fixture (see `.github/workflows/docs.yml`), using the
in-tree fixture so no GHC build is needed.

## Build with real Haddock output

From the repo root:

```bash
cabal v2-haddock all --haddock-html
```

Then ingest:

```bash
cd website
npm run ingest                         # auto-discovers cabal output
# or pass directories explicitly:
npm run ingest -- --input ../dist-newstyle/build/.../doc/html/wireform-core
```

The ingester:

1. Walks every `<package>/<Mod-Name>.html` page produced by Haddock.
2. Parses module name, description, synopsis, and per-declaration
   signatures + doc strings + "Source" links via cheerio.
3. Emits `src/content/docs/api/<package>/<mod-slug>.mdx` with a
   fumadocs-style layout (synopsis card, type-signature highlighting,
   constructor/field/method sub-blocks, "Source" pill).
4. Auto-generates `src/content/generated/sidebar.json` reflecting the
   `Mod.Sub.Leaf` hierarchy as nested sidebar groups.
5. Copies the original Haddock HTML into `public/haddock/<package>/` so
   "Source" links keep resolving against the colourised source files
   that Haddock already produced.

## Deploy

`npm run build` produces a static site under `dist/`.

The canonical deploy is GitHub Pages at
`https://iand675.github.io/wireform-/`, so `astro.config.mjs` defaults
`site` + `base` to that prefix. The config auto-detects other targets:

- **Vercel** — when `VERCEL=1` is set (Vercel sets this automatically),
  `base` collapses to `/` and `site` is filled from
  `VERCEL_PROJECT_PRODUCTION_URL` / `VERCEL_URL`. No `vercel.json` needed;
  set the project root to `website/`.
- **Anywhere else / custom domain** — set `SITE_URL` (e.g.
  `https://docs.example.com`) and `SITE_BASE` (default `/`) before
  `npm run build`. These always win over the auto-detected defaults.
