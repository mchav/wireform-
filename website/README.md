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

`npm run build` produces a static site under `dist/`. The site uses
`base: '/wireform-'` (matching the GitHub repo name); change that in
`astro.config.mjs` if you deploy to a different prefix.
