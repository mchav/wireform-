# wireform-stats

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Internal monorepo tooling. Walks the per-package
`wireform-*/README.md` files, finds AUTOGEN marker regions, and
rewrites the body of each from in-tree test, coverage, and benchmark
data. Renders both markdown tables and SVG bar charts (light + dark
variants) so the README stays useful without a JS-rendered build
step.

Not a Hackage release. Lives in the monorepo because it dogfoods
[`wireform-xml`](../wireform-xml/) (the SVG charts are emitted via
`XML.Encode.encodePretty`) and runs against in-tree benchmark and
test outputs.

## Marker grammar

Every managed README region is wrapped in a paired HTML comment:

```markdown
<!-- BEGIN_AUTOGEN <key> -->
... content owned by the regen tool ...
<!-- END_AUTOGEN <key> -->
```

The key is a free-form identifier. Anything outside the markers is
hand-edited and never touched. Anything inside is owned by the regen
tool and replaced wholesale on every run.

Defined keys:

| Key                       | Body                                                                                                  |
|---------------------------|-------------------------------------------------------------------------------------------------------|
| `tests`                   | One-line summary: total / passing / failures / errors / skipped / wall time. From `dist-stats/test-results/<pkg>.junit.xml`. |
| `coverage`                | One-line summary: top-level expressions, alternatives, top-level declarations percentages. From `dist-stats/coverage/<pkg>.hpc.txt`. |
| `coverage:table`          | Per-module expressions-used table. Same source as `coverage`.                                         |
| `bench:<id>`              | A `<picture>` element referencing two SVGs (light + dark) plus a markdown table + caption. From `wireform-<pkg>/bench-results/summary/<id>.json`. |

Adding a new benchmark means dropping a `BenchSummary` JSON into
`wireform-<pkg>/bench-results/summary/<id>.json` and a matching
`<!-- BEGIN_AUTOGEN bench:<id> --><!-- END_AUTOGEN bench:<id> -->`
pair somewhere in the README. The regen tool figures out the rest.

## Workflow

```bash
# 1. Collect raw data (slow; run when you want to refresh).
bash scripts/collect-stats.sh tests        # cabal test all -> JUnit XML
bash scripts/collect-stats.sh coverage     # cabal test all --enable-coverage -> hpc report
bash scripts/collect-stats.sh bench wireform-cbor:wireform-cbor-bench   # one bench
bash scripts/collect-stats.sh bench-all                                  # all benches (very slow)

# 2. Distill each criterion JSON into a BenchSummary by hand:
#    edit wireform-<pkg>/bench-results/summary/<id>.json with the
#    representative numbers. Commit the summary.

# 3. Re-render the SVG charts from the summaries.
cabal run wireform-stats:exe:regen-stats -- render-bench-charts

# 4. Stitch everything into the per-package READMEs.
cabal run wireform-stats:exe:regen-stats -- render

# 5. Refresh the shields.io endpoint badge JSON files.
cabal run wireform-stats:exe:regen-stats -- badges
```

Step 2 is intentionally manual: criterion's JSON output is wider
than the README needs (per-iteration measurements, regression
analysis, etc.), and you almost always want to eyeball the numbers
before committing them. Once you've got a summary you trust, every
subsequent step is deterministic and re-runnable.

## CI gate

[`.github/workflows/regen-stats.yml`](../.github/workflows/regen-stats.yml)
runs `regen-stats check` on every PR, fails the build if any
README's AUTOGEN regions are stale relative to what the regen tool
would produce from in-tree summary JSON files. A separate job runs
`collect-stats.sh tests` + `collect-stats.sh coverage` on every PR
and pushes a stats commit back to the PR branch when the rendered
diff is non-empty. The benchmark step is opt-in via
`workflow_dispatch` with `run_benchmarks: true`, since criterion is
too noisy on shared CI runners.

## What's in here

| Module                     | Role                                                                                  |
|----------------------------|---------------------------------------------------------------------------------------|
| `Wireform.Stats.Marker`    | Marker grammar + region rewriter.                                                     |
| `Wireform.Stats.SVG`       | SVG bar chart renderer with light / dark themes (uses `wireform-xml` for the DOM).    |
| `Wireform.Stats.Table`     | Markdown table renderer with column alignment.                                        |
| `Wireform.Stats.Bench`     | Benchmark types: criterion JSON parser, `BenchSummary` JSON I/O, distillation helpers, conversion to render inputs. |
| `Wireform.Stats.Test`      | JUnit XML parser (consumes tasty's `--xml=...` output via `wireform-xml`).            |
| `Wireform.Stats.Coverage`  | `hpc report --per-module` text parser.                                                |
| `Wireform.Stats.Shields`   | shields.io endpoint badge JSON emitter (tests + coverage badges).                     |

The executable is `regen-stats`; subcommands `render`,
`render-bench-charts`, `badges`, `check`. Run it with `--help` for
the full surface.

## Testing

```bash
cabal run wireform-stats:test:wireform-stats-test
```

Property-based and unit tests cover the marker round-trip, SVG
emission, JSON round-trip for `BenchSummary`, JUnit parsing, and HPC
parsing. No external deps required at test time.

## License

BSD-3-Clause.
