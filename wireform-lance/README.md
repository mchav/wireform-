# wireform-lance

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Lance](https://lancedb.github.io/lance/) reader for Haskell.
Parse the Lance data file format (40-byte footer, manifest envelope)
([`Lance.Format`](src/Lance/Format.hs)),
decode the protobuf-encoded manifest
([`Lance.Manifest`](src/Lance/Manifest.hs),
[`Lance.Pb.Lance.Table`](src/Lance/Pb/Lance/Table.hs),
[`Lance.Pb.Lance.File`](src/Lance/Pb/Lance/File.hs)),
walk a Lance dataset's versions
([`Lance.IO`](src/Lance/IO.hs)),
and surface the active fragment list, schema, writer version, and
version timestamps.

Lance is the columnar format LanceDB designed for ML and embedding
workloads. The high-level shape is similar to Parquet (a columnar
file format with a footer of metadata) but the design priorities
are different: random-access vector reads first (which matters for
ANN / k-NN lookup), schema evolution second (Lance has manifest-level
versioning, similar in spirit to Iceberg), and streaming append
third. The dataset format wraps multiple Lance data files plus a
manifest history under a single directory, so a dataset is to Lance
what a Delta or Iceberg table is to Parquet.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-lance,
  wireform-proto,      -- transitive: manifests are protobuf
  wireform-columnar,   -- transitive: shared columnar primitives
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-lance` to compile
locally.

## Hello world

Open a Lance dataset and list its currently-active data files:

```haskell
import qualified Lance.IO       as L
import qualified Lance.Manifest as LM

main :: IO ()
main = do
  result <- L.openLanceDataset "/path/to/lance-dataset"
  case result of
    Right ds  -> mapM_ print (LM.datasetActiveDataFilePaths ds)
    Left  err -> putStrLn err
```

## What's in here

| Module                       | Role                                                      |
|------------------------------|-----------------------------------------------------------|
| `Lance.Format`               | The two on-disk envelopes: 40-byte data-file footer + 16-byte manifest footer. `decodeFooter`, `decodeManifestFooter`. |
| `Lance.IO`                   | `openLanceFile`, `openLanceManifest`, `openLanceDataset`, `openLanceDatasetAt` (time travel by version), `findManifestVersions`, `decodeManifestFileName`, `encodeManifestFileName`, `latestManifestVersion`, `listDataFiles` |
| `Lance.Manifest`             | Typed protobuf decoder + dataset accessors (`datasetActiveDataFiles`, `datasetActiveDataFilePaths`, `datasetSchemaFields`, `LanceSchemaField`, `datasetWriterVersion`, `datasetTimestampMillis`) |
| `Lance.Pb.Lance.File`        | Auto-generated protobuf binding for Lance v2 file metadata (regenerate via `cabal run wireform-lance:gen-lance-pb`) |
| `Lance.Pb.Lance.File2`       | Auto-generated protobuf binding for Lance v2.1 file metadata |
| `Lance.Pb.Lance.Table`       | Auto-generated protobuf binding for the Lance manifest envelope |

## Reading a dataset

`openLanceDataset` walks the dataset directory:

1. Look for `_versions/<NNNNN>.manifest` files (the canonical
   manifest history).
2. Pick the latest version (or the version `openLanceDatasetAt`
   was given for time travel).
3. Decode the manifest's protobuf body, including the schema, the
   active fragments, the writer version, and the version
   timestamp.
4. Return the resulting `LanceDataset`.

The schema is materialised into a flat `LanceSchemaField` list via
`Lance.Manifest.datasetSchemaFields`. The active fragment list is
materialised into the absolute paths via
`Lance.Manifest.datasetActiveDataFilePaths`.

## File-level reading

For a single Lance data file (without the dataset wrapper),
`Lance.IO.openLanceFile` parses the 40-byte data-file footer and
exposes the byte ranges that the per-column metadata decoder would
consume. The protobuf `ColumnMetadata` decoder for individual data
files is downstream from this package; this module exposes the
underlying byte ranges and lets the caller decide what to do with
them.

## Codegen for the protobuf bindings

The three modules under `Lance.Pb.Lance.*` are generated from the
`.proto` files under [`proto/lance/`](proto/lance/) by the
in-tree `gen-lance-pb` executable, which uses
[`wireform-proto`](../wireform-proto/) under the hood:

```bash
cabal run wireform-lance:gen-lance-pb
```

Run from the workspace root so the relative `wireform-lance/proto/`
paths resolve.

## Testing

```bash
cabal test wireform-lance:lance-test
```

The HUnit suite covers the file footer, the manifest footer, the
manifest protobuf decoder, the active-fragment list, the schema
readout, the writer version, the version timestamp, and the time-
travel API.

### Cross-language interop with `pylance`

[`probe/Probe.hs`](probe/Probe.hs) round-trips against
[`pylance`](https://pypi.org/project/pylance/) (the Python binding
to the reference Rust implementation). Two probe modes:

- `--file`: parse a single Lance data file's footer.
- `--dataset`: parse a dataset directory and cross-check the
  versions list against `lance.dataset(...).versions()`, the active
  fragment list against `.get_fragments()`, the schema against
  `.schema`, and the writer version + version timestamp against
  the corresponding `pylance` accessors.

```bash
pip install pylance
cabal run wireform-lance-interop-probe -- --dataset /path/to/dataset
```

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Rust: the
  [reference Rust Lance implementation](https://github.com/lancedb/lance)
  used by `pylance`.
- Python: [`pylance`](https://pypi.org/project/pylance/), the same
  binding the interop probe uses.

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [Apache Lance project](https://lancedb.github.io/lance/)
- [Lance file format spec](https://lancedb.github.io/lance/format.html)
- [`pylance` PyPI](https://pypi.org/project/pylance/)
