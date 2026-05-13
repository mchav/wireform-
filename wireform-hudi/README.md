# wireform-hudi

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

[Apache Hudi](https://hudi.apache.org/) timeline reader for Haskell.
Parse the timeline ([`Hudi.Timeline`](src/Hudi/Timeline.hs)),
decode the Avro 1.x+ instant payload format
([`Hudi.Avro`](src/Hudi/Avro.hs)),
materialise the active-file snapshot at any point in the table's
history, and walk the time-travel API
([`Hudi.IO`](src/Hudi/IO.hs)).

Hudi is the third of the three lakehouse table formats (Iceberg,
Delta Lake, Hudi). Same goal: make a directory of Parquet files
behave like an ACID table on top of object storage. The shape is a
little different. Hudi has the strongest support for upserts among
the three, an explicit Copy-on-Write (CoW) vs Merge-on-Read (MoR)
choice per table, and a "timeline" of `instant` files (each with a
state of `requested` / `inflight` / `completed`) instead of a
monotonic commit log. The instant payload format moved from JSON
in 0.x to Avro in 1.x, with Avro now being the canonical encoding.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-hudi,
  wireform-avro,       -- transitive: 1.x+ instant payloads are Avro
  wireform-parquet,    -- transitive: data files are Parquet
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-hudi` to compile
locally.

## Hello world

Open a Hudi table and list its currently-active base files:

```haskell
import qualified Hudi.IO as HT
import qualified Data.Text as T

main :: IO ()
main = do
  result <- HT.openHudiTable "/path/to/hudi-table"
  case result of
    Right table -> mapM_ (putStrLn . T.unpack) (HT.activeBaseFilePaths table)
    Left  err   -> putStrLn err
```

For time travel, `openHudiTableAt` takes an instant identifier and
returns the snapshot the table was in at that instant.

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `Hudi.Timeline`  | `parseInstantFileName` (the `<timestamp>.<action>.<state>` filename grammar); sort and filter helpers; `HoodieCommitMetadata` + `HoodieWriteStat` JSON; `HoodieReplaceCommitMetadata` for replacecommit instants (supersedes prior file slices via `partitionToReplaceFileIds`); `HoodieCleanMetadata` + `HoodieCleanPartitionMetadata`; the `FileSlice` / `TableState` aggregate (`applyCommit`, `applyReplaceCommit`, `applyClean`, `tableStateFromCommits`) |
| `Hudi.Avro`      | Avro container-file decoder for the 1.x+ instant payload format. Wraps [`wireform-avro`](../wireform-avro/) with the embedded `HoodieCommitMetadata.avsc` schema. |
| `Hudi.IO`        | `openHudiTable`, `openHudiTableAt` (time travel), `activeFiles`, `activeBaseFilePaths` (flat snapshot), `tableSchemaFromCommits`, `scanTimeline`, `readHoodieProperties` |

## Reading a table

`openHudiTable` walks the `.hoodie/` directory:

1. Read `.hoodie/hoodie.properties` to get the table type
   (Copy-on-Write vs Merge-on-Read), table version, and Avro schema.
2. Walk the timeline, sorting instants by their `<timestamp>` prefix.
3. For each `completed` instant, decode its payload (JSON for legacy
   tables, Avro container file for 1.x+ tables) into a
   `HoodieCommitMetadata` / `HoodieReplaceCommitMetadata` /
   `HoodieCleanMetadata`.
4. Apply each instant to the cumulative `TableState`, with
   `INSERT` / `UPSERT` / `BULK_INSERT` adding to file slices,
   `INSERT_OVERWRITE` (via replacecommit) superseding prior slices,
   and `clean` instants pruning expired files.
5. Return the resulting `HudiTable`.

## Time travel

`openHudiTableAt` short-circuits the timeline at a specific instant
identifier instead of the latest one, returning the snapshot the
table was in at that instant.

## Testing

```bash
cabal test wireform-hudi:hudi-test
```

The HUnit suite covers the timeline parser, the JSON instant
decoder, the Avro instant decoder, the snapshot aggregator (CoW),
replacecommit handling (verifies `INSERT_OVERWRITE` correctly drops
the replaced fileId), and the time-travel API.

### Cross-language interop with `hudi-rs`

[`probe/Probe.hs`](probe/Probe.hs) round-trips against
[`hudi-rs`](https://github.com/apache/hudi-rs) (the Rust / Python
Hudi binding). Coverage:

- JSON-format instants (legacy 0.x).
- Avro 1.x+ instants.
- Replacecommit instants (verifies `INSERT_OVERWRITE` correctly
  drops the replaced `fileId` on both sides).

Out of scope so far: Merge-on-Read log-block decoding (CoW only),
record-level merge keys, the metadata table.

```bash
cabal run wireform-hudi-interop-probe
```

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Rust / Python: [`hudi-rs`](https://github.com/apache/hudi-rs), the
  same binding the interop probe uses.
- Java: the
  [Apache Hudi reference Java library](https://github.com/apache/hudi),
  used by Spark, Flink, Trino, Presto.

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [Apache Hudi protocol](https://hudi.apache.org/docs/timeline/)
- [Hudi project](https://hudi.apache.org/)
- [`hudi-rs`](https://github.com/apache/hudi-rs) (the Rust binding)
