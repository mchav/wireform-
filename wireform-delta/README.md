# wireform-delta

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

[Delta Lake](https://delta.io/) transaction-log reader for Haskell.
Parse JSON commit files ([`Delta.Log`](src/Delta/Log.hs)),
read Parquet checkpoint files
([`Delta.Checkpoint`](src/Delta/Checkpoint.hs)),
materialise the active-file snapshot at any point in the table's
history, walk the time-travel API
([`Delta.IO`](src/Delta/IO.hs)),
and answer the queries `DESCRIBE HISTORY` answers.

Delta Lake is the table format Databricks open-sourced after several
years of using it internally. Same shape as Iceberg and Hudi: a
metadata layer on top of Parquet data files, ACID semantics through
an append-only log, snapshot isolation, time travel, schema
evolution. The log lives under `_delta_log/` as a sequence of
`NNNNNNNNNNNNNNNNNNNN.json` commit files (one per transaction);
periodic `NNNNNNNNNNNNNNNNNNNN.checkpoint.parquet` files snapshot
the cumulative state so a reader doesn't have to replay every
commit since the table was created.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-delta,
  wireform-parquet,    -- transitive: checkpoint files are Parquet
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-delta` to compile
locally.

## Hello world

Open a Delta table and list its currently-active data files:

```haskell
import qualified Delta.IO as DT

main :: IO ()
main = do
  result <- DT.openDeltaTable "/path/to/delta-table"
  case result of
    Right table -> mapM_ putStrLn $ map T.unpack (DT.activeFilePaths table)
    Left  err   -> putStrLn err
```

For time travel, `openDeltaTableAt` takes a version number and returns
the snapshot the table was in at that version. `historyEntries`
returns the list of commits in chronological order, the same surface
`DESCRIBE HISTORY` exposes in the Spark / Delta integration.

## What's in here

| Module             | Role                                                      |
|--------------------|-----------------------------------------------------------|
| `Delta.Log`        | JSON commit-file actions (`AddAction`, `RemoveAction`, `MetaData`, `Protocol`, `CommitInfo`, `Cdc`, `Txn`, `DomainMetadata`); `parseLogLine`, `parseLogFile`; the `TableSnapshot` aggregate (`applyAction`, `snapshotFromActions`); `parseDeltaSchema`; `AddStats` (per-file column statistics); `LastCheckpoint` (the `_last_checkpoint` pointer file) |
| `Delta.Checkpoint` | Parquet checkpoint reader. Reconstructs `add` / `remove` / `metaData` / `protocol` records from the columnar layout, including `partitionValues`, `tags`, `partitionColumns`, `configuration`, `readerFeatures`, `writerFeatures`, and `deletionVector` |
| `Delta.IO`         | `openDeltaTable` (with checkpoint short-circuit), `openDeltaTableAt` (time travel), `historyEntries` (the `DESCRIBE HISTORY` surface), `activeFilePaths`, `dtActiveFiles`, `partitionedActiveFiles`, `findLastCheckpoint`, `findCommits`, `listLogEntries`, `readActions`, `readCheckpoint` |

## Reading a table

`openDeltaTable` walks the `_delta_log/` directory:

1. Look for the `_last_checkpoint` pointer file. If present, use it
   to find the most recent checkpoint Parquet file.
2. Read the checkpoint, materialising every active `AddAction` /
   `RemoveAction` / `MetaData` / `Protocol` it contains.
3. Replay every commit JSON file with version number greater than
   the checkpoint's, applying each action to the cumulative
   snapshot.
4. Return the resulting `DeltaTable`.

If no checkpoint exists, the reader replays the full commit log
from version 0. This is exactly the algorithm the Delta protocol
defines, and the same shape `delta-rs` and the Spark / Delta
integration use.

`openDeltaTableAt v` short-circuits at version `v` instead of the
latest version, returning the snapshot the table was in at that
specific commit.

## Time travel and history

`Delta.IO` exposes the working surface for time-travel queries:

```haskell
openDeltaTable    :: FilePath -> IO (Either String DeltaTable)
openDeltaTableAt  :: FilePath -> Word64 -> IO (Either String DeltaTable)
historyEntries    :: DeltaTable -> IO [HistoryEntry]
activeFilePaths   :: DeltaTable -> [Text]
dtActiveFiles     :: DeltaTable -> [AddAction]
```

`historyEntries` mirrors the result `DESCRIBE HISTORY <table>` returns
in the Spark / Delta integration: one entry per commit, with the
operation kind, operation parameters, timestamp, user metadata, and
read / write metrics where available.

## Testing

```bash
cabal test wireform-delta:delta-test
```

The HUnit suite covers the log parser, the checkpoint reader, the
snapshot aggregator, and the time-travel + history APIs against
fixtures captured from real Delta tables.

### Cross-language interop with `delta-rs`

[`probe/Probe.hs`](probe/Probe.hs) round-trips against
[`delta-rs`](https://github.com/delta-io/delta-rs) (the Rust /
Python Delta Lake binding). The probe creates Delta tables in five
shapes:

- Unpartitioned, no checkpoint.
- Partitioned, no checkpoint.
- Checkpointed (v11), with subsequent APPEND / OVERWRITE commits.
- Partitioned + checkpointed (cross-checks `partitionValues` map +
  `partitionColumns` list out of the checkpoint Parquet).
- Time-travel + history (cross-checks `historyEntries` against
  `DeltaTable.history()` and `openDeltaTableAt v=2` against
  `DeltaTable(.., version=2)`).

Out of scope so far: deletion-vector application (the package reads
deletion-vector references; applying them to a column read is
deferred), column mapping, V2 multi-part checkpoint format.

```bash
pip install deltalake
cabal run wireform-delta-interop-probe
```

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Rust / Python: [`delta-rs`](https://github.com/delta-io/delta-rs),
  the same binding the interop probe uses.
- Java / Scala: the
  [Delta Lake reference implementation](https://github.com/delta-io/delta).
- Kernel: [`delta-kernel-rs`](https://github.com/delta-io/delta-kernel-rs),
  the new shared kernel that the various engine integrations are
  converging on.

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [Delta Lake protocol specification](https://github.com/delta-io/delta/blob/master/PROTOCOL.md)
- [Delta Lake project](https://delta.io/)
- [`delta-rs`](https://github.com/delta-io/delta-rs) (the Rust binding)
