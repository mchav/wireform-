---
title: wireform-delta
description: "Delta Lake transaction log reader with JSON commit replay, Parquet checkpoints, schema parsing, time travel, version history, and partition tracking."
sidebar:
  order: 44
---

`wireform-delta` implements a Delta Lake transaction log reader. Delta Lake
stores table state as an append-only JSON commit log under `_delta_log/`,
with periodic Parquet checkpoints to avoid replaying every commit. Use this
package when you need to discover active data files, inspect table history,
or time-travel to a specific table version from Haskell.

## Key features

- **JSON commit replay** via `Delta.Log` action parsers
- **Checkpoint Parquet** reader that reconstructs add/remove/metadata actions
- **Schema parsing** from `metaData` actions
- **Time travel** to any committed version
- **Version history** matching the `DESCRIBE HISTORY` surface
- **Partition tracking** with `partitionValues` on active files
- **Interop-tested** against delta-rs

## Basic usage

Open a Delta table at its latest version and list the active data files:

```haskell
import qualified Delta.IO as DT
import qualified Data.Text as T

listActiveFiles :: FilePath -> IO ()
listActiveFiles tableRoot = do
  result <- DT.openDeltaTable tableRoot
  case result of
    Left err ->
      putStrLn err
    Right table -> do
      mapM_ (putStrLn . T.unpack) (DT.activeFilePaths table)
      putStrLn $
        "version="
          ++ show (DT.dtVersion table)
          ++ " active files="
          ++ show (length (DT.activeFilePaths table))
```

Time-travel to a specific version and inspect commit history:

```haskell
inspectHistory :: FilePath -> IO ()
inspectHistory tableRoot = do
  latest <- DT.openDeltaTable tableRoot
  case latest of
    Left err ->
      putStrLn err
    Right table -> do
      history <- DT.historyEntries table
      mapM_ print history
      atV2 <- DT.openDeltaTableAt tableRoot 2
      case atV2 of
        Left err ->
          putStrLn err
        Right snap ->
          putStrLn $
            "files at v2: "
              ++ show (length (DT.activeFilePaths snap))
```

For lower-level control, parse individual commit lines with `parseLogLine`
from `Delta.Log`, or read a checkpoint directly through `Delta.Checkpoint`.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Delta.Log` | Commit action types, `parseLogLine`, `snapshotFromActions` |
| `Delta.Checkpoint` | Parquet checkpoint decoder |
| `Delta.IO` | `openDeltaTable`, `openDeltaTableAt`, `historyEntries` |
| `Delta.Log` (re-exported) | `TableSnapshot`, `applyAction`, schema helpers |

## Reading algorithm

`openDeltaTable` follows the Delta protocol:

1. Read `_delta_log/_last_checkpoint` if present.
2. Load the referenced checkpoint Parquet file.
3. Replay JSON commits with version numbers above the checkpoint.
4. Return the resulting `DeltaTable` snapshot.

If no checkpoint exists, the reader replays the full commit log from version 0.

## Interop

The probe suite cross-checks active files, partition values, checkpoint
reconstruction, and time-travel versions against delta-rs on fixtures from
real Delta tables.
