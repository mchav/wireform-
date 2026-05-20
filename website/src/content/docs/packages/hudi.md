---
title: wireform-hudi
description: "Apache Hudi Copy-on-Write timeline reader with JSON and Avro instant payloads, replace-commit and clean handling, file slice tracking, and time travel."
sidebar:
  order: 45
---

`wireform-hudi` implements an Apache Hudi timeline reader for Copy-on-Write
tables. Hudi tracks every table mutation as an instant on a timeline under
`.hoodie/`, with commit metadata in JSON or Avro. Use this package when you
need to discover active base files, replay replace commits, or time-travel
to a specific instant from Haskell.

## Key features

- **JSON and Avro instant payloads** for commit, deltacommit, and cleaner actions
- **Replace-commit handling** that supersedes prior file slices
- **Clean instant handling** that removes obsolete files from the snapshot
- **File slice tracking** with `FileSlice` and `TableState`
- **Time travel** to any committed instant
- **Interop-tested** against hudi-rs

## Basic usage

Open a Hudi table and list the active base file paths:

```haskell
import qualified Hudi.IO as H
import qualified Hudi.Timeline as HT
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

listActiveBaseFiles :: FilePath -> IO ()
listActiveBaseFiles tableRoot = do
  result <- H.openHudiTable tableRoot
  case result of
    Left err ->
      putStrLn err
    Right table ->
      mapM_ (putStrLn . T.unpack) (H.activeBaseFilePaths table)
```

Time-travel to a specific instant and inspect the file slices:

```haskell
import qualified Hudi.Timeline as HT

inspectAtInstant :: FilePath -> Text -> IO ()
inspectAtInstant tableRoot instantTime = do
  result <- H.openHudiTableAt tableRoot instantTime
  case result of
    Left err ->
      putStrLn err
    Right table -> do
      let partitions = H.tsPartitions (H.hutState table)
      putStrLn $
        "instant="
          ++ T.unpack instantTime
          ++ " partitions="
          ++ show (Map.size partitions)
```

For lower-level timeline inspection, `scanTimeline` returns every parseable
instant file together with its absolute path, sorted by timestamp and state.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Hudi.Timeline` | Instant parsing, commit metadata, `TableState` |
| `Hudi.Avro` | Avro 1.x+ instant payload decoder |
| `Hudi.IO` | `openHudiTable`, `openHudiTableAt`, active file helpers |

## Timeline model

Hudi Copy-on-Write tables accumulate base Parquet files through commit
instants. A replacecommit instant drops file slices listed in
`partitionToReplaceFileIds`. A clean instant removes files that are no
longer referenced. `openHudiTable` replays the timeline in order to
produce the current `TableState`.

## Interop

The probe suite verifies JSON instants, Avro 1.x+ instants, and
replacecommit handling against hudi-rs, including `INSERT_OVERWRITE`
semantics that drop replaced file IDs.
