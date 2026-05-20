---
title: wireform-lance
description: "Lance columnar format reader with data file envelopes, versioned dataset manifests, and protobuf-based metadata."
sidebar:
  order: 46
---

`wireform-lance` implements a reader for the Lance columnar format and its
dataset layout. Lance targets ML and vector workloads with a versioned
manifest tree over fragment data files. Use this package when you need to
inspect Lance file footers, enumerate dataset versions, or discover active
data files from Haskell.

## Key features

- **Data file envelope** with 40-byte footer and fragment metadata
- **Dataset layout** with versioned manifests under `_versions/`
- **Protobuf-based metadata** generated from Lance `.proto` schemas
- **Version time travel** via `openLanceDatasetAt`
- **Interop-tested** against pylance

## Basic usage

Open a Lance dataset and inspect its active fragments and schema:

```haskell
import qualified Lance.IO as L

inspectDataset :: FilePath -> IO ()
inspectDataset datasetRoot = do
  result <- L.openLanceDataset datasetRoot
  case result of
    Left err ->
      putStrLn err
    Right ds -> do
      putStrLn $
        "version="
          ++ show (L.ldLatestVersion ds)
          ++ " data files="
          ++ show (length (L.ldDataFiles ds))
      mapM_ print (L.datasetSchemaFields ds)
```

Read a single `.lance` data file and inspect its envelope:

```haskell
openSingleFile :: FilePath -> IO ()
openSingleFile filePath = do
  result <- L.openLanceFile filePath
  case result of
    Left err ->
      putStrLn err
    Right file -> do
      putStrLn $
        "columns="
          ++ show (L.lfNumColumns (L.lfFooter file))
          ++ " footer ok"
```

List every committed manifest version under a dataset root:

```haskell
listVersions :: FilePath -> IO ()
listVersions datasetRoot = do
  versions <- L.findManifestVersions datasetRoot
  mapM_ (\(v, path) -> putStrLn (show v ++ " " ++ path)) versions
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Lance.Format` | Data file envelope and footer decode |
| `Lance.IO` | `openLanceFile`, `openLanceDataset`, manifest discovery |
| `Lance.Manifest` | Manifest protobuf decode, active file enumeration |
| `Lance.Pb.Lance.File` / `Lance.Pb.Lance.Table` | Generated protobuf types |

## Dataset layout

A Lance dataset on disk looks like:

```
/<root>.lance/
  data/<fragment-uuid>.lance
  _versions/<inv-version>.manifest
  _transactions/<id>.txn
```

Manifest filenames use an inverted version convention so directory listings
sort newest first. The decoders surface the real version number to callers.

## Interop

The probe suite cross-checks file footers, manifest bodies, active fragment
lists, schema readouts, and version timestamps against pylance.
