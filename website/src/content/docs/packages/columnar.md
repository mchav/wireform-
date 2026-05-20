---
title: wireform-columnar
description: "Unified columnar API across Parquet, Arrow, and ORC with streaming iterators, projection, predicate pushdown, partitioned datasets, and mmap-aware loading."
sidebar:
  order: 47
---

`wireform-columnar` provides a single Arrow-shaped API over wireform's three
columnar formats: Parquet, Arrow IPC, and ORC. Analytics pipelines often
need to switch wire formats without rewriting query logic. Use this package
when you want one encode/decode surface, pull-based streaming, and shared
predicate pushdown across formats.

## Key features

- **Single API** across Parquet, Arrow IPC, and ORC via `Wireform.Columnar`
- **Pull-based `Iter` and `IterIO` streaming** in `Columnar.Stream`
- **Projection and predicate pushdown** to skip unused columns and files
- **Partitioned multi-file datasets** with heterogeneous format support
- **Mmap-aware file loading** via `Columnar.IO` (mmap above 64 KiB by default)
- **SIMD bit-unpacking** for validity bitmaps and packed integers

## Basic usage

Pick a format, pass an Arrow schema and column batches, and encode:

```haskell
import qualified Wireform.Columnar as Col
import qualified Arrow.Types       as AT

encodeBatch
  :: Col.Format
  -> AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> ByteString
encodeBatch fmt schema batches =
  Col.encode fmt Col.defaultWriteOptions schema batches
```

Decode with projection so only the columns your query needs are materialized:

```haskell
import qualified Wireform.Columnar as Col

readProjected
  :: ByteString
  -> IO (Either String (AT.Schema, Col.Iter (V.Vector AC.ColumnArray)))
readProjected bytes = do
  case Col.decodeProjectedIter Col.Parquet Col.defaultReadOptions ["user_id", "amount"] bytes of
    Left err ->
      pure (Left err)
    Right (schema, iter) ->
      pure (Right (schema, iter))
```

Apply a filter predicate during decode to push selection into footer
statistics and format-specific indexes:

```haskell
import qualified Columnar.Predicate as P
import qualified Wireform.Columnar as Col

filteredIter
  :: ByteString
  -> Either String (AT.Schema, Col.Iter (V.Vector AC.ColumnArray))
filteredIter bytes =
  let pred =
        P.PAnd
          (P.PCol "amount" (P.PGt (P.PVInt64 1000)))
          (P.PCol "region" (P.PEq (P.PVText "us-west")))
  in Col.decodeFilteredIter Col.Parquet Col.defaultReadOptions pred bytes
```

For multi-file datasets, `decodeDatasetIter` and
`decodePartitionedDataset` stitch together partitioned directory trees.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Wireform.Columnar` | Unified encode/decode facade (`Format`, `encode`, `decodeIter`) |
| `Columnar.Stream` | `Iter`, `IterIO`, and streaming combinators |
| `Columnar.Predicate` | Shared `PValue` / `Predicate` vocabulary for pushdown |
| `Columnar.IO` | `loadFile`, `loadFileMmap`, `loadFileEager` |
| `Columnar.SIMD` | SIMD-accelerated bit unpack and RLE kernels |

## When to drop down

The unified facade covers the common 80% case. Reach for the per-format
modules when you need Parquet bloom filters, ORC stripe encryption, or
Arrow streaming IPC details that live in `Parquet.*`, `ORC.*`, or
`Arrow.*` directly.
