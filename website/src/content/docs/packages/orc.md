---
title: wireform-orc
description: "Apache ORC reader and writer with stripe-level access, RLE encodings, compression, bloom filters, statistics pushdown, stripe encryption, and an Arrow bridge."
sidebar:
  order: 42
---

`wireform-orc` implements the Apache ORC columnar file format. ORC is the
storage format behind Hive and many Hadoop-era data lakes, optimized for
large sequential scans with lightweight indexes per stripe. Use this
package when you need to read ORC files in Haskell, evaluate predicates
from stripe statistics, or bridge ORC columns into Arrow record batches.

## Key features

- **Stripe-level read** with lazy footer and stripe footer parsing
- **RLE v1 and v2** decoders for integer, boolean, and string columns
- **Compression** (Snappy, Zstd, LZ4) with configurable block sizes
- **Bloom filters** for equality predicate pruning at the stripe level
- **Statistics-based pushdown** via `ORC.Statistics`
- **Stripe encryption** for encrypted ORC files
- **Row indexes** for sub-stripe seek within a column
- **Arrow bridge** through `ORC.Arrow` and the `Wireform.Columnar` facade

## Basic usage

Load an ORC file from disk and inspect its stripe layout:

```haskell
import qualified Data.Vector as V
import qualified ORC.Read    as OR
import qualified ORC.Types   as OT

inspectOrcFile :: FilePath -> IO ()
inspectOrcFile path = do
  result <- OR.loadORCFilePath path
  case result of
    Left err ->
      putStrLn err
    Right orcFile -> do
      let footer = OR.ofFooter orcFile
      putStrLn $
        "stripes="
          ++ show (V.length (OT.orcStripes footer))
          ++ " rows="
          ++ show (OT.orcNumberOfRows footer)
```

Decode a single integer column from the first stripe:

```haskell
readIntColumn :: OR.ORCFile -> Either String (V.Vector (Maybe Int64))
readIntColumn orcFile =
  OR.readColumn orcFile 0 0 0
```

For filtered scans, build a predicate with `Columnar.Predicate` and pass
it to `ORC.streamStripesFilteredIter` or the unified
`Wireform.Columnar.decodeFilteredIter` entry point.

## Notable modules

| Module | Purpose |
|--------|---------|
| `ORC.Read` | `loadORCFilePath`, `openORCReader`, column decoders |
| `ORC.Write` | Stripe and file writer |
| `ORC.Footer` | File footer parse and metadata |
| `ORC.Stripe` | Stripe footer protobuf and stream layout |
| `ORC.Statistics` | Column statistics predicate evaluator |
| `ORC.BloomFilter` | Bloom filter decode and membership checks |
| `ORC.Encryption` | Stripe-level encryption |
| `ORC.RowIndex` | Row index decode for sub-stripe seek |
| `ORC.Arrow` | ORC to Arrow column bridge |
| `ORC.Derive` | Template Haskell deriver |

## Interop

ORC files written by Hive, Spark, and Trino round-trip through the reader
when they use the standard RLE and compression layouts exercised by the
package test fixtures.
