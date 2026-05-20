---
title: wireform-parquet
description: "Apache Parquet reader and writer with full encoding support, compression, bloom filters, page indexes, column encryption, predicate pushdown, and an Arrow bridge."
sidebar:
  order: 40
---

`wireform-parquet` implements the Apache Parquet columnar file format. Parquet
is the on-disk format behind most data warehouses and lakehouse table formats
(Iceberg, Delta Lake, Hudi). Use this package when you need to read or write
Parquet files directly in Haskell, with support for the encodings and
compression codecs that real-world writers emit.

## Key features

- **Full read and write** via `Parquet.HighLevel` and lower-level page APIs
- **All major encodings**: PLAIN, dictionary, DELTA_BINARY_PACKED,
  BYTE_STREAM_SPLIT, and hybrid RLE index pages
- **Compression codecs** behind Cabal flags: Snappy, Zstd, LZ4, Gzip, and
  Brotli
- **Bloom filters** and **page indexes** for sub-row-group predicate pruning
- **Modular column encryption** (AES-GCM) per the Parquet Modular Encryption
  spec
- **Predicate pushdown** over footer statistics, page indexes, and bloom filters
- **Nested columns** (lists, maps, structs) via `Parquet.Nested`
- **Arrow bridge** for typed record batches through `Parquet.Arrow`
- **Template Haskell deriver** via `Parquet.Derive`
- **Interop-tested** against pyarrow

## Basic usage

Most callers start with the high-level decode API for in-memory bytes, or
`openParquetReader` for mmap-aware streaming over large files on disk:

```haskell
import qualified Data.Vector        as V
import qualified Parquet.HighLevel  as PH
import qualified Parquet.Read       as PR
import qualified Parquet.Types      as PT

readParquetBytes :: ByteString -> IO ()
readParquetBytes bytes =
  case PH.decodeParquet PH.defaultReadOptions bytes of
    Left err ->
      putStrLn err
    Right pf ->
      let fm = PR.pfFooter pf
      in putStrLn $
           "rows="
             ++ show (PT.fmNumRows fm)
             ++ " rowGroups="
             ++ show (V.length (PT.fmRowGroups fm))

readParquetFile :: FilePath -> IO ()
readParquetFile path = do
  result <- PR.openParquetReader path
  case result of
    Left err ->
      putStrLn err
    Right (pf, _rowGroupIter) ->
      let fm = PR.pfFooter pf
      in putStrLn $
           "rows="
             ++ show (PT.fmNumRows fm)
             ++ " rowGroups="
             ++ show (V.length (PT.fmRowGroups fm))
```

For writing, pass an Arrow-shaped schema and column batches to
`encodeParquet`:

```haskell
import qualified Parquet.HighLevel as PH

writeParquet :: PH.Schema -> [V.Vector PH.ColumnData] -> ByteString
writeParquet schema rowGroups =
  PH.encodeParquet PH.defaultWriteOptions schema rowGroups
```

When you need projection or filter pushdown without loading every column,
use the predicate and aggregate modules together with the Arrow bridge or
the cross-format `Wireform.Columnar` facade.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Parquet.HighLevel` | `encodeParquet`, `decodeParquet`, `WriteOptions`, `ReadOptions` |
| `Parquet.Read` | `loadParquetFilePath`, `openParquetReader`, column chunk decoders |
| `Parquet.Write` | Page encoders, row group assembly, `buildParquetFile` |
| `Parquet.Footer` | Thrift-encoded footer parse and emit |
| `Parquet.Page` / `Parquet.PageIndex` | Data page headers and per-page statistics |
| `Parquet.BloomFilter` | Split-block bloom filter decode |
| `Parquet.Encryption` | Column-level and footer encryption (PME, AES-GCM) |
| `Parquet.Predicate` | Statistics and bloom-filter predicate evaluation |
| `Parquet.Aggregate` | `count(*)`, `count(col)`, `min`, `max` from footer stats |
| `Parquet.Arrow` | Parquet columns to Arrow `ColumnArray` bridge |
| `Parquet.Derive` | Template Haskell deriver with `wireform-derive` annotations |

## Interop

The reader handles files produced by pyarrow, parquet-cpp, and arrow-rs,
including dictionary-encoded strings, delta-packed integers, and
BYTE_STREAM_SPLIT floats. Cross-language round-trip tests live in the
package probe suite.
