---
title: wireform-arrow
description: "Apache Arrow IPC with schema framing, record batch encode/decode, typed record APIs, table projection, and optional zstd/lz4 compression."
sidebar:
  order: 41
---

`wireform-arrow` implements Apache Arrow IPC and the Arrow columnar data
model in Haskell. Arrow is the interchange format between analytics engines,
dataframe libraries, and columnar storage. Use this package when you need
typed record batches, schema-aware encoding, or a shared column vocabulary
that Parquet and ORC readers in wireform can target.

## Key features

- **Schema framing** and IPC message encode/decode via `Arrow.IPC`
- **Record batch** encode and decode for in-memory columnar data
- **Typed record API** with Template Haskell and Generic support
- **Table projection and subsetting** to read only the columns you need
- **Optional compression** (Zstd and LZ4 behind Cabal flags)
- **SIMD buffer validation** for record batch integrity checks
- **Streaming reader** for framed IPC streams in `Arrow.Stream`

## Basic usage

Define a record type, build a `Table`, and round-trip through IPC bytes:

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Trades where

import Arrow.Record
import Arrow.IPC (encodeIPCMessage, decodeIPCMessage)
import Arrow.Types (Message(..), RecordBatch(..))
import GHC.Generics (Generic)
import Data.Text (Text)

data Trade = Trade
  { tradeSym  :: !Text
  , tradeQty  :: !Int32
  , tradeNote :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

tradeTable :: Table Trade
tradeTable = table enc dec
  where
    enc =
      fieldE "sym"  tradeSym  utf8E
        <> fieldE "qty"  tradeQty  int32E
        <> fieldE "note" tradeNote (nullable utf8E)
    dec =
      Trade
        <$> columnD "sym"  utf8D
        <*> columnD "qty"  int32D
        <*> columnD "note" (nullableD utf8D)

roundTripTrades :: V.Vector Trade -> Either String (V.Vector Trade)
roundTripTrades trades = do
  let batches = encodeTable tradeTable trades
  msg <- decodeIPCMessage (encodeIPCMessage (RecordBatch (head batches)))
  case msg of
    RecordBatch rb -> decodeTable tradeTable rb
    _              -> Left "expected RecordBatch message"
```

Project column batches down to a subset of fields when the full schema is
larger than what your query needs:

```haskell
import Arrow.Record (projectTable, subsetTable)
import Arrow.Types (Schema)

projectSymQty :: Schema -> V.Vector ColumnArray -> Maybe (Schema, V.Vector ColumnArray)
projectSymQty schema cols =
  projectTable ["sym", "qty"] schema cols
```

For file-level IPC (the Arrow file format), use `Arrow.File` and
`Arrow.Stream` to read length-prefixed message sequences.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Arrow.Types` | Schema, field, and buffer types; `schemaFingerprint` |
| `Arrow.Column` | Column array builders and validators |
| `Arrow.Record` | Typed `Table`, `Encoder`, `Decoder`, projection helpers |
| `Arrow.Record.Generic` / `Arrow.Record.TH` | Generic and TH record derivation |
| `Arrow.Derive` | Annotation-driven deriver |
| `Arrow.IPC` | IPC message framing encode/decode |
| `Arrow.Stream` | Pull-based streaming IPC reader |
| `Arrow.File` | Arrow file format reader and writer |
| `Arrow.FlatBufferIPC` | FlatBuffer-backed IPC metadata path |
| `Arrow.Write` | Record batch and file writer |

## Compression

Enable Zstd or LZ4 with Cabal flags (`+zstd`, `+lz4`). Compressed IPC
streams follow the standard Arrow body compression layout; uncompressed
IPC remains the default for maximum interoperability.
