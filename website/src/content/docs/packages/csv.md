---
title: wireform-csv
description: "CSV, TSV, and pipe-separated encoding and decoding with TH deriving, streaming rows, and SIMD scanning."
sidebar:
  order: 24
---

`wireform-csv` handles delimiter-separated tabular data in Haskell. Spreadsheet
exports, log pipelines, and ETL jobs often arrive as CSV or TSV; this package
parses them with RFC 4180 semantics, configurable delimiters, and
SIMD-accelerated byte scanning. Derive `ToCSV`/`FromCSV` for typed rows, or use
the streaming API when files are too large to load at once.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveCSV` Template Haskell deriver | Map header rows to Haskell records with `wireform-derive` annotations; Generic defaults work for simple cases |
| Configurable delimiters | CSV (`,`), TSV (`\t`), pipe, or custom separators |
| Quoting and escaping | RFC 4180 quoted fields with embedded delimiters |
| Streaming row callbacks | `decodeStream` processes one row at a time with constant memory |
| SIMD newline and delimiter scan | `Wireform.FFI.findByteBS` accelerates field boundaries |
| Header row handling | Skip or capture the first row via `CSVConfig` |

## Basic usage

### Typed rows

Define a record, derive codecs with the Template Haskell deriver, and decode
an entire file into a `Vector` of rows.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import CSV.Class (ToCSV, FromCSV)
import CSV.Derive (deriveCSV)
import CSV.Decode (decodeRecords)
import CSV.Encode (encodeRecords)
import CSV.Value (defaultCSV)

data Row = Row
  { name  :: !Text
  , email :: !Text
  , score :: !Int
  } deriving stock (Generic)

$(deriveCSV ''Row)

loadRows :: ByteString -> Either String (Vector Row)
loadRows bs = decodeRecords defaultCSV bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToCSV Row` and
`instance FromCSV Row` declarations.

Use `defaultTSV` from `CSV.Value` when the input is tab-separated.

### Custom delimiter configuration

Build a `CSVConfig` when the file uses non-standard separators or omits a
header row.

```haskell
import CSV.Value (CSVConfig(..), CSVDocument(..), defaultCSV)
import CSV.Decode (decode)

pipeConfig :: CSVConfig
pipeConfig = defaultCSV
  { csvDelimiter = '|'
  , csvHasHeader = True
  }

parsePipeFile :: ByteString -> Either String CSVDocument
parsePipeFile = decode pipeConfig
```

### Streaming decode

For large inputs, `decodeStream` invokes a callback per row instead of
allocating a vector of the entire file.

```haskell
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import CSV.Decode (decodeStream)
import CSV.Value (defaultCSV)

streamRows :: ByteString -> (Vector Text -> IO ()) -> IO (Either String ())
streamRows bs handleRow = decodeStream defaultCSV bs handleRow

printEachRow :: ByteString -> IO ()
printEachRow bs =
  void $ streamRows bs $ \row ->
    print (V.toList row)
```

Each callback receives a `Vector Text` of fields for one row. Combine with
`fromCSVRow` inside the callback when you want typed values row by row.

## Performance

### Encode/decode

| Rows | encode | decode |
|------|--------|--------|
| 10 | 5.3 µs | 10.7 µs |
| 1000 | 640 µs | 1.40 ms |

Encode runs at roughly 640 ns per row at scale. Decode is about 2x slower due to field-type parsing.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-csv/bench-results/` for raw data.

## Notable modules

| Module | Role |
|--------|------|
| `CSV.Class` | `ToCSV` / `FromCSV` and Generic helpers |
| `CSV.Value` | `CSVDocument`, `CSVConfig`, `defaultCSV`, `defaultTSV` |
| `CSV.Decode` | `decode`, `decodeStream`, `decodeRecords` |
| `CSV.Encode` | `encode`, `encodeRecords` |
| `CSV.Derive` | Template Haskell deriver |
