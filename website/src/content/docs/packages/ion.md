---
title: wireform-ion
description: "Amazon Ion binary encoding and decoding with TH deriving, Ion Schema Language support, and a QuasiQuoter."
sidebar:
  order: 13
---

`wireform-ion` implements Amazon Ion, a superset of JSON designed for
high-volume structured data in AWS services such as QLDB and Ion-based data
lakes. Ion supports symbols, timestamps, decimals, and a rich type system in
both text and binary forms. Use this package when you exchange Ion payloads
with AWS tooling or need schema-checked Ion documents in Haskell.

## Key features

- **Template Haskell deriving** via `deriveIon` from `Ion.Derive`, with
  `wireform-derive` annotations; Generic defaults (empty instances) work for
  simple uncustomized records
- **Ion Schema Language (ISL)** parser for declarative schema definitions
- **Schema-driven codegen** that emits Haskell types and codec stubs from ISL
- **QuasiQuoter** for embedding Ion text literals at compile time
- **Dynamic values** via the untyped `Value` ADT for exploratory processing

## Basic usage

Derive Ion codecs for a record and round-trip through binary Ion:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Metrics where

import Ion.Class (ToIon, FromIon, encodeIon, decodeIon)
import Ion.Derive (deriveIon)
import GHC.Generics (Generic)
import Data.Text (Text)

data Metric = Metric
  { metricName  :: !Text
  , metricValue :: !Double
  }
  deriving stock (Show, Eq, Generic)

$(deriveIon ''Metric)

publish :: Metric -> ByteString
publish m = encodeIon m

consume :: ByteString -> Either String Metric
consume bs = decodeIon bs
```

For simple records with no custom wire naming, Generic defaults also work:
declare empty `instance ToIon Metric` and `instance FromIon Metric` after
`deriving stock (Show, Eq, Generic)`. Field names go to the wire verbatim and
annotations are not supported.

For schema-first workflows, define types in ISL and splice them at compile
time with the QuasiQuoter:

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module MetricsSchema where

import Ion.QQ (isl)

[isl|
  type::{ name: Metric, fields: { name: string, value: float } }
|]
```

This parses the ISL definition and generates a Haskell record with `ToIon`
and `FromIon` instances that match the schema field names and types.

## Performance

### Encode/decode (binary Ion)

| Payload | encode | decode |
|---------|--------|--------|
| Person | 324 ns | 410 ns |
| [Person] x 100 | 38.5 µs | 41.7 µs |

Sub-microsecond single-record performance. Batch operations scale linearly.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-ion/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Ion.Class` | `ToIon` / `FromIon`, `encodeIon`, `decodeIon` |
| `Ion.Encode` / `Ion.Decode` | Low-level binary Ion encode and decode |
| `Ion.Value` | Dynamic untyped `Value` ADT |
| `Ion.SchemaLang` | Ion Schema Language parser |
| `Ion.ISLSchema` / `Ion.ISLCodeGen` | ISL AST and Haskell code generator |
| `Ion.QQ` | QuasiQuoter for Ion text literals |
| `Ion.Derive` | Template Haskell deriver with `wireform-derive` annotations |
