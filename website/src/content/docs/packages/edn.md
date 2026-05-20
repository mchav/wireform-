---
title: wireform-edn
description: "Extensible Data Notation encoding and decoding with Generic deriving, Clojure literals, and a JSON bridge."
sidebar:
  order: 14
---

`wireform-edn` implements Extensible Data Notation (EDN), the text-based data
format used by Clojure and many ClojureScript tools. EDN is human-readable
like JSON but adds keywords, symbols, sets, tagged literals, and richer
numeric types. Use this package when you exchange data with Clojure services,
read EDN configuration files, or need a text format that maps naturally to
Clojure's data model.

EDN is a text format, not a binary codec. Payloads are UTF-8 encoded
documents rather than compact byte streams.

## Key features

- **Generic deriving** via `ToEDN` and `FromEDN` for records and sum types
- **Clojure literals** including keywords, symbols, sets, and tagged values
- **JSON bridge** for converting between EDN and Aeson `Value`
- **Dynamic values** via the untyped `Value` ADT for schema-less parsing
- **Direct encoding** for writing into pre-allocated buffers

## Basic usage

Define a record and derive EDN codecs. Records encode as EDN maps with
keyword keys:

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Point where

import EDN.Class (ToEDN, FromEDN, encodeEDN, decodeEDN)
import GHC.Generics (Generic)

data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToEDN, FromEDN)

toText :: Point -> ByteString
toText pt = encodeEDN pt

fromText :: ByteString -> Either String Point
fromText bs = decodeEDN bs
```

Tagged literals use EDN's `#tag` reader syntax. Build them with the dynamic
ADT when you need custom tags:

```haskell
import EDN.Value qualified as E

uuidTag :: Text -> E.Value
uuidTag s = E.Tagged "" "uuid" (E.String s)
```

Convert between EDN and JSON when bridging to HTTP APIs or Aeson-based tools:

```haskell
import EDN.JSON (toJSON, fromJSON)
import EDN.Value qualified as E
import Data.Aeson (Value)

bridgeToJson :: E.Value -> Value
bridgeToJson edn = toJSON edn

bridgeFromJson :: Value -> E.Value
bridgeFromJson json = fromJSON json
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `EDN.Class` | `ToEDN` / `FromEDN`, `encodeEDN`, `decodeEDN` |
| `EDN.Encode` / `EDN.Decode` | Low-level text encode and decode |
| `EDN.Value` | Dynamic `Value` ADT (keywords, symbols, sets, tags, ...) |
| `EDN.JSON` | EDN ↔ JSON conversion |
| `EDN.Derive` | Template Haskell deriver with `wireform-derive` annotations |
