---
title: wireform-fory
description: "Apache Fory cross-language serialization with reference tracking, meta-string compression, and pyfory 0.17 wire compatibility."
sidebar:
  order: 16
---

`wireform-fory` implements Apache Fory (formerly Apache Fury), a
cross-language serialization format optimized for RPC and data exchange
between JVM, Python, and other runtimes. Fory supports reference tracking,
meta-string compression, schema-hashed named structs, and chunked collections.
Use this package when you need wire-compatible payloads with Python services
using `pyfory` 0.17, or when shared subgraphs and large string tables make
reference tracking worthwhile.

Fory is more configuration-heavy than CBOR or MessagePack. Encoder options,
struct registries, and schema registration affect the on-wire layout.

## Key features

- **Template Haskell deriving** via `deriveFory` for records and algebraic
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Reference tracking** to deduplicate shared objects and cyclic graphs on
  the wire
- **Meta-string compression** for repeated field and type names
- **Named structs with schema hash** for pyfory-compatible `NAMED_STRUCT`
  layout
- **Chunked collections** for lists, sets, and maps with homogeneous element
  types
- **One-dimensional primitive arrays** (`BoolArray`, `Int32Array`, ...) with
  byte-identical layouts to pyfory's NumPy serializer
- **Wire compatibility** with `pyfory` 0.17 for the supported type set

## Basic usage

Derive Fory codecs with the Template Haskell deriver and round-trip through
the default encoder:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Event where

import Fory.Class (ToFory, FromFory, encodeFory, decodeFory)
import Fory.Derive (deriveFory)
import GHC.Generics (Generic)
import Data.Text (Text)

data Event = Event
  { eventId   :: !Int64
  , eventName :: !Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveFory ''Event)

send :: Event -> ByteString
send ev = encodeFory ev

receive :: ByteString -> Either String Event
receive bs = decodeFory bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToFory Event` and
`instance FromFory Event` declarations.

When the same object appears more than once in a graph, enable reference
tracking so subsequent occurrences encode as back-references:

```haskell
import Fory.Class (toFory)
import Fory.Encode (encodeWith)
import Fory.Options qualified as O

encodeWithRefs :: Event -> ByteString
encodeWithRefs ev =
  encodeWith (O.defaultEncodeOptions { O.eoRefTracking = True }) (toFory ev)
```

For pyfory-compatible named structs, register schemas in the encoder options
so the wire layout includes the 4-byte fingerprint hash:

```haskell
import Fory.Options qualified as O
import Fory.Struct (StructSchema, mkSchema)
import Fory.TypeId (INT32, STRING)

personSchema :: StructSchema
personSchema =
  mkSchema "myapp" "Person"
    [ ("name", STRING)
    , ("age", INT32)
    ]

encodePersonOpts :: O.EncodeOptions
encodePersonOpts =
  O.defaultEncodeOptions
    { O.eoStructRegistry = O.registerStruct personSchema O.emptyStructRegistry
    }
```

Use the primitive array newtypes when exchanging numeric buffers with Python
NumPy code:

```haskell
import Fory.Class (Int32Array(..), ToFory, FromFory, encodeFory, decodeFory)
import qualified Data.Vector.Storable as VS

timeseries :: VS.Vector Int32 -> ByteString
timeseries vec = encodeFory (Int32Array vec)

readTimeseries :: ByteString -> Either String (VS.Vector Int32)
readTimeseries bs = do
  Int32Array vec <- decodeFory bs
  pure vec
```

## Performance

### Encode/decode across representative shapes

| Shape | encode | decode |
|-------|--------|--------|
| int | 76 ns | 64 ns |
| string | 84 ns | 78 ns |
| bytes (1 KB) | 127 ns | 62 ns |
| Person struct | 262 ns | 456 ns |
| list[Person] x 100 | 6.7 µs | 10.7 µs |

Scalar encode/decode runs under 130 ns. Struct payloads are sub-microsecond. The 100-element list benchmark shows ~67 ns per element on encode and ~107 ns per element on decode, competitive with Fory implementations in other languages.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-fory/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Fory.Class` | `ToFory` / `FromFory`, primitive array newtypes, `Shared` wrapper |
| `Fory.Encode` / `Fory.Decode` | Pure encode and decode entry points |
| `Fory.IO` | In-place buffer encoder with ref and meta-string pools |
| `Fory.Options` | `EncodeOptions` / `DecodeOptions`, struct registry |
| `Fory.Struct` | `StructSchema` definitions for named struct wire layout |
| `Fory.Value` | Dynamic untyped value ADT |
| `Fory.MetaString` | Meta-string compression tables and encodings |
| `Fory.TypeId` | Wire type identifiers |
| `Fory.Derive` | Annotation-driven deriver with field renaming support |

## Interoperability

The package is verified against `pyfory` 0.17 for null, booleans, integers,
floats, strings, binary, chunked lists/sets/maps, named structs with
registered schemas, primitive arrays, reference tracking, and meta-string
compression. Cross-language interop for `NAMED_COMPATIBLE_STRUCT` (schema
evolution) is still in progress.
