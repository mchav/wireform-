---
title: wireform-flatbuffers
description: "Google FlatBuffers zero-copy serialization with vtable layout, schema codegen, and shared builder with Arrow IPC."
sidebar:
  order: 34
---

`wireform-flatbuffers` implements [Google FlatBuffers](https://flatbuffers.dev/),
a zero-copy serialization format designed for game engines, mobile apps, and
real-time inference serving. FlatBuffers lay out tables with vtables that index
field offsets, keeping scalars inline and strings or sub-tables behind indirection.
Use this package when you need to read large buffers without deserializing the
entire message, or when sharing the builder infrastructure with Arrow IPC.

## Key features

- **Typeclass API** via `ToFlatBuffers` and `FromFlatBuffers` with Template Haskell deriving
- **FlatBuffers IDL parser and codegen** from `.fbs` schema files
- **Vtable-based wire layout** with inline scalars and offset-indirected collections
- **Zero-copy view/reader** via `FlatBuffers.View` for schema-known access patterns
- **Shared builder** with Arrow IPC for cross-format buffer construction
- **QuasiQuoter** for inline `[flatbuffers| ... |]` schemas

## Basic usage

Derive instances for your table types, then encode and decode through the
value-level codec:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import FlatBuffers.Decode qualified as FBD
import FlatBuffers.Derive (ToFlatBuffers (..), FromFlatBuffers (..), deriveFlatBuffers, deriveView)
import FlatBuffers.Encode qualified as FBE
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Widget = Widget
  { widgetName  :: !Text
  , widgetCount :: !Int32
  , widgetPrice :: !Double
  }
  deriving stock (Show, Eq, Generic)

$(deriveFlatBuffers ''Widget)
$(deriveView ''Widget)

encodeWidget :: Widget -> ByteString
encodeWidget = FBE.encode . toFlatBuffers

decodeWidget :: ByteString -> Either String Widget
decodeWidget bs = do
  val <- FBD.decode bs
  fromFlatBuffers val

sample :: Widget
sample = Widget "wireform" 42 2.718

roundTrip :: Either String Widget
roundTrip = decodeWidget (encodeWidget sample)
```

For zero-copy reads on known schemas, use the view layer after encoding:

```haskell
import FlatBuffers.View (decodeRoot)

readWidget :: ByteString -> Either String Widget
readWidget = decodeRoot
```

Generate types from `.fbs` files:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import FlatBuffers.QQ (fbs)

[fbs|
  table Widget {
    name:string;
    count:int;
    price:double;
  }
  root_type Widget;
|]
```

```bash
wireform-gen flatbuffers -i schema.fbs -o src/Gen/
```

## Performance

### Encode/decode (zero-copy decode)

| Shape | encode | decode |
|-------|--------|--------|
| Person table | 774 ns | 134 ns |
| Person[100] vector | 94.6 µs | 70 ns |

Like Cap'n Proto, FlatBuffers decode is a zero-copy cursor. The decode cost is near-constant because only the root table offset is resolved; field access is lazy pointer arithmetic into the original buffer. Encode is proportional to the number of fields and vector elements.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-flatbuffers/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `FlatBuffers.Derive` | `ToFlatBuffers` / `FromFlatBuffers` and `deriveFlatBuffers` |
| `FlatBuffers.Encode` / `FlatBuffers.Decode` | High-level encoder and value-tree decoder |
| `FlatBuffers.View` | Zero-copy cursor access for schema-known tables |
| `FlatBuffers.Reader` | Low-level pointer-walking decoder |
| `FlatBuffers.Builder` | Vtable and offset builder |
| `FlatBuffers.Value` | Dynamic untyped `Value` ADT |
| `FlatBuffers.Schema` / `FlatBuffers.Parser` | Schema AST and `.fbs` parser |
| `FlatBuffers.CodeGen` / `FlatBuffers.QQ` | Haskell codegen and quasiquoter |
| `FlatBuffers.Registry` | Runtime schema registry |
