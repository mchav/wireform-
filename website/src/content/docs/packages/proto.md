---
title: Protocol Buffers
description: "Full proto2/proto3 support: IDL parser, code generation, TH splicing, JSON mapping, well-known types, and gRPC."
sidebar:
  order: 4
---

`wireform-proto` implements Protocol Buffers from the `.proto` IDL down to the
wire. It includes a parser, a code generator, Template Haskell splicing,
proto3 JSON mapping, text format, well-known types, extensions, dynamic
messages, and a type registry. It is the largest and oldest package in the
wireform ecosystem.

## Three ways to get Haskell types from `.proto` files

| Approach | When to use |
|----------|-------------|
| `$(loadProto "file.proto")` | Small projects where TH is acceptable; types land in the same module |
| `wireform-gen proto -i file.proto -o gen/` | Larger projects; CI-friendly; commit generated code |
| `protoc --wireform_out=DIR` | Organizations that standardize on `protoc` plugins |

All three produce the same output: record types with `MessageEncode`,
`MessageDecode`, and `MessageSize` instances, plus Aeson JSON instances and
`ProtoMessage` metadata.

## Template Haskell splicing

The fastest way to get started:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module MyModule where

import Proto.TH (loadProto)

$(loadProto "proto/person.proto")
```

This parses the `.proto` file at compile time and splices Haskell types into
your module. For files that import other `.proto` files, pass include
directories:

```haskell
import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["proto", "."] } "proto/api.proto")
```

### What gets generated

For each message, the splice produces:

- A Haskell record type with strict fields
- `MessageEncode` / `MessageSize` / `MessageDecode` instances
- `ToJSON` / `FromJSON` instances (proto3 canonical JSON)
- `Hashable`, `NFData`, `ProtoMessage` instances
- For enums: a sum type with an `Unknown` constructor for forward compatibility

### LoadOpts

| Field | Default | Effect |
|-------|---------|--------|
| `loIncludeDirs` | `["proto/", "."]` | Search paths for imports |
| `loFieldNaming` | `PrefixedFields` | `PrefixedFields` or `UnprefixedFields` |
| `loRepConfig` | `defaultRepConfig` | How proto types map to Haskell types |
| `loTHHooks` | none | Inject extra declarations per message |

## Standalone code generation

For projects where TH is not desirable:

```bash
cabal exec wireform-gen -- proto -i proto/person.proto -o gen/
```

This writes `.hs` files to `gen/`. Add `gen` to `hs-source-dirs` in your
`.cabal` file and list the generated modules in `exposed-modules` or
`other-modules`.

The `GenerateOpts` type controls output:

| Option | Default | Effect |
|--------|---------|--------|
| `genModulePrefix` | `"Proto.Gen"` | Haskell module namespace |
| `genFieldNaming` | `PrefixedFields` | Field naming convention |
| `genStrictFields` | `True` | Strict fields (bang patterns) |
| `genUnpackPrims` | `True` | `UNPACK` on numeric fields |
| `genDeriveGeneric` | `True` | Derive `Generic` |
| `genPackedRepeated` | `True` | Use packed encoding for repeated fields |
| `genLazySubmessages` | `False` | Lazy decode of nested messages |

## Encoding and decoding

The `Proto` umbrella module re-exports the primary API:

```haskell
import Proto

let bytes = encodeMessage myMessage
case decodeMessage bytes of
  Right msg -> use msg
  Left err  -> handleError err
```

### Encode

`encodeMessage` does a two-pass encode: first `messageSize` computes the exact
byte count, then `buildMessage` writes into a pre-allocated buffer. This avoids
the intermediate chunk copies that a streaming `Builder` would produce.

For streaming or framed output:

| Function | Use case |
|----------|----------|
| `encodeMessageLazy` | Lazy `ByteString` |
| `hPutMessage` | Write directly to a handle |
| `hPutMessageLen` | Length-prefixed framing |
| `buildMessageFramed` | gRPC-style length-delimited framing |

### Decode

`decodeMessage` returns `Either DecodeError a`. The decoder uses unboxed sums
internally, so the success path allocates only the final Haskell value. Unknown
fields are captured and round-tripped if the type has `HasExtensions`.

## Annotation-driven deriving

If you have hand-written Haskell types and want proto instances without a
`.proto` file, use `Proto.TH.Derive`:

```haskell
import Proto.TH.Derive (deriveProto)
import Wireform.Derive (tag, wireOverride, WireOverride(..))

data Event = Event
  { eventId   :: !Int64
  , eventName :: !Text
  , eventTime :: !Word64
  }

{-# ANN eventId   (tag 1) #-}
{-# ANN eventName (tag 2) #-}
{-# ANN eventTime (tag 3) #-}

deriveProto ''Event
```

Every field needs an explicit `tag`. The deriver supports `Maybe` fields,
repeated fields (`Vector`, `[]`), `Map`, oneofs, enums, and wire overrides
like `wireOverride WireZigZag`.

## Representation adapters

Proto fields map to Haskell types through configurable adapters in
`Proto.Repr`. The defaults are:

| Proto type | Default Haskell type |
|------------|---------------------|
| `string` | strict `Text` |
| `bytes` | strict `ByteString` |
| `repeated T` | `Vector T` |
| `map<K,V>` | `Map K V` (ordered) |

Override these via `RepConfig`:

```haskell
import Proto.Repr

myRepConfig = defaultRepConfig
  { configDefault = defaultFieldRep
      { fieldRepeated = listAdapter       -- use [] instead of Vector
      , fieldMap      = hashMapAdapter    -- use HashMap instead of Map
      }
  }

$(loadProtoWith defaultLoadOpts { loRepConfig = myRepConfig } "proto/api.proto")
```

Available adapters include `lazyTextAdapter`, `shortTextAdapter`,
`lazyBytesAdapter`, `shortBytesAdapter`, `unboxedVectorAdapter`, `seqAdapter`,
and `hashMapAdapter`.

## Dynamic messages

When you don't have generated types (e.g. processing arbitrary proto messages
at runtime), `Proto.Dynamic` gives you an untyped API:

```haskell
import Proto.Dynamic

let bytes = encodeDynamic myDynamicMessage
case decodeDynamic schema bytes of
  Right msg -> print (dynamicField "name" msg)
  Left err  -> handleError err
```

For better decode performance, compile a `ParseTable` from the schema once and
reuse it across many decodes with `decodeDynamicWithSchema`.

## Text format

`Proto.TextFormat` reads and writes the protobuf text format (`.pbtxt`):

```haskell
import Proto.TextFormat

let text = typedToTextPretty (Proxy @MyMessage) myMsg
case textToDynamic schema text of
  Right dynMsg -> use dynMsg
  Left err     -> handleError err
```

## Type registry

`Proto.Registry` provides an explicit registry of message types for use with
`Any` packing/unpacking and dynamic message dispatch:

```haskell
import Proto.Registry

let registry = emptyRegistry
      & registerMessage @MyMessage
      & registerMessage @OtherMessage

case lookupDecoder registry "type.googleapis.com/my.Message" of
  Just decoder -> decoder bytes
  Nothing      -> unknownType
```

`discoverRegistry` is a TH splice that scans all imported modules for
`IsMessage` instances and builds the registry automatically:

```haskell
myRegistry :: TypeRegistry
myRegistry = $(discoverRegistry)
```

## Well-known types

`Proto.Google.Protobuf.*` modules are code-generated from the upstream
`.proto` files in `proto/google/protobuf/`. Each well-known type has a
companion `*.Util` module with helper functions:

| Type | Util module | Key helpers |
|------|-------------|-------------|
| `Timestamp` | `Timestamp.Util` | RFC 3339 formatting, `getCurrentTimestamp` |
| `Duration` | `Duration.Util` | Arithmetic, conversion to/from seconds |
| `Any` | `Any.Util` | `packAny`, `unpackAny` with `TypeRegistry` |
| `FieldMask` | `FieldMask.Util` | Path operations, merging |
| `Struct` | `Struct.Util` | Conversion to/from Aeson `Value` |
| `Wrappers` | `Wrappers.Util` | `Int32Value`, `StringValue`, etc. |

## Proto3 JSON

The `Proto.Internal.JSON` modules implement the proto3 canonical JSON mapping.
The `ToJSON`/`FromJSON` instances generated by `loadProto` and `wireform-gen`
use this mapping automatically. It handles field name conversion (proto
`snake_case` to JSON `camelCase`), default value omission, `Any` type URLs,
well-known type special encodings, and `NullValue`.

## Extensions (proto2)

Proto2 extensions are supported via `Proto.Extension`:

```haskell
import Proto.Extension

let val = getExtension myExtField msg
let msg' = setExtension myExtField val msg
```

Extensions are carried as unknown fields in the wire format and decoded on
access.

## Conformance

`wireform-proto` passes 2,675 / 2,675 tests in the official protobuf
conformance suite, covering proto2 and proto3 binary encoding, proto3 JSON,
and text format.

## Performance

wireform-proto is 3-7x faster than proto-lens on both encode and decode. The
speedup comes from unboxed sums in the decoder, direct-write encoding, and
inlined field codecs.

### Decode: wireform-proto vs proto-lens

| Message shape | wireform-proto | proto-lens | Speedup |
|---------------|---------------|------------|---------|
| Small | 21 ns | 76 ns | 3.7x |
| Medium | 57 ns | 198 ns | 3.5x |
| Nested | 49 ns | 141 ns | 2.9x |
| Repeated | 715 ns | 2107 ns | 2.9x |

### Encode: wireform-proto vs proto-lens

| Message shape | wireform-proto | proto-lens | Speedup |
|---------------|---------------|------------|---------|
| Small | 26 ns | 146 ns | 5.7x |
| Medium | 54 ns | 272 ns | 5.0x |
| Nested | 45 ns | 321 ns | 7.1x |
| Repeated | 656 ns | 2701 ns | 4.1x |

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-proto/bench-results/` for raw data.
