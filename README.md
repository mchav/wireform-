# hs-proto

High-performance Protocol Buffers library for Haskell.

## Features

### Proto IDL Parser

Full proto2, proto3, and Editions (2023+) parser built on megaparsec.

- Messages, enums, services, oneofs, map fields, extensions
- Imports with include-directory resolution and cycle detection
- Custom options and aggregate literals
- Adjacent string literal concatenation

```haskell
import Proto.Parser (parseProtoFile)
import Proto.AST

Right pf <- pure (parseProtoFile "person.proto" contents)
-- pf :: ProtoFile
```

### Wire Format Codec

Zero-copy decoding with unboxed-sum result types. Encoding uses
`ByteString.Builder` with a two-pass size-aware strategy.

**Decoder characteristics:**

- Unboxed `(# (# a, Int# #) | DecodeError #)` result — no heap allocation
  for the success/failure envelope
- Inline fast paths for 1–2 byte varints (tags and small values)
- Zero-copy `ByteString` slices for bytes/string fields
- `withTag` CPS for branchless tag dispatch in generated decoders
- `UMaybe` / `TagResult#` to avoid boxing in the decode loop

**Encoder characteristics:**

- Two-pass encoding: compute `messageSize` first, then encode with exact
  allocation (`SizedBuilder`)
- Packed repeated field encoding for all scalar types
- `SizedBuilder` fuses size and builder — submessages are never
  materialised to intermediate `ByteString`s

```haskell
import Proto.Encode (encodeMessage, encodeMessageSized)
import Proto.Decode (decodeMessage)

let bytes = encodeMessage myMessage
let Right msg = decodeMessage bytes
```

### Code Generation

Two code generation paths: standalone executable and Template Haskell.

**Standalone generator** (`hs-proto-gen`):

```bash
hs-proto-gen generate -I proto -o gen --module-prefix Proto file.proto
```

**protoc plugin** (`protoc-gen-hs-proto`):

```bash
protoc --plugin=protoc-gen-hs-proto --hs-proto_out=gen file.proto
```

**Template Haskell:**

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH
$(loadProto "path/to/message.proto")
```

**QuasiQuoter:**

```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Proto.QQ

[proto|
  syntax = "proto3";
  message SearchRequest {
    string query = 1;
    int32 page_number = 2;
  }
|]
```

Generated code includes:

- Haskell record types with strict fields and `UNPACK` pragmas on
  primitive scalars
- `MessageEncode` / `MessageDecode` / `MessageSize` instances
- `ProtoToJSON` / `ProtoFromJSON` instances (using `json_name` annotations)
- `IsMessage` / `ProtoMessage` instances with full schema metadata
- `NFData` and `Generic` derivations
- Oneof fields as `Maybe`-wrapped sum types
- Map fields as `Map.Map`
- Packed repeated scalars as `VU.Vector` (unboxed) or `V.Vector` (boxed)
- Enum types with pattern synonyms for aliases (`allow_alias`)
- gRPC service stubs (server handler records, client stubs, method metadata)
- Module-level `registerModuleTypes` for type registry integration

### Configurable Representations

Override how string, bytes, repeated, and optional fields are
represented per-message or per-field:

| Proto type | Representations |
|---|---|
| `string` | `Text` (default, zero-copy), lazy `Text`, `ShortByteString`, `String` |
| `bytes` | `ByteString` (default, zero-copy), lazy `ByteString`, `ShortByteString` |
| `repeated` | `Vector` (default), `[]`, `Seq` |
| `optional` | `Maybe` (default), `Field` (explicit presence) |

```haskell
$(loadProtoWith (defaultLoadOpts { loRepConfig = defaultRepConfig
    { rcFieldOverrides = Map.fromList
        [ (("Blob","data"), defaultFieldRep { frBytes = LazyBytesRep })
        ]
    }})
    "file.proto")
```

### JSON Mapping

Canonical proto3 JSON encoding and decoding, dependency-free.

- Built-in `JsonValue` AST (no aeson dependency)
- `ProtoToJSON` / `ProtoFromJSON` typeclasses
- Compact and pretty-printed rendering
- Minimal recursive-descent parser
- Well-known type canonical representations (RFC 3339 timestamps,
  duration strings, field mask paths, Struct/Value ↔ native JSON)

### Schema Metadata

Runtime access to proto schema information via `ProtoMessage` and `HasField`:

```haskell
import Proto.Schema

protoMessageName (Proxy @MyMessage)   -- "example.MyMessage"
protoFieldDescriptors (Proxy @MyMessage)  -- Map of field descriptors
protoDefaultValue @MyMessage            -- default instance
```

### Lens Support

Optional van Laarhoven lenses compatible with `lens`, `microlens`, and `optics`:

```haskell
import Proto.Lens (field, view, set, over)

view (field @"seconds") timestamp
set (field @"seconds") 42 timestamp
over (field @"seconds") (+1) timestamp
```

### Dynamic Messages

Runtime protobuf manipulation without generated types:

```haskell
import Proto.Dynamic

let msg = setDynamicField 1 (DynString "hello") emptyDynamic
let bytes = encodeDynamic msg
let Right decoded = decodeDynamic bytes
```

### Text Format

Protobuf text format (pbtxt) serialisation for dynamic messages:

```haskell
import Proto.TextFormat

dynamicToTextPretty myDynamicMessage
-- 1: "hello"
-- 2: 42
```

### Streaming and Incremental Codecs

Length-delimited framing for gRPC-style message streams:

**Decoding:**

```haskell
import Proto.Decode.Stream

-- Lazy stream decode
let messages = decodeMessageStream lazyInput :: [Either DecodeError MyMsg]

-- Incremental (resumable) decode
let dec = decodeMessageIncremental :: IDecode MyMsg
-- Feed chunks: IPartial k -> k (Just chunk), IDone msg leftover, IFail err leftover
```

**Encoding:**

```haskell
import Proto.Encode.Lazy

-- Lazy stream encode
let output = encodeMessageStreamSized myMessages :: BL.ByteString

-- Push-based incremental encoder
let enc = newStreamEncoderSized :: IEncode MyMsg
-- IEncReady f -> f (Just msg) produces IEncChunk bytes ..., f Nothing -> IEncDone
```

### Schema Compatibility Checking

Confluent Schema Registry-style compatibility analysis:

```haskell
import Proto.Compat

let result = checkCompat Full newSchema oldSchema
isCompatible result  -- True/False
compatErrors result  -- [CompatError]
```

Supports `Backward`, `Forward`, `Full`, and their transitive variants.

### Type Registry

Runtime message type registry for `Any` support and dynamic dispatch:

```haskell
import Proto.Registry
import Proto.Registry.TH

myRegistry :: MessageRegistry
myRegistry = $(buildRegistry
  [ [t| Timestamp |]
  , [t| Duration |]
  ])
```

### Conformance Testing

Harness for the official protobuf conformance test runner:

```haskell
import Proto.Conformance (conformanceMain)

main :: IO ()
main = conformanceMain myHandler
```

### AST Inspection

Query and navigate parsed proto schemas:

```haskell
import Proto.Inspect

allMessages pf          -- all messages (flattened)
findMessage "Person" pf -- lookup by name
referencedTypes pf      -- all FTNamed references
summarize pf            -- structural summary
```

### Proto Printer

Round-trip proto files through the AST:

```haskell
import Proto.Print (printProtoFile)

let source = printProtoFile parsedProto
-- parse . print ≡ id (up to whitespace)
```

### Well-Known Types

Pre-generated modules for standard Google protobuf types:

| Module | Types |
|---|---|
| `Proto.Google.Protobuf.Timestamp` | `Timestamp` |
| `Proto.Google.Protobuf.Duration` | `Duration` |
| `Proto.Google.Protobuf.Empty` | `Empty` |
| `Proto.Google.Protobuf.Any` | `Any` |
| `Proto.Google.Protobuf.Struct` | `Struct`, `Value`, `ListValue`, `NullValue` |
| `Proto.Google.Protobuf.Wrappers` | `DoubleValue`, `FloatValue`, `Int64Value`, ... |
| `Proto.Google.Protobuf.FieldMask` | `FieldMask` |
| `Proto.Google.Protobuf.SourceContext` | `SourceContext` |
| `Proto.Google.Protobuf.Descriptor` | `FileDescriptorProto`, `DescriptorProto`, ... |
| `Proto.Google.Protobuf.Compiler.Plugin` | `CodeGeneratorRequest`, `CodeGeneratorResponse` |

## Module Map

```
Proto.AST                          .proto IDL abstract syntax tree
Proto.Parser                       IDL parser (megaparsec)
Proto.Parser.Lexer                 Lexer primitives
Proto.Parser.Resolver              Import resolution with include dirs

Proto.Wire                         Wire types, tags, field keys
Proto.Wire.Encode                  Low-level encoding primitives
Proto.Wire.Decode                  Low-level decoding (unboxed sums)
Proto.Wire.Result                  Three-way unboxed decode result

Proto.Encode                       High-level encoding typeclasses
Proto.Decode                       High-level decoding typeclasses
Proto.Decode.Stream                Streaming/incremental decoders
Proto.Encode.Lazy                  Lazy/streaming encoders
Proto.SizedBuilder                 Fused size+builder for exact allocation
Proto.VectorBuilder                Mutable growing vector (IO), GrowList (pure)
Proto.Church                       Church-encoded lists and CPS Maybe
Proto.Merge                        Proto merge semantics
Proto.FieldPresence                Explicit presence tracking (proto3 optional)

Proto.Message                      IsMessage typeclass (identity)
Proto.Schema                       Runtime schema metadata
Proto.Lens                         Van Laarhoven lenses for fields
Proto.Repr                         Configurable field representations

Proto.CodeGen                      Haskell code generation from AST
Proto.CodeGen.Combinators          Prettyprinter helpers
Proto.CodeGen.Types                Type mapping
Proto.CodeGen.Encode               Encoder generation
Proto.CodeGen.Decode               Decoder generation
Proto.CodeGen.Service              gRPC service stub generation
Proto.Descriptor.Convert           AST → FileDescriptorProto

Proto.TH                           Template Haskell code generation
Proto.QQ                           QuasiQuoter for inline proto
Proto.Setup                        Cabal pre-build hook

Proto.JSON                         Canonical proto3 JSON (dependency-free)
Proto.JSON.WellKnown               Well-known type JSON conversions
Proto.JSON.Aeson                   Bridge to aeson (user-side conversion)

Proto.Dynamic                      Dynamic (untyped) messages
Proto.TextFormat                   Text format (pbtxt) serialisation
Proto.Conformance                  Conformance test harness

Proto.Registry                     Message type registry
Proto.Registry.TH                  TH support for building registries

Proto.Options                      Standard option extraction
Proto.Options.Custom               Custom option extensions
Proto.Annotations                  Option querying utilities
Proto.Compat                       Schema compatibility checking
Proto.Print                        AST → proto source printer
Proto.Inspect                      AST query/navigation utilities

Proto.Google.Protobuf.*            Generated well-known types
Proto.Internal.Maybe               Unboxed Maybe (internal)
Proto.Internal.Either              Unboxed Either (internal)
```

## Building

```bash
cabal build all
```

## Testing

```bash
cabal test hs-proto-test
cabal test conformance-self-test
cabal test temporal-codegen-test
```

## Benchmarks

```bash
cabal exec hs-proto-bench
cabal exec bench-grow
cabal bench compare-bench     # requires proto-lens
```

## GHC Compatibility

Tested with GHC 9.2 through 9.12. Requires `GHC2021`.

## License

BSD-3-Clause. See [NOTICE](NOTICE) for third-party attributions.
