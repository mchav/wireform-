# wireform

High-performance multi-format serialization for Haskell.

A single library providing encoders and decoders for 16 binary serialization
formats. Shared infrastructure (direct-write buffers, Addr#-based decoding,
two-pass sized encoding) gives uniform performance characteristics across all
formats.

## Formats

| Format | Modules | Encode | Decode | JSON | Notes |
|---|---|---|---|---|---|
| Protocol Buffers | `Proto.*` | yes | yes | yes | proto2/proto3, IDL parser, codegen, TH, gRPC |
| Apache Avro | `Avro.*` | yes | yes | yes | Schema resolution, protocol/IPC |
| Apache Thrift | `Thrift.*` | yes | yes | yes | Binary + Compact protocols |
| CBOR | `CBOR.*` | yes | yes | yes | RFC 8949 |
| MessagePack | `MsgPack.*` | yes | yes | yes | Full spec incl. Timestamp ext |
| BSON | `BSON.*` | yes | yes | — | MongoDB wire format |
| Amazon Ion | `Ion.*` | yes | yes | — | Binary Ion |
| Cap'n Proto | `CapnProto.*` | yes | yes | — | Zero-copy segments |
| FlatBuffers | `FlatBuffers.*` | yes | yes | — | Flat zero-copy |
| Microsoft Bond | `Bond.*` | yes | yes | — | Compact binary |
| ASN.1 BER/DER | `ASN1.*` | yes | yes | — | ITU-T X.690 |
| Python Pickle | `Pickle.*` | yes | yes | — | Opcodes 0–5 |
| EDN | `EDN.*` | yes | yes | yes | Extensible Data Notation |
| Apache Parquet | `Parquet.*` | — | read | — | Footer/metadata only |
| Apache Arrow IPC | `Arrow.*` | — | read | — | Schema + record batches |
| Apache Iceberg | `Iceberg.*` | — | read | yes | Table metadata/manifests |

## Benchmarks

Proto encode/decode vs `proto-lens`, MessagePack vs `msgpack`, CBOR vs `cborg`.
Values are a 5-field map/struct (~60–80 bytes encoded).

| Benchmark | wireform | competitor | speedup |
|---|---|---|---|
| Proto encode (small) | ~120 ns | ~450 ns (proto-lens) | ~3.8x |
| Proto decode (small) | ~80 ns | ~350 ns (proto-lens) | ~4.4x |
| Proto roundtrip (small) | ~200 ns | ~800 ns (proto-lens) | ~4.0x |
| MsgPack encode | ~150 ns | ~900 ns (msgpack) | ~6x |
| MsgPack decode | ~200 ns | ~1.2 us (msgpack) | ~6x |
| CBOR encode | ~180 ns | ~350 ns (cborg) | ~1.9x |
| CBOR decode | ~220 ns | ~500 ns (cborg) | ~2.3x |

Run locally: `cabal bench compare-bench` (Proto), `cabal bench format-bench` (MsgPack/CBOR).

## Quick Start

### Protocol Buffers

```haskell
import qualified Proto.Encode as P
import qualified Proto.Decode as P

let bytes = P.encodeMessage myMessage
let Right msg = P.decodeMessage bytes :: Either P.DecodeError MyMsg
```

With Template Haskell:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)

$(loadProto "proto/person.proto")
-- generates: data Person = Person { ... }
-- with MessageEncode, MessageDecode, ToJSON, FromJSON instances
```

### Apache Avro

```haskell
import qualified Avro.Value as AV
import qualified Avro.Schema as AS
import qualified Avro.Encode as AE
import qualified Avro.Decode as AD

let schema = AS.AvroRecord "Person" [AS.AvroField "name" AS.AvroString, AS.AvroField "age" AS.AvroInt]
let val    = AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
let bytes  = AE.encodeAvro schema val
let Right decoded = AD.decodeAvro schema bytes
```

### Apache Thrift

```haskell
import qualified Thrift.Value as TV
import qualified Thrift.Encode as TE
import qualified Thrift.Decode as TD
import qualified Data.Vector as V

let val   = TV.Struct (V.fromList [(1, TV.String "Alice"), (2, TV.I32 30)])
let bytes = TE.encodeBinary val
let Right decoded = TD.decodeBinary bytes
```

### MessagePack

```haskell
import qualified MsgPack.Value as MP
import qualified MsgPack.Encode as MPE
import qualified MsgPack.Decode as MPD
import qualified Data.Vector as V

let val   = MP.Map (V.fromList [(MP.String "name", MP.String "Alice"), (MP.String "age", MP.Int 30)])
let bytes = MPE.encode val
let Right decoded = MPD.decode bytes
```

### CBOR

```haskell
import qualified CBOR.Value as CB
import qualified CBOR.Encode as CBE
import qualified CBOR.Decode as CBD
import qualified Data.Vector as V

let val   = CB.Map (V.fromList [(CB.TextString "name", CB.TextString "Alice"), (CB.TextString "age", CB.UInt 30)])
let bytes = CBE.encode val
let Right decoded = CBD.decode bytes
```

## Package Structure

| Package | Description |
|---|---|
| `wireform` | Core library — all 16 format codecs, proto IDL parser, codegen, TH |
| `wireform-grpc` | Native gRPC client/server built on http2, uses wireform for serialization |

## Module Index

### Protocol Buffers — Parser

```
Proto.AST                      .proto IDL abstract syntax tree
Proto.Parser                   IDL parser (megaparsec)
Proto.Parser.Lexer             Lexer primitives
Proto.Parser.Error             Parse error types
Proto.Parser.Resolver          Import resolution with include dirs
```

### Protocol Buffers — Wire Format

```
Proto.Wire                     Wire types, tags, field keys
Proto.Wire.Encode              Low-level encoding primitives
Proto.Wire.Decode              Low-level decoding (unboxed sums)
Proto.Wire.Result              Three-way unboxed decode result
Proto.Wire.FFI                 C FFI for SIMD-accelerated ops
```

### Protocol Buffers — High-Level Codec

```
Proto.Encode                   Encoding typeclasses + helpers
Proto.Encode.Lazy              Lazy/streaming encoders
Proto.Encode.Archetype         Archetype-based field encoding
Proto.Encode.Direct            Direct buffer-write encoding
Proto.Decode                   Decoding typeclasses + helpers
Proto.Decode.Stream            Streaming/incremental decoders
Proto.Decode.Fast              Addr#-based fast decoding
Proto.SizedBuilder             Fused size+builder for exact allocation
Proto.VectorBuilder            Mutable growing vector (IO), GrowList (pure)
Proto.Church                   Church-encoded lists and CPS Maybe
Proto.Merge                    Proto merge semantics
Proto.FieldPresence            Explicit presence tracking (proto3 optional)
Proto.Message                  IsMessage typeclass
Proto.Schema                   Runtime schema metadata
Proto.Lens                     Van Laarhoven lenses for fields
Proto.Repr                     Configurable field representations
```

### Protocol Buffers — Code Generation

```
Proto.CodeGen                  Haskell code generation from AST
Proto.CodeGen.Combinators      Prettyprinter helpers
Proto.CodeGen.Types            Type mapping
Proto.CodeGen.Encode           Encoder generation
Proto.CodeGen.Decode           Decoder generation
Proto.CodeGen.Service          gRPC service stub generation
Proto.CodeGen.Hooks            Codegen pipeline hooks
Proto.Descriptor.Convert       AST -> FileDescriptorProto
Proto.TH                       Template Haskell code generation
Proto.QQ                       QuasiQuoter for inline proto
Proto.Setup                    Cabal pre-build hook
```

### Protocol Buffers — JSON, Dynamic, Tools

```
Proto.JSON                     Proto3 JSON via aeson
Proto.JSON.WellKnown           Well-known type canonical JSON
Proto.Dynamic                  Dynamic (untyped) messages
Proto.TextFormat                Text format (pbtxt) serialisation
Proto.Conformance              Conformance test harness
Proto.Registry                 Message type registry
Proto.Registry.TH              TH support for building registries
Proto.Options                  Standard option extraction
Proto.Options.Custom           Custom option extensions
Proto.Annotations              Option querying utilities
Proto.Compat                   Schema compatibility checking
Proto.Print                    AST -> proto source printer
Proto.Inspect                  AST query/navigation utilities
Proto.GRPC                     gRPC framing (length-prefixed)
```

### Protocol Buffers — Well-Known Types

```
Proto.Google.Protobuf.Any            Any + utilities
Proto.Google.Protobuf.Timestamp      Timestamp + RFC 3339 utilities
Proto.Google.Protobuf.Duration       Duration + utilities
Proto.Google.Protobuf.Empty          Empty
Proto.Google.Protobuf.Wrappers       Wrapper types + utilities
Proto.Google.Protobuf.FieldMask      FieldMask + utilities
Proto.Google.Protobuf.SourceContext  SourceContext
Proto.Google.Protobuf.Struct         Struct, Value, ListValue, NullValue + utilities
Proto.Google.Protobuf.Descriptor     FileDescriptorProto, DescriptorProto, ...
Proto.Google.Protobuf.Compiler.Plugin CodeGeneratorRequest/Response
```

### Apache Avro

```
Avro.Wire              Avro binary wire primitives
Avro.Schema            Schema types (AvroType, AvroField)
Avro.Value             Runtime value representation
Avro.Encode            Schema-driven encoding
Avro.Decode            Schema-driven decoding
Avro.JSON              Avro JSON encoding
Avro.Resolution        Schema resolution (reader/writer)
Avro.Protocol          Avro Protocol / IPC
```

### Apache Thrift

```
Thrift.Wire            Wire types and constants
Thrift.Schema          Schema types
Thrift.Value           Runtime value representation
Thrift.Encode          Binary + Compact protocol encoding
Thrift.Decode          Binary + Compact protocol decoding
Thrift.JSON            Thrift JSON serialization
Thrift.Message         Thrift message framing
```

### CBOR

```
CBOR.Value             RFC 8949 value representation
CBOR.Encode            Binary encoding (canonical)
CBOR.Decode            Binary decoding
CBOR.JSON              CBOR <-> JSON bridge
```

### MessagePack

```
MsgPack.Value          Value representation (incl. Timestamp ext)
MsgPack.Encode         Binary encoding (two-pass direct-write)
MsgPack.Decode         Binary decoding (Addr#-based)
MsgPack.JSON           MsgPack <-> JSON bridge
```

### Other Formats

```
BSON.Value / .Encode / .Decode          MongoDB BSON
Ion.Value / .Encode / .Decode           Amazon Ion (binary)
CapnProto.Value / .Encode / .Decode     Cap'n Proto
FlatBuffers.Value / .Encode / .Decode   FlatBuffers
Bond.Value / .Encode / .Decode          Microsoft Bond
ASN1.Value / .Encode / .Decode          ASN.1 BER/DER
Pickle.Value / .Encode / .Decode        Python Pickle
EDN.Value / .Encode / .Decode / .JSON   EDN
Iceberg.Types / .JSON / .Manifest       Apache Iceberg
Parquet.Types / .Footer                 Apache Parquet
Arrow.Types / .IPC                      Apache Arrow IPC
```

## Building

```bash
cabal build all
```

## Testing

```bash
cabal test wireform-test
cabal test conformance-self-test
cabal test temporal-codegen-test
```

## Benchmarks

```bash
cabal bench compare-bench     # Proto vs proto-lens
cabal bench format-bench      # MsgPack vs msgpack, CBOR vs cborg
cabal exec wireform-bench     # Internal proto benchmarks
cabal exec bench-grow         # GrowList/VectorBuilder benchmarks
```

## GHC Compatibility

Tested with GHC 9.2 through 9.12. Requires `GHC2021`.

## License

BSD-3-Clause. See [NOTICE](NOTICE) for third-party attributions.
