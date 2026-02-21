# Missing Features: hs-proto vs Other Protobuf Libraries

Analysis comparing hs-proto against proto-lens (Haskell), prost (Rust), and the
official Google protobuf runtimes (Java, Python, Go, C++).

---

## What hs-proto already has

- Full proto2/proto3 parser with all IDL constructs
- High-performance wire encode/decode (CPS decoder, SizedBuilder, SIMD FFI)
- Code generation (standalone CLI + TH + QuasiQuoter + Cabal Setup hook)
- Well-known types: Any, Timestamp, Duration, Empty, Wrappers, FieldMask,
  SourceContext, Struct/Value/ListValue
- Proto3 JSON mapping (custom AST, no aeson dependency)
- Schema metadata typeclasses (ProtoMessage, HasField, ProtoEnum, ProtoService)
- Van Laarhoven lenses (no lens dependency)
- Message merge semantics
- Field presence tracking (proto3 optional)
- Unknown field preservation
- Lazy submessage decoding
- Schema compatibility checking (Confluent-style)
- Configurable field representations (Text/ShortText/LazyText, ByteString variants, Vector/List/Seq)
- AST printer, inspector, and query module
- Standard protobuf options extraction
- Import resolution with transitive dependency tracking
- Cross-language option mapping
- DEPRECATED pragma generation
- Packed repeated field encoding/decoding
- Python interop conformance tests
- Benchmark suite vs proto-lens

---

## Missing features, ordered by impact

### 1. gRPC Service Stub Generation (HIGH)

Every major protobuf ecosystem ships gRPC codegen: proto-lens has
`proto-lens-grpc`, prost has `tonic`, and the official runtimes all generate
client/server stubs from `service` definitions.

hs-proto parses services into `ServiceDef`/`RpcDef` and stores
`ProtoService`/`MethodDescriptor` metadata, but the code generator discards
them (`TLService _svc -> []` in `CodeGen.hs:155`). No client stubs, server
interfaces, or streaming abstractions are generated.

What's needed:
- Generated service typeclasses or records with method signatures
- Client stub functions (unary, client-streaming, server-streaming, bidi)
- Server handler type signatures
- Integration point for an HTTP/2 + gRPC transport (e.g. http2-grpc-haskell
  or warp-grpc)

### 2. protoc Plugin Mode (HIGH)

proto-lens operates as a `protoc` plugin (`protoc-gen-haskell`), receiving
`CodeGeneratorRequest` (a `FileDescriptorSet`) and returning
`CodeGeneratorResponse`. This enables use with `buf generate`, polyglot
builds, and protoc's own import resolution.

hs-proto has its own parser and resolver but cannot participate in protoc
pipelines. Adding a `protoc-gen-hs-proto` binary that reads
`CodeGeneratorRequest` from stdin would unlock:
- `buf generate` integration
- Protoc's battle-tested import/dependency resolution
- Polyglot monorepo builds where one `protoc` invocation generates code for
  multiple languages

What's needed:
- Decode `google.protobuf.compiler.CodeGeneratorRequest` from stdin
- Map `FileDescriptorProto` to hs-proto's internal AST or generate directly
- Emit `CodeGeneratorResponse` to stdout
- This requires implementing or generating the `descriptor.proto` and
  `plugin.proto` message types

### 3. Protobuf Editions Support (HIGH)

Protobuf Editions (2023, 2024) replace `syntax = "proto2"/"proto3"` with
`edition = "2023"` and per-feature settings. proto-lens 0.9.0.0 added
editions support. All official runtimes support it. This is the stated
future direction of protobuf.

hs-proto's parser only recognizes `Proto2 | Proto3`. It doesn't parse the
`edition` keyword or feature settings (`features.field_presence`,
`features.enum_type`, `features.repeated_field_encoding`, etc.).

What's needed:
- Parse `edition = "2023"` syntax
- Parse `features` blocks at file/message/field/enum scope
- Map feature settings to existing behavior flags (e.g.,
  `field_presence = EXPLICIT` maps to current optional handling)

### 4. Full descriptor.proto Types (MEDIUM-HIGH)

Java, Python, Go, and prost all expose the full `google/protobuf/descriptor.proto`
type hierarchy as generated types: `FileDescriptorProto`,
`DescriptorProto`, `FieldDescriptorProto`, `EnumDescriptorProto`,
`ServiceDescriptorProto`, `MethodDescriptorProto`, etc.

hs-proto has a custom `ProtoMessage`/`HasField` metadata system and a
`protoFileDescriptorBytes` field that defaults to `""`. The actual
descriptor.proto messages are not implemented. This blocks:
- protoc plugin mode (which needs to decode `FileDescriptorSet`)
- Runtime reflection equivalent to Java's `Descriptor` API
- Descriptor pool / registry for dynamic message construction

What's needed:
- Generate or hand-write Haskell types for `descriptor.proto`
- Generate or hand-write types for `compiler/plugin.proto`
- Populate `protoFileDescriptorBytes` in generated code with the actual
  serialized FileDescriptorProto

### 5. Text Format (pbtxt) Serialization (MEDIUM)

The protobuf text format is a human-readable serialization used for config
files, test fixtures, debugging output, and `.pbtxt` files. Java, Python,
Go, and C++ all support it natively. proto-lens does not have it either.

hs-proto has `Proto.Print` (prints .proto IDL from the AST) but no text
format serializer/deserializer for message values.

What's needed:
- `messageToText :: IsMessage a => a -> Text` using field descriptors for
  field names
- `textToMessage :: IsMessage a => Text -> Either String a` parser
- Handle special cases: Any expansion, enum names, repeated field syntax,
  map entries, message literal syntax

### 6. Dynamic Messages (MEDIUM)

Java's `DynamicMessage`, Python's `message_factory`, and Go's `dynamicpb`
allow constructing and manipulating messages at runtime using only
descriptors, without any generated code.

hs-proto's `DynamicMessage` in `Proto.Google.Protobuf.Any` is just an
existential wrapper for Any unpacking. There's no general-purpose dynamic
message type that can hold arbitrary fields based on a descriptor.

What's needed:
- A `DynamicMessage` type backed by a `Map FieldNumber DynamicValue`
- `DynamicValue` sum type covering all proto value types
- `MessageEncode`/`MessageDecode` instances driven by a runtime descriptor
- Useful for proxies, middleware, schema registries, and tooling

### 7. Conformance Test Suite (MEDIUM)

Google publishes an official [conformance test runner](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
that validates JSON and binary serialization across implementations.
Passing the conformance suite is the standard bar for a production-grade
protobuf library.

hs-proto has Python interop tests (Hedgehog property tests round-tripping
through Python), which is good but not the same as the official
conformance suite.

What's needed:
- Implement the conformance test harness (read `ConformanceRequest` from
  stdin, write `ConformanceResponse` to stdout)
- This requires the descriptor.proto types (see #4)
- Wire format, JSON, and (optionally) text format conformance

### 8. Streaming / Incremental Decode (MEDIUM)

While hs-proto has `encodeLazy` for lazy output, decoding requires a
complete strict `ByteString` upfront. Libraries like prost and the Go
runtime support incremental/streaming decode from an `io.Reader` or
similar.

What's needed:
- A streaming decoder that works with conduit, streaming, or pipes
- Incremental parsing from a `Handle` or network socket
- Important for large messages and gRPC streaming

### 9. Well-Known Type JSON Special Cases (LOW-MEDIUM)

The proto3 JSON spec mandates special JSON handling for well-known types:
- `Timestamp` → RFC 3339 string (`"2024-01-15T12:00:00Z"`)
- `Duration` → `"3600.000s"` string
- `Struct`/`Value` → native JSON object/value
- `Wrappers` → unwrapped value directly
- `FieldMask` → `"foo,bar.baz"` comma-separated paths
- `Any` → `{"@type": "...", ...inlined fields}`

hs-proto's `Proto.JSON` module documents these rules in comments but
the `ProtoToJSON`/`ProtoFromJSON` instances for well-known types are not
implemented. The JSON parser is also minimal (arrays and objects parse as
empty).

What's needed:
- `ProtoToJSON`/`ProtoFromJSON` instances for all well-known types
- A real JSON parser (or optional aeson interop)
- Handle special-case JSON names (`lowerCamelCase` field name mapping)

### 10. aeson Integration (LOW-MEDIUM)

The custom `JsonValue` AST avoids an aeson dependency, which is a
reasonable design choice. But in practice, most Haskell applications use
aeson. An optional `hs-proto-aeson` package (or a cabal flag) providing
`ToJSON`/`FromJSON` instances would significantly improve ergonomics.

What's needed:
- Optional aeson integration (flag or separate package)
- Generated `ToJSON`/`FromJSON` instances following proto3 JSON spec
- Bridge between `JsonValue` and `aeson`'s `Value`

### 11. Group Fields (proto2) (LOW)

Proto2 groups (wire types 3/4, `WireStartGroup`/`WireEndGroup`) are
defined in the wire format module but the decoder's `skipField` may not
properly handle them (needs to recursively skip until the matching end
group tag). Groups are deprecated but still encountered in legacy schemas.

What's needed:
- Verify `skipField` handles groups correctly (recursive skip to end group)
- Optionally decode group fields in generated code

### 12. Custom Option Extensions (LOW)

hs-proto parses custom option syntax `[(my.custom.option) = value]` into
the AST but doesn't provide a way to define or resolve custom option
extensions at the type level. proto-lens and the official runtimes allow
defining new options via `extend google.protobuf.FieldOptions { ... }` and
accessing them at runtime.

What's needed:
- Resolve extension option types against the descriptor type hierarchy
- Typed access to custom option values in generated code

### 13. Map Field Codegen Completeness (LOW)

The code generator handles map fields in type declarations (`genMapFieldDecl`)
but the TH path (`Proto.TH`) only processes `MEField` elements, skipping
`MEMapField` and `MEOneof`. This means TH-generated types may be missing
map and oneof fields.

What's needed:
- Handle `MEMapField` in `extractMessageFields` / TH generation
- Handle `MEOneof` in TH generation (sum type + field)
- Corresponding encode/decode logic in TH

### 14. Enum Alias Support in Codegen (LOW)

The parser and options system recognize `allow_alias`, but the code
generator derives `Enum` for enum types, which requires distinct
constructors for each value. Proto enums with aliases (two names for the
same number) will fail.

What's needed:
- Detect `allow_alias` enums
- Generate manual `toProtoEnum`/`fromProtoEnum` instead of derived `Enum`
- Or generate pattern synonyms for aliases

---

## Summary prioritization

| Priority | Feature | Effort |
|----------|---------|--------|
| P0 | gRPC service codegen | Large |
| P0 | protoc plugin mode | Medium |
| P0 | Protobuf Editions | Medium |
| P1 | descriptor.proto types | Medium |
| P1 | Text format (pbtxt) | Medium |
| P1 | Conformance test suite | Medium |
| P1 | Dynamic messages | Medium |
| P2 | Streaming decode | Medium |
| P2 | Well-known type JSON | Small |
| P2 | aeson integration | Small |
| P3 | Group field handling | Small |
| P3 | Custom option extensions | Medium |
| P3 | Map/oneof in TH codegen | Small |
| P3 | Enum alias codegen | Small |
