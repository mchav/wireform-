# wireform Development Guidelines

## Code Generation Principles

**All message types must come from the code generator.** This includes well-known
types (`Timestamp`, `Duration`, `Struct`, etc.), descriptor types, and benchmark
types. Hand-written wire encode/decode instances are not permitted because they
drift from what the code generator produces and mask codegen bugs.

- Well-known types live in `src/Proto/Google/Protobuf/*.hs` and are generated
  from the `.proto` files in `proto/google/protobuf/`.
- Supplementary logic (e.g. `packAny`, RFC 3339 formatting, `TypeRegistry`)
  belongs in companion modules like `Proto.Google.Protobuf.Any.Util` or
  `Proto.JSON.WellKnown`. These import the generated types but never define
  wire-level instances.
- Benchmark comparison types must also be code-generated so that benchmarks
  measure the *actual* codegen output, not idealised hand-written decoders.

## Performance

### Allocation discipline

- **Unboxed sums** for finite branching (success / failure / end-of-input).
  Never use boxed `Either` or `Maybe` on an internal hot path.
- **`withTag` CPS** for the decode loop tag dispatch, where continuations are
  statically known lambdas that GHC will inline.
- **Unboxed `Int#`** for offsets threaded through the decoder.
- Avoid `IORef` in benchmarks where an unboxed accumulator loop suffices.

### String / Text handling

- Never round-trip through `String`. No `T.pack (show n)`, no
  `reads (T.unpack t)`, no `T.pack . show`. Use `Data.Text.Builder` or
  direct numeric-to-Text conversion instead.
- For integer formatting, write directly to a `Builder` or use a purpose-built
  `intToText` helper.
- For parsing integers from `Text`, use `Data.Text.Read.decimal` /
  `Data.Text.Read.signed` rather than `reads . T.unpack`.

### Numeric patterns

- When you need both quotient and remainder, use `divMod` or `quotRem` in a
  single call rather than separate `div` and `mod` on the same operands.
- Prefer `quot`/`rem` over `div`/`mod` for non-negative values (avoids the
  sign-correction branch).

### Data structures

- **No plain tuples** in domain-specific return types. Define a small strict
  record with `{-# UNPACK #-}` on numeric fields. Tuples hide meaning and
  prevent GHC from unboxing nested fields.
- **GrowList is a last resort.** Each `snoc` allocates a cons cell + a
  `GrowList` node (≈48 bytes on 64-bit). Prefer:
  1. `VecBuilder` (IO-based doubling array) when inside IO/ST.
  2. `Data.Vector.create` + `MV.grow` in an ST block when the final size
     is unknown but the builder can be scoped.
  3. If stuck in a pure context (the Decoder monad), a chunked representation
     with amortised allocation (e.g. small arrays of 64 elements, chained)
     is better than a cons-per-element list.

### Decoder monad style

- The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`.
  All primitives (`getVarint`, `getText`, etc.) return unboxed sums.
- In hand-optimised decoders, use `withTag` + direct `runDecoder#` calls for
  each field. In generated code, the monadic `do` notation with `getTagOrU`
  is acceptable (slightly less optimal but far simpler to generate).
- Always `{-# INLINE messageDecoder #-}` on instances.

## Code style

- Do not use list comprehensions. Prefer `do` block syntax or
  higher-order functions.
- Prefer datatype-specific functions with better complexity over
  `toList` / `fromList` conversions.
- No `threadDelay` in tests.
- Keep lens usage to where the alternative would be unwieldy; comment
  complex lens expressions.
- Property-based tests via Hedgehog. Do not test things inherent to
  the language (e.g. setting a record field and reading it back).

## Module layout

### Per-format packages

Each wire format lives in its own `wireform-<format>` package under
the workspace root:

```
wireform-core           -- shared low-level primitives (FFI, hashing, encode buffers)
wireform-derive         -- annotation vocabulary (Modifier / NameStyle / TypeInfo)
                           and the per-backend reify logic shared by every deriver
wireform-proto          -- protobuf (proto2 / proto3) IDL parser + codegen + runtime
wireform-cbor           -- RFC 8949 CBOR
wireform-msgpack        -- MessagePack
wireform-thrift         -- Apache Thrift binary / compact
wireform-bson           -- BSON (MongoDB)
wireform-ion            -- Amazon Ion
wireform-edn            -- EDN
wireform-toml           -- TOML
wireform-bencode        -- BitTorrent bencode
wireform-asn1           -- ASN.1 BER / DER
wireform-avro           -- Apache Avro
wireform-bond           -- Microsoft Bond
wireform-flatbuffers    -- Google FlatBuffers
wireform-capnproto      -- Cap'n Proto
wireform-arrow          -- Apache Arrow IPC
wireform-parquet        -- Apache Parquet
wireform-orc            -- Apache ORC
wireform-iceberg        -- Apache Iceberg (table format)
wireform-columnar       -- columnar internals (shared by Arrow/Parquet/ORC)
wireform-csv / -ndjson / -xml / -html  -- text-oriented formats
wireform-grpc           -- gRPC over wireform-proto
```

### Per-format module conventions

Inside each format package:

```
<Format>                       -- top-level API umbrella (re-exports)
<Format>.Encode / .Decode      -- typeclass + encode/decode primitives
<Format>.Class                 -- the public typeclass(es) (e.g. ToCBOR / FromCBOR)
<Format>.Derive                -- annotation-driven Template Haskell deriver
<Format>.Value                 -- (where applicable) the dynamic Value ADT
<Format>.JSON                  -- bridge to/from JSON for self-describing formats
```

The `Derive` module imports `Wireform.Derive` and consumes the
shared `Modifier` vocabulary; derivers are structural twins of one
another so that adding a new format mostly involves cloning the
nearest existing `<Format>.Derive` and adapting the value-mapping
calls.

### Proto-specific layout (legacy)

The protobuf package predates the per-format split and keeps its
historical structure:

```
Proto.AST                       -- .proto IDL AST
Proto.Parser / Proto.Parser.*   -- IDL parser
Proto.Wire / Proto.Wire.*       -- wire format primitives
Proto.Encode / Proto.Decode     -- high-level encode/decode typeclasses
Proto.CodeGen / Proto.CodeGen.* -- pure-text Haskell code generation
Proto.TH                        -- IDL → TH bridge (loadProto)
Proto.Derive                    -- annotation-driven TH deriver (umbrella)
Proto.Derive.Internal           -- body builders reusable by IDL bridges
Proto.Google.Protobuf.*         -- generated well-known types (from .proto)
Proto.Google.Protobuf.*.Util    -- supplementary logic for well-known types
Proto.JSON / Proto.JSON.*       -- JSON mapping
Proto.Schema                    -- runtime type metadata
Proto.Dynamic                   -- dynamic (untyped) messages
Proto.TextFormat                -- pbtxt serialisation
```

## Annotation-driven deriver vocabulary

Every per-format `Derive` module accepts the same `Modifier`
vocabulary from `wireform-derive`. Rule of thumb when adding a new
constructor to `Wireform.Derive.Modifier.Modifier`:

1. Bump `wireform-derive` to a new version (any constructor add is
   technically API-breaking under PVP).
2. Extend `Wireform.Derive.ModifierInfo.ModifierInfo` with the
   resolved field, plus a `ConflictX` constructor in `ModifierError`.
3. Wire the new field into `mergeOne` and `shadowOne`.
4. Per-format derivers consult `mi<Whatever>` and silently ignore
   the field if the modifier does not apply to their backend (e.g.
   `miMapKey` is proto-only).

Backend-specific payloads that should not pollute the core ADT use
the `Wireform.Derive.Extension.BackendModifier` typeclass — see
`XmlFieldOpt`, `HtmlFieldOpt`, and `Asn1Tag` for examples.
