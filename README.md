# wireform

There is a small ritual you perform every time you reach for a Haskell
serialization library. Open Hackage. Find the package for the format
you need. Brace yourself for the audit.

Is it fast, or did someone prove a point about elegance and never come
back to benchmark it? Does it pass the upstream conformance suite, or
just the tests the author happened to think of? Has anyone touched it
since GHC 8.10? Does it transitively pull in eighty modules of lens
because the author wanted one helper? Does its API resemble the other
six serialization libraries already in your build plan, or do you have
a new typeclass to learn, a new error type to pattern-match on, and a
new way to spell `decode`?

I have done that audit a lot. After the third or fourth round you
notice that the custom code you keep writing on top of these packages
is the same custom code, lightly rearranged: the same allocation
tricks, the same Template Haskell deriver boilerplate, the same
property-test scaffolding, the same emergency bridge to JSON for the
format that didn't think it would need one. wireform is what happens
when you stop performing the audit and start writing the library you
wish was already on Hackage. Once. For thirty formats at the same time.

In practice, that's a monorepo of roughly thirty format packages where
every one shares the same allocation-disciplined core (`wireform-core`),
the same annotation-driven Template Haskell deriver (`wireform-derive`),
the same per-format Hedgehog suite, and, where an upstream conformance
suite exists, an opt-in test runner that wires it up. Protobuf runs
against the official `protocolbuffers/protobuf` harness. TOML runs
against `toml-test`. YAML runs against `yaml-test-suite`. Iceberg,
Delta Lake, Hudi, and Lance round-trip through their respective Python
or Rust readers. Fory rides on `pyfory`. Kafka rides on a live broker.
The answer to "is this format actually conformant?" stops being "I sure
hope so" and starts being "here is the suite, here is the score."

The minimal-dependency posture is the same instinct pointed at your
build plan. Each per-format package depends on `wireform-core`,
`wireform-derive`, and the third-party libraries the format genuinely
needs. Usually that's `bytestring`, `text`, `vector`, and whatever
schema library is unavoidable, and nothing else. No format drags in
another format's deps. If you only need CBOR, you only build CBOR.

One annotated Haskell record gives you wire codecs for Protocol
Buffers, CBOR, MessagePack, Thrift, JSON, BSON, Amazon Ion, EDN, TOML,
YAML, Bencode, NDJSON, CSV, XML, HTML, ASN.1, Avro, Bond, FlatBuffers,
Cap'n Proto, Apache Arrow, Apache Parquet, Apache ORC, Apache Iceberg,
and Apache Fory. The Kafka and gRPC wire protocols ship as native
clients. Delta Lake, Hudi, and Lance ship as table-format readers on
top of the columnar core.

That leaves the "is it fast" half of the audit. Every format draws on
the same small set of allocation-disciplined primitives: unboxed sums
for finite branching, `Int#`-threaded decoders, sized two-pass encoding,
SIMD-accelerated scanners for XML and HTML, and C FFI for the hottest
paths. Generated code has to stay competitive with hand-written codecs,
because "you can always write it faster yourself" is exactly the
dynamic that got us into this mess in the first place.

A note on scope: wireform is unapologetically maximalist. If it parses,
renders, encodes, decodes, frames, or otherwise shuffles bytes between
two systems, it's in scope. The current thirty packages are a starting
position, not a ceiling. New format packages are welcome and actively
wanted, provided they clear the same bar the existing ones had to:
fast enough to prove it with a benchmark, tested hard enough to prove
it with the format's official conformance suite (or, where no such
suite exists, with an explicit interop test against another language's
implementation), wired into the shared annotation deriver so users
don't learn a new API per format, and dependency-light enough not to
drag the rest of the build plan in. Acceptance criteria in full are
under [Adding a new format](#adding-a-new-format).

---

## Table of contents

- [What's in here](#whats-in-here)
- [Package layout](#package-layout)
- [The annotation-driven deriver](#the-annotation-driven-deriver)
- [Protocol Buffers](#protocol-buffers)
- [Building](#building)
- [Testing](#testing)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

---

## What's in here

Each format ships with the machinery you actually need to use it in
production, not just `encode` / `decode`:

- Encode and decode for every format, sharing the same allocation
  primitives.
- Generic and annotation-driven deriving that targets every format from a
  single set of pragmas.
- IDL parsers and code generators for `.proto`, `.avsc` / `.avdl`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, ISL, CDDL, XSD, and Iceberg
  table metadata.
- Streaming and incremental decoders for protobuf, MsgPack, CBOR, XML,
  NDJSON, and YAML.
- RPC framing for gRPC (length-prefix), Thrift (binary and compact),
  msgpack-rpc, and Avro IPC.
- A native Kafka client: TCP / TLS / SASL, compression, version
  negotiation, transactions, consumer groups, pipelining, OpenTelemetry.
- Container file I/O for Avro OCF, Parquet (footer, page index, bloom
  filter, column reads), Arrow IPC, ORC (postscript and stripes), and
  Iceberg (table metadata and manifests).
- Table-format readers for Delta Lake (transaction log + checkpoints +
  time travel), Hudi (timeline + Avro instants), and Lance (data file +
  manifest + dataset versions).
- Schema resolution and evolution for Avro, plus proto2 / proto3
  compatibility helpers.
- Dynamic untyped protobuf messages and `.pbtxt` text-format I/O.
- An XML pipeline that goes beyond encode/decode: SIMD-accelerated SAX,
  zero-copy DOM, chunk-fed incremental and concurrent parse, an XPath-lite
  query language, and an XSLT 1.0 subset.
- HTML5 with a spec-compliant tokenizer and tree builder, SIMD serializer,
  CSS selectors, and a streaming rewriter.

---

## Package layout

The repo is a workspace of `wireform-*` packages. Each per-format package
is self-contained and depends only on `wireform-core` (shared primitives),
`wireform-derive` (annotation vocabulary), and the third-party libraries
its format actually needs.

| Package | Role |
|---------|------|
| `wireform-core` | Shared FFI primitives (encode buffers, hashing) used by every format package |
| `wireform-derive` | Annotation-driven Template Haskell deriver core (`Modifier`, `NameStyle`, `TypeInfo`, `BackendModifier`) |
| `wireform-columnar` | SIMD-accelerated columnar primitives (bit-unpacking, predicate vocabulary, pull-based iter combinators) shared by Arrow, Parquet, and ORC |
| `wireform-proto` | Protocol Buffers (proto2 / proto3): IDL parser, codegen, runtime, JSON mapping, well-known types |
| `wireform-cbor` | CBOR (RFC 8949) values, encoding/decoding, JSON bridge, CDDL schema and codegen |
| `wireform-msgpack` | MessagePack values, encoding/decoding, JSON bridge, msgpack-rpc |
| `wireform-thrift` | Apache Thrift binary and compact protocol, schema, JSON mapping |
| `wireform-bson` | BSON (MongoDB wire format) |
| `wireform-ion` | Amazon Ion binary values, ISL schema, codegen |
| `wireform-edn` | Extensible Data Notation (EDN) |
| `wireform-toml` | TOML 1.0 / 1.1 |
| `wireform-yaml` | YAML 1.2 (block + flow, anchors / aliases, tags, multi-document streams) |
| `wireform-bencode` | BitTorrent bencode |
| `wireform-fory` | Apache Fory (formerly Fury) cross-language serialization, wire-compatible with `pyfory` 0.17 |
| `wireform-asn1` | ASN.1 BER / DER (ITU-T X.690) |
| `wireform-avro` | Apache Avro: schema resolution, JSON conversion, IPC protocol, container files |
| `wireform-bond` | Microsoft Bond compact binary |
| `wireform-flatbuffers` | FlatBuffers zero-copy flat serialization |
| `wireform-capnproto` | Cap'n Proto zero-copy serialization |
| `wireform-arrow` | Apache Arrow IPC schema framing and record batch materialization |
| `wireform-parquet` | Apache Parquet metadata, column pages, page index, bloom filter, read/write |
| `wireform-orc` | Apache ORC metadata, stripes, read/write, predicate evaluator |
| `wireform-iceberg` | Apache Iceberg table metadata, manifests, schema evolution, catalogs |
| `wireform-delta` | Delta Lake transaction log + checkpoints + time travel |
| `wireform-hudi` | Apache Hudi timeline reader (Copy-on-Write, JSON + Avro instants) |
| `wireform-lance` | Apache Lance data file + manifest + dataset reader |
| `wireform-csv` | CSV / TSV / pipe-separated |
| `wireform-ndjson` | Newline-delimited JSON |
| `wireform-xml` | High-performance XML SAX/DOM, XPath, XSLT 1.0 subset, codegen |
| `wireform-html` | HTML5 tokenizer, tree builder, DOM, CSS selectors, streaming rewriter |
| `wireform-grpc` | gRPC client/server (vendored from `grapesy`) over `wireform-proto` |
| `wireform-kafka` | Native Apache Kafka client (TCP / TLS / SASL / compression / consumer groups / transactions / pipelining / OTel) |

### Module conventions inside each format package

Every per-format package follows the same internal layout, so once you
know one you know them all. See [`agents.md`](agents.md) for the full
contributor guide.

```
<Format>                       -- top-level API umbrella (re-exports)
<Format>.Encode / .Decode      -- encode/decode primitives + typeclass dispatch
<Format>.Class                 -- public typeclass(es) (e.g. ToCBOR / FromCBOR)
<Format>.Derive                -- annotation-driven Template Haskell deriver
<Format>.Value                 -- (where applicable) the dynamic Value ADT
<Format>.JSON                  -- bridge to/from JSON for self-describing formats
```

The `Proto.*` package predates the per-format split and keeps its
historical `Proto.AST` / `Proto.Parser.*` / `Proto.Wire.*` /
`Proto.CodeGen.*` / `Proto.TH` / `Proto.Derive.*` /
`Proto.Google.Protobuf.*` layout. See [`agents.md`](agents.md) for the
full Proto map.

---

## The annotation-driven deriver

`wireform-derive` provides a single annotation vocabulary that drives
instance generation for every supported wire format. One `{-# ANN ... #-}`
pragma on a Haskell record, one TH splice per format you care about, and
the same vocabulary picks up renames, tags, defaults, and per-backend
overrides for all of them.

The vocabulary lives in `Wireform.Derive.Modifier`:

- `rename "wireKey"`, `renameStyle SnakeCase`, `renameWith 'fn`,
  `renameIdiomatic` control wire-key text. `Idiomatic` resolves at splice
  time to the right convention per backend (camel for JSON, snake for
  proto and Avro, kebab for HTML and EDN, verbatim for CBOR / MsgPack /
  Thrift).
- `tag N` is the explicit field number / Thrift field ID / Bond ID /
  proto field number / Iceberg field ID. Required by formats that need
  it (proto, Bond) and ignored by formats that don't.
- `skip`, `defaults 'fn`, `required`, `optional`, `coerced 'Target`,
  `flatten`, `wireOverride WireZigZag` and `WireFixed` are the standard
  knobs.
- `forBackend backendJSON (rename "fullName")`,
  `disableFor [backendJSON]`, `forBackends [backendCBOR, backendMsgPack] ...`
  are per-backend overrides that shadow globals without conflicting.
- `mapKey MapKeyString` and `oneof "envelope_choice"` are proto-specific
  shape hints that other backends ignore.
- `extension XmlFieldOpt`, `extension HtmlFieldOpt`, `extension Asn1Tag`
  are typed per-backend payloads, plumbed through the `BackendModifier`
  typeclass so format packages can ship their own configuration types
  without pushing them into the core ADT.

### Worked example: one record, four wire formats

From [`examples/DeriveExample.hs`](examples/DeriveExample.hs) (run with
`cabal run example-derive`). One `Person` record carries snake_case as the
default for every backend, a JSON-only camelCase override on
`personFullName`, and a JSON-only `skip` on `personSecret`:

```haskell
data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  , personBalance  :: !Int64
  , personSecret   :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}

{-# ANN personFullName (tag 1) #-}
{-# ANN personAge      (tag 2) #-}
{-# ANN personBalance  (tag 3) #-}
{-# ANN personSecret   (tag 4) #-}

{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}
{-# ANN personBalance  (renameStyle SnakeCase) #-}
{-# ANN personSecret   (renameStyle SnakeCase) #-}

{-# ANN personFullName (forBackend backendJSON (rename "fullName")) #-}
{-# ANN personSecret   (forBackend backendJSON skip) #-}

DProto.deriveProto    ''Person
DCBOR.deriveCBOR      ''Person
DMP.deriveMsgPack     ''Person
DAeson.deriveJSON     ''Person
```

`personFullName` becomes `full_name` on every binary wire but `fullName`
in JSON, and `personSecret` is omitted from JSON entirely. All driven by
the same annotations.

### How the per-backend derivers compose

Every per-format package ships a `<Format>.Derive` module that imports
`Wireform.Derive` and consumes the same `Modifier` vocabulary. The
derivers all follow the same shape, so adding support for a new format
usually means cloning the nearest existing `<Format>.Derive` and adapting
the value-mapping calls. The currently shipping derivers cover roughly
25 backends: Aeson, CBOR, MsgPack, Thrift, Proto, BSON, Ion, EDN, TOML,
YAML, Bencode, NDJSON, CSV, XML, HTML, ASN.1, FlatBuffers, Cap'n Proto,
Avro, Bond, Fory, Arrow, Parquet, ORC, Iceberg.

### `BackendModifier` extensions

Some backends need per-backend configuration that doesn't make sense to
push into the core `Modifier` ADT. The `BackendModifier` typeclass is
the escape hatch:

```haskell
class (Eq a, Show a, Read a, Typeable a) => BackendModifier a where
  backendModifierTag :: Proxy a -> Text
```

Each backend declares its own ADT under a unique tag namespace. Current
in-tree examples:

- `XmlFieldOpt = AsAttribute | AsElement` (`wireform-xml.field-opt`).
- `HtmlFieldOpt = AsAttr | AsChild` (`wireform-html.field-opt`).
- `Asn1Tag = Implicit Int | Explicit Int | Universal`
  (`wireform-asn1.field-opt`).

Multiple extensions can coexist on the same `Name` because their tags
differ. Annotations attach via `extension`, and per-backend deriver code
reads them via `lookupExtension` / `lookupExtensions` / `hasExtension`.

---

## Protocol Buffers

`wireform-proto` is the largest package and predates the per-format
split. It carries its own IDL parser, AST, pure-text code generator,
TH/QQ splices, JSON mapping, well-known types, dynamic decoder, and
`.pbtxt` text format. There are two TH-driven paths into the deriver,
both of which feed the same body builders in `Proto.Derive.Internal`:

1. **`Proto.TH.loadProto "path/to/file.proto"`** is the IDL bridge. It
   parses the `.proto` file, generates the data declarations and the
   wire codec instances, and preserves unknown fields automatically. All
   five proto3 field shapes (singular, `Maybe a`, repeated
   `Vector` / list / `Seq`, `map<K, V>`, `oneof`, `enum`) are bridged
   end-to-end. Custom string and bytes representations (`LazyText`,
   `ShortText`, `HsString`, `LazyBytes`, `ShortBytes`) are honoured per
   field through `loadProtoWith`.
2. **`Proto.Derive.deriveProto`** is the annotation-driven path on a
   Haskell record where every field carries an explicit `tag N`. It
   auto-detects each field's shape from the Haskell type: scalars,
   submessages, `Maybe` wrappers, plus `Vector` / `[]` / `Seq` repeated
   containers, `Map.Map` map fields, sum-of-tagged-singletons oneofs,
   and `Enum`-shaped types (every constructor nullary). Repeated packable
   scalars are encoded packed by default; the deriver accepts both packed
   and unpacked on the read side regardless of which the writer chose.
   The IDL bridge is only required for cases the reify graph can't see
   (e.g. types declared in the same splice).
3. **`Proto.Derive.deriveProtoFromTranslated`** is the explicit-shape
   entry used by IDL bridges that need to call the deriver from inside
   their own splice. It sidesteps the GHC TH stage restriction that
   prevents `qReify`-ing types declared in the same splice.

The TH-emitted code uses `Proto.Encode.Archetype` (`archVarint`,
`archFixed64`, ...) so the encode hot path is identical in shape to what
the pure-text codegen produces. `Proto.QQ` ships an inline quasiquoter,
`Proto.Setup` is the Cabal setup hook for pre-build codegen, and
`protoc-gen-wireform` is a `protoc` plugin (`--wireform_out=DIR`).
Proto2 typed extensions (`HasExtensions`), unknown-field preservation,
and dynamic / `.pbtxt` decoding all carry through.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)

$(loadProto "examples/proto/simple.proto")
-- Generates: GetPersonRequest, ListPeopleRequest, AddPersonResponse, ...

let req = defaultGetPersonRequest { personId = 42 }
let bytes = encodeMessage req       -- 2 bytes (0x08 0x2a)
case decodeMessage bytes of
  Right (decoded :: GetPersonRequest) -> ...
```

For custom field representations, `loadProtoWith` accepts a `LoadOpts`
with field- and message-level overrides. See
[`examples/CustomReprExample.hs`](examples/CustomReprExample.hs).

---

## Building

```bash
cabal update
cabal build all
```

`cabal build all` builds the whole workspace. LLVM is off workspace-wide
(`-fasm` is set in `cabal.project`), so a vanilla GHC toolchain is enough
to build the repo. Compiling with the LLVM backend (`-fllvm`) adds
noticeable compile time but produces measurably faster runtime code, so
production builds typically want it on.

The deriver-related work uses `--builddir=dist-derive` to keep its build
state separate from `dist-newstyle/`:

```bash
cabal build all --builddir=dist-derive
```

A Nix flake is provided. Every `wireform-*` package plus the umbrella
`wireform` is wired into the haskell-package overlay, so `nix develop`
brings up a shell containing every workspace package's deps. Pick a GHC
by name:

```bash
nix develop          # default (currently GHC 9.8)
nix develop .#ghc96  # GHC 9.6
nix develop .#ghc910 # GHC 9.10
```

Per-format packages are also reachable as `nix build .#wireform-proto`,
`.#wireform-iceberg`, etc. The `wireform` umbrella is the default
`nix build` output.

---

## Testing

```bash
cabal test all
```

### Protobuf conformance suite

`wireform-proto` ships an end-to-end harness that runs the official
[upstream protobuf conformance suite](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
against `loadProto`-generated codecs. The harness skips cleanly when the
upstream runner isn't built:

```bash
# One-time: clone + build the upstream runner (~10 min, requires
# git, cmake, a C++17 toolchain).
bash wireform-proto/test-conformance/scripts/build-conformance-runner.sh

# Then:
cabal test wireform-proto:protobuf-conformance-test
```

Current baseline against `protocolbuffers/protobuf@v28.2`: 2675
successes, 0 skipped, 0 expected failures, 0 unexpected failures, across
the proto3 and proto2 binary/json suites.

Well-known types (`Timestamp`, `Duration`, `Wrappers`, `Empty`, `Any`,
`FieldMask`, `Struct`, `Value`, `ListValue`, `NullValue`) are supported
via a per-FQN registry in `Proto.TH.lookupWkt` that routes `loadProto`
references to the pre-generated `Proto.Google.Protobuf.*` modules. The
JSON encoder and parser use the proto3-canonical helpers in
`Proto.JSON.WellKnown` (RFC 3339 for Timestamps, `"1.5s"` for Durations,
base64 for Bytes wrappers, bare-value for the rest).

`TEXT_FORMAT` output is supported via
`Proto.TextFormat.typedToTextPretty`, which walks a typed message via
its `ProtoMessage` descriptors and emits pbtxt with field names.

See [`wireform-proto/test-conformance/README.md`](wireform-proto/test-conformance/README.md)
for the architecture and how to add expected failures.

### Other test suites

Annotation-driven deriver coverage spans every per-format package, with
hundreds of tests across the `wireform-*-derive-test` suites plus a
shared core suite under `wireform-derive`. Each format package also has
its own non-derive test suite for the codecs, value ADT, schema parsers,
and protocol framing.

Several format packages also have opt-in interop tests against
upstream runners (silent skip when the runner isn't installed):

- `wireform-toml` against [`toml-test`](https://github.com/toml-lang/toml-test)
  via `TOML_TEST_SUITE=...`.
- `wireform-yaml` against [`yaml-test-suite`](https://github.com/yaml/yaml-test-suite)
  via `YAML_TEST_SUITE=...`.
- `wireform-iceberg` against `pyiceberg` + `fastavro`.
- `wireform-delta` against `delta-rs`.
- `wireform-hudi` against `hudi-rs`.
- `wireform-lance` against `pylance`.
- `wireform-fory` against `pyfory` (45 / 45 cases passing for
  the implemented type set; pyfory-compatible
  `NAMED_COMPATIBLE_STRUCT` is the remaining gap).
- `wireform-kafka` against a live broker via
  `WIREFORM_KAFKA_BROKER=host:port`.

---

## Examples

Runnable from the workspace root with `cabal run <name>`:

| Command | What it shows |
|---------|---------------|
| `example-derive` | One `Person` record producing proto + CBOR + MsgPack + JSON in a single TH splice |
| `example-th` | `loadProto` generating proto types and a round-trip encoding |
| `example-extensions` | proto2 typed extensions: `setExtension`, repeated extensions, unknown-field preservation through wire round-trip |
| `example-custom-repr` | `loadProtoWith` with `LazyBytes` / `ShortBytes` / `ListRep` field overrides |
| `example-qq` | `Proto.QQ` inline quasiquoter |
| `example-codegen` | `wireform-gen`-style proto codegen in-process |
| `example-setup-hook` | Cabal `Proto.Setup` pre-build codegen hook |
| `example-basic` / `example-protobuf` | Hand-written protobuf encode/decode |
| `example-any` / `example-wellknown` | `google.protobuf.Any` and well-known types |
| `example-msgpack` / `example-cbor` / `example-bson` / `example-edn` / `example-ion` | Generic deriving for schema-less binary formats |
| `example-thrift` / `example-avro` / `example-capnproto` / `example-flatbuffers` / `example-bond` / `example-asn1` | Schema-driven IDL formats |
| `example-xml` | Generic XML encode/decode |
| `example-parquet` / `example-arrow` / `example-iceberg` / `example-iceberg-pipeline` | Analytics file formats and metadata |
| `example-dataframe-bridge` (`+dataframe-bridge` flag) | Write a Parquet file with `wireform-parquet`, read it back through the [`dataframe`](https://hackage.haskell.org/package/dataframe) library, run aggregations, cross-check against pure-Haskell ground truth. Behind a Cabal flag because the `dataframe` dep tree is large; build with `cabal run example-dataframe-bridge -fdataframe-bridge`. |

---



## License

BSD-3-Clause. See [LICENSE](LICENSE) for the full license text and
third-party attributions.
