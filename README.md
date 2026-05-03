# wireform

**One Haskell monorepo unifying serialization across 23+ wire formats** —
Protocol Buffers, CBOR, MessagePack, Thrift, JSON, BSON, Amazon Ion, EDN,
TOML, Bencode, NDJSON, CSV, XML, HTML, ASN.1, Avro, Bond, FlatBuffers,
Cap'n Proto, Apache Arrow, Apache Parquet, Apache ORC, and Apache Iceberg —
behind a single annotation-driven derivation system that lets one
`{-# ANN ... #-}`-annotated record drive instance generation for every
backend simultaneously.

The whole stack is built on top of a small set of allocation-disciplined
core primitives (unboxed sums, `Int#`-threaded decoders, sized two-pass
encoding, SIMD-accelerated XML/HTML, C FFI for hot paths) so generated
code is competitive with hand-written codecs.

> **Active development.** The annotation-driven deriver and the proto
> IDL bridge that ties `loadProto` to it are landing in
> [PR #18](https://github.com/iand675/wireform-/pull/18).

---

## Table of contents

- [What's in here](#whats-in-here)
- [Package layout](#package-layout)
- [The annotation-driven deriver](#the-annotation-driven-deriver)
- [Protocol Buffers](#protocol-buffers)
- [Building](#building)
- [Testing](#testing)
- [Examples](#examples)
- [Status / what's incomplete](#status--whats-incomplete)
- [Contributing](#contributing)
- [License](#license)

---

## What's in here

This is not a thin "encode/decode" library — each format ships with the
machinery you actually need to use it in production:

- **Encode / decode** for every format, with the same allocation-disciplined
  primitives across all of them.
- **Generic / annotation-driven deriving** that targets every format from a
  single set of pragmas.
- **IDL parsers and code generators** for `.proto`, `.avsc` / `.avdl`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, ISL, CDDL, XSD, Iceberg
  table metadata.
- **Streaming and incremental decoders** for protobuf, MsgPack, CBOR,
  XML, NDJSON.
- **RPC framing** for gRPC (length-prefix), Thrift (binary + compact),
  msgpack-rpc, Avro IPC.
- **Container file I/O** for Avro OCF, Parquet (footer + page index +
  bloom filter + column reads), Arrow IPC, ORC (postscript + stripes),
  Iceberg (table metadata + manifests).
- **Schema resolution and evolution** for Avro and proto2 ↔ proto3
  compatibility.
- **Dynamic / untyped** protobuf messages and `.pbtxt` text-format I/O.
- **XML pipeline** beyond encode/decode: SIMD-accelerated SAX, zero-copy
  DOM, chunk-fed incremental + concurrent parse, XPath-lite queries, an
  XSLT 1.0 subset.
- **HTML5** spec-compliant tokenizer / tree builder, SIMD serializer, CSS
  selectors.

---

## Package layout

The repo is a workspace of `wireform-*` packages. Each per-format package
is self-contained and depends only on `wireform-core` (shared primitives),
`wireform-derive` (annotation vocabulary), and the third-party libraries
its format actually needs.

| Package | Role |
|---------|------|
| `wireform-core` | Shared FFI primitives (encode buffers, hashing) for every format package |
| `wireform-derive` | Annotation-driven Template Haskell deriver core (`Modifier` / `NameStyle` / `TypeInfo` / `BackendModifier`) |
| `wireform-columnar` | SIMD-accelerated columnar primitives (bit-unpacking) shared by Arrow / Parquet / ORC |
| `wireform-proto` | Protocol Buffers (proto2 / proto3): IDL parser, codegen, runtime, JSON mapping, well-known types |
| `wireform-cbor` | CBOR (RFC 8949) values, encoding/decoding, JSON bridge, CDDL |
| `wireform-msgpack` | MessagePack values, encoding/decoding, JSON bridge, msgpack-rpc |
| `wireform-thrift` | Apache Thrift binary + compact protocol, schema, JSON mapping |
| `wireform-bson` | BSON (MongoDB wire format) |
| `wireform-ion` | Amazon Ion binary values, ISL schema, codegen |
| `wireform-edn` | Extensible Data Notation (EDN) |
| `wireform-toml` | TOML |
| `wireform-bencode` | BitTorrent bencode |
| `wireform-asn1` | ASN.1 BER / DER (ITU-T X.690) |
| `wireform-avro` | Apache Avro: schema resolution, JSON conversion, IPC protocol |
| `wireform-bond` | Microsoft Bond compact binary |
| `wireform-flatbuffers` | FlatBuffers zero-copy flat serialization |
| `wireform-capnproto` | Cap'n Proto zero-copy serialization |
| `wireform-arrow` | Apache Arrow IPC schema framing and record batch materialization |
| `wireform-parquet` | Apache Parquet metadata, column pages, page index, bloom filter, read/write |
| `wireform-orc` | Apache ORC metadata, stripes, read/write |
| `wireform-iceberg` | Apache Iceberg table metadata, manifests, schema evolution |
| `wireform-csv` | CSV / TSV |
| `wireform-ndjson` | Newline-delimited JSON |
| `wireform-xml` | High-performance XML SAX/DOM, XPath, XSLT, codegen |
| `wireform-html` | HTML5 tokenizer, tree builder, DOM, CSS selectors, streaming rewriter |
| `wireform-grpc` | Native gRPC client/server over `wireform-proto` |

### Module conventions inside each format package

Every per-format package follows the same internal layout, so once you
know one you know them all. See [AGENTS.md](AGENTS.md) for the full
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
`Proto.Google.Protobuf.*` layout — see [AGENTS.md](AGENTS.md) for the
full Proto map.

---

## The annotation-driven deriver

`wireform-derive` provides a **single annotation vocabulary** that drives
instance generation for every supported wire format. One `{-# ANN ... #-}`
pragma on a Haskell record, one TH splice per format you care about, and
the same vocabulary picks up renames, tags, defaults, and per-backend
overrides for all of them.

The vocabulary lives in `Wireform.Derive.Modifier`:

- `rename "wireKey"`, `renameStyle SnakeCase`, `renameWith 'fn`,
  `renameIdiomatic` — control wire-key text. `Idiomatic` resolves at
  splice time to the right convention per backend (camel for JSON, snake
  for proto/Avro, kebab for HTML/EDN, verbatim for CBOR/MsgPack/Thrift).
- `tag N` — explicit field number / Thrift field ID / Bond ID / proto
  field number / Iceberg field ID. Required by formats that need it
  (proto, Bond) and ignored by formats that don't.
- `skip`, `defaults 'fn`, `required`, `optional`, `coerced 'Target`,
  `flatten`, `wireOverride WireZigZag` / `WireFixed` — standard knobs.
- `forBackend backendJSON (rename "fullName")`,
  `disableFor [backendJSON]`, `forBackends [backendCBOR, backendMsgPack] …`
  — per-backend overrides that shadow globals without conflicting.
- `mapKey MapKeyString`, `oneof "envelope_choice"` — proto-specific shape
  hints (other backends ignore them).
- `extension XmlFieldOpt` / `extension HtmlFieldOpt` / `extension Asn1Tag`
  — typed per-backend payloads via the `BackendModifier` typeclass, so
  format packages can ship their own configuration types without
  pushing them into the core ADT.

### Worked example: one record, four wire formats

From [`examples/DeriveExample.hs`](examples/DeriveExample.hs) (run with
`cabal run example-derive`) — one `Person` carries snake-case as the
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

Every per-format package ships its own `<Format>.Derive` module that
imports `Wireform.Derive` and consumes the same `Modifier` vocabulary.
The derivers are structural twins of one another, so adding support for
a new format mostly means cloning the nearest existing `<Format>.Derive`
and adapting the value-mapping calls. Currently shipping derivers cover
**23 backends**: Aeson, CBOR, MsgPack, Thrift, Proto, BSON, Ion, EDN,
TOML, Bencode, NDJSON, CSV, XML, HTML, ASN.1, FlatBuffers, Cap'n Proto,
Avro, Bond, Arrow, Parquet, ORC, Iceberg.

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
`.pbtxt` text format. There are now **two TH-driven paths into the
deriver**, both of which feed the same body builders in
`Proto.Derive.Internal`:

1. **`Proto.TH.loadProto "path/to/file.proto"`** — IDL bridge. Parses
   the `.proto` file, generates the data declarations + the wire codec
   instances, and preserves unknown fields automatically. As of the
   late-night rewire all five proto3 field shapes (singular,
   `Maybe a`, repeated `Vector`/list/`Seq`, `map<K,V>`, `oneof`,
   `enum`) are fully bridged end-to-end. Custom string / bytes
   representations (`LazyText`, `ShortText`, `HsString`, `LazyBytes`,
   `ShortBytes`) are honoured per-field through `loadProtoWith`. The
   `wireform-proto-derive-test` suite jumped from 27 to 34 tests when
   the oneof rewire landed earlier today.
2. **`Proto.Derive.deriveProto`** — annotation-driven path on a Haskell
   record where every field carries an explicit `tag N`. Field types
   are picked from the Haskell type via `pickFieldType`. Convenient when
   the canonical schema is the Haskell type rather than a `.proto`
   file. Currently auto-detects scalars, submessages, and `Maybe`
   wrappers; for `Vector` / `Map` / oneof / enum fields use the IDL
   bridge instead (see Outstanding work below).
3. **`Proto.Derive.deriveProtoFromTranslated`** — explicit-shape entry
   used by IDL bridges that need to call the deriver from inside their
   own splice. Sidesteps the GHC TH stage restriction that prevents
   `qReify`-ing types declared in the same splice.

The TH-emitted code uses `Proto.Encode.Archetype` (`archVarint`,
`archFixed64`, …) so the encode hot path is identical in shape to what
the pure-text codegen produces. **`Proto.QQ`** ships an inline
quasiquoter; `Proto.Setup` is the Cabal setup hook for pre-build codegen;
`protoc-gen-wireform` is a `protoc` plugin (`--wireform_out=DIR`).
Proto2 typed extensions (`HasExtensions`), unknown-field preservation,
and dynamic / `.pbtxt` decoding all carry through. Oneofs work
end-to-end as of today's rewire.

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
with field- and message-level overrides — see
[`examples/CustomReprExample.hs`](examples/CustomReprExample.hs).

---

## Building

```bash
cabal update
cabal build all
```

`cabal build all` builds the whole workspace. LLVM is OFF
workspace-wide (`-fasm` set in `cabal.project`), so a vanilla GHC
toolchain is sufficient.

The deriver-related work uses `--builddir=dist-derive` to keep its
build state separate from `dist-newstyle/`:

```bash
cabal build all --builddir=dist-derive
```

The Nix flake's package set has not yet been updated to include the
per-format packages added during the recent split, so `nix develop`
currently doesn't evaluate. `cabal` works directly against the system
GHC (or against `NIX_GHC` if that env var is set). Re-wiring the flake
is on the [outstanding-work list](#status--whats-incomplete).

---

## Testing

```bash
cabal test all
```

Annotation-driven deriver coverage spans 23 backends with hundreds of
tests across the `wireform-*-derive-test` suites — every per-format
package ships its own deriver test suite plus a shared core suite under
`wireform-derive`. Today's milestone: `wireform-proto-derive-test` grew
from 27 to 34 when the `loadProto` oneof rewire landed (a new
`oneof_regression.proto` fixture covers empty / label-only / each
variant individually / proto3 last-wins overwrite semantics).

Each format package also has its own non-derive test suite for the
codecs, value ADT, schema parsers, and protocol framing.

---

## Examples

Runnable from the workspace root with `cabal run <name>`:

| Command | What it shows |
|---------|---------------|
| `example-derive` | One `Person` record → proto + CBOR + MsgPack + JSON in a single TH splice |
| `example-th` | `loadProto` → generated proto types and round-trip encoding |
| `example-extensions` | proto2 typed extensions: `setExtension` / repeated extensions / unknown-field preservation through wire round-trip |
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

---

## Status / what's incomplete

This monorepo is under active development on
[PR #18](https://github.com/iand675/wireform-/pull/18). Known gaps that
are tracked but not yet landed:

- **Hand-coded golden bytes for the proto byte-equivalence regression.**
  The four byte-equivalence tests in `Test.Proto.Derive` previously
  compared two implementations (`loadProto` vs
  `deriveProtoFromTranslated`); after the `loadProto` rewire both paths
  go through the same body builders in `Proto.Derive.Internal`, so the
  assertion has degraded to "the bridge agrees with itself". Adding
  hand-computed reference bytes would restore the meaningful regression.
- **Annotation-driven `deriveProto` doesn't auto-detect repeated /
  map / enum / oneof yet.** The detection logic exists in the IDL
  bridge as `tfRepeated` / `tfMapKey` / `tfIsEnum` / `tfOneofVariants`
  shape hints; lifting it into `analyseField` (with type-shape detection
  for outer `Vector` / `[]` / `Map` constructors and reify-driven enum
  detection) is the next obvious step. Annotation users currently can't
  say "this `Vector Foo` is a repeated submessage" without going
  through the bridge.
- **Packed encoding for repeated scalars.** Proto3's default for
  packable repeated scalars is packed; the bridge currently emits
  unpacked. The decoder already accepts both per the proto3 spec, so
  this is encoder-side only.
- **Per-variant string/bytes reps for oneof variants.** `loadProto`'s
  `FieldRep` config doesn't apply to oneof variants today (the
  `OneofField` AST node has no `FieldRep` slot). Oneof variants always
  use strict `Text` / `ByteString` regardless of how the parent message
  is configured.
- **Top-level enum `loadProto`.** `Proto.TH` translates `FTNamed` as
  `PFSubmessage`, which means top-level enum types still encode as
  length-delimited submessages rather than varints. Routing `TLEnum`
  through the bridge with `PFEnum` requires building an `Enum` instance
  for the generated enum type and tracking which named types are
  enums vs. messages at the call site.
- **Other proto features.** `ProtoMessage` schema metadata, proto3 JSON
  with the canonical camelCase + well-known-type rules, `Hashable`
  derivation.
- **Nix flake's package set.** The 18 newly added `wireform-*` packages
  aren't wired into the flake yet, so `nix develop` doesn't evaluate.
  `cabal build` works directly against the system / `NIX_GHC` toolchain
  in the meantime.

---

## Contributing

Contributor notes, code-generation principles, allocation discipline
rules, and per-package conventions live in
[**AGENTS.md**](AGENTS.md). Highlights:

- Every message type comes from the code generator. Hand-written wire
  encode/decode instances are not permitted because they drift from
  what the code generator produces and mask codegen bugs.
- Unboxed sums for finite branching, `Int#`-threaded offsets, no boxed
  `Either` / `Maybe` on the hot path.
- Never round-trip through `String`. No list comprehensions. No
  `threadDelay` in tests. Property-based tests via Hedgehog.
- The four-step recipe for adding a new constructor to
  `Wireform.Derive.Modifier.Modifier` is in AGENTS.md.

---

## License

BSD-3-Clause. See [NOTICE](NOTICE) for third-party attributions.
