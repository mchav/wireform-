# wireform

You need to serialize a thing. Maybe it's protobuf, because the team
next door decided proto is what events look like. Maybe it's Avro,
because Iceberg is involved. Maybe it's CBOR, because something nearby
speaks COSE. The exact format doesn't matter. The next ten minutes are
roughly the same regardless.

You open Hackage. There are usually a couple of packages. Time to do
the audit.

**Is it fast?** The README doesn't say. The benchmarks live in a
`bench/` directory last touched in 2019, when GHC was on a different
number. Inconclusive.

**Does it pass the upstream conformance suite?** The README doesn't
mention a conformance suite. There is, of course, one. Every serious
format has one. You've just never seen one wired into a Haskell test
suite, and this package isn't going to break the streak.

**Has anyone touched it lately?** Define "lately." There's a commit
from six months ago that says `bump bounds`, and before that one from
two years ago that says `wip`. The author has, in the intervening
period, started writing Rust full-time. Understandable, but
inconvenient.

**Will it pull half of Hackage in?** Let's check the cabal file.
`bytestring`, fine. `text`, fine. `lens`, because the author wanted
`view _Just` somewhere. `lens-aeson`, naturally. A `containers` upper
bound that hasn't shifted since 2014. You start to wonder if maybe
you should just write this yourself.

**Will the API match the rest of your stack?** It will not. The JSON
library you already use spells it `eitherDecodeStrict`. The CBOR
library spells it `deserialiseFromBytes`. The MsgPack library spells
it `unpack`. They all return slightly different error types, none of
which are interconvertible. You write a small adapter the first time
you reach for them. By the third time, you start suspecting the
adapter is the actual thing you're building.

You do this audit a lot. After enough rounds you notice that the audit
*is* the work, and that the custom code you keep writing on top of
these packages is, with cosmetic differences, the same custom code:
same allocation tricks, same Template Haskell deriver boilerplate,
same property tests, same emergency bridge to JSON for the format
that didn't think it would need one.

Surely someone has already solved this, you think. Rust has `serde`, for example.

Thus, `wireform` was born.

In practice that's a monorepo of roughly thirty format packages where
every one shares the extremely performant core utilities (`wireform-core`),
the same annotation-driven Template Haskell deriver (`wireform-derive`),
aggressively complete test suites, and, where an upstream conformance
suite exists, an opt-in test runner that wires it up. For example, Protobuf runs
against the official `protocolbuffers/protobuf` harness. TOML runs
against `toml-test`. YAML runs against `yaml-test-suite`. Iceberg,
Delta Lake, Hudi, and Lance round-trip through their respective Python
or Rust readers. Fory rides on `pyfory`. Kafka clients test against a live broker.
The answer to "is this format actually conformant?" stops being "I
sure hope so" and starts being "here is the suite, here is the score."

The minimal-dependency posture is the same instinct pointed at your
build plan. Each per-format package depends on `wireform-core`,
`wireform-derive`, and the third-party libraries the format genuinely
needs. Usually that's `bytestring`, `text`, `vector`, and whatever
schema library is unavoidable, and nothing else. If you only need CBOR, you only build CBOR.

By taking this approach, one annotated Haskell record can give you wire 
codecs for Protocol Buffers, CBOR, MessagePack, Thrift, JSON, BSON, Amazon Ion, EDN, TOML,
YAML, Bencode, NDJSON, CSV, XML, HTML, ASN.1, Avro, Bond, FlatBuffers,
Cap'n Proto, Apache Arrow, Apache Parquet, Apache ORC, Apache Iceberg,
and Apache Fory. Delta Lake, Hudi, and Lance ship as table-format readers on
top of the columnar core.

The Kafka and gRPC client and server code is native Haskell rather than depending
on C libraries, which allow us to build them more easily and exceed the performance of the official implementations by avoiding the overhead of FFI calls.

Every format draws on the same handful of
heavily optimized primitives. SIMD-accelerated parsing, zero-copy encoding/decoding where possible, and specialized C kernels for the hottest paths. All generated code in the repo
is held to the standard that it is has to be as fast as, or faster than, hand-written codecs.

`wireform` as a project is unapologetically maximalist. If it
parses, renders, encodes, decodes, frames, or otherwise shuffles bytes
between two systems, it's in scope. The current thirty packages are a starting
point for the project, but new format packages are welcome and actively
wanted, provided they clear the same bar the existing ones had to:
fast enough to rival C/Rust/Zig, minimal garbage collection overhead, 
tested hard enough to prove it fully conforms with the format's official conformance suite (or, where no such
suite exists, with an explicit interop test against another language's
implementation), wired into the shared annotation deriver so users
don't learn a new API per format, and dependency-light enough to not raise eyebrows. Acceptance criteria in full are
under [Adding a new format](#adding-a-new-format).

---

## Table of contents

- [What's in here](#whats-in-here)
- [Package layout](#package-layout)
- [The annotation-driven deriver](#the-annotation-driven-deriver)
- [Building](#building)
- [Testing](#testing)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

---

## What's in here

Each format ships with the machinery you actually need to use it in
production, not just `encode` / `decode`, including but not limited to:

- Encode and decode for every format, sharing the same allocation
  primitives.
- Generic and annotation-driven deriving that targets every format from a
  single set of pragmas.
- IDL parsers and code generators for `.proto`, `.avsc` / `.avdl`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, ISL, CDDL, XSD, and Iceberg
  table metadata.
- Streaming and incremental decoders for a wide array of formats, such as protobuf, MsgPack, CBOR, XML, NDJSON, and YAML.
- RPC framing for gRPC (length-prefix), Thrift (binary and compact),
  msgpack-rpc, and Avro IPC.
- A native Kafka client: TCP / TLS / SASL, compression, version
  negotiation, transactions, consumer groups, pipelining, Kafka Streams support, and built-in OpenTelemetry instrumentation.
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
- A world-class HTML5 DOM library with a spec-compliant tokenizer and tree builder, CSS selector support, and a streaming rewriter. According to earlier benchmarks I've run, it is possibly the fastest open-source HTML5 DOM library in the world.

---

## Package layout

The repo is a workspace of `wireform-*` packages. Each per-format package
is self-contained and depends only on `wireform-core` (shared primitives),
`wireform-derive` (annotation vocabulary), and the third-party libraries
its format actually needs.

| Package | Role |
|---------|------|
| [`wireform-core`](wireform-core/README.md) | Shared FFI primitives (encode buffers, hashing) used by every format package |
| [`wireform-derive`](wireform-derive/README.md) | Annotation-driven Template Haskell deriver core (`Modifier`, `NameStyle`, `TypeInfo`, `BackendModifier`) |
| [`wireform-columnar`](wireform-columnar/README.md) | SIMD-accelerated columnar primitives (bit-unpacking, predicate vocabulary, pull-based iter combinators) shared by Arrow, Parquet, and ORC |
| [`wireform-proto`](wireform-proto/README.md) | Protocol Buffers (proto2 / proto3): IDL parser, codegen, runtime, JSON mapping, well-known types |
| [`wireform-cbor`](wireform-cbor/README.md) | CBOR (RFC 8949) values, encoding/decoding, JSON bridge, CDDL schema and codegen |
| [`wireform-msgpack`](wireform-msgpack/README.md) | MessagePack values, encoding/decoding, JSON bridge, msgpack-rpc |
| [`wireform-thrift`](wireform-thrift/README.md) | Apache Thrift binary and compact protocol, schema, JSON mapping |
| [`wireform-bson`](wireform-bson/README.md) | BSON (MongoDB wire format) |
| [`wireform-ion`](wireform-ion/README.md) | Amazon Ion binary values, ISL schema, codegen |
| [`wireform-edn`](wireform-edn/README.md) | Extensible Data Notation (EDN) |
| [`wireform-toml`](wireform-toml/README.md) | TOML 1.0 / 1.1 |
| [`wireform-yaml`](wireform-yaml/README.md) | YAML 1.2 (block + flow, anchors / aliases, tags, multi-document streams). It is the fastest YAML library for Haskell, has no external C library dependencies, and is the only one that passes 100% of the YAML 1.2 conformance suite. Also, it is the only one that is hardened against e.g. billion laughs attacks. |
| [`wireform-bencode`](wireform-bencode/README.md) | BitTorrent bencode |
| [`wireform-fory`](wireform-fory/README.md) | Apache Fory (formerly Fury) cross-language serialization, wire-compatible with `pyfory` 0.17 |
| [`wireform-asn1`](wireform-asn1/README.md) | ASN.1 BER / DER (ITU-T X.690) |
| [`wireform-avro`](wireform-avro/README.md) | Apache Avro: schema resolution, JSON conversion, IPC protocol, container files |
| [`wireform-bond`](wireform-bond/README.md) | Microsoft Bond compact binary |
| [`wireform-flatbuffers`](wireform-flatbuffers/README.md) | FlatBuffers zero-copy flat serialization |
| [`wireform-capnproto`](wireform-capnproto/README.md) | Cap'n Proto zero-copy serialization |
| [`wireform-arrow`](wireform-arrow/README.md) | Apache Arrow IPC schema framing and record batch materialization |
| [`wireform-parquet`](wireform-parquet/README.md) | Apache Parquet metadata, column pages, page index, bloom filter, read/write |
| [`wireform-orc`](wireform-orc/README.md) | Apache ORC metadata, stripes, read/write, predicate evaluator |
| [`wireform-iceberg`](wireform-iceberg/README.md) | Apache Iceberg table metadata, manifests, schema evolution, catalogs |
| [`wireform-delta`](wireform-delta/README.md) | Delta Lake transaction log + checkpoints + time travel |
| [`wireform-hudi`](wireform-hudi/README.md) | Apache Hudi timeline reader (Copy-on-Write, JSON + Avro instants) |
| [`wireform-lance`](wireform-lance/README.md) | Apache Lance data file + manifest + dataset reader |
| [`wireform-csv`](wireform-csv/README.md) | CSV / TSV / pipe-separated |
| [`wireform-ndjson`](wireform-ndjson/README.md) | Newline-delimited JSON |
| [`wireform-xml`](wireform-xml/README.md) | High-performance XML SAX/DOM, XPath, XSLT 1.0 subset, codegen |
| [`wireform-html`](wireform-html/README.md) | HTML5 tokenizer, tree builder, DOM, CSS selectors, streaming rewriter |
| [`wireform-grpc`](wireform-grpc/README.md) | gRPC client/server (vendored from `grapesy`) over `wireform-proto` |
| [`wireform-kafka`](wireform-kafka/README.md) | Native Apache Kafka client (TCP / TLS / SASL / compression / consumer groups / transactions / pipelining / OTel) |

### Module conventions inside each format package

Every per-format package follows the same internal layout:

```
<Format>                       -- top-level API umbrella (re-exports)
<Format>.Encode / .Decode      -- encode/decode primitives + typeclass dispatch
<Format>.Class                 -- public typeclass(es) (e.g. ToCBOR / FromCBOR)
<Format>.Derive                -- annotation-driven Template Haskell deriver
<Format>.Value                 -- (where applicable) the dynamic Value ADT
<Format>.JSON                  -- bridge to/from JSON for self-describing formats
```

`wireform-proto` additionally carries its own IDL parser
(`Proto.Parser`), code generator (`Proto.CodeGen`), TH splices
(`Proto.TH`), and well-known type modules (`Proto.Google.Protobuf.*`).
See the [wireform-proto README](wireform-proto/README.md) for the
full module map.

See [`agents.md`](agents.md) for the contributor guide.

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

### Conformance and interop suites

Annotation-driven deriver coverage spans every per-format package, with
hundreds of tests across the `wireform-*-derive-test` suites plus a
shared core suite under `wireform-derive`. Each format package also has
its own non-derive test suite for the codecs, value ADT, schema parsers,
and protocol framing.

Several format packages have opt-in interop tests against upstream
runners (silent skip when the runner isn't installed):

- `wireform-proto` against the official
  [`protobuf conformance suite`](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
  (2675/2675 tests passing).
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

## Contributing

Contributor notes, code-generation principles, allocation discipline
rules, and per-package conventions live in [`agents.md`](agents.md).
Highlights:

- Every message type comes from the code generator. Hand-written wire
  encode/decode instances are not permitted because they drift from what
  the code generator produces and mask codegen bugs.
- Unboxed sums for finite branching, `Int#`-threaded offsets, no boxed
  `Either` or `Maybe` on the hot path.
- Never round-trip through `String`. No list comprehensions. No
  `threadDelay` in tests. Property-based tests via Hedgehog.
- The four-step recipe for adding a new constructor to
  `Wireform.Derive.Modifier.Modifier` is in `agents.md`.

### Adding a new format

The maximalist scope from the intro turns into a checklist. A new
`wireform-<format>` package is in good shape to land when:

- **It's fast, and there's a benchmark to prove it.** At least one
  benchmark has to compare encode and decode against the next-best
  Hackage library, or, if no Haskell equivalent exists, against the
  reference implementation in another language. The goal isn't
  "fastest possible at any cost"; the goal is "no obvious slowness
  that a future user is going to discover for us." The shared
  `wireform-core` primitives (`Wireform.Encode.Direct`, the `Decoder`
  newtype, the `Wireform.FFI` helpers) exist precisely so you don't
  have to reinvent the allocation-discipline layer.
- **It's tested against something other than itself.** If the format
  has an official upstream conformance suite (and most of them do),
  wire it up as an opt-in test runner. The pattern is in
  [`wireform-proto/test-conformance/`](wireform-proto/test-conformance/)
  and the various `*-interop` test-suites: silent skip when the
  upstream runner isn't installed locally, full diff when it is. If
  the format genuinely has no upstream suite, ship interop tests
  against an implementation in another language. Python or Rust
  round-trip suites are the established pattern (see
  `wireform-iceberg/probe/`, `wireform-fory-interop/`, and
  `wireform-delta`'s `delta-rs` cross-checks).
- **It plugs into the shared deriver.** Every per-format package ships
  `<Format>.Derive` and consumes `Wireform.Derive.Modifier`
  annotations. The deriver shape is the same in every existing
  package, so cloning the nearest sibling and adapting the
  value-mapping calls is the path of least resistance. Format-specific
  knobs that don't fit the core vocabulary go through
  `BackendModifier` extensions; see `XmlFieldOpt`, `HtmlFieldOpt`, and
  `Asn1Tag` for the shape.
- **The IDL goes through codegen, not hand-written instances.** If
  your format has a schema language, the parser lives in
  `<Format>.Parser`, the codegen in `<Format>.CodeGen`, and the
  per-message instances are generated. Hand-written wire instances
  drift from what the codegen produces and mask codegen bugs; the
  rule is in `agents.md` and isn't negotiable. Generated files are
  output, not source: edit the codegen and regenerate, never edit the
  generated module directly.
- **Direct C dependencies need a justification.** New deps are fine when
  the format actually requires them. Heavy or flaky-to-install
  transitive trees go behind a Cabal flag (the existing `+zstd`,
  `+lz4`, `+rest-client`, `+brotli`, `+snappy`, `+python-interop`,
  and `+dataframe-bridge` flags are examples of this). If you only need
  CBOR, pulling in CBOR shouldn't pull in three columnar formats.
- **The module layout matches the per-format convention** from
  [Package layout](#package-layout). That convention is enforced by
  reviewers, not by tooling, so a quick `git diff` against a sibling
  package before opening a PR will save round trips.

Then file a PR. Smaller PRs that add the package skeleton + decoder +
property tests first, then layer codegen, IDL, and interop in
follow-ups, review faster than one giant drop.

---

## License

BSD-3-Clause. See [LICENSE](LICENSE) for the full license text and
third-party attributions.
