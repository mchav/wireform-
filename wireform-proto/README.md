# wireform-proto

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

`wireform-proto` is a fully conformant, high-performance Protocol Buffer implementation for Haskell.

It supports both proto2 and proto3 formats with its own IDL parser, which means no `protoc` binary is needed.

It is the fastest Haskell protobuf implementation available, with encode and decode performance within the same order of magnitude as the official C++ implementation.

`wireform-proto` is one package in the [`wireform`][wireform] monorepo.
The umbrella `wireform` package re-exports its public API as
`Wireform.Proto`.

[wireform]: https://github.com/iand675/wireform-

---

## The idea

Every protobuf library makes a fundamental choice about how messages
look in the host language. wireform-proto's answer is: *plain Haskell
records*. No optics layer, no opaque constructors, no `defMessage`.
You get types that look like the types you'd write by hand, except the
wire codecs come for free.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH     (loadProto)
import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage)

$(loadProto "proto/person.proto")
```

That splice parses your `.proto` at compile time and generates records
like this:

```haskell
data Person = Person
  { personName :: !Text
  , personAge  :: {-# UNPACK #-} !Int32
  } deriving stock (Show, Eq, Generic)
```

Construction is record syntax. Pattern matching works. The compiler
tells you when you've forgotten a field.

```haskell
let alice = Person { personName = "Alice", personAge = 30 }
let bytes = encodeMessage alice
case decodeMessage bytes of
  Right p  -> print (personName p)
  Left err -> print err
```

That's the whole API for the common case. But there are several
ways to get here depending on how your project is set up.

---

## Ways to use it

wireform-proto gives you six entry points to the same codegen
machinery. They all produce the same wire-format instances -- the
difference is where and when the code generation happens.

### `loadProto` -- TH splice from a `.proto` file

The simplest path. Point it at a `.proto` file and get types +
instances in a single splice.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)

$(loadProto "proto/messages.proto")
```

All messages and enums from the file (and its imports) land in scope.
No build system setup, no generated files to commit. The `.proto` is
parsed at compile time by wireform's own parser -- `protoc` is not
involved.

`loadProtoWith` accepts a `LoadOpts` for customizing field
representations (see [Custom field representations](#custom-field-representations)
below).

### `Proto.QQ` -- inline quasi-quoter

For one-off messages or quick prototyping, you can write the proto
definition directly in your Haskell source:

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Proto.QQ (proto)

[proto|
  syntax = "proto3";
  message SearchRequest {
    string query = 1;
    int32 page_number = 2;
    int32 result_per_page = 3;
  }
|]
```

`SearchRequest` is now a regular Haskell type with encode/decode/JSON
instances, defined right where you need it.

### `Proto.Derive` -- annotation-driven, no `.proto` file

If you'd rather define your Haskell types first and derive the wire
format from annotations, `deriveProto` does that:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.Derive (deriveProto, tag)

data Measurement = Measurement
  { sensorId    :: !Text
  , temperature :: !Double
  , timestamp   :: {-# UNPACK #-} !Int64
  } deriving stock (Show, Eq, Generic)

{-# ANN type Measurement ("Measurement" :: String) #-}
{-# ANN sensorId    (tag 1) #-}
{-# ANN temperature (tag 2) #-}
{-# ANN timestamp   (tag 3) #-}

deriveProto ''Measurement
```

This is useful when the Haskell types are the source of truth and you
want protobuf as a serialization format rather than a schema language.
You get the same `MessageEncode` / `MessageDecode` / `MessageSize`
instances as every other path.

### `Proto.Setup` -- Cabal pre-build hook

For projects that prefer generated `.hs` files on disk -- reviewable,
committable, visible to HLS without a TH rebuild --  `Proto.Setup`
provides a Cabal hook:

```haskell
-- Setup.hs
import Distribution.Simple
import Proto.Setup

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
      protoGenPreBuildHook defaultProtoGenConfig
        { pgcProtoDir     = "proto"
        , pgcOutputDir    = "gen"
        , pgcModulePrefix = "Proto.Gen"
        }
      preBuild simpleUserHooks args flags
  }
```

```yaml
# in your .cabal file
build-type: Custom

custom-setup
  setup-depends: base, wireform-proto, Cabal, directory, filepath, text

library
  hs-source-dirs: src, gen
```

The hook is incremental -- it only regenerates when a `.proto` file
is newer than its corresponding `.hs` output.

### `protoc-gen-wireform` -- protoc plugin

If your build system already runs `protoc` (Bazel, Nix, Make, or a
polyglot monorepo), wireform-proto ships a standard protoc plugin:

```bash
protoc --plugin=protoc-gen-wireform=$(cabal list-bin protoc-gen-wireform) \
       --wireform_out=gen/ \
       proto/*.proto
```

This reads `CodeGeneratorRequest` from stdin and writes Haskell source
files via the same code generation machinery that backs everything
else.

### `Proto.CodeGen` -- pure-text code generator

The lowest-level entry point. `generateModuleText` takes a parsed
`ProtoFile` AST and returns the complete Haskell module source as
`Text`. No TH, no IO -- just a pure function:

```haskell
import Proto.Parser  (parseProtoFile)
import Proto.CodeGen (generateModuleText, defaultGenerateOpts)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  src <- TIO.readFile "message.proto"
  case parseProtoFile "message.proto" src of
    Left err -> print err
    Right pf -> do
      let code = generateModuleText
                   defaultGenerateOpts { genModulePrefix = "MyApp.Proto" }
                   mempty "message.proto" pf
      TIO.writeFile "gen/MyApp/Proto/Message.hs" code
```

This is what `Proto.Setup`, `protoc-gen-wireform`, and `loadProto` all
call under the hood. Use it directly when you need full control --
custom CLI tools, non-Cabal build systems, or code generation as part
of a larger workflow.

### Which one should I use?

| Method | When to use it |
|:---|:---|
| `loadProto` | Most projects. Simple, no build setup. |
| `Proto.QQ` | Quick prototyping, one-off messages, tests. |
| `Proto.Derive` | Haskell types are the source of truth, not `.proto` files. |
| `Proto.Setup` | You want generated `.hs` files on disk (reviewable, committable). |
| `protoc-gen-wireform` | Your build system already runs `protoc`. |
| `Proto.CodeGen` | Custom tooling, full control over the generation pipeline. |

All six produce the same wire-format instances. The generated code is
byte-identical.

---

## Custom field representations

Not every field is best served by strict `Text` and `ByteString`.
`loadProtoWith` lets you override representations per-field or
per-message:

```haskell
$(loadProtoWith (defaultLoadOpts
    { loRepConfig = defaultRepConfig
        { configFieldOverrides = Map.fromList
            [ (("BlobMsg","data"),     defaultFieldRep { fieldBytes = LazyBytesRep  })
            , (("IdMsg","identifier"), defaultFieldRep { fieldBytes = ShortBytesRep })
            ]
        , configMessageOverrides = Map.fromList
            [ ("Config", defaultFieldRep { fieldRepeated = ListRep })
            ]
        }
    })
  "proto/my_service.proto")
```

This generates `BlobMsg` with a lazy `ByteString` data field (good
for large payloads you might not fully consume), `IdMsg` with a
`ShortByteString` identifier (unpinned, GC-friendly for small IDs),
and `Config` with `[Text]` instead of `Vector Text` for small
collections where the list overhead doesn't matter.

Available overrides: `LazyText`, `ShortText`, `LazyBytesRep`,
`ShortBytesRep`, `ListRep`, `SeqRep`.

---

## Multi-format

Because wireform-proto generates plain records, the same type can
participate in the broader `wireform` annotation system. One
`{-# ANN ... #-}` pragma on a record can drive instance generation
for protobuf, CBOR, MessagePack, and JSON simultaneously. The details
live in [`wireform-derive`](../wireform-derive/).

---

## Performance

These numbers come from `cabal bench compare-bench`, encoding and
decoding identical messages through wireform-proto and proto-lens.
Four message shapes: a 3-field scalar message, an 8-field mixed
message, a nested submessage, and a repeated message with 50 packed
ints + 20 strings + 10 nested items.

#### Encode

| Message    | wireform | wireform (LLVM) | proto-lens | speedup |
|:-----------|----------:|----------------:|-----------:|--------:|
| Small      |    26 ns  |      **23 ns**  |    145 ns  | **6.3x** |
| Medium     |    54 ns  |      **52 ns**  |    280 ns  | **5.4x** |
| Nested     |    45 ns  |      **42 ns**  |    320 ns  | **7.6x** |
| Repeated   |   657 ns  |     **500 ns**  |  2,646 ns  | **5.3x** |

#### Decode

| Message    | wireform | wireform (LLVM) | proto-lens | speedup |
|:-----------|----------:|----------------:|-----------:|--------:|
| Small      |    21 ns  |      **20 ns**  |     77 ns  | **3.9x** |
| Medium     |    57 ns  |      **61 ns**  |    201 ns  | **3.3x** |
| Nested     |    49 ns  |      **50 ns**  |    144 ns  | **2.9x** |
| Repeated   |   694 ns  |     **623 ns**  |  2,067 ns  | **3.3x** |

#### Roundtrip (encode then decode)

| Message    | wireform | wireform (LLVM) | proto-lens | speedup |
|:-----------|----------:|----------------:|-----------:|--------:|
| Small      |    76 ns  |      **75 ns**  |    218 ns  | **2.9x** |
| Medium     |   201 ns  |     **191 ns**  |    472 ns  | **2.5x** |
| Nested     |   156 ns  |     **140 ns**  |    450 ns  | **3.2x** |

*Criterion, GHC 9.8.4, `-O2`, Apple Silicon (M-series). Schema and
runner in [`compare-bench/`](../compare-bench/). Run them yourself:
`cabal bench compare-bench`. LLVM column uses `-fllvm` on
wireform packages; proto-lens stays NCG. LLVM gives up to 27%
improvement on repeated-field messages where loop overhead dominates.*

Encode and decode are nearly symmetric in cost. For a typical
3-field message, both encoding and decoding take ~20--23 ns with
LLVM. Larger messages scale linearly — a 50-element packed-repeated
field with nested submessages round-trips in just over 1 μs.
Builder output can also be streamed directly to a `Handle` without
materialising an intermediate `ByteString`.

---

## Also included

* **Proto3 canonical JSON** -- `json_name` overrides, base64 bytes,
  string-encoded 64-bit integers, NaN/Infinity sentinels.
* **Well-known types** -- `Timestamp`, `Duration`, `Any`, `FieldMask`,
  `Struct`, `Value`, `ListValue`, `NullValue`, `Wrappers`, `Empty`,
  `SourceContext`, with supplementary logic (`packAny`, RFC 3339
  formatting, `TypeRegistry`, `FieldMask` ops).
* **Proto2 typed extensions**, unknown-field preservation, dynamic /
  untyped messages, `.pbtxt` text format, runtime `MessageRegistry`.
* **Streaming + incremental decoders** (`Proto.Decode.Stream` /
  `Proto.Decode.Streaming`).
* **gRPC service-method codegen** (`Proto.GRPC`). Wire framing lives
  in [`wireform-grpc`](../wireform-grpc/).

---

## Conformance

**2675 / 2675** tests pass against the official [upstream protobuf
conformance suite][upstream-conformance] (`protocolbuffers/protobuf@v28.2`),
covering proto3 + proto2 binary and JSON. Zero unexpected failures.

[upstream-conformance]: https://github.com/protocolbuffers/protobuf/tree/main/conformance

---

## Comparison to proto-lens

[proto-lens][proto-lens] has been around since 2016 and covers the
full proto2/proto3 surface. If you're evaluating the two, here's where
they differ.

[proto-lens]: https://github.com/google/proto-lens

| | wireform-proto | proto-lens |
|:---|:---|:---|
| **Record style** | Plain records, direct field access | Opaque constructors, lens-only access |
| **Construction** | Record syntax; missing fields are compile errors | `defMessage & field .~ val`; missing fields silent |
| **Pattern matching** | Yes | No (lens getters only) |
| **Type inference** | Concrete field types | Lens chains often need annotations |
| **Schema evolution** | New fields break call sites (good) | New fields get silent defaults |
| **Encode speed** | 5--8x faster | -- |
| **Decode speed** | 3--4x faster | -- |
| **Field representation** | Configurable per-field | Fixed |

### Where proto-lens has the edge

* **Ecosystem maturity.** More packages depend on it, more Stack
  Overflow answers reference it, more edge cases have been found and
  fixed over the years.
* **HasField polymorphism.** proto-lens's `HasField` class lets you
  write functions generic over any message containing a given field
  name. wireform-proto's records are nominal.
* **Optics integration.** If your codebase already lives in `lens` or
  `optics`, proto-lens plugs right in. wireform-proto records work
  with `OverloadedRecordDot` and plain selectors; you can derive
  optics separately if you want them.

---

## License

BSD-3-Clause. See [`LICENSE`](LICENSE) for the full text and
third-party attributions.
