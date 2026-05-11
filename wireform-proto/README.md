# wireform-proto

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

A high-performance Protocol Buffers (proto2 / proto3) implementation
for Haskell -- IDL parser, code generator, runtime, JSON mapping,
well-known types, conformance harness.

`wireform-proto` is one package in the [`wireform`][wireform] monorepo;
the umbrella `wireform` package re-exports the public API as
`Wireform.Proto`.

[wireform]: https://github.com/iand675/wireform-

## At a glance

* **`.proto` IDL parser** -- proto2 + proto3, full reference resolution,
  options, services, oneofs, maps, extensions, custom options.
* **Pure-text Haskell code generator** -- records / enums / oneofs / maps /
  services, well-known type imports, proto2 typed extensions, embedded
  `FileDescriptorProto` bytes per module.
* **Annotation-driven Template Haskell deriver** (`Proto.Derive`) -- emit
  wire codecs straight off a hand-written Haskell record using
  `{-# ANN ... (tag N) #-}` annotations and shape inference.  No `.proto`
  file required.
* **`loadProto` / `loadProtoWith`** -- TH splice that runs the parser and
  the deriver together; field- and message-level representation
  overrides (`LazyText`, `ShortText`, `ListRep`, `LazyBytes`, …) are
  supported.
* **Inline quasi-quoter** (`Proto.QQ`) for one-off types.
* **Cabal `Setup.hs` hook** (`Proto.Setup`) for pre-build codegen.
* **`protoc` plugin** (`protoc-gen-wireform`, `--wireform_out=DIR`).
* **Proto3 canonical JSON** with `json_name` override, base64 bytes,
  string-encoded 64-bit integers, NaN / Infinity sentinels.
* **Well-known types** (`Timestamp`, `Duration`, `Any`, `FieldMask`,
  `Struct`, `Value`, `ListValue`, `NullValue`, `Wrappers`, `Empty`,
  `SourceContext`) generated from bundled `.proto` files plus
  supplementary logic (`packAny`, RFC 3339 formatting, `TypeRegistry`,
  `FieldMask` ops).
* **Proto2 typed extensions**, unknown-field preservation, dynamic /
  untyped messages, `.pbtxt` text format, runtime `MessageRegistry`.
* **Streaming + incremental decoders** (`Proto.Decode.Stream` /
  `Proto.Decode.Streaming`).
* **gRPC service-method codegen** (`Proto.GRPC`).  The wire framing
  lives in the `wireform-grpc` package.

## Performance posture

`wireform-proto` is designed to be allocation-disciplined enough to
beat hand-written Haskell decoders on its own benchmarks.

* **Unboxed-sum decoder** (`Decoder a` returns
  `(# (# a, Int# #) | DecodeError #)`).  No boxed `Either` or `Maybe`
  on the hot path.
* **Two-pass sized encoding** (`MessageSize` + `MessageEncode`):
  compute the wire-format size once, then write tag + length-prefix +
  payload directly into a single right-sized buffer with no
  intermediate lazy chunks.
* **Pre-computed tag bytes** (`Proto.Encode.Archetype`) bake field
  tags into the `.data` section so the encoder emits a single
  `memcpy` per field rather than re-running the varint arithmetic.
* **Packed repeated field encode/decode** with branchless
  varint sizing (CLZ-based) and bulk-copy fast paths for fixed-width
  scalars (single `memcpy` from the wire on little-endian targets).
* **Lazy submessage decoding** (`LazyMessage`) defers parsing for
  fields the consumer never reads.

A few cross-package primitives back the hot path -- SWAR /
SIMD-accelerated UTF-8 validation, varint pre-count, and ASCII
checks live in `wireform-core` (`Wireform.FFI`).

## Quick start

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)
import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage)

$(loadProto "examples/proto/simple.proto")

main :: IO ()
main = do
  let req   = defaultGetPersonRequest { personId = 42 }
  let bytes = encodeMessage req           -- 2 bytes: 0x08 0x2a
  case decodeMessage bytes of
    Right (decoded :: GetPersonRequest) ->
      putStrLn $ "decoded id = " <> show (personId decoded)
    Left err -> putStrLn $ "decode failed: " <> show err
```

Or via `Proto.Derive` -- no `.proto` file required:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import qualified Proto.Derive as DProto

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}

{-# ANN personFullName (tag 1) #-}
{-# ANN personAge      (tag 2) #-}

DProto.deriveProto ''Person
```

Both paths feed the same body builders in `Proto.Derive.Internal`, so
the generated wire-codec instances are byte-identical.

## Conformance

`wireform-proto` ships an end-to-end harness that runs the official
[upstream protobuf conformance suite][upstream-conformance] against
`loadProto`-generated codecs.

```bash
bash wireform-proto/test-conformance/scripts/build-conformance-runner.sh
cabal test wireform-proto:protobuf-conformance-test
```

Today's baseline against `protocolbuffers/protobuf@v28.2`:
**2675 successes, 0 unexpected failures** across the proto3 + proto2
binary and JSON suites.

[upstream-conformance]: https://github.com/protocolbuffers/protobuf/tree/main/conformance

## Documentation

The detailed contributor guide (codegen principles, allocation
discipline, module map) lives in
[`agents.md`](../agents.md) at the workspace root.

## License

BSD-3-Clause.  See [`NOTICE`](NOTICE) for third-party attributions
(unpacked-maybe, unpacked-either).
