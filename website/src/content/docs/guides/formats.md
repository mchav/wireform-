---
title: Format catalogue
description: Every format, protocol, and capability in the wireform ecosystem.
sidebar:
  order: 2
---

## Schema & IDL formats

| Format | Package | Encode/Decode | Codegen | Streaming | RPC / Framing |
|--------|---------|:---:|:---:|:---:|---|
| Protocol Buffers | `wireform-proto` | Yes | `loadProto` / `wireform-gen proto` / `protoc` | `Proto.Decode.Stream` | gRPC (via `wireform-grpc`) |
| Avro | `wireform-avro` | Yes | `wireform-gen avro` | — | Avro IPC, Container (OCF) |
| Thrift | `wireform-thrift` | Yes | `wireform-gen thrift` | — | Binary + Compact protocol |
| Bond | `wireform-bond` | Yes | `wireform-gen bond` | — | — |
| Cap'n Proto | `wireform-capnproto` | Yes | `wireform-gen capnp` | — | — |
| FlatBuffers | `wireform-flatbuffers` | Yes | `wireform-gen fbs` | — | — |
| ASN.1 | `wireform-asn1` | Yes (BER/DER) | `wireform-gen asn1` | — | — |

## Binary value formats

| Format | Package | Generic | Value ADT | JSON bridge | Streaming |
|--------|---------|:---:|:---:|:---:|:---:|
| MessagePack | `wireform-msgpack` | `MsgPack.Class` | Yes | Yes | `MsgPack.Stream` |
| CBOR | `wireform-cbor` | `CBOR.Class` | Yes | Yes | `CBOR.Stream` |
| BSON | `wireform-bson` | `BSON.Class` | Yes | — | — |
| Ion | `wireform-ion` | `Ion.Class` | Yes | — | Yes |
| EDN | `wireform-edn` | `EDN.Class` | Yes | — | — |
| Bencode | `wireform-bencode` | `Bencode.Class` | Yes | — | — |
| Fory | `wireform-fory` | — | Yes | — | — |

## Text & markup formats

| Format | Package | Generic | Streaming | Special |
|--------|---------|:---:|:---:|---|
| XML | `wireform-xml` | `XML.Class` | SAX + incremental | SIMD parser, XPath, XSLT 1.0, XSD codegen, zero-copy DOM |
| HTML5 | `wireform-html` | `HTML.Class` | Streaming rewriter | Spec-compliant tree builder, CSS selectors, SIMD serializer |
| YAML | `wireform-yaml` | `YAML.Class` | — | YAML 1.2, 100% yaml-test-suite, annotated AST |
| TOML | `wireform-toml` | `TOML.Class` | — | TOML 1.0/1.1, toml-test conformant |
| CSV | `wireform-csv` | `CSV.Class` | — | CSV / TSV / pipe-separated |
| NDJSON | `wireform-ndjson` | `NDJSON.Class` | Yes | JSON lines |

## Analytics & lake formats

| Format | Package | Reader | Writer | Extras |
|--------|---------|:---:|:---:|---|
| Parquet | `wireform-parquet` | Full | Full | All encodings, compression, bloom filters, page index, encryption, predicate pushdown |
| Arrow IPC | `wireform-arrow` | Full | Full | Schema framing, record batch materialization, zstd/lz4 compression |
| ORC | `wireform-orc` | Full | Partial | Integer RLE v1/v2, compression, predicate evaluator |
| Iceberg | `wireform-iceberg` | Full | — | Schema evolution, partition transforms, deletion vectors, Puffin, catalog clients (REST, Glue, Hadoop, SQL) |
| Delta Lake | `wireform-delta` | Partial | — | Transaction log, checkpoints, time travel |
| Hudi | `wireform-hudi` | Partial | — | Timeline reader, copy-on-write |
| Lance | `wireform-lance` | Partial | — | Data files, manifests, dataset versions |

## Messaging & RPC

| Protocol | Package | Capabilities |
|----------|---------|---|
| gRPC | `wireform-grpc` | Client/server, unary + streaming (server, client, bidi), HTTP/2, TLS |
| Kafka | `wireform-kafka` | Producer, consumer, transactions, Kafka Streams DSL (KStream, KTable, windowed aggregations, joins), SASL auth (PLAIN, SCRAM, OAUTHBEARER, MSK IAM), compression codecs, OpenTelemetry instrumentation |

## Shared infrastructure

| Package | Purpose |
|---------|---|
| `wireform-core` | FFI/SIMD primitives — packed varint, SWAR UTF-8, SIMD scanning, endianness helpers |
| `wireform-derive` | Annotation vocabulary for Generic deriving across all 25+ backends |
| `wireform-columnar` | Format-agnostic columnar primitives — pull iterators, predicate pushdown, mmap I/O, SIMD bit-unpacking |

For cross-cutting concerns (streaming, RPC, container files, dynamic messages)
see [Get started — Step 6: Go deeper](/guides/getting-started/#step-6--go-deeper).
