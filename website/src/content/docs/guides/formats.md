---
title: Format catalogue
description: Every wire format wireform supports, what it can do, and the modules to look at.
sidebar:
  order: 2
---

| Format | Encode/Decode | Generic | Schema codegen | Streaming | Container / RPC |
|--------|:-------------:|:-------:|:--------------:|:---------:|-----------------|
| Protocol Buffers | Yes | — | `loadProto` / `wireform-gen proto` | `Proto.Decode.Stream` | `Proto.GRPC` framing |
| Avro | Yes | — | `wireform-gen avro` | — | `Avro.Container` (OCF) |
| Thrift | Yes | — | `wireform-gen thrift` | — | `Thrift.Message` |
| MessagePack | Yes | `MsgPack.Class` | — | `MsgPack.Stream` | `MsgPack.RPC` |
| CBOR | Yes | `CBOR.Class` | CDDL | `CBOR.Stream` | — |
| BSON | Yes | `BSON.Class` | — | — | — |
| Cap'n Proto | Yes | — | `wireform-gen capnp` | — | — |
| FlatBuffers | Yes | — | `wireform-gen fbs` | — | — |
| Bond | Yes | — | `wireform-gen bond` | — | — |
| ASN.1 | Yes | — | `wireform-gen asn1` | — | — |
| XML | Yes | `XML.Class` | XSD | `XML.SAX` / `XML.Incremental` | — |
| HTML | Yes | `HTML.Class` | — | — | — |
| TOML | Yes | `TOML.Class` | — | — | — |
| EDN | Yes | `EDN.Class` | — | — | — |
| Ion | Yes | `Ion.Class` | ISL | — | — |
| Bencode | Yes | `Bencode.Class` | — | — | — |
| CSV | Yes | `CSV.Class` | — | — | — |
| NDJSON | Yes | `NDJSON.Class` | — | streaming-friendly | — |
| Parquet | metadata + reader/writer | — | (schema only) | — | — |
| ORC | metadata + reader | — | (schema only) | — | — |
| Arrow IPC | full | — | (schema only) | — | — |
| Iceberg | metadata | — | (schema only) | — | — |

For the cross-cutting concerns (RPC, streaming, container files, dynamic
messages) see [Get started — Step 6: Go deeper](/guides/getting-started/#step-6--go-deeper).
