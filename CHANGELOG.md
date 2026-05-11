# Changelog

## 0.1.0.0 -- 2026

Initial release of the `wireform` umbrella package and its per-format
siblings.

### Formats

`Wireform.*` facade modules ship for every format the workspace covers:

* **Schema / IDL binary**: Protocol Buffers, Apache Thrift, Apache Avro,
  Apache Bond, FlatBuffers, Cap'n Proto, ASN.1.
* **Schema-less binary**: CBOR (RFC 8949), MessagePack, BSON, Amazon Ion,
  Apache Fory (Fury), Bencode.
* **Text**: JSON (via NDJSON), EDN, TOML, YAML, CSV, XML, HTML5.
* **Columnar / table**: Apache Arrow IPC, Apache Parquet, Apache ORC,
  Apache Iceberg, Delta Lake, Apache Hudi, Apache Lance.
* **Streaming / RPC**: gRPC framing (`wireform-grpc`), Apache Kafka
  protocol + native client (`wireform-kafka`).

### Highlights

* **Annotation-driven deriver** (`wireform-derive`) -- one `{-# ANN ... #-}`
  vocabulary drives instance generation for every backend.  Per-format
  derivers live in their respective `wireform-*` packages.
* **High-performance hot paths**:
    * Unboxed-sum decoder result types (no boxed `Either` / `Maybe` on
      the decode loop).
    * Two-pass sized encoders that allocate exactly the right buffer
      and write tag + length + payload in a single pass.
    * SWAR / SIMD C kernels in `wireform-core` (`Wireform.FFI`) for
      UTF-8 validation, packed-varint pre-scan, byte / NUL / JSON
      escape scanning, and Iceberg partition-bound comparison.
    * Direct-write `Wireform.Encode.Direct` buffer for the columnar
      packages.
* **IDL parsers and code generators** for `.proto`, `.avsc` / `.avdl`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, ISL, CDDL, XSD, Iceberg
  table metadata.
* **Streaming / incremental** decoders for protobuf, MsgPack, CBOR,
  XML, NDJSON.
* **Container file I/O** for Avro OCF, Parquet (footer + page index +
  bloom filter + column chunks), Arrow IPC (file + stream), ORC
  (stripes + statistics), Iceberg (manifests + table metadata), Delta
  Lake transaction log, Hudi timeline, Lance.
* **Schema resolution / evolution** for Avro and proto2 / proto3
  compatibility.
* **Dynamic / untyped** protobuf messages, `.pbtxt` text format, CBOR
  diagnostic notation (RFC 8949), and a runtime `MessageRegistry`.
* **Multi-format codegen CLI** (`wireform-gen`) that targets every
  IDL-backed format from a single binary.
* **Protobuf conformance** harness driving the upstream
  `conformance_test_runner` end-to-end: 2675 successes, 0 unexpected
  failures against `protocolbuffers/protobuf@v28.2`.

### Toolchain

* CI: GHC 9.6.4 and GHC 9.8.4.
* `cabal-version: 3.0` across the workspace.
* LLVM is OFF workspace-wide (`-fasm` set in `cabal.project`); a
  vanilla GHC toolchain is sufficient.
