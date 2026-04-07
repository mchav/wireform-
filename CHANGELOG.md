# Changelog

## 0.1.0.0 — 2026-04-07

* Initial release
* 16 serialization formats: Protobuf, Avro, Thrift, MessagePack, CBOR, BSON, Ion, EDN, Cap'n Proto, FlatBuffers, Iceberg, Bond, ASN.1, Parquet, Pickle, Arrow IPC
* 2-4x faster than competing Haskell libraries (proto-lens, msgpack, cborg, pinch, avro)
* Direct-write encoder, Addr#-based fast decoder, SIMD-accelerated C primitives
* Typeclass-based encode/decode with GHC.Generics deriving
* Schema parsers for .thrift, .avsc, .bond IDL files
* Code generation for Protobuf, Avro, Thrift (TH + standalone)
* Streaming decode for Protobuf, MessagePack, CBOR
* Avro schema resolution, container file support
* gRPC framing, Thrift RPC headers, Avro IPC protocol, MsgPack-RPC
* CBOR diagnostic notation (RFC 8949)
* 1391 tests
