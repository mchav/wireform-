---
title: wireform-core
description: "Shared SIMD and FFI primitives that power every format package."
sidebar:
  order: 3
---

`wireform-core` contains the C and SIMD-accelerated primitives shared by every
`wireform-*` format package. You generally don't import it directly. It exists
so that the hot paths common to multiple formats (varint decoding, UTF-8
validation, byte scanning, JSON escaping, hashing) are implemented once and
tuned in one place.

## Why a shared core matters

Serialization formats share a surprising amount of low-level machinery. Varints
appear in Protocol Buffers, Avro, Thrift, and CBOR. UTF-8 validation matters
everywhere that decodes text. JSON escaping shows up in any format that has a
JSON bridge. Rather than reimplementing these per-format and optimizing them
separately, `wireform-core` centralizes them behind a C FFI boundary with
SWAR (SIMD Within A Register) and hardware SIMD implementations.

## What's inside

### `Wireform.FFI`

C-backed primitives from `cbits/fast_decode.c` and `cbits/fast_scan.c`. These
use SWAR bit tricks and, where available, SSE/AVX/NEON intrinsics.

| Category | Key functions |
|----------|---------------|
| Varint decode | `decodeVarintSWAR`, `countPackedVarints`, `packedAllSingleByte` |
| UTF-8 validation | `validateUtf8SWAR` |
| Byte scanning | `findByte`, `findNul`, `isAscii` |
| JSON escaping | `findJsonEscape`, `escapeJSONStringBS`, `escapeJSONText` |
| Text decode | `decodeTextFast` (UTF-8 bytes to `Text`, fast path) |
| Endianness | `readBE16H` .. `readBE64H`, `readLE16H` .. `readLE64H` (and write variants) |
| C encode helpers | `encodeLengthDelimitedC`, `encodeVarintFieldC` |

The `ByteString` variants (`findByteBS`, `findNulBS`, `isAsciiBS`,
`findJsonEscapeBS`) operate directly on pinned byte arrays without copying.

### `Wireform.Encode.Direct`

An offset-based encoder that writes directly into a pre-allocated buffer.
Format-specific encoders build on this to avoid intermediate `Builder`
allocations when the output size is known or bounded.

| Function | What it writes |
|----------|---------------|
| `directEncode` | Top-level entry: allocate N bytes, run a write callback, return `ByteString` |
| `dVarint` | LEB128 varint |
| `dWord32LE` / `dWord64LE` | Fixed-width little-endian |
| `dFloatLE` / `dDoubleLE` | IEEE 754 |
| `dBytes` / `dText` | Length-prefixed byte/text blobs |
| `dVarintField`, `dStringField`, ... | Tag + value pairs (proto wire format) |

### `Wireform.Hash`

Hashing and bitmap primitives from `cbits/wireform_hash_simd.c`, used by
Parquet bloom filters, Iceberg partition transforms, and internal hash tables.

| Function | Algorithm |
|----------|-----------|
| `murmur3_32` | Murmur3 (32-bit), used by Iceberg bucket transforms |
| `xxh64` | XXHash64 with configurable seed |
| `bucketLong` / `bucketBytes` | Iceberg bucket partition hash |
| `roaringDecodeArray` / `roaringDecodeBitset` | Roaring bitmap container decode |
| `roaringContains` | Point-query a decoded bitmap container |
| `roaringEncodeArray` / `roaringEncodeBitset` | Encode bitmap containers |

## C sources

The package ships three C source files:

| File | Contents |
|------|----------|
| `cbits/fast_decode.c` | Varint SWAR, UTF-8 validation, C encode helpers |
| `cbits/fast_scan.c` | Byte/NUL/ASCII/whitespace/JSON-escape scanners |
| `cbits/wireform_hash_simd.c` | Murmur3, XXH64, Roaring codec |

These use `__attribute__((target(...)))` for multi-versioning so the correct
SIMD path is selected at runtime without requiring compile-time `-msse4.2` or
`-mavx2` flags.

## When you'd import this directly

Almost never. The format packages (`wireform-proto`, `wireform-cbor`, etc.)
depend on `wireform-core` internally and expose higher-level APIs. The main
reasons to import `wireform-core` directly:

1. You're writing a new format package for wireform and need the shared encode
   buffer or scanning primitives.
2. You need `xxh64` or `murmur3_32` for non-wireform purposes and want to
   avoid pulling in a separate hashing library.
3. You're debugging a performance issue in a format decoder and want to
   benchmark the underlying C primitives in isolation.
