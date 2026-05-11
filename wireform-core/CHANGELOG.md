# Changelog for wireform-core

## 0.1.0.0 -- 2026

Initial release.

* `Wireform.FFI` -- C FFI surface for the per-format encoders /
  decoders:
    * Packed-varint pre-scan (`countPackedVarints`,
      `packedAllSingleByte`).
    * SWAR UTF-8 validation (`validateUtf8SWAR`).
    * Branchless 8-byte varint decode (`decodeVarintSWAR`).
    * Page-boundary relocation helper (`relocatePageBoundary`).
    * C-native scalar field encoders (`encodeLengthDelimitedC`,
      `encodeVarintFieldC`, `encodeBoolFieldC`).
    * SIMD NUL / byte / ASCII / JSON-escape / EDN-whitespace
      scanners (`findNul`, `findByte`, `isAscii`,
      `findJsonEscape`, `skipWhitespace`).
    * SIMD Iceberg partition-bounds comparison (`compareBounds`).
    * SIMD Arrow IPC buffer validation
      (`validateArrowBuffers`).
    * BE / LE endianness helpers (`readBE16H` … `writeLE64H`).
    * Text decoding via the `text` library's bundled simdutf
      (`decodeTextFast`).
* `Wireform.Encode.Direct` -- shared direct-write encode buffer
  primitive (`Buf`) for the per-format encoders.
* `Wireform.Hash` -- SIMD-accelerated hashing helpers.

C sources live in `cbits/`; the vendored `simde` headers ship in
`include/simde/`.
