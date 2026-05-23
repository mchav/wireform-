# Changelog for wireform-core

## Unreleased

* `Wireform.Ring` -- gave `MagicRing` a phantom type parameter `s`
  modelled after `Control.Monad.ST.ST`.  `withMagicRing` is now
  rank-2 (`Int -> (forall s. MagicRing s -> IO a) -> IO a`), which
  seals `s` inside the ring's scope.  New `RingSlice s` type (a
  pointer + length tagged with the ring's `s`) replaces ad-hoc
  `ByteString` slicing for callers that want type-system-enforced
  safety against dangling references after a refill.  `copyRingSlice
  :: RingSlice s -> IO ByteString` is the explicit escape hatch.

* `Wireform.Transport` -- replaced the typed `transportRing ::
  MagicRing` field with three raw fields (`transportRingBaseField`,
  `transportRingSizeField`, `transportRingMaskField`).  This keeps
  `Transport` un-parameterised so the new `s` does not have to
  cascade through every downstream `Transport`-using package.  The
  existing `transportRing :: Transport -> MagicRing s` getter
  remains for backwards compatibility but is polymorphic in `s`, i.e.
  un-scoped from a safety standpoint.  Transport constructors must
  populate the three raw fields explicitly.

* `Wireform.Parser.Driver` / `Wireform.Parser.Error` -- detect the
  case where the streaming parser asks for more bytes than the ring
  can ever hold.  Previously this deadlocked: the producer cannot
  make room (head is pinned at @tail + ringSize@), the consumer is
  suspended waiting for bytes the producer cannot deliver, and the
  driver's wait loop spins on a no-progress @MoreData@ from the
  transport.  The driver now short-circuits with a new
  `ParseRingOverflow` error variant (carrying the parser position,
  requested byte count, and ring size) before suspending.

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
