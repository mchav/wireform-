---
title: The Decoder monad
description: How wireform's decoder is structured for performance.
sidebar:
  order: 2
---

:::caution[Not yet on Hackage]
wireform is in heavy development and has not been published to Hackage yet.
Internal details described here may change.
:::

The `Decoder` newtype wraps an unboxed sum that branches into success
(with the new offset) or failure. All primitives — `getVarint`, `getText`,
etc. — return unboxed sums, so the decoder hot path allocates only what
the user actually keeps.

## Key patterns

### `withTag` CPS dispatch

The decode loop reads a tag and immediately hands control to a
continuation that knows the layout for that tag. Because the continuation
is a statically-known lambda, GHC inlines it and produces a jump table
in Core.

### Unboxed offsets

Offsets are passed as `Int#` rather than boxed `Int` so the offset never
escapes to the heap. This is the single biggest win on decode benchmarks
relative to other Haskell implementations.

### Generated code

In generated code the monadic `do` notation is acceptable — it's slightly
less optimal but far simpler to generate. The performance gap is small
enough that it only matters in tight inner loops.

## Format-specific decoders

Each format package exposes its own `Decode` module built on the same
pattern:

| Package | Decoder module |
|---------|----------------|
| Protocol Buffers | `Proto.Decode` |
| Avro | `Avro.Decode` |
| MessagePack | `MsgPack.Decode` |
| CBOR | `CBOR.Decode` |
| Thrift | `Thrift.Decode` |
| BSON | `BSON.Decode` |

The streaming variants (`Proto.Decode.Stream`, `MsgPack.Stream`,
`CBOR.Stream`) layer incremental chunked parsing on top of the same
core decoder.
