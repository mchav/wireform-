---
title: The Decoder monad
description: How wireform's allocation-tight decoder is structured, and why.
sidebar:
  order: 2
---

The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`
— an unboxed sum that branches into success (with the new offset) or
failure. All primitives (`getVarint`, `getText`, etc.) return unboxed sums,
so the decoder hot path allocates only what the user actually keeps.

Two important patterns:

- **`withTag` CPS dispatch.** The decode loop reads a tag and immediately
  hands control to a continuation that knows the layout for that tag.
  Because the continuation is a statically-known lambda, GHC inlines it and
  produces a small jump-table-style core.
- **Unboxed `Int#` offsets.** We pass `Int#` rather than boxed `Int` so the
  offset never escapes to the heap. This is the single biggest win on
  decode benchmarks against the next-fastest implementation.

In generated code the monadic `do` notation with `getTagOrU` is acceptable
(it's slightly less optimal but far simpler to generate).

See the [API reference](/api/) for the full surface, especially
`Wireform.Core.Decoder` and the format-specific `Decode` modules.
