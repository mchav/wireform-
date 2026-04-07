# wireform Development Guidelines

## Code Generation Principles

**All message types must come from the code generator.** This includes well-known
types (`Timestamp`, `Duration`, `Struct`, etc.), descriptor types, and benchmark
types. Hand-written wire encode/decode instances are not permitted because they
drift from what the code generator produces and mask codegen bugs.

- Well-known types live in `src/Proto/Google/Protobuf/*.hs` and are generated
  from the `.proto` files in `proto/google/protobuf/`.
- Supplementary logic (e.g. `packAny`, RFC 3339 formatting, `TypeRegistry`)
  belongs in companion modules like `Proto.Google.Protobuf.Any.Util` or
  `Proto.JSON.WellKnown`. These import the generated types but never define
  wire-level instances.
- Benchmark comparison types must also be code-generated so that benchmarks
  measure the *actual* codegen output, not idealised hand-written decoders.

## Performance

### Allocation discipline

- **Unboxed sums** for finite branching (success / failure / end-of-input).
  Never use boxed `Either` or `Maybe` on an internal hot path.
- **`withTag` CPS** for the decode loop tag dispatch, where continuations are
  statically known lambdas that GHC will inline.
- **Unboxed `Int#`** for offsets threaded through the decoder.
- Avoid `IORef` in benchmarks where an unboxed accumulator loop suffices.

### String / Text handling

- Never round-trip through `String`. No `T.pack (show n)`, no
  `reads (T.unpack t)`, no `T.pack . show`. Use `Data.Text.Builder` or
  direct numeric-to-Text conversion instead.
- For integer formatting, write directly to a `Builder` or use a purpose-built
  `intToText` helper.
- For parsing integers from `Text`, use `Data.Text.Read.decimal` /
  `Data.Text.Read.signed` rather than `reads . T.unpack`.

### Numeric patterns

- When you need both quotient and remainder, use `divMod` or `quotRem` in a
  single call rather than separate `div` and `mod` on the same operands.
- Prefer `quot`/`rem` over `div`/`mod` for non-negative values (avoids the
  sign-correction branch).

### Data structures

- **No plain tuples** in domain-specific return types. Define a small strict
  record with `{-# UNPACK #-}` on numeric fields. Tuples hide meaning and
  prevent GHC from unboxing nested fields.
- **GrowList is a last resort.** Each `snoc` allocates a cons cell + a
  `GrowList` node (≈48 bytes on 64-bit). Prefer:
  1. `VecBuilder` (IO-based doubling array) when inside IO/ST.
  2. `Data.Vector.create` + `MV.grow` in an ST block when the final size
     is unknown but the builder can be scoped.
  3. If stuck in a pure context (the Decoder monad), a chunked representation
     with amortised allocation (e.g. small arrays of 64 elements, chained)
     is better than a cons-per-element list.

### Decoder monad style

- The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`.
  All primitives (`getVarint`, `getText`, etc.) return unboxed sums.
- In hand-optimised decoders, use `withTag` + direct `runDecoder#` calls for
  each field. In generated code, the monadic `do` notation with `getTagOrU`
  is acceptable (slightly less optimal but far simpler to generate).
- Always `{-# INLINE messageDecoder #-}` on instances.

## Code style

- Do not use list comprehensions. Prefer `do` block syntax or
  higher-order functions.
- Prefer datatype-specific functions with better complexity over
  `toList` / `fromList` conversions.
- No `threadDelay` in tests.
- Keep lens usage to where the alternative would be unwieldy; comment
  complex lens expressions.
- Property-based tests via Hedgehog. Do not test things inherent to
  the language (e.g. setting a record field and reading it back).

## Module layout

```
Proto.AST                      -- .proto IDL AST
Proto.Parser / Proto.Parser.*  -- IDL parser
Proto.Wire / Proto.Wire.*      -- wire format primitives
Proto.Encode / Proto.Decode    -- high-level encode/decode typeclasses
Proto.CodeGen / Proto.CodeGen.* -- Haskell code generation
Proto.Google.Protobuf.*        -- generated well-known types (from .proto)
Proto.Google.Protobuf.*.Util   -- supplementary logic for well-known types
Proto.JSON / Proto.JSON.*      -- JSON mapping
Proto.Schema                   -- runtime type metadata
Proto.Dynamic                  -- dynamic (untyped) messages
Proto.TextFormat                -- pbtxt serialisation
```
