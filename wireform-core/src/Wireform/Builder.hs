{- | Efficient byte-string builder for all wireform format packages.

This module re-exports the vendored fast-builder engine from
"Wireform.Builder.FastBuilder" and "Wireform.Builder.Internal.Prim".
All wireform packages should depend on this module rather than
importing the internal modules directly.

The underlying builder type and all primitives are exported
directly — no newtype wrapping — so GHC can optimise as
aggressively as with a direct import of the engine module.

= Usage

@
import Wireform.Builder

myEncoder :: MyMsg -> Builder
myEncoder msg = word8 0x0A <> byteString (encodePayload msg)

-- strict output
let bs = toStrictByteString (myEncoder msg)

-- or write straight to a handle
hPutBuilder stdout (myEncoder msg)
@
-}
module Wireform.Builder (
  -- * Builder type
  Builder,

  -- * Running builders
  toStrictByteString,
  toLazyByteString,
  hPutBuilder,
  hPutBuilderLen,
  hPutBuilderWith,

  -- * Performance tuning
  rebuild,

  -- * Bounded / fixed primitives
  primBounded,
  primFixed,

  -- * ByteString → Builder
  byteString,
  byteStringInsert,
  byteStringCopy,
  byteStringThreshold,

  -- * Single byte
  word8,
  int8,

  -- * Little-endian
  word16LE,
  word32LE,
  word64LE,
  int16LE,
  int32LE,
  int64LE,
  floatLE,
  doubleLE,

  -- * Big-endian
  word16BE,
  word32BE,
  word64BE,
  int16BE,
  int32BE,
  int64BE,
  floatBE,
  doubleBE,

  -- * Decimal
  intDec,
  wordDec,
  int64Dec,
  word64Dec,

  -- * Hexadecimal
  wordHex,
  word8Hex,

  -- * Text helpers
  charUtf8,
  stringUtf8,
  char7,
  string7,

  -- * Builder internals (advanced — for compression sinks, etc.)
  module Wireform.Builder.FastBuilder,
) where

import Wireform.Builder.FastBuilder
import Wireform.Builder.Internal.Prim

