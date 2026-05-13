{- | Efficient byte-string builder for all wireform format packages.

This module re-exports the vendored fast-builder engine from
"Wireform.Builder.FastBuilder" and "Wireform.Builder.Internal.Prim".
All wireform packages should depend on this module rather than
importing the internal modules directly.

== Builder type

'Builder' is a zero-copy, chunk-based byte-string builder. It is a
'Data.Monoid.Monoid', so fragments are combined with @('<>')@. No
bytes are copied until you run the builder with one of the output
functions below.

== Running a builder

  ['toStrictByteString'] Materialise the builder into a single strict
    'Data.ByteString.ByteString'. Allocates one buffer and fills it.

  ['toLazyByteString'] Materialise into a lazy
    'Data.ByteString.Lazy.ByteString' (a list of strict chunks).
    Good when the output is large or will be streamed further.

  ['hPutBuilder'] Write the builder directly to a 'System.IO.Handle'
    (e.g. a file or socket) without materialising an intermediate
    'Data.ByteString.ByteString'. Uses chunked I\/O internally.

  ['hPutBuilderLen'] Like 'hPutBuilder' but also returns the number
    of bytes written.

  ['hPutBuilderWith'] Like 'hPutBuilderLen' with explicit control over
    the initial and subsequent buffer capacities.

== Stream transforms

'StreamSink' and 'withStreamTransform' allow you to interpose a
streaming transformation (e.g. compression) between the builder and
its output. The 'StreamSink' receives raw pointer\/length pairs as
the builder fills buffers; you can feed them into zstd, gzip, or any
other streaming codec.

== Usage example

@
import Wireform.Builder

myEncoder :: MyMsg -> Builder
myEncoder msg = word8 0x0A \<\> byteString (encodePayload msg)

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

