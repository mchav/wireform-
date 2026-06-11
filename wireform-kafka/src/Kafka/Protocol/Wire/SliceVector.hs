{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Kafka.Protocol.Wire.SliceVector
Description : Compact vector-of-byte-slices over a single 'ForeignPtr'

A 'SliceVector' is a vector of 'ByteString' slices that all
share a single backing 'ForeignPtr Word8' buffer. The slices
are stored as @(Int32 offset, Int32 length)@ pairs in an
unboxed 'Data.Vector.Unboxed.Vector', so the whole structure
is:

  * one 'ForeignPtr Word8' reference (one GC root for the
    backing buffer, regardless of how many slices it contains);
  * one unboxed 'Data.Vector.Unboxed.Vector (Int32, Int32)'
    of @(offset, length)@ pairs (16 bytes per slice in the
    common case, vs 24 bytes plus a separate ForeignPtr root
    for a 'Vector ByteString').

The motivating use case is the consumer's record-batch
decode path: a 50 MiB fetch response with 100 K records used
to produce a 'V.Vector ByteString' where every key+value
slice carried its own 'ForeignPtr' reference. Each ByteString
is a 24-byte 'BS.PS' header, plus its 'ForeignPtr' costs ~32
bytes of GC overhead — so 100 K records cost ~5.6 MiB of
header / GC bookkeeping just to hold the references. With
'SliceVector' the same data is one 'ForeignPtr' + a tightly-
packed @(Int32, Int32)@ array (1.6 MiB for 100 K entries,
plus the ~16-byte 'SliceVector' wrapper).

== When to use

Use 'SliceVector' when:

  * You have many byte slices that all live inside the same
    source buffer (a fetch response, a record batch, a set of
    headers). The shared 'ForeignPtr' keeps the source alive
    once for the whole vector.
  * Per-slice memory overhead matters (long-lived caches; large
    record batches; tight GC loops).

Use 'V.Vector ByteString' or @[ByteString]@ when:

  * Slices come from different source buffers (each
    'ByteString' carries its own 'ForeignPtr', and you'd be
    forced to allocate a synthetic 'ForeignPtr' to share).
  * You need the standard 'Vector' API surface ('V.map',
    fusion, etc.).

== Lifetime

The 'ForeignPtr Word8' reference is the single source of
truth for the backing buffer's lifetime. As long as the
'SliceVector' is reachable, the buffer it slices over stays
alive — even if every individual slice ('indexBS') is
collected. That mirrors how 'BS.PS fp off len' keeps a single
'ForeignPtr' alive through any number of slices.

== Building

Construct via:

  * 'fromForeignPtr' — explicit 'ForeignPtr' + offset / length
    pairs;
  * 'fromByteStrings' — a list of 'ByteString' slices that
    /must/ already share the same source buffer (asserted at
    runtime — the helper crashes if they don't, since it
    can't otherwise build a single coherent slice index).

Construction by /walking/ the source buffer once + appending
slice descriptors as you decode is the intended fast path;
that's what the codegen emits for arrays of length-prefixed
bytes / nested structs.
-}
module Kafka.Protocol.Wire.SliceVector (
  -- * Types
  SliceVector,

  -- * Construction
  empty,
  singleton,
  fromForeignPtr,
  fromByteStrings,
  fromForeignPtrSlices,

  -- * Indexing
  length,
  null,
  indexBS,
  indexUnsafe,
  (!),

  -- * Iteration
  toList,
  toListBS,
  foldlSlices',
  foldlBS',
  forSlices_,

  -- * Conversion to standard types
  toVector,

  -- * Internal accessors (exposed for the codegen, not for

  --   application code)
  sliceVectorBuffer,
  sliceVectorOffsets,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Generics (Generic)
import Prelude hiding (length, null)


----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

{- | Compact vector of byte slices that all share a single
'ForeignPtr Word8' backing buffer. See the module
documentation for design rationale.

The 'Eq' / 'Show' instances compare slice /contents/, not
backing-pointer identity — two 'SliceVector's whose
'indexBS' values are pairwise equal compare 'True', even if
they point at different buffers. (This matches the 'Eq'
semantics of the underlying 'ByteString' slices.)
-}
data SliceVector = SliceVector
  { sliceVectorBuffer :: {-# UNPACK #-} !(ForeignPtr Word8)
  {- ^ The shared backing buffer. Codegen-only — application
  code should index via 'indexBS' / 'toListBS'.
  -}
  , sliceVectorOffsets :: {-# UNPACK #-} !(VU.Vector (Int32, Int32))
  {- ^ Per-slice @(offset, length)@ pairs. The offset is the
  byte position inside @sliceVectorBuffer@; the length is
  the slice's byte count. Codegen-only.
  -}
  }
  deriving stock (Generic)


instance Show SliceVector where
  showsPrec d sv =
    showParen (d > 10) $
      showString "SliceVector "
        . showsPrec 11 (toListBS sv)


instance Eq SliceVector where
  a == b = toListBS a == toListBS b


----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

{- | The empty 'SliceVector'. The backing 'ForeignPtr' is the
shared one inside the empty 'ByteString' (a zero-byte
buffer) — never indexed since 'length' is 0.
-}
{-# NOINLINE empty #-}
empty :: SliceVector
empty =
  let !(fp, _, _) = BSI.toForeignPtr BSI.empty
  in SliceVector
       { sliceVectorBuffer = fp
       , sliceVectorOffsets = VU.empty
       }


{- | Build a 'SliceVector' with a single slice over the given
source 'ForeignPtr'.
-}
{-# INLINE singleton #-}
singleton
  :: ForeignPtr Word8
  -> Int32
  -- ^ slice offset (within the backing buffer)
  -> Int32
  -- ^ slice length
  -> SliceVector
singleton fp off len =
  SliceVector
    { sliceVectorBuffer = fp
    , sliceVectorOffsets = VU.singleton (off, len)
    }


{- | Build a 'SliceVector' from a 'ForeignPtr' and a list of
@(offset, length)@ pairs that live inside that buffer.

The pairs are not validated against the backing buffer's
size (we don't know it; 'ForeignPtr Word8' is opaque about
length). Out-of-bounds offsets surface only when
'indexBS' is called.
-}
{-# INLINE fromForeignPtr #-}
fromForeignPtr
  :: ForeignPtr Word8
  -> [(Int32, Int32)]
  -> SliceVector
fromForeignPtr fp pairs =
  SliceVector
    { sliceVectorBuffer = fp
    , sliceVectorOffsets = VU.fromList pairs
    }


{- | 'fromForeignPtr' with the offsets pre-built into an
unboxed 'Data.Vector.Unboxed.Vector'. The codegen-emitted
decoders use this so they can build the offset vector with
'Data.Vector.Unboxed.Mutable.unsafeWrite' without having to
round-trip through a list.
-}
{-# INLINE fromForeignPtrSlices #-}
fromForeignPtrSlices
  :: ForeignPtr Word8
  -> VU.Vector (Int32, Int32)
  -> SliceVector
fromForeignPtrSlices = SliceVector


{- | Build a 'SliceVector' from a list of 'ByteString' slices
that /must/ already share the same source 'ForeignPtr'.

The helper crashes (with 'error') if the input is empty or
if any slice points at a different 'ForeignPtr' than the
first slice — there's no sensible single-buffer
representation for slices over different sources, and
silently allocating a fresh backing buffer would defeat the
whole point of this type.

Use 'fromForeignPtr' / 'fromForeignPtrSlices' from the
decoder side, where you have the source 'ForeignPtr' in
scope and don't need this assertion.
-}
fromByteStrings :: [ByteString] -> SliceVector
fromByteStrings [] = empty
fromByteStrings (b0 : bs0) =
  let !(fp0, off0, len0) = BSI.toForeignPtr b0
      go !acc [] = SliceVector fp0 (VU.fromList (reverse acc))
      go !acc (b : rest) =
        let !(fp, off, len) = BSI.toForeignPtr b
        in if fp == fp0
             then go ((fromIntegral off, fromIntegral len) : acc) rest
             else
               error
                 "Kafka.Protocol.Wire.SliceVector.fromByteStrings: \
                 \input slices share more than one ForeignPtr; use \
                 \'fromForeignPtr' or 'fromForeignPtrSlices' from \
                 \the decoder, where the source buffer is known."
  in go [(fromIntegral off0, fromIntegral len0)] bs0


----------------------------------------------------------------------
-- Indexing
----------------------------------------------------------------------

-- | Number of slices in the vector.
{-# INLINE length #-}
length :: SliceVector -> Int
length = VU.length . sliceVectorOffsets


-- | True when the vector contains zero slices.
{-# INLINE null #-}
null :: SliceVector -> Bool
null = VU.null . sliceVectorOffsets


{- | Bounds-checked indexing. Returns the slice as a fresh
/zero-copy/ 'ByteString' that re-uses the 'SliceVector''s
backing 'ForeignPtr'.
-}
indexBS :: SliceVector -> Int -> ByteString
indexBS sv i =
  case sliceVectorOffsets sv VU.!? i of
    Nothing ->
      error
        ( "Kafka.Protocol.Wire.SliceVector.indexBS: index "
            ++ show i
            ++ " out of range [0,"
            ++ show (length sv)
            ++ ")"
        )
    Just (!o, !l) ->
      BSI.fromForeignPtr
        (sliceVectorBuffer sv)
        (fromIntegral o)
        (fromIntegral l)


{- | Unsafe (no bounds check) variant of 'indexBS'. Use when
the index is known good — e.g. inside a counted loop.
-}
{-# INLINE indexUnsafe #-}
indexUnsafe :: SliceVector -> Int -> ByteString
indexUnsafe sv i =
  let !(o, l) = VU.unsafeIndex (sliceVectorOffsets sv) i
  in BSI.fromForeignPtr
       (sliceVectorBuffer sv)
       (fromIntegral o)
       (fromIntegral l)


-- | Operator alias for 'indexBS' (bounds-checked).
{-# INLINE (!) #-}
(!) :: SliceVector -> Int -> ByteString
(!) = indexBS


----------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------

{- | Convert to a list of @(offset, length)@ pairs. Useful for
tests + size checks; production code should use 'toListBS'
or one of the fold helpers.
-}
{-# INLINE toList #-}
toList :: SliceVector -> [(Int32, Int32)]
toList = VU.toList . sliceVectorOffsets


-- | Convert to a list of zero-copy 'ByteString' slices.
{-# INLINE toListBS #-}
toListBS :: SliceVector -> [ByteString]
toListBS sv =
  [indexUnsafe sv i | i <- [0 .. length sv - 1]]


{- | Strict left-fold over the @(offset, length)@ pairs without
materialising any 'ByteString'. The fastest way to walk the
vector when the body only needs the lengths or offsets.
-}
{-# INLINE foldlSlices' #-}
foldlSlices' :: (b -> Int32 -> Int32 -> b) -> b -> SliceVector -> b
foldlSlices' f z sv =
  VU.foldl' (\acc (o, l) -> f acc o l) z (sliceVectorOffsets sv)


{- | Strict left-fold that hands each slice to the function as
a zero-copy 'ByteString'. Use when the body wants to inspect
bytes but the per-iteration 'ByteString' header is fine.
-}
{-# INLINE foldlBS' #-}
foldlBS' :: (b -> ByteString -> b) -> b -> SliceVector -> b
foldlBS' f z sv =
  VU.ifoldl' (\acc i _ -> f acc (indexUnsafe sv i)) z (sliceVectorOffsets sv)


{- | Effectful sibling of 'foldlBS_': run an 'IO' action for
each slice in order. The slice is handed in as a zero-copy
'ByteString'.
-}
{-# INLINE forSlices_ #-}
forSlices_ :: SliceVector -> (ByteString -> IO ()) -> IO ()
forSlices_ sv f =
  let !n = length sv
      go !i
        | i >= n = pure ()
        | otherwise = f (indexUnsafe sv i) >> go (i + 1)
  in go 0


----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

{- | Materialise a 'V.Vector ByteString' from the slice
vector. The output 'Vector' carries one separate
'ByteString' per element, each pointing at the source
buffer; the original 'SliceVector''s backing 'ForeignPtr'
stays alive through the slices' references. Allocates @O(n)@
'ByteString' headers.

Use this only at the boundary where the application code
expects a 'V.Vector ByteString'; otherwise prefer
'foldlBS'' / 'forSlices_' which avoid the per-slice
allocation.
-}
{-# INLINE toVector #-}
toVector :: SliceVector -> V.Vector ByteString
toVector sv =
  V.generate (length sv) (indexUnsafe sv)
