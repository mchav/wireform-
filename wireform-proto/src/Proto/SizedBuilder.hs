{-# LANGUAGE BangPatterns #-}
-- | Church-encoded sized builder: fuses size calculation with serialization.
--
-- A 'SizedBuilder' carries both the computed byte size and the builder
-- in a single value. This avoids the need for a separate size-computation
-- pass when encoding submessages — the size is accumulated alongside
-- the builder as fields are appended.
--
-- 'toByteString' allocates a single strict ByteString of exactly the
-- right size and fills it in one pass — no intermediate lazy chunks
-- or recopying.
module Proto.SizedBuilder
  ( -- * Core type
    SizedBuilder

    -- * Construction
  , empty
  , sized

    -- * Running (strict ByteString — single allocation)
  , toByteString
  , toByteStringFromBuilder
  , toBuilder
  , size
  , toLazyByteString

    -- * Combinators
  , withSubMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as BL

import Proto.Wire.Encode (putVarint, varintSize)

-- | A builder that tracks its own byte size.
data SizedBuilder = SizedBuilder
  { sbSize    :: {-# UNPACK #-} !Int
  , sbBuilder :: !B.Builder
  }

instance Semigroup SizedBuilder where
  SizedBuilder s1 b1 <> SizedBuilder s2 b2 =
    SizedBuilder (s1 + s2) (b1 <> b2)
  {-# INLINE (<>) #-}

instance Monoid SizedBuilder where
  mempty = SizedBuilder 0 mempty
  {-# INLINE mempty #-}

empty :: SizedBuilder
empty = mempty
{-# INLINE empty #-}

sized :: Int -> B.Builder -> SizedBuilder
sized = SizedBuilder
{-# INLINE sized #-}

toBuilder :: SizedBuilder -> B.Builder
toBuilder = sbBuilder
{-# INLINE toBuilder #-}

size :: SizedBuilder -> Int
size = sbSize
{-# INLINE size #-}

-- | Produce a strict ByteString using a single allocation of exactly
-- the right size. The builder writes directly into the pre-allocated
-- buffer with no intermediate lazy chunks or recopying.
toByteString :: SizedBuilder -> ByteString
toByteString (SizedBuilder sz bld) = toByteStringFromBuilder sz bld
{-# INLINE toByteString #-}

-- | Produce a strict ByteString from a Builder when the exact size is known.
-- Allocates a single buffer of @sz@ bytes and writes into it directly.
-- If the builder produces exactly @sz@ bytes (which it should when the
-- size was computed correctly), the result is a single zero-copy ByteString.
toByteStringFromBuilder :: Int -> B.Builder -> ByteString
toByteStringFromBuilder sz bld =
  -- untrimmedStrategy: first chunk = sz bytes, subsequent = sz bytes.
  -- Since we know the exact size, the builder should fill the first chunk
  -- completely and produce a single-chunk lazy ByteString.
  let strategy = BE.untrimmedStrategy sz sz
      lbs = BE.toLazyByteStringWith strategy BL.empty bld
  in case BL.toChunks lbs of
    [chunk] -> chunk
    chunks  -> BL.toStrict (BL.fromChunks chunks)
{-# INLINE toByteStringFromBuilder #-}

toLazyByteString :: SizedBuilder -> BL.ByteString
toLazyByteString (SizedBuilder sz bld) =
  BE.toLazyByteStringWith (BE.untrimmedStrategy sz sz) BL.empty bld
{-# INLINE toLazyByteString #-}

withSubMessage :: SizedBuilder -> SizedBuilder
withSubMessage sb =
  let !payloadSize = sbSize sb
      !lenPrefixBuilder = putVarint (fromIntegral payloadSize)
  in SizedBuilder
       { sbSize    = varintSize (fromIntegral payloadSize) + payloadSize
       , sbBuilder = lenPrefixBuilder <> sbBuilder sb
       }
{-# INLINE withSubMessage #-}
