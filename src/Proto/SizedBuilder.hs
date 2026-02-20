{-# LANGUAGE BangPatterns #-}
-- | Church-encoded sized builder: fuses size calculation with serialization.
--
-- A 'SizedBuilder' carries both the computed byte size and the builder
-- in a single value. This avoids the need for a separate size-computation
-- pass when encoding submessages — the size is accumulated alongside
-- the builder as fields are appended.
--
-- This is the Church encoding of a (Int, Builder) pair, where the
-- consumer gets both values via a single continuation call rather
-- than pattern-matching a tuple.
module Proto.SizedBuilder
  ( -- * Core type
    SizedBuilder

    -- * Construction
  , empty
  , sized

    -- * Running
  , toBuilder
  , toByteString
  , size
  , toLazyByteString

    -- * Combinators
  , withSubMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import Proto.Wire.Encode (putVarint)

-- | A builder that tracks its own byte size.
-- Internally this is just a strict pair, but the API is designed
-- so that size and builder are always computed together (fused).
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

-- | Create an empty sized builder.
empty :: SizedBuilder
empty = mempty
{-# INLINE empty #-}

-- | Create a sized builder from a known size and builder.
sized :: Int -> B.Builder -> SizedBuilder
sized = SizedBuilder
{-# INLINE sized #-}

-- | Extract the builder.
toBuilder :: SizedBuilder -> B.Builder
toBuilder = sbBuilder
{-# INLINE toBuilder #-}

-- | Extract the total size in bytes.
size :: SizedBuilder -> Int
size = sbSize
{-# INLINE size #-}

-- | Produce a strict ByteString.
toByteString :: SizedBuilder -> ByteString
toByteString = BL.toStrict . B.toLazyByteString . sbBuilder

-- | Produce a lazy ByteString.
toLazyByteString :: SizedBuilder -> BL.ByteString
toLazyByteString = B.toLazyByteString . sbBuilder

-- | Wrap a SizedBuilder as a length-delimited submessage.
-- Since the size is already known, this avoids materializing the
-- submessage to compute its length — it just prepends the varint size.
withSubMessage :: SizedBuilder -> SizedBuilder
withSubMessage sb =
  let !payloadSize = sbSize sb
      !lenPrefixBuilder = putVarint (fromIntegral payloadSize)
  in SizedBuilder
       { sbSize    = varintSizeOf payloadSize + payloadSize
       , sbBuilder = lenPrefixBuilder <> sbBuilder sb
       }
  where
    varintSizeOf :: Int -> Int
    varintSizeOf !n
      | n < 0x80       = 1
      | n < 0x4000     = 2
      | n < 0x200000   = 3
      | n < 0x10000000 = 4
      | otherwise       = 5
    {-# INLINE varintSizeOf #-}
{-# INLINE withSubMessage #-}
