{-# LANGUAGE BangPatterns #-}

{- | Bucket transforms and Murmur3 32-bit hash, byte-compatible with
Apache Iceberg's @org.apache.iceberg.util.BucketUtil@.

Iceberg uses a non-standard variant of Murmur3 32-bit: the seed is fixed
to @0@, and the hash is computed over a canonical binary representation
of the value (little-endian 8-byte long, UTF-8 string, IEEE 754 long
bits for floats). The bucket transform is then
@(hash & Integer.MAX_VALUE) % N@.

Backed by the SIMDe-portable C kernel in
@wireform-core/cbits/wireform_hash_simd.c@ via "Wireform.Hash". Pure
Haskell reference implementations live in "Iceberg.Murmur3.Pure" for
the criterion benchmark; user code should use the entry points in
this module.
-}
module Iceberg.Murmur3 (
  -- * Hash
  murmur3_32,

  -- * Bucket transforms (one per Iceberg source type)
  bucketInt,
  bucketLong,
  bucketString,
  bucketBytes,
) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Wireform.Hash qualified as Hash


{- | Murmur3 32-bit, seed 0, exactly the function Iceberg's
@BucketUtil.hash@ wraps. Returns a signed 'Int32' so callers can mask
with @Integer.MAX_VALUE@ as the spec describes.
-}
murmur3_32 :: ByteString -> Int32
murmur3_32 = Hash.murmur3_32
{-# INLINE murmur3_32 #-}


-- | Iceberg @bucket[N]@ on an int / date column.
bucketInt :: Int -> Int32 -> Int
bucketInt n v = Hash.bucketLong n (fromIntegral v)
{-# INLINE bucketInt #-}


-- | Iceberg @bucket[N]@ on a long / timestamp / timestamptz column.
bucketLong :: Int -> Int64 -> Int
bucketLong = Hash.bucketLong
{-# INLINE bucketLong #-}


-- | Iceberg @bucket[N]@ on a string column (UTF-8 byte hash).
bucketString :: Int -> Text -> Int
bucketString n = Hash.bucketBytes n . TE.encodeUtf8
{-# INLINE bucketString #-}


-- | Iceberg @bucket[N]@ on a binary / fixed / uuid / decimal column.
bucketBytes :: Int -> ByteString -> Int
bucketBytes = Hash.bucketBytes
{-# INLINE bucketBytes #-}
