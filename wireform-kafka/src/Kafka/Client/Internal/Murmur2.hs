{-# LANGUAGE BangPatterns #-}

{-|
Module      : Kafka.Client.Internal.Murmur2
Description : Kafka-compatible murmur2 hash for the default partitioner

Kafka's @DefaultPartitioner@ routes records by
@(murmur2(key) & 0x7FFFFFFF) % numPartitions@. Every official
client (JVM, librdkafka, kafka-go, …) uses the same murmur2
variant + seed so a record produced with @key=\"foo\"@ from any
language lands on the same partition.

The previous implementation used 'Data.Hashable.hash' (siphash),
which means our partitioner /did not agree with the rest of the
ecosystem/. Keys would be routed to different partitions than
the same key produced from a JVM or librdkafka client, breaking
per-key ordering in mixed-client deployments.

This module mirrors @org.apache.kafka.common.utils.Utils.murmur2(byte[])@
exactly, with the same seed (@0x9747b28c@), constants
(@m=0x5bd1e995@, @r=24@), and tail-handling switch.

Test vectors are derived from the JVM's output and live in
'Client.Murmur2Spec'.
-}
module Kafka.Client.Internal.Murmur2
  ( -- * Hash
    murmur2
    -- * Partitioner helper
  , partitionForKey
  , toPositive
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32)
import Data.Word (Word32)

-- | Kafka-compatible murmur2 hash (32-bit).
--
-- The result is reinterpreted as 'Int32' (signed) to match the
-- JVM client's return type — callers needing a non-negative
-- value should pipe through 'toPositive'.
murmur2 :: ByteString -> Int32
murmur2 bs =
  let
    !m    = 0x5bd1e995 :: Word32
    !r    = 24         :: Int
    !seed = 0x9747b28c :: Word32
    !n    = BS.length bs
    !len4 = n `shiftR` 2     -- n / 4
    !h0   = seed `xor` fromIntegral n

    -- Loop over the 4-byte chunks. 'BSU.unsafeIndex' is safe:
    -- we know the index is in range from the loop bound.
    body !i !h
      | i >= len4 = h
      | otherwise =
          let !i4 = i `shiftL` 2
              !b0 = fromIntegral (BSU.unsafeIndex bs  i4)      :: Word32
              !b1 = fromIntegral (BSU.unsafeIndex bs (i4 + 1)) :: Word32
              !b2 = fromIntegral (BSU.unsafeIndex bs (i4 + 2)) :: Word32
              !b3 = fromIntegral (BSU.unsafeIndex bs (i4 + 3)) :: Word32
              !k0 = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
              !k1 = k0 * m
              !k2 = k1 `xor` (k1 `shiftR` r)
              !k3 = k2 * m
              !h1 = h * m
              !h2 = h1 `xor` k3
          in body (i + 1) h2

    !hAfterBody = body 0 h0

    -- Tail: 0..3 leftover bytes. Mirrors the JVM's switch
    -- (case 3 falls through to 2, falls through to 1, falls
    -- through to the multiply).
    !tailStart = len4 `shiftL` 2
    !rem_      = n .&. 3

    !ht1 =
      if rem_ >= 3
        then let !b = fromIntegral (BSU.unsafeIndex bs (tailStart + 2)) :: Word32
             in hAfterBody `xor` (b `shiftL` 16)
        else hAfterBody

    !ht2 =
      if rem_ >= 2
        then let !b = fromIntegral (BSU.unsafeIndex bs (tailStart + 1)) :: Word32
             in ht1 `xor` (b `shiftL` 8)
        else ht1

    !ht3 =
      if rem_ >= 1
        then let !b = fromIntegral (BSU.unsafeIndex bs tailStart) :: Word32
                 !x = ht2 `xor` b
             in x * m
        else ht2

    -- Final mix.
    !h4 = ht3 `xor` (ht3 `shiftR` 13)
    !h5 = h4  * m
    !h6 = h5  `xor` (h5  `shiftR` 15)
  in
    fromIntegral h6 :: Int32

-- | Coerce a possibly-negative 'Int32' into the non-negative
-- range Kafka uses for partition selection. Mirrors the JVM's
-- @Utils.toPositive@.
{-# INLINE toPositive #-}
toPositive :: Int32 -> Int32
toPositive n = n .&. 0x7FFFFFFF

-- | Pick a partition for a key the same way the JVM's
-- @DefaultPartitioner@ does:
--
-- @
-- partition = (murmur2(key) & 0x7FFFFFFF) % numPartitions
-- @
{-# INLINE partitionForKey #-}
partitionForKey :: ByteString -> Int32 -> Int32
partitionForKey key numPartitions
  | numPartitions <= 0 = 0
  | otherwise          = toPositive (murmur2 key) `mod` numPartitions
