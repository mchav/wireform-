{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Bits ((.&.), shiftL, shiftR, xor)
#ifdef WORDS_BIGENDIAN
import Data.Bits ((.|.))
import Data.Word (Word8)
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import qualified Foreign.Ptr
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.IO (unsafePerformIO)

-- | Kafka-compatible murmur2 hash (32-bit).
--
-- The result is reinterpreted as 'Int32' (signed) to match the
-- JVM client's return type — callers needing a non-negative
-- value should pipe through 'toPositive'.
--
-- == Implementation
--
-- The body loop reads each 4-byte chunk as a single unaligned
-- @Word32@ load via 'peekByteOff'. Murmur2 is a little-endian
-- algorithm by spec, so on the dominant little-endian targets
-- (x86-64, AArch64, …) this is a single @MOV@ that the previous
-- byte-by-byte assembly would have taken four loads + three
-- shifts + three ORs to materialise. On the rare big-endian
-- target we fall back to the byte-assembly path, which preserves
-- the algorithm's defined byte order regardless of host endianness.
--
-- Verified byte-identical with the JVM's
-- @org.apache.kafka.common.utils.Utils.murmur2(byte[])@ output
-- via the canonical test vectors in 'Client.Murmur2Spec'.
murmur2 :: ByteString -> Int32
murmur2 bs = unsafePerformIO $ do
  let
    !m    = 0x5bd1e995 :: Word32
    !r    = 24         :: Int
    !seed = 0x9747b28c :: Word32
    !n    = BS.length bs
    !len4 = n `shiftR` 2     -- n / 4
    !h0   = seed `xor` fromIntegral n
    !(fp, off, _) = BSI.toForeignPtr bs

  hAfterBody <- withForeignPtr fp $ \basePtr -> do
    let !p0 = basePtr `plusPtr` off
        body !i !h
          | i >= len4 = pure h
          | otherwise = do
              -- Single unaligned 32-bit load. On LE hosts (x86-64,
              -- AArch64) this is one MOV; the spec's
              -- "little-endian Word32 from 4 bytes starting at i*4"
              -- matches the host byte order so no swap is needed.
              -- On BE hosts we fall back below to the
              -- byte-by-byte path.
              !k0 <- loadWord32LE p0 (i `shiftL` 2)
              let !k1 = k0 * m
                  !k2 = k1 `xor` (k1 `shiftR` r)
                  !k3 = k2 * m
                  !h1 = h * m
                  !h2 = h1 `xor` k3
              body (i + 1) h2
    body 0 h0

  -- Tail: 0..3 leftover bytes. Mirrors the JVM's switch
  -- (case 3 falls through to 2, falls through to 1, falls
  -- through to the multiply).
  let !tailStart = len4 `shiftL` 2
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
  pure (fromIntegral h6 :: Int32)

-- | Load 4 bytes starting at @basePtr + off@ as a little-endian
-- 'Word32'. On little-endian hosts (the common case) this is a
-- single unaligned 32-bit load; on big-endian hosts we have to
-- assemble the value from individual byte reads to preserve
-- Murmur2's defined LE byte order. The conditional is resolved
-- at compile time by CPP from the host's
-- 'WORDS_BIGENDIAN' macro.
{-# INLINE loadWord32LE #-}
loadWord32LE :: forall a. Foreign.Ptr.Ptr a -> Int -> IO Word32
#ifdef WORDS_BIGENDIAN
loadWord32LE basePtr off = do
  -- BE host: synthesise the LE Word32 from explicit byte reads.
  b0 <- peekByteOff basePtr  off      :: IO Word8
  b1 <- peekByteOff basePtr (off + 1) :: IO Word8
  b2 <- peekByteOff basePtr (off + 2) :: IO Word8
  b3 <- peekByteOff basePtr (off + 3) :: IO Word8
  pure $! fromIntegral b0
        .|. (fromIntegral b1 `shiftL`  8)
        .|. (fromIntegral b2 `shiftL` 16)
        .|. (fromIntegral b3 `shiftL` 24)
#else
loadWord32LE basePtr off = peekByteOff basePtr off
#endif

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
