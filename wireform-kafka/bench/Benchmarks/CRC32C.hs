{-# LANGUAGE BangPatterns #-}

{- |
Module      : Benchmarks.CRC32C
Description : Benchmarks for CRC32C implementations
Copyright   : (c) 2025
License     : BSD-3-Clause

This module benchmarks the hardware-accelerated CRC32C implementation
against a naive reference implementation to measure performance improvements.
-}
module Benchmarks.CRC32C (benchmarks) where

import Benchmarks.Util (mkBenchData)
import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word32, Word8)
import Kafka.Protocol.CRC32C qualified as CRC


-- -----------------------------------------------------------------------------
-- Naive Reference Implementation
-- -----------------------------------------------------------------------------

{- | Naive CRC32C implementation using the Castagnoli polynomial.
This is a simple, unoptimized reference implementation for benchmarking.
-}
naiveCrc32c :: ByteString -> Word32
naiveCrc32c bs = BS.foldl' crc32cByte 0xFFFFFFFF bs `xor` 0xFFFFFFFF
  where
    -- CRC32C (Castagnoli) polynomial: 0x1EDC6F41
    -- Reversed: 0x82F63B78
    crc32cByte :: Word32 -> Word8 -> Word32
    crc32cByte !crc !byte =
      let !crc' = crc `xor` fromIntegral byte
          step !c =
            if c .&. 1 /= 0
              then (c `shiftR` 1) `xor` 0x82F63B78
              else c `shiftR` 1
      in step $ step $ step $ step $ step $ step $ step $ step crc'


-- | Naive incremental CRC32C computation
naiveCrc32cAppend :: Word32 -> ByteString -> Word32
naiveCrc32cAppend !crc bs = BS.foldl' crc32cByte crc bs
  where
    crc32cByte :: Word32 -> Word8 -> Word32
    crc32cByte !c !byte =
      let !c' = c `xor` fromIntegral byte
          step !x =
            if x .&. 1 /= 0
              then (x `shiftR` 1) `xor` 0x82F63B78
              else x `shiftR` 1
      in step $ step $ step $ step $ step $ step $ step $ step c'


-- -----------------------------------------------------------------------------
-- Benchmarks
-- -----------------------------------------------------------------------------

-- | All CRC32C benchmarks
benchmarks :: Benchmark
benchmarks =
  bgroup
    "CRC32C"
    [ benchSmall
    , benchMedium
    , benchLarge
    , benchIncremental
    ]


-- | Small input benchmarks (16B, 64B, 256B)
benchSmall :: Benchmark
benchSmall =
  bgroup
    "Small"
    [ bgroup
        "16B"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData 16)
        , bench "naive" $ whnf naiveCrc32c (mkBenchData 16)
        ]
    , bgroup
        "64B"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData 64)
        , bench "naive" $ whnf naiveCrc32c (mkBenchData 64)
        ]
    , bgroup
        "256B"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData 256)
        , bench "naive" $ whnf naiveCrc32c (mkBenchData 256)
        ]
    ]


-- | Medium input benchmarks (1KB, 4KB, 16KB)
benchMedium :: Benchmark
benchMedium =
  bgroup
    "Medium"
    [ bgroup
        "1KB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData 1024)
        , bench "naive" $ whnf naiveCrc32c (mkBenchData 1024)
        ]
    , bgroup
        "4KB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData (4 * 1024))
        , bench "naive" $ whnf naiveCrc32c (mkBenchData (4 * 1024))
        ]
    , bgroup
        "16KB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData (16 * 1024))
        , bench "naive" $ whnf naiveCrc32c (mkBenchData (16 * 1024))
        ]
    ]


-- | Large input benchmarks (64KB, 256KB, 1MB)
benchLarge :: Benchmark
benchLarge =
  bgroup
    "Large"
    [ bgroup
        "64KB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData (64 * 1024))
        , bench "naive" $ whnf naiveCrc32c (mkBenchData (64 * 1024))
        ]
    , bgroup
        "256KB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData (256 * 1024))
        , bench "naive" $ whnf naiveCrc32c (mkBenchData (256 * 1024))
        ]
    , bgroup
        "1MB"
        [ bench "hardware-accelerated" $ whnf CRC.crc32c (mkBenchData (1024 * 1024))
        , bench "naive" $ whnf naiveCrc32c (mkBenchData (1024 * 1024))
        ]
    ]


-- | Incremental CRC computation benchmarks
benchIncremental :: Benchmark
benchIncremental =
  bgroup
    "Incremental"
    [ bgroup
        "4KB/2chunks"
        [ bench "hardware-accelerated" $ whnf benchIncremental2HW (mkBenchData (4 * 1024))
        , bench "naive" $ whnf benchIncremental2Naive (mkBenchData (4 * 1024))
        ]
    , bgroup
        "64KB/4chunks"
        [ bench "hardware-accelerated" $ whnf benchIncremental4HW (mkBenchData (64 * 1024))
        , bench "naive" $ whnf benchIncremental4Naive (mkBenchData (64 * 1024))
        ]
    , bgroup
        "1MB/4chunks"
        [ bench "hardware-accelerated" $ whnf benchIncremental4HW (mkBenchData (1024 * 1024))
        , bench "naive" $ whnf benchIncremental4Naive (mkBenchData (1024 * 1024))
        ]
    ]


-- | Hardware-accelerated incremental with 2 chunks
benchIncremental2HW :: ByteString -> Word32
benchIncremental2HW !bs =
  let (chunk1, chunk2) = BS.splitAt (BS.length bs `div` 2) bs
  in CRC.crc32cFinalize $
       CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2


-- | Naive incremental with 2 chunks
benchIncremental2Naive :: ByteString -> Word32
benchIncremental2Naive !bs =
  let (chunk1, chunk2) = BS.splitAt (BS.length bs `div` 2) bs
  in naiveCrc32cAppend (naiveCrc32cAppend 0xFFFFFFFF chunk1) chunk2 `xor` 0xFFFFFFFF


-- | Hardware-accelerated incremental with 4 chunks
benchIncremental4HW :: ByteString -> Word32
benchIncremental4HW !bs =
  let chunkSize = BS.length bs `div` 4
      chunk1 = BS.take chunkSize bs
      chunk2 = BS.take chunkSize (BS.drop chunkSize bs)
      chunk3 = BS.take chunkSize (BS.drop (chunkSize * 2) bs)
      chunk4 = BS.drop (chunkSize * 3) bs
  in CRC.crc32cFinalize $
       CRC.crc32cAppend
         ( CRC.crc32cAppend
             ( CRC.crc32cAppend
                 (CRC.crc32cAppend CRC.crc32cInit chunk1)
                 chunk2
             )
             chunk3
         )
         chunk4


-- | Naive incremental with 4 chunks
benchIncremental4Naive :: ByteString -> Word32
benchIncremental4Naive !bs =
  let chunkSize = BS.length bs `div` 4
      chunk1 = BS.take chunkSize bs
      chunk2 = BS.take chunkSize (BS.drop chunkSize bs)
      chunk3 = BS.take chunkSize (BS.drop (chunkSize * 2) bs)
      chunk4 = BS.drop (chunkSize * 3) bs
  in naiveCrc32cAppend
       ( naiveCrc32cAppend
           ( naiveCrc32cAppend
               (naiveCrc32cAppend 0xFFFFFFFF chunk1)
               chunk2
           )
           chunk3
       )
       chunk4
       `xor` 0xFFFFFFFF
