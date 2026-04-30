-- | Microbenchmarks comparing the C/SIMDe kernels in "Iceberg.SIMD" to the
-- pure-Haskell reference implementations they replaced.
--
-- Run with:
--
-- @
-- cabal bench wireform-iceberg:iceberg-bench
-- @
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Vector.Storable as VS
import Data.Word (Word16, Word32)

import Criterion.Main (bench, bgroup, defaultMain, env, nf, whnf)

import qualified Iceberg.DeletionVector as DV
import qualified Iceberg.Murmur3 as M
import qualified Iceberg.SIMD as SIMD

deterministic :: Int -> BS.ByteString
deterministic n = BS.pack (take n (cycle [0..255]))

-- A 1024-position deletion vector, the kind a real Iceberg position-delete
-- file produces for a single row group.
sampleDV :: DV.DeletionVector
sampleDV = DV.addPositions [fromIntegral i :: Int64 | i <- [(0 :: Int) , 4 .. 4000]] DV.emptyDV

sampleDVBytes :: BS.ByteString
sampleDVBytes = DV.encodeDV sampleDV

main :: IO ()
main = defaultMain
  [ bgroup "Murmur3 32-bit"
      [ env (pure (deterministic 8))    $ \bs -> bgroup "8 B"
          [ bench "C"    $ whnf SIMD.murmur3_32 bs
          , bench "pure" $ whnf M.murmur3_32_pure bs
          ]
      , env (pure (deterministic 64))   $ \bs -> bgroup "64 B"
          [ bench "C"    $ whnf SIMD.murmur3_32 bs
          , bench "pure" $ whnf M.murmur3_32_pure bs
          ]
      , env (pure (deterministic 1024)) $ \bs -> bgroup "1 KiB"
          [ bench "C"    $ whnf SIMD.murmur3_32 bs
          , bench "pure" $ whnf M.murmur3_32_pure bs
          ]
      , env (pure (deterministic 65536)) $ \bs -> bgroup "64 KiB"
          [ bench "C"    $ whnf SIMD.murmur3_32 bs
          , bench "pure" $ whnf M.murmur3_32_pure bs
          ]
      ]
  , bgroup "bucket[16] long"
      [ bench "C"    $ whnf (SIMD.bucketLong 16) (34 :: Int64)
      , bench "pure" $ whnf (M.bucketLong_pure 16) (34 :: Int64)
      ]
  , bgroup "XXH64"
      [ env (pure (deterministic 64))    $ \bs -> bench "64 B C" $
          whnf (SIMD.xxh64 0) bs
      , env (pure (deterministic 1024))  $ \bs -> bench "1 KiB C" $
          whnf (SIMD.xxh64 0) bs
      , env (pure (deterministic 65536)) $ \bs -> bench "64 KiB C" $
          whnf (SIMD.xxh64 0) bs
      ]
  , bgroup "Deletion vector decode (1001 positions)"
      [ bench "C"    $ nf DV.decodeDV       sampleDVBytes
      , bench "pure" $ nf DV.decodeDV_pure  sampleDVBytes
      ]
  , bgroup "Deletion vector contains"
      [ env (pure sampleDV) $ \dv -> bgroup ""
          [ bench "C"    $ whnf (\p -> DV.containsPosition p dv) (1024 :: Int64)
          , bench "pure" $ whnf (\p -> case DV.deletedPositions dv of
                                         xs -> p `elem` xs)
                                (1024 :: Int64)
          ]
      ]
  , bgroup "Roaring ARRAY container"
      [ env (pure (VS.fromList [fromIntegral i :: Word16 | i <- [(0 :: Int), 2 .. 2000]])) $ \lows ->
          bgroup ""
          [ bench "encode (C)"
              $ whnf SIMD.roaringEncodeArray lows
          , let payload = SIMD.roaringEncodeArray lows
            in bench "decode (C)"
                  $ nf (SIMD.roaringDecodeArray payload (VS.length lows))
                       (0 :: Word16Tagged)
          , let payload = SIMD.roaringEncodeArray lows
            in bench "contains hit"
                  $ whnf (\v -> SIMD.roaringContains SIMD.ArrayContainer payload (VS.length lows) v)
                         (1000 :: Word16)
          , let payload = SIMD.roaringEncodeArray lows
            in bench "contains miss"
                  $ whnf (\v -> SIMD.roaringContains SIMD.ArrayContainer payload (VS.length lows) v)
                         (3 :: Word16)
          ]
      ]
  , bgroup "Roaring BITSET container"
      [ env (pure (VS.fromList [fromIntegral i :: Word16 | i <- [(0 :: Int) , 1 .. 30000]])) $ \lows ->
          bgroup ""
          [ bench "encode" $ whnf SIMD.roaringEncodeBitset lows
          , let payload = SIMD.roaringEncodeBitset lows
            in bench "decode" $ nf (SIMD.roaringDecodeBitset payload) (0 :: Word16Tagged)
          , let payload = SIMD.roaringEncodeBitset lows
            in bench "contains hit"
                $ whnf (\v -> SIMD.roaringContains SIMD.BitsetContainer payload 0 v)
                       (12345 :: Word16)
          ]
      ]
  ]

-- | Type alias used as the @hi@ argument to 'SIMD.roaringDecodeArray' and
-- 'SIMD.roaringDecodeBitset'. We tag it locally so that the @0@ literals
-- above pick the right instance without needing a top-level type
-- annotation per call.
type Word16Tagged = Word32
