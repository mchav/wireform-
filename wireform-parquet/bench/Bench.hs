-- | Microbenchmark comparing the Parquet bloom filter's XXH64 hot path
-- between the C/SIMDe kernel (via @Wireform.Hash.xxh64@, exported as
-- @Parquet.XXH64.xxh64@) and the pure Haskell reference
-- (@Parquet.XXH64.xxh64_pure@).
--
-- Run with:
--
-- @
-- cabal bench wireform-parquet:parquet-bench
-- @
module Main (main) where

import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64)

import Criterion.Main (bench, bgroup, defaultMain, env, whnf)

import qualified Parquet.BloomFilter as BF
import qualified Parquet.XXH64 as XXH

deterministic :: Int -> BS.ByteString
deterministic n = BS.pack (take n (cycle [0..255]))

-- | Build a 1024-entry bloom filter sized for 1% FP rate.
filledFilter :: BF.Sbbf
filledFilter =
  let !bf0 = BF.newSbbf (BF.optimalNumBytes 1024 0.01)
      !hashes = take 1024 ([fromIntegral i :: Word64 | i <- [(1 :: Int) ..]])
   in foldl' (flip BF.sbbfInsertHash) bf0 hashes

main :: IO ()
main = defaultMain
  [ bgroup "Parquet XXH64"
      [ env (pure (deterministic 8))    $ \bs -> bgroup "8 B"
          [ bench "C"    $ whnf XXH.xxh64       bs
          , bench "pure" $ whnf XXH.xxh64_pure  bs
          ]
      , env (pure (deterministic 64))   $ \bs -> bgroup "64 B"
          [ bench "C"    $ whnf XXH.xxh64       bs
          , bench "pure" $ whnf XXH.xxh64_pure  bs
          ]
      , env (pure (deterministic 1024)) $ \bs -> bgroup "1 KiB"
          [ bench "C"    $ whnf XXH.xxh64       bs
          , bench "pure" $ whnf XXH.xxh64_pure  bs
          ]
      , env (pure (deterministic 65536)) $ \bs -> bgroup "64 KiB"
          [ bench "C"    $ whnf XXH.xxh64       bs
          , bench "pure" $ whnf XXH.xxh64_pure  bs
          ]
      ]

  -- Bloom-filter end-to-end: insert / check share the same XXH64 path
  -- and the C kernel speedup compounds (one hash per call).
  , bgroup "Parquet bloom filter"
      [ bench "sbbfInsert (8 B value)"
          $ whnf (\bs -> BF.sbbfInsert bs (BF.newSbbf 1024)) (deterministic 8)
      , bench "sbbfCheck hit"
          $ whnf (\h -> BF.sbbfCheckHash h filledFilter) (1 :: Word64)
      , bench "sbbfCheck miss"
          $ whnf (\h -> BF.sbbfCheckHash h filledFilter) (0xdeadbeef :: Word64)
      , bench "sbbfInsert pure-hash equivalent"
          $ whnf (\bs -> BF.sbbfInsertHash (XXH.xxh64_pure bs) (BF.newSbbf 1024))
                 (deterministic 8)
      ]
  ]
