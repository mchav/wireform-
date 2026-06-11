{- | Microbenchmark comparing the Parquet bloom filter's XXH64 hot path
between the C/SIMDe kernel ("Wireform.Hash") and the pure Haskell
reference ("XXH64Pure"). Pure version lives in the bench tree only.

Run with:

@
cabal bench wireform-parquet:parquet-bench
@
-}
module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, whnf)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Word (Word64)
import Parquet.BloomFilter qualified as BF
import Wireform.Hash qualified as Hash
import XXH64Pure qualified as Pure


deterministic :: Int -> BS.ByteString
deterministic n = BS.pack (take n (cycle [0 .. 255]))


filledFilter :: BF.Sbbf
filledFilter =
  let !bf0 = BF.newSbbf (BF.optimalNumBytes 1024 0.01)
      !hashes = take 1024 ([fromIntegral i :: Word64 | i <- [(1 :: Int) ..]])
  in foldl' (flip BF.sbbfInsertHash) bf0 hashes


main :: IO ()
main =
  defaultMain
    [ bgroup
        "Parquet XXH64"
        [ env (pure (deterministic 8)) $ \bs -> bgroup "8 B" (xs bs)
        , env (pure (deterministic 64)) $ \bs -> bgroup "64 B" (xs bs)
        , env (pure (deterministic 1024)) $ \bs -> bgroup "1 KiB" (xs bs)
        , env (pure (deterministic 65536)) $ \bs -> bgroup "64 KiB" (xs bs)
        ]
    , bgroup
        "Parquet bloom filter"
        [ bench "sbbfInsert (8 B value)" $
            whnf (\bs -> BF.sbbfInsert bs (BF.newSbbf 1024)) (deterministic 8)
        , bench "sbbfCheck hit" $
            whnf (\h -> BF.sbbfCheckHash h filledFilter) (1 :: Word64)
        , bench "sbbfCheck miss" $
            whnf (\h -> BF.sbbfCheckHash h filledFilter) (0xdeadbeef :: Word64)
        ]
    ]
  where
    xs bs =
      [ bench "C" $ whnf (Hash.xxh64 0) bs
      , bench "pure" $ whnf Pure.xxh64_pure bs
      ]
