{- | Microbenchmarks comparing the C/SIMDe kernels in "Wireform.Hash" to
pure-Haskell reference implementations.

Run with:

@
cabal bench wireform-iceberg:iceberg-bench
@
-}
module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf, whnf)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32)
import Iceberg.DeletionVector qualified as DV
import Iceberg.Murmur3 qualified as M
import PureRef qualified as Pure
import Wireform.Hash qualified as Hash


deterministic :: Int -> BS.ByteString
deterministic n = BS.pack (take n (cycle [0 .. 255]))


-- A 1024-position deletion vector, the kind a real Iceberg position-delete
-- file produces for a single row group.
sampleDV :: DV.DeletionVector
sampleDV = DV.addPositions [fromIntegral i :: Int64 | i <- [(0 :: Int), 4 .. 4000]] DV.emptyDV


sampleDVBytes :: BS.ByteString
sampleDVBytes = DV.encodeDV sampleDV


main :: IO ()
main =
  defaultMain
    [ bgroup
        "Murmur3 32-bit"
        [ env (pure (deterministic 8)) $ \bs -> bgroup "8 B" (mh bs)
        , env (pure (deterministic 64)) $ \bs -> bgroup "64 B" (mh bs)
        , env (pure (deterministic 1024)) $ \bs -> bgroup "1 KiB" (mh bs)
        , env (pure (deterministic 65536)) $ \bs -> bgroup "64 KiB" (mh bs)
        ]
    , bgroup
        "bucket[16] long"
        [ bench "C" $ whnf (M.bucketLong 16) (34 :: Int64)
        , bench "pure" $ whnf (Pure.bucketLong_pure 16) (34 :: Int64)
        ]
    , bgroup
        "XXH64 (always C-backed)"
        [ env (pure (deterministic 64)) $ \bs -> bench "64 B" (whnf (Hash.xxh64 0) bs)
        , env (pure (deterministic 1024)) $ \bs -> bench "1 KiB" (whnf (Hash.xxh64 0) bs)
        , env (pure (deterministic 65536)) $ \bs -> bench "64 KiB" (whnf (Hash.xxh64 0) bs)
        ]
    , bgroup
        "Deletion vector decode (1001 positions)"
        [ bench "C" $ nf DV.decodeDV sampleDVBytes
        , bench "pure" $ nf Pure.decodeDV_pure sampleDVBytes
        ]
    , bgroup
        "Deletion vector contains"
        [ env (pure sampleDV) $ \dv ->
            bgroup
              ""
              [ bench "C" $ whnf (\p -> DV.containsPosition p dv) (1024 :: Int64)
              , bench "pure" $
                  whnf
                    ( \p -> case DV.deletedPositions dv of
                        xs -> p `elem` xs
                    )
                    (1024 :: Int64)
              ]
        ]
    , bgroup
        "Roaring ARRAY container (always C-backed)"
        [ env (pure (VS.fromList [fromIntegral i :: Word16 | i <- [(0 :: Int), 2 .. 2000]])) $ \lows ->
            bgroup
              ""
              [ bench "encode" $ whnf Hash.roaringEncodeArray lows
              , let payload = Hash.roaringEncodeArray lows
                in bench "decode" $
                     nf
                       (Hash.roaringDecodeArray payload (VS.length lows))
                       (0 :: Word32)
              , let payload = Hash.roaringEncodeArray lows
                in bench "contains hit" $
                     whnf
                       (\v -> Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) v)
                       (1000 :: Word16)
              , let payload = Hash.roaringEncodeArray lows
                in bench "contains miss" $
                     whnf
                       (\v -> Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) v)
                       (3 :: Word16)
              ]
        ]
    , bgroup
        "Roaring BITSET container (always C-backed)"
        [ env (pure (VS.fromList [fromIntegral i :: Word16 | i <- [(0 :: Int), 1 .. 30000]])) $ \lows ->
            bgroup
              ""
              [ bench "encode" $ whnf Hash.roaringEncodeBitset lows
              , let payload = Hash.roaringEncodeBitset lows
                in bench "decode" $ nf (Hash.roaringDecodeBitset payload) (0 :: Word32)
              , let payload = Hash.roaringEncodeBitset lows
                in bench "contains hit" $
                     whnf
                       (\v -> Hash.roaringContains Hash.BitsetContainer payload 0 v)
                       (12345 :: Word16)
              ]
        ]
    ]
  where
    mh bs =
      [ bench "C" $ whnf M.murmur3_32 bs
      , bench "pure" $ whnf Pure.murmur3_32_pure bs
      ]
