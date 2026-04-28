{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import System.Exit (exitFailure)

import Parquet.BloomFilter
import Parquet.PageIndex
import Parquet.Types
import Parquet.XXH64

main :: IO ()
main = do
  -- XXH64 reference vectors.
  expectHash "" "ef46db3751d8e999"
  expectHash "abc" "44bc2cf5ad770999"
  expectHash "Nobody inspects the spammish repetition" "fbcea83c8a378bf1"
  -- 32 bytes of 'a' is exactly one bulk stripe (xxhsum -H1 reference).
  expectHashBs (BS.replicate 32 0x61) "856e843298f99ad7"
  -- 64 bytes (two stripes) — exercises the bulk-phase merge.
  expectHashBs (BS.replicate 64 0x62) "ecbaf4bdf26b6349"

  -- OffsetIndex round-trip.
  let oi = OffsetIndex
        { oiPageLocations = V.fromList
            [ PageLocation 100 200 0
            , PageLocation 300 250 50
            ]
        , oiUnencodedByteArrayDataBytes = Just (V.fromList [42, 99])
        }
  expect "OffsetIndex round-trip"
    (decodeOffsetIndex (encodeOffsetIndex oi) == Right oi)

  -- ColumnIndex round-trip with all optional fields.
  let ci = ColumnIndex
        { ciNullPages = V.fromList [False, False, True]
        , ciMinValues = V.fromList [BSC.pack "a", BSC.pack "b", BS.empty]
        , ciMaxValues = V.fromList [BSC.pack "z", BSC.pack "y", BS.empty]
        , ciBoundaryOrder = OrderAscending
        , ciNullCounts = Just (V.fromList [0, 0, 100])
        , ciRepetitionLevelHistograms = Just (V.fromList [10, 5])
        , ciDefinitionLevelHistograms = Just (V.fromList [3, 8])
        }
  expect "ColumnIndex round-trip"
    (decodeColumnIndex (encodeColumnIndex ci) == Right ci)

  -- Bloom filter membership.
  let sbbf0 = newSbbf 1024
      values = ["alpha", "beta", "gamma", "delta", "epsilon"]
      sbbf  = foldr (sbbfInsert . BSC.pack) sbbf0 values
  mapM_ (\v -> expect ("bloom contains " ++ v)
                 (sbbfCheck (BSC.pack v) sbbf)) values

  -- Golden vector from arrow-rs / parquet-mr: a 32-byte bitset produced
  -- by parquet-mr for the strings "a0".."a9" must report all of them
  -- present.  This proves byte-compatibility of our XXH64 + block layout
  -- with the reference writer.
  let goldenBits = BS.pack
        [ 200, 1, 80, 20, 64, 68, 8, 109, 6, 37, 4, 67, 144, 80, 96, 32
        , 8, 132, 43, 33, 0, 5, 99, 65, 2, 0, 224, 44, 64, 78, 96, 4 ]
      goldenSbbf = newSbbfFromBytes goldenBits
  mapM_ (\i -> let v = "a" <> show i in
                 expect ("golden contains " ++ v)
                   (sbbfCheck (BSC.pack v) goldenSbbf))
        [(0 :: Int) .. 9]

  -- Bloom filter false-positive sanity.
  let sbbfBig0 = newSbbf 2048
      inserted = map (BSC.pack . ("inserted-" <>) . show) [0 .. 255 :: Int]
      probes   = map (BSC.pack . ("probe-" <>) . show)   [0 .. 255 :: Int]
      sbbfBig  = foldr sbbfInsert sbbfBig0 inserted
      fp = length (filter (`sbbfCheck` sbbfBig) probes)
  expect ("bloom FP rate (got " ++ show fp ++ ")") (fp <= 16)

  -- Bloom encode/decode round-trip.
  let bs = encodeBloomFilter sbbf
  case decodeBloomFilter bs of
    Left e -> failTest ("decodeBloomFilter: " ++ e)
    Right (_hdr, sbbf') -> do
      expect "decoded numBytes" (sbbfNumBytes sbbf' == sbbfNumBytes sbbf)
      mapM_ (\v -> expect ("decoded contains " ++ v)
                     (sbbfCheck (BSC.pack v) sbbf')) values

  putStrLn "All Parquet page-index / bloom-filter tests passed."

expectHash :: String -> String -> IO ()
expectHash s expected = expectHashBs (BSC.pack s) expected

expectHashBs :: BS.ByteString -> String -> IO ()
expectHashBs bs expected =
  let actual = pad16 (showHex (xxh64 bs) "")
  in unless (actual == expected) $
       failTest ("xxh64 " ++ show bs ++ " expected " ++ expected
                  ++ " got " ++ actual)

pad16 :: String -> String
pad16 s = replicate (16 - length s) '0' ++ s

expect :: String -> Bool -> IO ()
expect what ok = do
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
