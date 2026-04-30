{-# LANGUAGE OverloadedStrings #-}
-- | Round-trip tests for the new ORC date / timestamp / decimal writers
-- against the existing column readers.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Exit (exitFailure)

import qualified ORC.BloomFilter as BF
import qualified ORC.Read
import qualified ORC.RowIndex as RI
import qualified ORC.Write
import ORC.Read (ORCTimestamp (..), decodeDateColumn, decodeDecimalColumn, decodeTimestampColumn)
import ORC.Write
  ( encodeDateColumn
  , encodeDecimalColumn
  , encodeORCNano
  , encodeTimestampColumn
  )

main :: IO ()
main = do
  let !dateVals = VP.fromList [0 :: Int32, 1, -1, 365, 18000, -100]
      !dateBs   = encodeDateColumn dateVals
  case decodeDateColumn (VP.length dateVals) dateBs Nothing of
    Right vs -> do
      let actual = map (\x -> case x of Just v -> v; Nothing -> error "null") (V.toList vs)
      expect "date round-trip" (actual == VP.toList dateVals)
    Left e -> failTest ("decodeDateColumn: " ++ e)

  -- Timestamp: a few values with different trailing-zero shapes.
  let !secs  = VP.fromList [0 :: Int64, 1700_000_000, -1, 12345]
      !nanos = VP.fromList [0 :: Int64, 500_000_000, 123, 999_999_999]
      (!secBs, !nanoBs) = encodeTimestampColumn secs nanos
  case decodeTimestampColumn (VP.length secs) secBs nanoBs Nothing of
    Right vs ->
      let pairs = zip (VP.toList secs) (VP.toList nanos)
          actual = [ (s, n) | Just (ORCTimestamp s n) <- V.toList vs ]
       in expect "timestamp round-trip" (actual == pairs)
    Left e -> failTest ("decodeTimestampColumn: " ++ e)

  -- Decimal64: just the unscaled integers.
  let !dec = VP.fromList [123 :: Int64, -456, 0, 999_999_999_999_999]
      !decBs = encodeDecimalColumn dec
  case decodeDecimalColumn (VP.length dec) 4 decBs Nothing of
    Right vs ->
      let actual = [ v | Just v <- V.toList vs ]
       in expect "decimal64 round-trip" (actual == VP.toList dec)
    Left e -> failTest ("decodeDecimalColumn: " ++ e)

  -- encodeORCNano: 500_000_000 has 8 trailing zeros; clamped to 7 by spec.
  expect "encodeORCNano(0) == 0"   (encodeORCNano 0 == 0)
  expect "encodeORCNano(123) == 123 << 3"
    (encodeORCNano 123 == (123 * 8))
  expect "encodeORCNano clamps trailing-zero scale at 7"
    (let v = encodeORCNano 500_000_000
         scale = v `mod` 8
      in scale == 7)

  -- BLOOM_FILTER_UTF8 round-trip: insert -> contains for present
  -- and absent strings, plus integer membership.
  let bf0 = BF.emptyBloom 1000 0.01
      bf  = foldl (\acc s -> BF.insertString s acc) bf0 ["alpha", "beta", "gamma"]
  expect "bloom contains 'alpha'" (BF.containsString "alpha" bf)
  expect "bloom contains 'beta'"  (BF.containsString "beta"  bf)
  expect "bloom contains 'gamma'" (BF.containsString "gamma" bf)
  expect "bloom does NOT contain 'delta' (1% FPP, deterministic input)"
    (not (BF.containsString "delta" bf))

  let intBf = foldl (\acc i -> BF.insertInt64 i acc)
                    (BF.emptyBloom 1000 0.01) [1, 7, 42, 999, -3]
  expect "int bloom contains 42"   (BF.containsInt64 42 intBf)
  expect "int bloom does NOT contain 8000" (not (BF.containsInt64 8000 intBf))

  -- Wire encoding: BloomFilterIndex prefix is the proto length-delimited
  -- field 1; with two entries we should see two outer length prefixes.
  let twoEntries = BF.encodeBloomFilterIndex [bf, bf]
  expect "BloomFilterIndex non-empty" (BS.length twoEntries > 0)

  -- Row index: encode two entries, verify the stream is non-empty and
  -- starts with the expected protobuf field-1 tag (length-delimited).
  let rie1 = RI.RowIndexEntry [0, 0] BS.empty
      rie2 = RI.RowIndexEntry [123, 4567] (BS.singleton 0x42)
      idx  = RI.encodeRowIndex [rie1, rie2]
  expect "RowIndex non-empty"   (BS.length idx > 0)
  expect "RowIndex starts with field-1 length-delimited tag"
    (BS.head idx == 0x0A)

  -- DECIMAL128 round-trip via encodeDecimalRawColumn / decodeDecimal128Stream
  -- with values that overflow Int64 to exercise the Integer path.
  let bigPos =  10 ^ (30 :: Int) + 7              -- 30-digit positive
      bigNeg = -(10 ^ (35 :: Int) + 1)            -- 35-digit negative
      d128Vals = V.fromList ([0, 1, -1, 12345, bigPos, bigNeg] :: [Integer])
      (d128Data, _scaleStream) = ORC.Write.encodeDecimalRawColumn d128Vals 4
  case ORC.Read.decodeDecimal128Stream (V.length d128Vals) d128Data of
    Right vs -> expect "DECIMAL128 round-trip (incl. >Int64 magnitudes)"
                       (vs == d128Vals)
    Left  e  -> failTest ("decodeDecimal128Stream: " ++ e)

  putStrLn "All ORC writer tests passed."

expect :: String -> Bool -> IO ()
expect name True  = putStrLn ("OK: " ++ name)
expect name False = do
  putStrLn ("FAIL: " ++ name)
  exitFailure

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
