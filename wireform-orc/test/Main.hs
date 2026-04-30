{-# LANGUAGE OverloadedStrings #-}
-- | Round-trip tests for the new ORC date / timestamp / decimal writers
-- against the existing column readers.
module Main (main) where

import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Exit (exitFailure)

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
