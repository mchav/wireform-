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
import qualified ORC.Encryption as Enc
import qualified ORC.Read
import qualified ORC.RowIndex as RI
import qualified ORC.Write
import ORC.Read
  ( ORCTimestamp (..)
  , decodeDateColumn
  , decodeDecimalColumn
  , decodeStringColumn
  , decodeTimestampColumn
  )
import ORC.Write
  ( encodeDateColumn
  , encodeDecimalColumn
  , encodeORCNano
  , encodeStringDictColumn
  , encodeTimestampColumn
  )
import qualified Data.Text as T

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

  -- DICTIONARY_V2 string column round-trip. The writer emits three
  -- streams (DATA = per-row indices, LENGTH = dict entry lengths,
  -- DICTIONARY_DATA = raw UTF-8 bytes); decodeStringColumn auto-
  -- dispatches to the dictionary decoder when the dictionary stream
  -- is non-empty.
  let !dictInput = V.fromList
        (map T.pack ["alpha", "beta", "alpha", "gamma", "beta", "alpha"])
      (!dictData, !dictLen, !dictDictBytes) = encodeStringDictColumn dictInput
  case decodeStringColumn (V.length dictInput) dictData dictLen dictDictBytes Nothing of
    Right vs ->
      let actual = [ t | Just t <- V.toList vs ]
       in expect "DICTIONARY_V2 string round-trip" (actual == V.toList dictInput)
    Left e -> failTest ("decodeStringColumn DICTIONARY_V2: " ++ e)

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

  -- Encryption: AES-CTR encrypt/decrypt round-trip + per-stripe key
  -- derivation + protobuf encoders for the footer Encryption field.
  let key128 = BS.replicate 16 0x42
      stripeId = 7
      streamOff = 1024
      iv = Enc.deriveStreamIv stripeId streamOff
      plain = BS.pack [0..63]
  case Enc.aesCtrXor key128 iv plain of
    Left e -> failTest ("AES-CTR encrypt: " ++ e)
    Right ct -> do
      expect "AES-CTR ciphertext length matches plaintext"
        (BS.length ct == BS.length plain)
      expect "AES-CTR ciphertext differs from plaintext"
        (ct /= plain)
      case Enc.aesCtrXor key128 iv ct of
        Right pt -> expect "AES-CTR round-trip" (pt == plain)
        Left e   -> failTest ("AES-CTR decrypt: " ++ e)

  case Enc.encryptStripeKey key128 stripeId of
    Left e -> failTest ("encryptStripeKey: " ++ e)
    Right sk -> expect "encryptStripeKey returns a key the same size as the local key"
                       (BS.length sk == BS.length key128)

  -- Protobuf encoders: just check they produce non-empty byte
  -- strings starting with the right field tag for the first
  -- present field. Full byte-compat against orc-java is exercised
  -- by the round-trip writer; here we just ensure the messages
  -- serialize.
  let dm = Enc.DataMask "redact" ["full"] [3, 4, 5]
      ek = Enc.EncryptionKey "kms-master" 1 Enc.AES_CTR_128
      ev = Enc.EncryptionVariant 3 0 (BS.pack [1, 2, 3, 4])
      enc = Enc.Encryption [dm] [ek] [ev] Enc.ProviderHadoop
      encBs = Enc.encodeEncryption enc
  expect "encodeEncryption produces non-empty bytes"
    (BS.length encBs > 0)
  expect "encodeDataMask first byte is field-1 length-delimited"
    (BS.head (Enc.encodeDataMask dm) == 0x0A)
  expect "encodeEncryptionKey first field is name (tag 1)"
    (BS.head (Enc.encodeEncryptionKey ek) == 0x0A)
  expect "encodeEncryptionVariant first field is root (tag 1, varint)"
    (BS.head (Enc.encodeEncryptionVariant ev) == 0x08)

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
