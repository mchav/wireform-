{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0017.Compression
Description : librdkafka @tests\/0017-compression.c@

librdkafka's @0017-compression@ produces and consumes a small
record set with each compression codec (none, gzip, snappy, lz4,
zstd) and asserts the consumed bytes match what was produced.

We test the same property at the codec layer (round-trip every
codec on a representative payload) without needing a broker. The
broker-side variant lives in our integration suite and runs when
@WIREFORM_KAFKA_BROKER@ is set.
-}
module Conformance.T0017.Compression (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Compression.Types as C

tests :: TestTree
tests = testGroup "0017-compression"
  [ codecRoundTrip C.NoCompression "NoCompression"
  , codecRoundTrip C.Gzip          "Gzip"
  , codecRoundTrip C.Snappy        "Snappy"
  , codecRoundTrip C.Lz4           "Lz4"
  , codecRoundTrip C.Zstd          "Zstd"
  , testGroup "edge cases"
      [ allEmpty
      , singleByte
      , largeBlob
      ]
  ]

codecRoundTrip :: C.CompressionCodec -> String -> TestTree
codecRoundTrip codec name = testCase ("round-trip " <> name) $ do
  let input = "Hello, librdkafka conformance: 0017-compression!"
  result <- roundTrip codec input
  result @?= Right input

allEmpty :: TestTree
allEmpty = testCase "empty input round-trips for every codec" $
  forEachCodec $ \codec -> do
    r <- roundTrip codec BS.empty
    r @?= Right BS.empty

singleByte :: TestTree
singleByte = testCase "single byte round-trips for every codec" $
  forEachCodec $ \codec -> do
    let bs = BS.singleton 0xab
    r <- roundTrip codec bs
    r @?= Right bs

largeBlob :: TestTree
largeBlob = testCase "10 KiB blob round-trips for every codec" $
  forEachCodec $ \codec -> do
    let bs = BS.replicate (10 * 1024) 0x41
    r <- roundTrip codec bs
    r @?= Right bs

forEachCodec :: (C.CompressionCodec -> IO ()) -> IO ()
forEachCodec k = mapM_ k
  [ C.NoCompression
  , C.Gzip
  , C.Snappy
  , C.Lz4
  , C.Zstd
  ]

roundTrip :: C.CompressionCodec -> ByteString -> IO (Either String ByteString)
roundTrip codec input = do
  cR <- C.compress codec input
  case cR of
    Left err -> pure (Left ("compress: " <> err))
    Right bs -> C.decompress codec bs
