{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip + interop tests for the pointer-input
-- 'Kafka.Compression.Ring.decompressFromPtr' entry points.  Compress
-- a payload through the legacy 'BS'-based API, hand the resulting
-- bytes to the ring-based decompressor as a raw pointer, and
-- verify we get the original payload back.
module Compression.RingSpec (ringCompressionTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (castPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

import qualified Kafka.Compression.Ring as Ring
import qualified Kafka.Compression.Types as Compression
import Kafka.Compression.Types (CompressionCodec (..))

ringCompressionTests :: TestTree
ringCompressionTests = testGroup "Compression.Ring (pointer-input decompress)"
  [ roundTrip "NoCompression" NoCompression sampleSmall
  , roundTrip "NoCompression / 64 KiB"   NoCompression sampleBig
  , roundTrip "Snappy / small"           Snappy        sampleSmall
  , roundTrip "Snappy / 64 KiB"          Snappy        sampleBig
  , roundTrip "Snappy / 1 MiB"           Snappy        sampleMega
  , roundTrip "Zstd / small"             Zstd          sampleSmall
  , roundTrip "Zstd / 64 KiB"            Zstd          sampleBig
  , roundTrip "Zstd / 1 MiB"             Zstd          sampleMega
  , roundTrip "Gzip / small"             Gzip          sampleSmall
  , roundTrip "Gzip / 64 KiB"            Gzip          sampleBig
  , roundTrip "Lz4  / small"             Lz4           sampleSmall
  , roundTrip "Lz4  / 64 KiB"            Lz4           sampleBig
  , testCase  "empty input -> empty output (all codecs)" $
      mapM_ (\c -> roundtripBs c BS.empty) [NoCompression, Gzip, Snappy, Lz4, Zstd]
  ]

-- | Compress @payload@ through the legacy BS-based API, then
-- decompress the resulting bytes via the ring entry point that
-- takes a raw pointer (matching what the magic-ring transport
-- hands to 'Kafka.Protocol.RecordBatchWire').
roundTrip :: String -> CompressionCodec -> BS.ByteString -> TestTree
roundTrip label codec payload = testCase label (roundtripBs codec payload)

roundtripBs :: CompressionCodec -> BS.ByteString -> IO ()
roundtripBs codec payload = do
  compE <- Compression.compress codec payload
  case compE of
    Left err -> assertFailure ("compress failed: " <> err)
    Right compressed -> do
      decompE <- BSU.unsafeUseAsCStringLen compressed $ \(p, l) ->
        Ring.decompressFromPtr codec (castPtr p) l
      case decompE of
        Left err -> assertFailure ("decompressFromPtr failed: " <> err)
        Right got -> got @?= payload

sampleSmall :: BS.ByteString
sampleSmall = "the quick brown fox jumps over the lazy dog"

sampleBig :: BS.ByteString
sampleBig = BS.concat (replicate 1024 sampleSmall <> ["padding"])

sampleMega :: BS.ByteString
sampleMega = BS.concat (replicate (16 * 1024) sampleSmall)
