{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Round-trip + interop tests for the pointer-input
'Kafka.Compression.Ring.decompressFromPtr' entry points.  Compress
a payload through the legacy 'BS'-based API, hand the resulting
bytes to the ring-based decompressor as a raw pointer, and
verify we get the original payload back.
-}
module Compression.RingSpec (ringCompressionTests) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Kafka.Compression.Ring qualified as Ring
import Kafka.Compression.Types (CompressionCodec (..))
import Kafka.Compression.Types qualified as Compression
import Test.Syd
import Wireform.Ring (ringBase, withMagicRing)
import Wireform.Ring qualified


ringCompressionTests :: Spec
ringCompressionTests =
  describe "Compression.Ring (pointer-input decompress)" $
    sequence_
      [ describe "decompressFromPtr (fresh ByteString output)" $
          sequence_
            [ roundTrip "NoCompression" NoCompression sampleSmall
            , roundTrip "NoCompression / 64 KiB" NoCompression sampleBig
            , roundTrip "Snappy / small" Snappy sampleSmall
            , roundTrip "Snappy / 64 KiB" Snappy sampleBig
            , roundTrip "Snappy / 1 MiB" Snappy sampleMega
            , roundTrip "Zstd / small" Zstd sampleSmall
            , roundTrip "Zstd / 64 KiB" Zstd sampleBig
            , roundTrip "Zstd / 1 MiB" Zstd sampleMega
            , roundTrip "Gzip / small" Gzip sampleSmall
            , roundTrip "Gzip / 64 KiB" Gzip sampleBig
            , roundTrip "Gzip / 1 MiB" Gzip sampleMega
            , roundTrip "Lz4  / small" Lz4 sampleSmall
            , roundTrip "Lz4  / 64 KiB" Lz4 sampleBig
            , roundTrip "Lz4  / 1 MiB" Lz4 sampleMega
            , it "empty input -> empty output (all codecs)" $
                mapM_ (\c -> roundtripBs c BS.empty) [NoCompression, Gzip, Snappy, Lz4, Zstd]
            ]
      , describe "decompressIntoRing (caller-supplied magic ring)" $
          sequence_
            [ ringRoundTrip "NoCompression / 64 KiB" NoCompression sampleBig
            , ringRoundTrip "Snappy / 64 KiB" Snappy sampleBig
            , ringRoundTrip "Snappy / 1 MiB" Snappy sampleMega
            , ringRoundTrip "Zstd   / 64 KiB" Zstd sampleBig
            , ringRoundTrip "Zstd   / 1 MiB" Zstd sampleMega
            , ringRoundTrip "Gzip   / 64 KiB" Gzip sampleBig
            , ringRoundTrip "Gzip   / 1 MiB" Gzip sampleMega
            , ringRoundTrip "Lz4    / 64 KiB" Lz4 sampleBig
            , ringRoundTrip "Lz4    / 1 MiB" Lz4 sampleMega
            , it "snappy reuses the same ring across two decompressions" $ do
                compE1 <- Compression.compress Snappy sampleSmall
                compE2 <- Compression.compress Snappy sampleBig
                case (compE1, compE2) of
                  (Right c1, Right c2) ->
                    withMagicRing (4 * 1024 * 1024) $ \dst -> do
                      r1 <- decompressViaRing Snappy c1 dst
                      r1 `shouldBe` Right sampleSmall
                      r2 <- decompressViaRing Snappy c2 dst
                      r2 `shouldBe` Right sampleBig
                  _ -> expectationFailure "compress failed"
            ]
      ]


{- | Compress @payload@ through the legacy BS-based API, then
decompress the resulting bytes via the ring entry point that
takes a raw pointer (matching what the magic-ring transport
hands to 'Kafka.Protocol.RecordBatchWire').
-}
roundTrip :: String -> CompressionCodec -> BS.ByteString -> Spec
roundTrip label codec payload = it label (roundtripBs codec payload)


roundtripBs :: CompressionCodec -> BS.ByteString -> IO ()
roundtripBs codec payload = do
  compE <- Compression.compress codec payload
  case compE of
    Left err -> expectationFailure ("compress failed: " <> err)
    Right compressed -> do
      decompE <- BSU.unsafeUseAsCStringLen compressed $ \(p, l) ->
        Ring.decompressFromPtr codec (castPtr p) l
      case decompE of
        Left err -> expectationFailure ("decompressFromPtr failed: " <> err)
        Right got -> got `shouldBe` payload


sampleSmall :: BS.ByteString
sampleSmall = "the quick brown fox jumps over the lazy dog"


sampleBig :: BS.ByteString
sampleBig = BS.concat (replicate 1024 sampleSmall <> ["padding"])


sampleMega :: BS.ByteString
sampleMega = BS.concat (replicate (16 * 1024) sampleSmall)


------------------------------------------------------------------------
-- decompressIntoRing helpers
------------------------------------------------------------------------

ringRoundTrip :: String -> CompressionCodec -> BS.ByteString -> Spec
ringRoundTrip label codec payload = it label $ do
  compE <- Compression.compress codec payload
  case compE of
    Left err -> expectationFailure ("compress failed: " <> err)
    Right compressed ->
      withMagicRing (4 * 1024 * 1024) $ \dst -> do
        r <- decompressViaRing codec compressed dst
        r `shouldBe` Right payload


{- | Convenience: drive 'Ring.decompressIntoRing' against the given
magic ring and snapshot the result as a fresh 'BS.ByteString' so
the test can compare it to the expected payload.  In production
callers walk the ring slice directly via 'ringBase'+length without
the snapshot copy.
-}
decompressViaRing
  :: CompressionCodec
  -> BS.ByteString
  -> Wireform.Ring.MagicRing s
  -> IO (Either String BS.ByteString)
decompressViaRing codec compressed dst = do
  r <- BSU.unsafeUseAsCStringLen compressed $ \(p, l) ->
    Ring.decompressIntoRing codec (castPtr p) l dst
  case r of
    Left e -> pure (Left (show e))
    Right produced -> do
      let !base = ringBase dst
      -- Snapshot the ring slice so the test result lives past the
      -- next 'decompressIntoRing' call that might overwrite the
      -- ring's bytes.  Production callers consume the slice
      -- in-place before reusing the ring.
      fp <- BSI.mallocByteString produced
      withForeignPtr fp $ \destBuf -> BSI.memcpy destBuf base produced
      pure (Right (BSI.fromForeignPtr fp 0 produced))
