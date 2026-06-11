{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Benchmark: sendBuilder (via ByteString) vs sendBuilderDirect (ring sink).

Uses an in-memory send transport backed by a magic ring so the
benchmark isolates builder-to-ring staging cost without any I/O.
-}
module Main where

import Criterion.Main
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word
import Foreign.Ptr (Ptr)
import Wireform.Builder
import Wireform.Ring.Internal (ringBase, ringMask, ringSize, withMagicRing)
import Wireform.Transport.Send


------------------------------------------------------------------------
-- In-memory send transport (no I/O, immediate drain)
------------------------------------------------------------------------

{- | Build a 'SendTransport' over a magic ring that acts as an
instant drain: every 'sendPublishHead' immediately advances the
tail to match, so the ring never fills.  This isolates the
builder staging cost.
-}
mkMemorySendTransport
  :: Ptr Word8
  -> Int
  -> Int
  -> IO SendTransport
mkMemorySendTransport base sz msk = do
  headRef <- newIORef (0 :: Word64)
  tailRef <- newIORef (0 :: Word64)
  let publish h = do
        writeIORef headRef h
        writeIORef tailRef h
  pure
    SendTransport
      { sendRingBase = base
      , sendRingSize = sz
      , sendRingMask = msk
      , sendLoadTail = readIORef tailRef
      , sendLoadHead = readIORef headRef
      , sendPublishHead = publish
      , sendWaitSpace = \_ -> do
          tl <- readIORef tailRef
          pure (SendSpaceAvailable tl)
      , sendFlush = pure ()
      , sendShutdownWrite = pure ()
      , sendClose = pure ()
      }


------------------------------------------------------------------------
-- Builders of various sizes
------------------------------------------------------------------------

-- 64-byte payload: small protocol header / Kafka produce ack
smallBuilder :: Builder
smallBuilder =
  mconcat
    [ word32BE 0x00000001
    , word32BE 0x00000040
    , byteStringCopy (BS.replicate 56 0xAA)
    ]


-- 256-byte payload: typical Kafka produce record
mediumBuilder :: Builder
mediumBuilder =
  mconcat
    [ word32BE 0x00000002
    , word32BE 0x00000100
    , byteStringCopy (BS.replicate 248 0xBB)
    ]


-- 1 KiB payload: HTTP response header block
kiloBuilder :: Builder
kiloBuilder =
  mconcat
    [ word32BE 0x00000003
    , word32BE 0x00000400
    , byteStringCopy (BS.replicate 1016 0xCC)
    ]


-- 4 KiB payload: typical HTTP response body chunk
fourKBuilder :: Builder
fourKBuilder =
  mconcat
    [ word32BE 0x00000004
    , word32BE 0x00001000
    , byteStringCopy (BS.replicate 4088 0xDD)
    ]


-- 16 KiB payload: large message, exercises ring overflow
sixteenKBuilder :: Builder
sixteenKBuilder =
  mconcat
    [ word32BE 0x00000005
    , word32BE 0x00004000
    , byteStringCopy (BS.replicate 16376 0xEE)
    ]


-- 64 KiB payload: stresses ring chunking
sixtyFourKBuilder :: Builder
sixtyFourKBuilder =
  mconcat
    [ word32BE 0x00000006
    , word32BE 0x00010000
    , byteStringCopy (BS.replicate 65528 0xFF)
    ]


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main =
  withMagicRing (256 * 1024) $ \ring -> do
    let !base = ringBase ring
        !sz = ringSize ring
        !msk = ringMask ring
    t <- mkMemorySendTransport base sz msk

    let benchPair name builder =
          bgroup
            name
            [ bench "direct (RingSink)" $ nfIO (sendBuilderDirect t builder)
            , bench "via ByteString (old)" $ nfIO (sendBuilderViaByteString t builder)
            ]

    defaultMain
      [ benchPair "64 B" smallBuilder
      , benchPair "256 B" mediumBuilder
      , benchPair "1 KiB" kiloBuilder
      , benchPair "4 KiB" fourKBuilder
      , benchPair "16 KiB" sixteenKBuilder
      , benchPair "64 KiB" sixtyFourKBuilder
      ]
