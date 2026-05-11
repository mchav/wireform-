{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Kafka.Streams.Serde.Windowed
-- Description : Composite Serde for 'WindowedKey'
--
-- Mirrors Java's @TimeWindowedSerializer\/Deserializer@. Encodes a
-- 'WindowedKey' as
--
-- @
-- <keyLen :: Word32 BE> <key bytes> <windowStart :: Int64 BE>
-- @
module Kafka.Streams.Serde.Windowed
  ( windowedSerde
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftR, shiftL, (.|.))
import Data.Word (Word32)
import Data.Int (Int64)

import Kafka.Streams.Serde
  ( Serde (..)
  , int64Serde
  )
import Kafka.Streams.State.Store (WindowedKey (..))
import Kafka.Streams.Time (Timestamp (..))

windowedSerde :: forall k. Serde k -> Serde (WindowedKey k)
windowedSerde inner = Serde
  { serialize = \(WindowedKey k (Timestamp ts)) ->
      let !kBytes = serialize inner k
          !kLen   = fromIntegral (BS.length kBytes) :: Word32
          !lenBs  = BL.toStrict (BB.toLazyByteString (BB.word32BE kLen))
          !tsBs   = serialize int64Serde ts
       in BS.concat [lenBs, kBytes, tsBs]
  , deserialize = \b ->
      if BS.length b < 4 + 8
        then Left "windowedSerde: payload too short"
        else
          let header = BS.take 4 b
              hi a   = fromIntegral (BS.index header a) :: Word32
              !kLen  = (hi 0 `shiftL` 24)
                   .|. (hi 1 `shiftL` 16)
                   .|. (hi 2 `shiftL` 8)
                   .|.  hi 3
              !kStart  = 4
              !kEnd    = kStart + fromIntegral kLen
              !tsStart = kEnd
              !tsEnd   = tsStart + 8
           in if BS.length b /= tsEnd
                then Left
                  $ "windowedSerde: expected "
                    <> show tsEnd <> " bytes, got "
                    <> show (BS.length b)
                else do
                  let !kBytes = BS.take (fromIntegral kLen) (BS.drop kStart b)
                      !tsBs   = BS.take 8 (BS.drop tsStart b)
                  k  <- deserialize inner kBytes
                  ts <- deserialize int64Serde tsBs
                  Right (WindowedKey k (Timestamp ts))
  }
