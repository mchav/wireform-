{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.Serde.Windowed
Description : Composite Serde for 'WindowedKey'

Mirrors Java's @TimeWindowedSerializer\/Deserializer@. Encodes a
'WindowedKey' as

@
<keyLen :: Word32 BE> <key bytes> <windowStart :: Int64 BE>
@
-}
module Kafka.Streams.Serde.Windowed (
  windowedSerde,
) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Word (Word32)
import Kafka.Streams.Serde (
  HasSerde (..),
  Serde (..),
  int64Serde,
 )
import Kafka.Streams.State.Store (WindowedKey (..))
import Kafka.Streams.Time (Timestamp (..))
import Wireform.Builder qualified as BB

-- | A 'WindowedKey' inherits the inner key's serde and composes
-- it with the standard window-start framing. This lives here
-- (rather than alongside 'WindowedKey' in
-- 'Kafka.Streams.State.Store') so it can reuse 'windowedSerde'
-- directly. It's an orphan instance in the strict sense, but
-- only between two types both owned by this library — no
-- downstream clash risk.
instance HasSerde k => HasSerde (WindowedKey k) where
  serde = windowedSerde serde


windowedSerde :: forall k. Serde k -> Serde (WindowedKey k)
windowedSerde inner =
  Serde
    { serialize = \(WindowedKey k (Timestamp ts)) ->
        let !kBytes = serialize inner k
            !kLen = fromIntegral (BS.length kBytes) :: Word32
            !lenBs = BL.toStrict (BB.toLazyByteString (BB.word32BE kLen))
            !tsBs = serialize int64Serde ts
        in BS.concat [lenBs, kBytes, tsBs]
    , deserialize = \b ->
        if BS.length b < 4 + 8
          then Left "windowedSerde: payload too short"
          else
            let header = BS.take 4 b
                hi a = fromIntegral (BS.index header a) :: Word32
                !kLen =
                  (hi 0 `shiftL` 24)
                    .|. (hi 1 `shiftL` 16)
                    .|. (hi 2 `shiftL` 8)
                    .|. hi 3
                !kStart = 4
                !kEnd = kStart + fromIntegral kLen
                !tsStart = kEnd
                !tsEnd = tsStart + 8
            in if BS.length b /= tsEnd
                then
                  Left $ T.pack $
                    "windowedSerde: expected "
                      <> show tsEnd
                      <> " bytes, got "
                      <> show (BS.length b)
                else do
                  let !kBytes = BS.take (fromIntegral kLen) (BS.drop kStart b)
                      !tsBs = BS.take 8 (BS.drop tsStart b)
                  k <- deserialize inner kBytes
                  ts <- deserialize int64Serde tsBs
                  Right (WindowedKey k (Timestamp ts))
    }
