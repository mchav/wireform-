{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SnapshotFooterRecord
Description : Kafka SnapshotFooterRecord message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SnapshotFooterRecord
  (
    SnapshotFooterRecord(..),
    maxSnapshotFooterRecordVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data SnapshotFooterRecord = SnapshotFooterRecord
  {

  -- | The version of the snapshot footer record.

  -- Versions: 0+
  snapshotFooterRecordVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SnapshotFooterRecord.
maxSnapshotFooterRecordVersion :: Int16
maxSnapshotFooterRecordVersion = 0




-- | Worst-case wire size of a SnapshotFooterRecord.
wireMaxSizeSnapshotFooterRecord :: Int -> SnapshotFooterRecord -> Int
wireMaxSizeSnapshotFooterRecord _version msg =
  0
  + 2
  + 1

-- | Direct-poke encoder for SnapshotFooterRecord.
wirePokeSnapshotFooterRecord :: Int -> Ptr Word8 -> SnapshotFooterRecord -> IO (Ptr Word8)
wirePokeSnapshotFooterRecord version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (snapshotFooterRecordVersion msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke SnapshotFooterRecord : unsupported version: " ++ show version

-- | Direct-poke decoder for SnapshotFooterRecord.
wirePeekSnapshotFooterRecord :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SnapshotFooterRecord, Ptr Word8)
wirePeekSnapshotFooterRecord version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_version, p1) <- W.peekInt16BE p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (SnapshotFooterRecord { snapshotFooterRecordVersion = f0_version }, pTagsEnd)
  | otherwise = error $ "wirePeek SnapshotFooterRecord : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec SnapshotFooterRecord where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSnapshotFooterRecord (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSnapshotFooterRecord (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSnapshotFooterRecord (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}