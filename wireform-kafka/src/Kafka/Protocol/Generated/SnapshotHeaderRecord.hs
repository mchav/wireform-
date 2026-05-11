{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SnapshotHeaderRecord
Description : Kafka SnapshotHeaderRecord message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SnapshotHeaderRecord
  (
    SnapshotHeaderRecord(..),
    maxSnapshotHeaderRecordVersion
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




data SnapshotHeaderRecord = SnapshotHeaderRecord
  {

  -- | The version of the snapshot header record.

  -- Versions: 0+
  snapshotHeaderRecordVersion :: !(Int16)
,

  -- | The append time of the last record from the log contained in this snapshot.

  -- Versions: 0+
  snapshotHeaderRecordLastContainedLogTimestamp :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SnapshotHeaderRecord.
maxSnapshotHeaderRecordVersion :: Int16
maxSnapshotHeaderRecordVersion = 0




-- | Worst-case wire size of a SnapshotHeaderRecord.
wireMaxSizeSnapshotHeaderRecord :: Int -> SnapshotHeaderRecord -> Int
wireMaxSizeSnapshotHeaderRecord _version msg =
  0
  + 2
  + 8
  + 1

-- | Direct-poke encoder for SnapshotHeaderRecord.
wirePokeSnapshotHeaderRecord :: Int -> Ptr Word8 -> SnapshotHeaderRecord -> IO (Ptr Word8)
wirePokeSnapshotHeaderRecord version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (snapshotHeaderRecordVersion msg)
    p2 <- W.pokeInt64BE p1 (snapshotHeaderRecordLastContainedLogTimestamp msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke SnapshotHeaderRecord : unsupported version: " ++ show version

-- | Direct-poke decoder for SnapshotHeaderRecord.
wirePeekSnapshotHeaderRecord :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SnapshotHeaderRecord, Ptr Word8)
wirePeekSnapshotHeaderRecord version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_version, p1) <- W.peekInt16BE p0 endPtr
    (f1_lastcontainedlogtimestamp, p2) <- W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (SnapshotHeaderRecord { snapshotHeaderRecordVersion = f0_version, snapshotHeaderRecordLastContainedLogTimestamp = f1_lastcontainedlogtimestamp }, pTagsEnd)
  | otherwise = error $ "wirePeek SnapshotHeaderRecord : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec SnapshotHeaderRecord where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSnapshotHeaderRecord (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSnapshotHeaderRecord (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSnapshotHeaderRecord (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}