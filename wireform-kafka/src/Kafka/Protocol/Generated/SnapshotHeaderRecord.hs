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
    encodeSnapshotHeaderRecord,
    decodeSnapshotHeaderRecord,
    maxSnapshotHeaderRecordVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E




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

-- | Encode SnapshotHeaderRecord with the given API version.
encodeSnapshotHeaderRecord :: MonadPut m => E.ApiVersion -> SnapshotHeaderRecord -> m ()
encodeSnapshotHeaderRecord version msg
  | version == 0 =
    do
      serialize (snapshotHeaderRecordVersion msg)
      serialize (snapshotHeaderRecordLastContainedLogTimestamp msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SnapshotHeaderRecord with the given API version.
decodeSnapshotHeaderRecord :: MonadGet m => E.ApiVersion -> m SnapshotHeaderRecord
decodeSnapshotHeaderRecord version
  | version == 0 =
    do
      fieldversion <- deserialize
      fieldlastcontainedlogtimestamp <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SnapshotHeaderRecord
        {
        snapshotHeaderRecordVersion = fieldversion
        ,
        snapshotHeaderRecordLastContainedLogTimestamp = fieldlastcontainedlogtimestamp
        }
  | otherwise = fail $ "Unsupported version: " ++ show version