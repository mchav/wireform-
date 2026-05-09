{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeQuorumRequest
Description : Kafka DescribeQuorumRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 55.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeQuorumRequest
  (
    DescribeQuorumRequest(..),
    TopicData(..),
    PartitionData(..),
    encodeDescribeQuorumRequest,
    decodeDescribeQuorumRequest,
    maxDescribeQuorumRequestVersion
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
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The partitions to describe.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      }


-- | The topics to describe.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partitions to describe.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }



data DescribeQuorumRequest = DescribeQuorumRequest
  {

  -- | The topics to describe.

  -- Versions: 0+
  describeQuorumRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeQuorumRequest.
maxDescribeQuorumRequestVersion :: Int16
maxDescribeQuorumRequestVersion = 2

-- | Encode DescribeQuorumRequest with the given API version.
encodeDescribeQuorumRequest :: MonadPut m => E.ApiVersion -> DescribeQuorumRequest -> m ()
encodeDescribeQuorumRequest version msg
  | version >= 0 && version <= 2 =
    do
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (describeQuorumRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeQuorumRequest with the given API version.
decodeDescribeQuorumRequest :: MonadGet m => E.ApiVersion -> m DescribeQuorumRequest
decodeDescribeQuorumRequest version
  | version >= 0 && version <= 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeQuorumRequest
        {
        describeQuorumRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeQuorumRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
