{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterShareGroupOffsetsRequest
Description : Kafka AlterShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 91.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterShareGroupOffsetsRequest
  (
    AlterShareGroupOffsetsRequest(..),
    AlterShareGroupOffsetsRequestTopic(..),
    AlterShareGroupOffsetsRequestPartition(..),
    encodeAlterShareGroupOffsetsRequest,
    decodeAlterShareGroupOffsetsRequest,
    maxAlterShareGroupOffsetsRequestVersion
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


-- | Each partition to alter offsets for.
data AlterShareGroupOffsetsRequestPartition = AlterShareGroupOffsetsRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  alterShareGroupOffsetsRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  alterShareGroupOffsetsRequestPartitionStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsRequestPartition with version-aware field handling.
encodeAlterShareGroupOffsetsRequestPartition :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequestPartition -> m ()
encodeAlterShareGroupOffsetsRequestPartition version amsg =
  do
    serialize (alterShareGroupOffsetsRequestPartitionPartitionIndex amsg)
    serialize (alterShareGroupOffsetsRequestPartitionStartOffset amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsRequestPartition with version-aware field handling.
decodeAlterShareGroupOffsetsRequestPartition :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequestPartition
decodeAlterShareGroupOffsetsRequestPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsRequestPartition
      {
      alterShareGroupOffsetsRequestPartitionPartitionIndex = fieldpartitionindex
      ,
      alterShareGroupOffsetsRequestPartitionStartOffset = fieldstartoffset
      }


-- | The topics to alter offsets for.
data AlterShareGroupOffsetsRequestTopic = AlterShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)
,

  -- | Each partition to alter offsets for.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopicPartitions :: !(KafkaArray (AlterShareGroupOffsetsRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsRequestTopic with version-aware field handling.
encodeAlterShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequestTopic -> m ()
encodeAlterShareGroupOffsetsRequestTopic version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsRequestTopicTopicName amsg)) else serialize (alterShareGroupOffsetsRequestTopicTopicName amsg)
    E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsRequestPartition (case P.unKafkaArray (alterShareGroupOffsetsRequestTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsRequestTopic with version-aware field handling.
decodeAlterShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequestTopic
decodeAlterShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsRequestPartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsRequestTopic
      {
      alterShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      ,
      alterShareGroupOffsetsRequestTopicPartitions = fieldpartitions
      }



data AlterShareGroupOffsetsRequest = AlterShareGroupOffsetsRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  alterShareGroupOffsetsRequestGroupId :: !(KafkaString)
,

  -- | The topics to alter offsets for.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopics :: !(KafkaArray (AlterShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterShareGroupOffsetsRequest.
maxAlterShareGroupOffsetsRequestVersion :: Int16
maxAlterShareGroupOffsetsRequestVersion = 0

-- | Encode AlterShareGroupOffsetsRequest with the given API version.
encodeAlterShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequest -> m ()
encodeAlterShareGroupOffsetsRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (alterShareGroupOffsetsRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsRequestTopic (case P.unKafkaArray (alterShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterShareGroupOffsetsRequest with the given API version.
decodeAlterShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequest
decodeAlterShareGroupOffsetsRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterShareGroupOffsetsRequest
        {
        alterShareGroupOffsetsRequestGroupId = fieldgroupid
        ,
        alterShareGroupOffsetsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAlterShareGroupOffsetsRequest' / 'decodeAlterShareGroupOffsetsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AlterShareGroupOffsetsRequest where
  wireCodec = Just (WC.serialShimCodec encodeAlterShareGroupOffsetsRequest decodeAlterShareGroupOffsetsRequest)
  {-# INLINE wireCodec #-}
