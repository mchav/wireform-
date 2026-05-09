{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareAcknowledgeRequest
Description : Kafka ShareAcknowledgeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 79.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareAcknowledgeRequest
  (
    ShareAcknowledgeRequest(..),
    AcknowledgeTopic(..),
    AcknowledgePartition(..),
    AcknowledgementBatch(..),
    encodeShareAcknowledgeRequest,
    decodeShareAcknowledgeRequest,
    maxShareAcknowledgeRequestVersion
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


-- | Record batches to acknowledge.
data AcknowledgementBatch = AcknowledgementBatch
  {

  -- | First offset of batch of records to acknowledge.

  -- Versions: 0+
  acknowledgementBatchFirstOffset :: !(Int64)
,

  -- | Last offset (inclusive) of batch of records to acknowledge.

  -- Versions: 0+
  acknowledgementBatchLastOffset :: !(Int64)
,

  -- | Array of acknowledge types - 0:Gap,1:Accept,2:Release,3:Reject,4:Renew.

  -- Versions: 0+
  acknowledgementBatchAcknowledgeTypes :: !(KafkaArray (Int8))

  }
  deriving (Eq, Show, Generic)


-- | Encode AcknowledgementBatch with version-aware field handling.
encodeAcknowledgementBatch :: MonadPut m => E.ApiVersion -> AcknowledgementBatch -> m ()
encodeAcknowledgementBatch version amsg =
  do
    serialize (acknowledgementBatchFirstOffset amsg)
    serialize (acknowledgementBatchLastOffset amsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (acknowledgementBatchAcknowledgeTypes amsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int8"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AcknowledgementBatch with version-aware field handling.
decodeAcknowledgementBatch :: MonadGet m => E.ApiVersion -> m AcknowledgementBatch
decodeAcknowledgementBatch version =
  do
    fieldfirstoffset <- deserialize
    fieldlastoffset <- deserialize
    fieldacknowledgetypes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AcknowledgementBatch
      {
      acknowledgementBatchFirstOffset = fieldfirstoffset
      ,
      acknowledgementBatchLastOffset = fieldlastoffset
      ,
      acknowledgementBatchAcknowledgeTypes = fieldacknowledgetypes
      }


-- | The partitions containing records to acknowledge.
data AcknowledgePartition = AcknowledgePartition
  {

  -- | The partition index.

  -- Versions: 0+
  acknowledgePartitionPartitionIndex :: !(Int32)
,

  -- | Record batches to acknowledge.

  -- Versions: 0+
  acknowledgePartitionAcknowledgementBatches :: !(KafkaArray (AcknowledgementBatch))

  }
  deriving (Eq, Show, Generic)


-- | Encode AcknowledgePartition with version-aware field handling.
encodeAcknowledgePartition :: MonadPut m => E.ApiVersion -> AcknowledgePartition -> m ()
encodeAcknowledgePartition version amsg =
  do
    serialize (acknowledgePartitionPartitionIndex amsg)
    E.encodeVersionedArray version 0 encodeAcknowledgementBatch (case P.unKafkaArray (acknowledgePartitionAcknowledgementBatches amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AcknowledgePartition with version-aware field handling.
decodeAcknowledgePartition :: MonadGet m => E.ApiVersion -> m AcknowledgePartition
decodeAcknowledgePartition version =
  do
    fieldpartitionindex <- deserialize
    fieldacknowledgementbatches <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcknowledgementBatch
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AcknowledgePartition
      {
      acknowledgePartitionPartitionIndex = fieldpartitionindex
      ,
      acknowledgePartitionAcknowledgementBatches = fieldacknowledgementbatches
      }


-- | The topics containing records to acknowledge.
data AcknowledgeTopic = AcknowledgeTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  acknowledgeTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions containing records to acknowledge.

  -- Versions: 0+
  acknowledgeTopicPartitions :: !(KafkaArray (AcknowledgePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode AcknowledgeTopic with version-aware field handling.
encodeAcknowledgeTopic :: MonadPut m => E.ApiVersion -> AcknowledgeTopic -> m ()
encodeAcknowledgeTopic version amsg =
  do
    serialize (acknowledgeTopicTopicId amsg)
    E.encodeVersionedArray version 0 encodeAcknowledgePartition (case P.unKafkaArray (acknowledgeTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AcknowledgeTopic with version-aware field handling.
decodeAcknowledgeTopic :: MonadGet m => E.ApiVersion -> m AcknowledgeTopic
decodeAcknowledgeTopic version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcknowledgePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AcknowledgeTopic
      {
      acknowledgeTopicTopicId = fieldtopicid
      ,
      acknowledgeTopicPartitions = fieldpartitions
      }



data ShareAcknowledgeRequest = ShareAcknowledgeRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  shareAcknowledgeRequestGroupId :: !(KafkaString)
,

  -- | The member ID.

  -- Versions: 0+
  shareAcknowledgeRequestMemberId :: !(KafkaString)
,

  -- | The current share session epoch: 0 to open a share session; -1 to close it; otherwise increments for

  -- Versions: 0+
  shareAcknowledgeRequestShareSessionEpoch :: !(Int32)
,

  -- | Whether Renew type acknowledgements present in AcknowledgementBatches.

  -- Versions: 2+
  shareAcknowledgeRequestIsRenewAck :: !(Bool)
,

  -- | The topics containing records to acknowledge.

  -- Versions: 0+
  shareAcknowledgeRequestTopics :: !(KafkaArray (AcknowledgeTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareAcknowledgeRequest.
maxShareAcknowledgeRequestVersion :: Int16
maxShareAcknowledgeRequestVersion = 2

-- | Encode ShareAcknowledgeRequest with the given API version.
encodeShareAcknowledgeRequest :: MonadPut m => E.ApiVersion -> ShareAcknowledgeRequest -> m ()
encodeShareAcknowledgeRequest version msg
  | version == 1 =
    do
      serialize (toCompactString (shareAcknowledgeRequestGroupId msg))
      serialize (toCompactString (shareAcknowledgeRequestMemberId msg))
      serialize (shareAcknowledgeRequestShareSessionEpoch msg)
      E.encodeVersionedArray version 0 encodeAcknowledgeTopic (case P.unKafkaArray (shareAcknowledgeRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      serialize (toCompactString (shareAcknowledgeRequestGroupId msg))
      serialize (toCompactString (shareAcknowledgeRequestMemberId msg))
      serialize (shareAcknowledgeRequestShareSessionEpoch msg)
      serialize (shareAcknowledgeRequestIsRenewAck msg)
      E.encodeVersionedArray version 0 encodeAcknowledgeTopic (case P.unKafkaArray (shareAcknowledgeRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareAcknowledgeRequest with the given API version.
decodeShareAcknowledgeRequest :: MonadGet m => E.ApiVersion -> m ShareAcknowledgeRequest
decodeShareAcknowledgeRequest version
  | version == 1 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldsharesessionepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcknowledgeTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareAcknowledgeRequest
        {
        shareAcknowledgeRequestGroupId = fieldgroupid
        ,
        shareAcknowledgeRequestMemberId = fieldmemberid
        ,
        shareAcknowledgeRequestShareSessionEpoch = fieldsharesessionepoch
        ,
        shareAcknowledgeRequestIsRenewAck = False
        ,
        shareAcknowledgeRequestTopics = fieldtopics
        }

  | version == 2 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldsharesessionepoch <- deserialize
      fieldisrenewack <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcknowledgeTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareAcknowledgeRequest
        {
        shareAcknowledgeRequestGroupId = fieldgroupid
        ,
        shareAcknowledgeRequestMemberId = fieldmemberid
        ,
        shareAcknowledgeRequestShareSessionEpoch = fieldsharesessionepoch
        ,
        shareAcknowledgeRequestIsRenewAck = fieldisrenewack
        ,
        shareAcknowledgeRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ShareAcknowledgeRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
