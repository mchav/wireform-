{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareFetchRequest
Description : Kafka ShareFetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 78.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareFetchRequest
  (
    ShareFetchRequest(..),
    FetchTopic(..),
    FetchPartition(..),
    AcknowledgementBatch(..),
    ForgottenTopic(..),
    encodeShareFetchRequest,
    decodeShareFetchRequest,
    maxShareFetchRequestVersion
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


-- | The partitions to fetch.
data FetchPartition = FetchPartition
  {

  -- | The partition index.

  -- Versions: 0+
  fetchPartitionPartitionIndex :: !(Int32)
,

  -- | The maximum bytes to fetch from this partition. 0 when only acknowledgement with no fetching is requ

  -- Versions: 0
  fetchPartitionPartitionMaxBytes :: !(Int32)
,

  -- | Record batches to acknowledge.

  -- Versions: 0+
  fetchPartitionAcknowledgementBatches :: !(KafkaArray (AcknowledgementBatch))

  }
  deriving (Eq, Show, Generic)


-- | Encode FetchPartition with version-aware field handling.
encodeFetchPartition :: MonadPut m => E.ApiVersion -> FetchPartition -> m ()
encodeFetchPartition version fmsg =
  do
    serialize (fetchPartitionPartitionIndex fmsg)
    when (version == 0) $
      serialize (fetchPartitionPartitionMaxBytes fmsg)
    E.encodeVersionedArray version 0 encodeAcknowledgementBatch (case P.unKafkaArray (fetchPartitionAcknowledgementBatches fmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FetchPartition with version-aware field handling.
decodeFetchPartition :: MonadGet m => E.ApiVersion -> m FetchPartition
decodeFetchPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldpartitionmaxbytes <- if version == 0
      then deserialize
      else pure (0)
    fieldacknowledgementbatches <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcknowledgementBatch
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FetchPartition
      {
      fetchPartitionPartitionIndex = fieldpartitionindex
      ,
      fetchPartitionPartitionMaxBytes = fieldpartitionmaxbytes
      ,
      fetchPartitionAcknowledgementBatches = fieldacknowledgementbatches
      }


-- | The topics to fetch.
data FetchTopic = FetchTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  fetchTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  fetchTopicPartitions :: !(KafkaArray (FetchPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode FetchTopic with version-aware field handling.
encodeFetchTopic :: MonadPut m => E.ApiVersion -> FetchTopic -> m ()
encodeFetchTopic version fmsg =
  do
    serialize (fetchTopicTopicId fmsg)
    E.encodeVersionedArray version 0 encodeFetchPartition (case P.unKafkaArray (fetchTopicPartitions fmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FetchTopic with version-aware field handling.
decodeFetchTopic :: MonadGet m => E.ApiVersion -> m FetchTopic
decodeFetchTopic version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFetchPartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FetchTopic
      {
      fetchTopicTopicId = fieldtopicid
      ,
      fetchTopicPartitions = fieldpartitions
      }


-- | The partitions to remove from this share session.
data ForgottenTopic = ForgottenTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  forgottenTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions indexes to forget.

  -- Versions: 0+
  forgottenTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode ForgottenTopic with version-aware field handling.
encodeForgottenTopic :: MonadPut m => E.ApiVersion -> ForgottenTopic -> m ()
encodeForgottenTopic version fmsg =
  do
    serialize (forgottenTopicTopicId fmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (forgottenTopicPartitions fmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ForgottenTopic with version-aware field handling.
decodeForgottenTopic :: MonadGet m => E.ApiVersion -> m ForgottenTopic
decodeForgottenTopic version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ForgottenTopic
      {
      forgottenTopicTopicId = fieldtopicid
      ,
      forgottenTopicPartitions = fieldpartitions
      }



data ShareFetchRequest = ShareFetchRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  shareFetchRequestGroupId :: !(KafkaString)
,

  -- | The member ID.

  -- Versions: 0+
  shareFetchRequestMemberId :: !(KafkaString)
,

  -- | The current share session epoch: 0 to open a share session; -1 to close it; otherwise increments for

  -- Versions: 0+
  shareFetchRequestShareSessionEpoch :: !(Int32)
,

  -- | The maximum time in milliseconds to wait for the response.

  -- Versions: 0+
  shareFetchRequestMaxWaitMs :: !(Int32)
,

  -- | The minimum bytes to accumulate in the response.

  -- Versions: 0+
  shareFetchRequestMinBytes :: !(Int32)
,

  -- | The maximum bytes to fetch. See KIP-74 for cases where this limit may not be honored.

  -- Versions: 0+
  shareFetchRequestMaxBytes :: !(Int32)
,

  -- | The maximum number of records to fetch. This limit can be exceeded for alignment of batch boundaries

  -- Versions: 1+
  shareFetchRequestMaxRecords :: !(Int32)
,

  -- | The optimal number of records for batches of acquired records and acknowledgements.

  -- Versions: 1+
  shareFetchRequestBatchSize :: !(Int32)
,

  -- | Whether Renew type acknowledgements present in AcknowledgementBatches.

  -- Versions: 2+
  shareFetchRequestIsRenewAck :: !(Bool)
,

  -- | The topics to fetch.

  -- Versions: 0+
  shareFetchRequestTopics :: !(KafkaArray (FetchTopic))
,

  -- | The partitions to remove from this share session.

  -- Versions: 0+
  shareFetchRequestForgottenTopicsData :: !(KafkaArray (ForgottenTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareFetchRequest.
maxShareFetchRequestVersion :: Int16
maxShareFetchRequestVersion = 2

-- | Encode ShareFetchRequest with the given API version.
encodeShareFetchRequest :: MonadPut m => E.ApiVersion -> ShareFetchRequest -> m ()
encodeShareFetchRequest version msg
  | version == 1 =
    do
      serialize (toCompactString (shareFetchRequestGroupId msg))
      serialize (toCompactString (shareFetchRequestMemberId msg))
      serialize (shareFetchRequestShareSessionEpoch msg)
      serialize (shareFetchRequestMaxWaitMs msg)
      serialize (shareFetchRequestMinBytes msg)
      serialize (shareFetchRequestMaxBytes msg)
      serialize (shareFetchRequestMaxRecords msg)
      serialize (shareFetchRequestBatchSize msg)
      E.encodeVersionedArray version 0 encodeFetchTopic (case P.unKafkaArray (shareFetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeForgottenTopic (case P.unKafkaArray (shareFetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      serialize (toCompactString (shareFetchRequestGroupId msg))
      serialize (toCompactString (shareFetchRequestMemberId msg))
      serialize (shareFetchRequestShareSessionEpoch msg)
      serialize (shareFetchRequestMaxWaitMs msg)
      serialize (shareFetchRequestMinBytes msg)
      serialize (shareFetchRequestMaxBytes msg)
      serialize (shareFetchRequestMaxRecords msg)
      serialize (shareFetchRequestBatchSize msg)
      serialize (shareFetchRequestIsRenewAck msg)
      E.encodeVersionedArray version 0 encodeFetchTopic (case P.unKafkaArray (shareFetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeForgottenTopic (case P.unKafkaArray (shareFetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareFetchRequest with the given API version.
decodeShareFetchRequest :: MonadGet m => E.ApiVersion -> m ShareFetchRequest
decodeShareFetchRequest version
  | version == 1 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldsharesessionepoch <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldmaxrecords <- deserialize
      fieldbatchsize <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeForgottenTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareFetchRequest
        {
        shareFetchRequestGroupId = fieldgroupid
        ,
        shareFetchRequestMemberId = fieldmemberid
        ,
        shareFetchRequestShareSessionEpoch = fieldsharesessionepoch
        ,
        shareFetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        shareFetchRequestMinBytes = fieldminbytes
        ,
        shareFetchRequestMaxBytes = fieldmaxbytes
        ,
        shareFetchRequestMaxRecords = fieldmaxrecords
        ,
        shareFetchRequestBatchSize = fieldbatchsize
        ,
        shareFetchRequestIsRenewAck = False
        ,
        shareFetchRequestTopics = fieldtopics
        ,
        shareFetchRequestForgottenTopicsData = fieldforgottentopicsdata
        }

  | version == 2 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldsharesessionepoch <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldmaxrecords <- deserialize
      fieldbatchsize <- deserialize
      fieldisrenewack <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeForgottenTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareFetchRequest
        {
        shareFetchRequestGroupId = fieldgroupid
        ,
        shareFetchRequestMemberId = fieldmemberid
        ,
        shareFetchRequestShareSessionEpoch = fieldsharesessionepoch
        ,
        shareFetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        shareFetchRequestMinBytes = fieldminbytes
        ,
        shareFetchRequestMaxBytes = fieldmaxbytes
        ,
        shareFetchRequestMaxRecords = fieldmaxrecords
        ,
        shareFetchRequestBatchSize = fieldbatchsize
        ,
        shareFetchRequestIsRenewAck = fieldisrenewack
        ,
        shareFetchRequestTopics = fieldtopics
        ,
        shareFetchRequestForgottenTopicsData = fieldforgottentopicsdata
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeShareFetchRequest' / 'decodeShareFetchRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ShareFetchRequest where
  wireCodec = Just (WC.serialShimCodec encodeShareFetchRequest decodeShareFetchRequest)
  {-# INLINE wireCodec #-}
