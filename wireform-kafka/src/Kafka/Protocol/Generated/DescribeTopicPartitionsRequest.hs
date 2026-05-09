{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTopicPartitionsRequest
Description : Kafka DescribeTopicPartitionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 75.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTopicPartitionsRequest
  (
    DescribeTopicPartitionsRequest(..),
    TopicRequest(..),
    Cursor(..),
    encodeDescribeTopicPartitionsRequest,
    decodeDescribeTopicPartitionsRequest,
    maxDescribeTopicPartitionsRequestVersion
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


-- | The topics to fetch details for.
data TopicRequest = TopicRequest
  {

  -- | The topic name.

  -- Versions: 0+
  topicRequestName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicRequest with version-aware field handling.
encodeTopicRequest :: MonadPut m => E.ApiVersion -> TopicRequest -> m ()
encodeTopicRequest version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicRequestName tmsg)) else serialize (topicRequestName tmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicRequest with version-aware field handling.
decodeTopicRequest :: MonadGet m => E.ApiVersion -> m TopicRequest
decodeTopicRequest version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicRequest
      {
      topicRequestName = fieldname
      }


-- | The first topic and partition index to fetch details for.
data Cursor = Cursor
  {

  -- | The name for the first topic to process.

  -- Versions: 0+
  cursorTopicName :: !(KafkaString)
,

  -- | The partition index to start with.

  -- Versions: 0+
  cursorPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode Cursor with version-aware field handling.
encodeCursor :: MonadPut m => E.ApiVersion -> Cursor -> m ()
encodeCursor version cmsg =
  do
    if version >= 0 then serialize (toCompactString (cursorTopicName cmsg)) else serialize (cursorTopicName cmsg)
    serialize (cursorPartitionIndex cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Cursor with version-aware field handling.
decodeCursor :: MonadGet m => E.ApiVersion -> m Cursor
decodeCursor version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindex <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Cursor
      {
      cursorTopicName = fieldtopicname
      ,
      cursorPartitionIndex = fieldpartitionindex
      }



data DescribeTopicPartitionsRequest = DescribeTopicPartitionsRequest
  {

  -- | The topics to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsRequestTopics :: !(KafkaArray (TopicRequest))
,

  -- | The maximum number of partitions included in the response.

  -- Versions: 0+
  describeTopicPartitionsRequestResponsePartitionLimit :: !(Int32)
,

  -- | The first topic and partition index to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsRequestCursor :: !(Nullable (Cursor))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTopicPartitionsRequest.
maxDescribeTopicPartitionsRequestVersion :: Int16
maxDescribeTopicPartitionsRequestVersion = 0

-- | Encode DescribeTopicPartitionsRequest with the given API version.
encodeDescribeTopicPartitionsRequest :: MonadPut m => E.ApiVersion -> DescribeTopicPartitionsRequest -> m ()
encodeDescribeTopicPartitionsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeTopicRequest (case P.unKafkaArray (describeTopicPartitionsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeTopicPartitionsRequestResponsePartitionLimit msg)
      case (describeTopicPartitionsRequestCursor msg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeCursor version val } }
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeTopicPartitionsRequest with the given API version.
decodeDescribeTopicPartitionsRequest :: MonadGet m => E.ApiVersion -> m DescribeTopicPartitionsRequest
decodeDescribeTopicPartitionsRequest version
  | version == 0 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicRequest
      fieldresponsepartitionlimit <- deserialize
      fieldcursor <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeCursor version; _ -> fail "Invalid nullable flag" } }
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeTopicPartitionsRequest
        {
        describeTopicPartitionsRequestTopics = fieldtopics
        ,
        describeTopicPartitionsRequestResponsePartitionLimit = fieldresponsepartitionlimit
        ,
        describeTopicPartitionsRequestCursor = fieldcursor
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeTopicPartitionsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
