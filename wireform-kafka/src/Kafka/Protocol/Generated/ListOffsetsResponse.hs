{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListOffsetsResponse
Description : Kafka ListOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 2.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListOffsetsResponse
  (
    ListOffsetsResponse(..),
    ListOffsetsTopicResponse(..),
    ListOffsetsPartitionResponse(..),
    encodeListOffsetsResponse,
    decodeListOffsetsResponse,
    maxListOffsetsResponseVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC


-- | Each partition in the response.
data ListOffsetsPartitionResponse = ListOffsetsPartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  listOffsetsPartitionResponsePartitionIndex :: !(Int32)
,

  -- | The partition error code, or 0 if there was no error.

  -- Versions: 0+
  listOffsetsPartitionResponseErrorCode :: !(Int16)
,

  -- | The timestamp associated with the returned offset.

  -- Versions: 1+
  listOffsetsPartitionResponseTimestamp :: !(Int64)
,

  -- | The returned offset.

  -- Versions: 1+
  listOffsetsPartitionResponseOffset :: !(Int64)
,

  -- | The leader epoch associated with the returned offset.

  -- Versions: 4+
  listOffsetsPartitionResponseLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode ListOffsetsPartitionResponse with version-aware field handling.
encodeListOffsetsPartitionResponse :: MonadPut m => E.ApiVersion -> ListOffsetsPartitionResponse -> m ()
encodeListOffsetsPartitionResponse version lmsg =
  do
    serialize (listOffsetsPartitionResponsePartitionIndex lmsg)
    serialize (listOffsetsPartitionResponseErrorCode lmsg)
    when (version >= 1) $
      serialize (listOffsetsPartitionResponseTimestamp lmsg)
    when (version >= 1) $
      serialize (listOffsetsPartitionResponseOffset lmsg)
    when (version >= 4) $
      serialize (listOffsetsPartitionResponseLeaderEpoch lmsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListOffsetsPartitionResponse with version-aware field handling.
decodeListOffsetsPartitionResponse :: MonadGet m => E.ApiVersion -> m ListOffsetsPartitionResponse
decodeListOffsetsPartitionResponse version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fieldtimestamp <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldoffset <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- if version >= 4
      then deserialize
      else pure ((-1))
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListOffsetsPartitionResponse
      {
      listOffsetsPartitionResponsePartitionIndex = fieldpartitionindex
      ,
      listOffsetsPartitionResponseErrorCode = fielderrorcode
      ,
      listOffsetsPartitionResponseTimestamp = fieldtimestamp
      ,
      listOffsetsPartitionResponseOffset = fieldoffset
      ,
      listOffsetsPartitionResponseLeaderEpoch = fieldleaderepoch
      }


-- | Each topic in the response.
data ListOffsetsTopicResponse = ListOffsetsTopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  listOffsetsTopicResponseName :: !(KafkaString)
,

  -- | Each partition in the response.

  -- Versions: 0+
  listOffsetsTopicResponsePartitions :: !(KafkaArray (ListOffsetsPartitionResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode ListOffsetsTopicResponse with version-aware field handling.
encodeListOffsetsTopicResponse :: MonadPut m => E.ApiVersion -> ListOffsetsTopicResponse -> m ()
encodeListOffsetsTopicResponse version lmsg =
  do
    if version >= 6 then serialize (toCompactString (listOffsetsTopicResponseName lmsg)) else serialize (listOffsetsTopicResponseName lmsg)
    E.encodeVersionedArray version 6 encodeListOffsetsPartitionResponse (case P.unKafkaArray (listOffsetsTopicResponsePartitions lmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListOffsetsTopicResponse with version-aware field handling.
decodeListOffsetsTopicResponse :: MonadGet m => E.ApiVersion -> m ListOffsetsTopicResponse
decodeListOffsetsTopicResponse version =
  do
    fieldname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsPartitionResponse
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListOffsetsTopicResponse
      {
      listOffsetsTopicResponseName = fieldname
      ,
      listOffsetsTopicResponsePartitions = fieldpartitions
      }



data ListOffsetsResponse = ListOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  listOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  listOffsetsResponseTopics :: !(KafkaArray (ListOffsetsTopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListOffsetsResponse.
maxListOffsetsResponseVersion :: Int16
maxListOffsetsResponseVersion = 10

-- | KafkaMessage instance for ListOffsetsResponse.
instance KafkaMessage ListOffsetsResponse where
  messageApiKey = 2
  messageMinVersion = 1
  messageMaxVersion = 10
  messageFlexibleVersion = Just 6

-- | Encode ListOffsetsResponse with the given API version.
encodeListOffsetsResponse :: MonadPut m => E.ApiVersion -> ListOffsetsResponse -> m ()
encodeListOffsetsResponse version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 6 encodeListOffsetsTopicResponse (case P.unKafkaArray (listOffsetsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 5 =
    do
      serialize (listOffsetsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopicResponse (case P.unKafkaArray (listOffsetsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 6 && version <= 10 =
    do
      serialize (listOffsetsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopicResponse (case P.unKafkaArray (listOffsetsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListOffsetsResponse with the given API version.
decodeListOffsetsResponse :: MonadGet m => E.ApiVersion -> m ListOffsetsResponse
decodeListOffsetsResponse version
  | version == 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopicResponse
      pure ListOffsetsResponse
        {
        listOffsetsResponseThrottleTimeMs = 0
        ,
        listOffsetsResponseTopics = fieldtopics
        }

  | version >= 2 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopicResponse
      pure ListOffsetsResponse
        {
        listOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listOffsetsResponseTopics = fieldtopics
        }

  | version >= 6 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListOffsetsResponse
        {
        listOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listOffsetsResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ListOffsetsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
