{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupOffsetsResponse
Description : Kafka DeleteShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 92.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupOffsetsResponse
  (
    DeleteShareGroupOffsetsResponse(..),
    DeleteShareGroupOffsetsResponseTopic(..),
    encodeDeleteShareGroupOffsetsResponse,
    decodeDeleteShareGroupOffsetsResponse,
    maxDeleteShareGroupOffsetsResponseVersion
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


-- | The results for each topic.
data DeleteShareGroupOffsetsResponseTopic = DeleteShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,

  -- | The topic-level error code, or 0 if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicErrorCode :: !(Int16)
,

  -- | The topic-level error message, or null if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteShareGroupOffsetsResponseTopic with version-aware field handling.
encodeDeleteShareGroupOffsetsResponseTopic :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsResponseTopic -> m ()
encodeDeleteShareGroupOffsetsResponseTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsResponseTopicTopicName dmsg)) else serialize (deleteShareGroupOffsetsResponseTopicTopicName dmsg)
    serialize (deleteShareGroupOffsetsResponseTopicTopicId dmsg)
    serialize (deleteShareGroupOffsetsResponseTopicErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsResponseTopicErrorMessage dmsg)) else serialize (deleteShareGroupOffsetsResponseTopicErrorMessage dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteShareGroupOffsetsResponseTopic with version-aware field handling.
decodeDeleteShareGroupOffsetsResponseTopic :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsResponseTopic
decodeDeleteShareGroupOffsetsResponseTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteShareGroupOffsetsResponseTopic
      {
      deleteShareGroupOffsetsResponseTopicTopicName = fieldtopicname
      ,
      deleteShareGroupOffsetsResponseTopicTopicId = fieldtopicid
      ,
      deleteShareGroupOffsetsResponseTopicErrorCode = fielderrorcode
      ,
      deleteShareGroupOffsetsResponseTopicErrorMessage = fielderrormessage
      }



data DeleteShareGroupOffsetsResponse = DeleteShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseErrorMessage :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseResponses :: !(KafkaArray (DeleteShareGroupOffsetsResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupOffsetsResponse.
maxDeleteShareGroupOffsetsResponseVersion :: Int16
maxDeleteShareGroupOffsetsResponseVersion = 0

-- | Encode DeleteShareGroupOffsetsResponse with the given API version.
encodeDeleteShareGroupOffsetsResponse :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsResponse -> m ()
encodeDeleteShareGroupOffsetsResponse version msg
  | version == 0 =
    do
      serialize (deleteShareGroupOffsetsResponseThrottleTimeMs msg)
      serialize (deleteShareGroupOffsetsResponseErrorCode msg)
      serialize (toCompactString (deleteShareGroupOffsetsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeDeleteShareGroupOffsetsResponseTopic (case P.unKafkaArray (deleteShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupOffsetsResponse with the given API version.
decodeDeleteShareGroupOffsetsResponse :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsResponse
decodeDeleteShareGroupOffsetsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteShareGroupOffsetsResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupOffsetsResponse
        {
        deleteShareGroupOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteShareGroupOffsetsResponseErrorCode = fielderrorcode
        ,
        deleteShareGroupOffsetsResponseErrorMessage = fielderrormessage
        ,
        deleteShareGroupOffsetsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version