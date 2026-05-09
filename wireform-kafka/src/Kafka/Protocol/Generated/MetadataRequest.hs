{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.MetadataRequest
Description : Kafka MetadataRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 3.



Valid versions: 0-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.MetadataRequest
  (
    MetadataRequest(..),
    MetadataRequestTopic(..),
    encodeMetadataRequest,
    decodeMetadataRequest,
    maxMetadataRequestVersion
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The topics to fetch metadata for.
data MetadataRequestTopic = MetadataRequestTopic
  {

  -- | The topic id.

  -- Versions: 10+
  metadataRequestTopicTopicId :: !(KafkaUuid)
,

  -- | The topic name.

  -- Versions: 0+
  metadataRequestTopicName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataRequestTopic with version-aware field handling.
encodeMetadataRequestTopic :: MonadPut m => E.ApiVersion -> MetadataRequestTopic -> m ()
encodeMetadataRequestTopic version mmsg =
  do
    when (version >= 10) $
      serialize (metadataRequestTopicTopicId mmsg)
    if version >= 9 then serialize (toCompactString (metadataRequestTopicName mmsg)) else serialize (metadataRequestTopicName mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataRequestTopic with version-aware field handling.
decodeMetadataRequestTopic :: MonadGet m => E.ApiVersion -> m MetadataRequestTopic
decodeMetadataRequestTopic version =
  do
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldname <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataRequestTopic
      {
      metadataRequestTopicTopicId = fieldtopicid
      ,
      metadataRequestTopicName = fieldname
      }



data MetadataRequest = MetadataRequest
  {

  -- | The topics to fetch metadata for.

  -- Versions: 0+
  metadataRequestTopics :: !(KafkaArray (MetadataRequestTopic))
,

  -- | If this is true, the broker may auto-create topics that we requested which do not already exist, if 

  -- Versions: 4+
  metadataRequestAllowAutoTopicCreation :: !(Bool)
,

  -- | Whether to include cluster authorized operations.

  -- Versions: 8-10
  metadataRequestIncludeClusterAuthorizedOperations :: !(Bool)
,

  -- | Whether to include topic authorized operations.

  -- Versions: 8+
  metadataRequestIncludeTopicAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for MetadataRequest.
maxMetadataRequestVersion :: Int16
maxMetadataRequestVersion = 13

-- | KafkaMessage instance for MetadataRequest.
instance KafkaMessage MetadataRequest where
  messageApiKey = 3
  messageMinVersion = 0
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode MetadataRequest with the given API version.
encodeMetadataRequest :: MonadPut m => E.ApiVersion -> MetadataRequest -> m ()
encodeMetadataRequest version msg
  | version == 8 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeClusterAuthorizedOperations msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)


  | version >= 9 && version <= 10 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeClusterAuthorizedOperations msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 11 && version <= 13 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 3 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)


  | version >= 4 && version <= 7 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode MetadataRequest with the given API version.
decodeMetadataRequest :: MonadGet m => E.ApiVersion -> m MetadataRequest
decodeMetadataRequest version
  | version == 8 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 9 && version <= 10 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 11 && version <= 13 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 0 && version <= 3 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = True
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = False
        }

  | version >= 4 && version <= 7 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeMetadataRequest' / 'decodeMetadataRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec MetadataRequest where
  wireCodec = Just (WC.serialShimCodec encodeMetadataRequest decodeMetadataRequest)
  {-# INLINE wireCodec #-}
