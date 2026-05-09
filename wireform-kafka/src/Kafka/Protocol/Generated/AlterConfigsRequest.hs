{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterConfigsRequest
Description : Kafka AlterConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 33.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterConfigsRequest
  (
    AlterConfigsRequest(..),
    AlterConfigsResource(..),
    AlterableConfig(..),
    encodeAlterConfigsRequest,
    decodeAlterConfigsRequest,
    maxAlterConfigsRequestVersion
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


-- | The configurations.
data AlterableConfig = AlterableConfig
  {

  -- | The configuration key name.

  -- Versions: 0+
  alterableConfigName :: !(KafkaString)
,

  -- | The value to set for the configuration key.

  -- Versions: 0+
  alterableConfigValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterableConfig with version-aware field handling.
encodeAlterableConfig :: MonadPut m => E.ApiVersion -> AlterableConfig -> m ()
encodeAlterableConfig version amsg =
  do
    if version >= 2 then serialize (toCompactString (alterableConfigName amsg)) else serialize (alterableConfigName amsg)
    if version >= 2 then serialize (toCompactString (alterableConfigValue amsg)) else serialize (alterableConfigValue amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterableConfig with version-aware field handling.
decodeAlterableConfig :: MonadGet m => E.ApiVersion -> m AlterableConfig
decodeAlterableConfig version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterableConfig
      {
      alterableConfigName = fieldname
      ,
      alterableConfigValue = fieldvalue
      }


-- | The updates for each resource.
data AlterConfigsResource = AlterConfigsResource
  {

  -- | The resource type.

  -- Versions: 0+
  alterConfigsResourceResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  alterConfigsResourceResourceName :: !(KafkaString)
,

  -- | The configurations.

  -- Versions: 0+
  alterConfigsResourceConfigs :: !(KafkaArray (AlterableConfig))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterConfigsResource with version-aware field handling.
encodeAlterConfigsResource :: MonadPut m => E.ApiVersion -> AlterConfigsResource -> m ()
encodeAlterConfigsResource version amsg =
  do
    serialize (alterConfigsResourceResourceType amsg)
    if version >= 2 then serialize (toCompactString (alterConfigsResourceResourceName amsg)) else serialize (alterConfigsResourceResourceName amsg)
    E.encodeVersionedArray version 2 encodeAlterableConfig (case P.unKafkaArray (alterConfigsResourceConfigs amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterConfigsResource with version-aware field handling.
decodeAlterConfigsResource :: MonadGet m => E.ApiVersion -> m AlterConfigsResource
decodeAlterConfigsResource version =
  do
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterableConfig
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterConfigsResource
      {
      alterConfigsResourceResourceType = fieldresourcetype
      ,
      alterConfigsResourceResourceName = fieldresourcename
      ,
      alterConfigsResourceConfigs = fieldconfigs
      }



data AlterConfigsRequest = AlterConfigsRequest
  {

  -- | The updates for each resource.

  -- Versions: 0+
  alterConfigsRequestResources :: !(KafkaArray (AlterConfigsResource))
,

  -- | True if we should validate the request, but not change the configurations.

  -- Versions: 0+
  alterConfigsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterConfigsRequest.
maxAlterConfigsRequestVersion :: Int16
maxAlterConfigsRequestVersion = 2

-- | Encode AlterConfigsRequest with the given API version.
encodeAlterConfigsRequest :: MonadPut m => E.ApiVersion -> AlterConfigsRequest -> m ()
encodeAlterConfigsRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 2 encodeAlterConfigsResource (case P.unKafkaArray (alterConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (alterConfigsRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 encodeAlterConfigsResource (case P.unKafkaArray (alterConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (alterConfigsRequestValidateOnly msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterConfigsRequest with the given API version.
decodeAlterConfigsRequest :: MonadGet m => E.ApiVersion -> m AlterConfigsRequest
decodeAlterConfigsRequest version
  | version == 2 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResource
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterConfigsRequest
        {
        alterConfigsRequestResources = fieldresources
        ,
        alterConfigsRequestValidateOnly = fieldvalidateonly
        }

  | version >= 0 && version <= 1 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResource
      fieldvalidateonly <- deserialize
      pure AlterConfigsRequest
        {
        alterConfigsRequestResources = fieldresources
        ,
        alterConfigsRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version