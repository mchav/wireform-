{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.IncrementalAlterConfigsRequest
Description : Kafka IncrementalAlterConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 44.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.IncrementalAlterConfigsRequest
  (
    IncrementalAlterConfigsRequest(..),
    AlterConfigsResource(..),
    AlterableConfig(..),
    encodeIncrementalAlterConfigsRequest,
    decodeIncrementalAlterConfigsRequest,
    maxIncrementalAlterConfigsRequestVersion
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


-- | The configurations.
data AlterableConfig = AlterableConfig
  {

  -- | The configuration key name.

  -- Versions: 0+
  alterableConfigName :: !(KafkaString)
,

  -- | The type (Set, Delete, Append, Subtract) of operation.

  -- Versions: 0+
  alterableConfigConfigOperation :: !(Int8)
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
    if version >= 1 then serialize (toCompactString (alterableConfigName amsg)) else serialize (alterableConfigName amsg)
    serialize (alterableConfigConfigOperation amsg)
    if version >= 1 then serialize (toCompactString (alterableConfigValue amsg)) else serialize (alterableConfigValue amsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterableConfig with version-aware field handling.
decodeAlterableConfig :: MonadGet m => E.ApiVersion -> m AlterableConfig
decodeAlterableConfig version =
  do
    fieldname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigoperation <- deserialize
    fieldvalue <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterableConfig
      {
      alterableConfigName = fieldname
      ,
      alterableConfigConfigOperation = fieldconfigoperation
      ,
      alterableConfigValue = fieldvalue
      }


-- | The incremental updates for each resource.
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
    if version >= 1 then serialize (toCompactString (alterConfigsResourceResourceName amsg)) else serialize (alterConfigsResourceResourceName amsg)
    E.encodeVersionedArray version 1 encodeAlterableConfig (case P.unKafkaArray (alterConfigsResourceConfigs amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterConfigsResource with version-aware field handling.
decodeAlterConfigsResource :: MonadGet m => E.ApiVersion -> m AlterConfigsResource
decodeAlterConfigsResource version =
  do
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeAlterableConfig
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterConfigsResource
      {
      alterConfigsResourceResourceType = fieldresourcetype
      ,
      alterConfigsResourceResourceName = fieldresourcename
      ,
      alterConfigsResourceConfigs = fieldconfigs
      }



data IncrementalAlterConfigsRequest = IncrementalAlterConfigsRequest
  {

  -- | The incremental updates for each resource.

  -- Versions: 0+
  incrementalAlterConfigsRequestResources :: !(KafkaArray (AlterConfigsResource))
,

  -- | True if we should validate the request, but not change the configurations.

  -- Versions: 0+
  incrementalAlterConfigsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for IncrementalAlterConfigsRequest.
maxIncrementalAlterConfigsRequestVersion :: Int16
maxIncrementalAlterConfigsRequestVersion = 1

-- | Encode IncrementalAlterConfigsRequest with the given API version.
encodeIncrementalAlterConfigsRequest :: MonadPut m => E.ApiVersion -> IncrementalAlterConfigsRequest -> m ()
encodeIncrementalAlterConfigsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 1 encodeAlterConfigsResource (case P.unKafkaArray (incrementalAlterConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (incrementalAlterConfigsRequestValidateOnly msg)


  | version == 1 =
    do
      E.encodeVersionedArray version 1 encodeAlterConfigsResource (case P.unKafkaArray (incrementalAlterConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (incrementalAlterConfigsRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode IncrementalAlterConfigsRequest with the given API version.
decodeIncrementalAlterConfigsRequest :: MonadGet m => E.ApiVersion -> m IncrementalAlterConfigsRequest
decodeIncrementalAlterConfigsRequest version
  | version == 0 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeAlterConfigsResource
      fieldvalidateonly <- deserialize
      pure IncrementalAlterConfigsRequest
        {
        incrementalAlterConfigsRequestResources = fieldresources
        ,
        incrementalAlterConfigsRequestValidateOnly = fieldvalidateonly
        }

  | version == 1 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeAlterConfigsResource
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure IncrementalAlterConfigsRequest
        {
        incrementalAlterConfigsRequestResources = fieldresources
        ,
        incrementalAlterConfigsRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec IncrementalAlterConfigsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
