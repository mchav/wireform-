{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ApiVersionsResponse
Description : Kafka ApiVersionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 18.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ApiVersionsResponse
  (
    ApiVersionsResponse(..),
    ApiVersion(..),
    SupportedFeatureKey(..),
    FinalizedFeatureKey(..),
    encodeApiVersionsResponse,
    decodeApiVersionsResponse,
    maxApiVersionsResponseVersion
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


-- | The APIs supported by the broker.
data ApiVersion = ApiVersion
  {

  -- | The API index.

  -- Versions: 0+
  apiVersionApiKey :: !(Int16)
,

  -- | The minimum supported version, inclusive.

  -- Versions: 0+
  apiVersionMinVersion :: !(Int16)
,

  -- | The maximum supported version, inclusive.

  -- Versions: 0+
  apiVersionMaxVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode ApiVersion with version-aware field handling.
encodeApiVersion :: MonadPut m => E.ApiVersion -> ApiVersion -> m ()
encodeApiVersion version amsg =
  do
    serialize (apiVersionApiKey amsg)
    serialize (apiVersionMinVersion amsg)
    serialize (apiVersionMaxVersion amsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ApiVersion with version-aware field handling.
decodeApiVersion :: MonadGet m => E.ApiVersion -> m ApiVersion
decodeApiVersion version =
  do
    fieldapikey <- deserialize
    fieldminversion <- deserialize
    fieldmaxversion <- deserialize
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ApiVersion
      {
      apiVersionApiKey = fieldapikey
      ,
      apiVersionMinVersion = fieldminversion
      ,
      apiVersionMaxVersion = fieldmaxversion
      }


-- | Features supported by the broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.
data SupportedFeatureKey = SupportedFeatureKey
  {

  -- | The name of the feature.

  -- Versions: 3+
  supportedFeatureKeyName :: !(KafkaString)
,

  -- | The minimum supported version for the feature.

  -- Versions: 3+
  supportedFeatureKeyMinVersion :: !(Int16)
,

  -- | The maximum supported version for the feature.

  -- Versions: 3+
  supportedFeatureKeyMaxVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode SupportedFeatureKey with version-aware field handling.
encodeSupportedFeatureKey :: MonadPut m => E.ApiVersion -> SupportedFeatureKey -> m ()
encodeSupportedFeatureKey version smsg =
  do
    when (version >= 3) $
      if version >= 3 then serialize (toCompactString (supportedFeatureKeyName smsg)) else serialize (supportedFeatureKeyName smsg)
    when (version >= 3) $
      serialize (supportedFeatureKeyMinVersion smsg)
    when (version >= 3) $
      serialize (supportedFeatureKeyMaxVersion smsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode SupportedFeatureKey with version-aware field handling.
decodeSupportedFeatureKey :: MonadGet m => E.ApiVersion -> m SupportedFeatureKey
decodeSupportedFeatureKey version =
  do
    fieldname <- if version >= 3
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldminversion <- if version >= 3
      then deserialize
      else pure (0)
    fieldmaxversion <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure SupportedFeatureKey
      {
      supportedFeatureKeyName = fieldname
      ,
      supportedFeatureKeyMinVersion = fieldminversion
      ,
      supportedFeatureKeyMaxVersion = fieldmaxversion
      }


-- | List of cluster-wide finalized features. The information is valid only if FinalizedFeaturesEpoch >= 0.
data FinalizedFeatureKey = FinalizedFeatureKey
  {

  -- | The name of the feature.

  -- Versions: 3+
  finalizedFeatureKeyName :: !(KafkaString)
,

  -- | The cluster-wide finalized max version level for the feature.

  -- Versions: 3+
  finalizedFeatureKeyMaxVersionLevel :: !(Int16)
,

  -- | The cluster-wide finalized min version level for the feature.

  -- Versions: 3+
  finalizedFeatureKeyMinVersionLevel :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode FinalizedFeatureKey with version-aware field handling.
encodeFinalizedFeatureKey :: MonadPut m => E.ApiVersion -> FinalizedFeatureKey -> m ()
encodeFinalizedFeatureKey version fmsg =
  do
    when (version >= 3) $
      if version >= 3 then serialize (toCompactString (finalizedFeatureKeyName fmsg)) else serialize (finalizedFeatureKeyName fmsg)
    when (version >= 3) $
      serialize (finalizedFeatureKeyMaxVersionLevel fmsg)
    when (version >= 3) $
      serialize (finalizedFeatureKeyMinVersionLevel fmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FinalizedFeatureKey with version-aware field handling.
decodeFinalizedFeatureKey :: MonadGet m => E.ApiVersion -> m FinalizedFeatureKey
decodeFinalizedFeatureKey version =
  do
    fieldname <- if version >= 3
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmaxversionlevel <- if version >= 3
      then deserialize
      else pure (0)
    fieldminversionlevel <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FinalizedFeatureKey
      {
      finalizedFeatureKeyName = fieldname
      ,
      finalizedFeatureKeyMaxVersionLevel = fieldmaxversionlevel
      ,
      finalizedFeatureKeyMinVersionLevel = fieldminversionlevel
      }



data ApiVersionsResponse = ApiVersionsResponse
  {

  -- | The top-level error code.

  -- Versions: 0+
  apiVersionsResponseErrorCode :: !(Int16)
,

  -- | The APIs supported by the broker.

  -- Versions: 0+
  apiVersionsResponseApiKeys :: !(KafkaArray (ApiVersion))
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  apiVersionsResponseThrottleTimeMs :: !(Int32)
,

  -- | Features supported by the broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.

  -- Versions: 3+
  apiVersionsResponseSupportedFeatures :: !(KafkaArray (SupportedFeatureKey))
,

  -- | The monotonically increasing epoch for the finalized features information. Valid values are >= 0. A 

  -- Versions: 3+
  apiVersionsResponseFinalizedFeaturesEpoch :: !(Int64)
,

  -- | List of cluster-wide finalized features. The information is valid only if FinalizedFeaturesEpoch >= 

  -- Versions: 3+
  apiVersionsResponseFinalizedFeatures :: !(KafkaArray (FinalizedFeatureKey))
,

  -- | Set by a KRaft controller if the required configurations for ZK migration are present.

  -- Versions: 3+
  apiVersionsResponseZkMigrationReady :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ApiVersionsResponse.
maxApiVersionsResponseVersion :: Int16
maxApiVersionsResponseVersion = 4

-- | Encode ApiVersionsResponse with the given API version.
encodeApiVersionsResponse :: MonadPut m => E.ApiVersion -> ApiVersionsResponse -> m ()
encodeApiVersionsResponse version msg
  | version == 0 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 1 && version <= 2 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (apiVersionsResponseThrottleTimeMs msg)


  | version >= 3 && version <= 4 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (apiVersionsResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ApiVersionsResponse with the given API version.
decodeApiVersionsResponse :: MonadGet m => E.ApiVersion -> m ApiVersionsResponse
decodeApiVersionsResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = 0
        ,
        apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = (-1)
        ,
        apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseZkMigrationReady = False
        }

  | version >= 1 && version <= 2 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      fieldthrottletimems <- deserialize
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = (-1)
        ,
        apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseZkMigrationReady = False
        }

  | version >= 3 && version <= 4 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = (-1)
        ,
        apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseZkMigrationReady = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ApiVersionsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
