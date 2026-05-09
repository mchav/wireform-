{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListConfigResourcesResponse
Description : Kafka ListConfigResourcesResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 74.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListConfigResourcesResponse
  (
    ListConfigResourcesResponse(..),
    ConfigResource(..),
    encodeListConfigResourcesResponse,
    decodeListConfigResourcesResponse,
    maxListConfigResourcesResponseVersion
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


-- | Each config resource in the response.
data ConfigResource = ConfigResource
  {

  -- | The resource name.

  -- Versions: 0+
  configResourceResourceName :: !(KafkaString)
,

  -- | The resource type.

  -- Versions: 1+
  configResourceResourceType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode ConfigResource with version-aware field handling.
encodeConfigResource :: MonadPut m => E.ApiVersion -> ConfigResource -> m ()
encodeConfigResource version cmsg =
  do
    if version >= 0 then serialize (toCompactString (configResourceResourceName cmsg)) else serialize (configResourceResourceName cmsg)
    when (version >= 1) $
      serialize (configResourceResourceType cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ConfigResource with version-aware field handling.
decodeConfigResource :: MonadGet m => E.ApiVersion -> m ConfigResource
decodeConfigResource version =
  do
    fieldresourcename <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- if version >= 1
      then deserialize
      else pure (16)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ConfigResource
      {
      configResourceResourceName = fieldresourcename
      ,
      configResourceResourceType = fieldresourcetype
      }



data ListConfigResourcesResponse = ListConfigResourcesResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  listConfigResourcesResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  listConfigResourcesResponseErrorCode :: !(Int16)
,

  -- | Each config resource in the response.

  -- Versions: 0+
  listConfigResourcesResponseConfigResources :: !(KafkaArray (ConfigResource))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListConfigResourcesResponse.
maxListConfigResourcesResponseVersion :: Int16
maxListConfigResourcesResponseVersion = 1

-- | Encode ListConfigResourcesResponse with the given API version.
encodeListConfigResourcesResponse :: MonadPut m => E.ApiVersion -> ListConfigResourcesResponse -> m ()
encodeListConfigResourcesResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (listConfigResourcesResponseThrottleTimeMs msg)
      serialize (listConfigResourcesResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeConfigResource (case P.unKafkaArray (listConfigResourcesResponseConfigResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListConfigResourcesResponse with the given API version.
decodeListConfigResourcesResponse :: MonadGet m => E.ApiVersion -> m ListConfigResourcesResponse
decodeListConfigResourcesResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldconfigresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeConfigResource
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListConfigResourcesResponse
        {
        listConfigResourcesResponseThrottleTimeMs = fieldthrottletimems
        ,
        listConfigResourcesResponseErrorCode = fielderrorcode
        ,
        listConfigResourcesResponseConfigResources = fieldconfigresources
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ListConfigResourcesResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
