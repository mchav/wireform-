{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterConfigsResponse
Description : Kafka AlterConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 33.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterConfigsResponse
  (
    AlterConfigsResponse(..),
    AlterConfigsResourceResponse(..),
    encodeAlterConfigsResponse,
    decodeAlterConfigsResponse,
    maxAlterConfigsResponseVersion
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


-- | The responses for each resource.
data AlterConfigsResourceResponse = AlterConfigsResourceResponse
  {

  -- | The resource error code.

  -- Versions: 0+
  alterConfigsResourceResponseErrorCode :: !(Int16)
,

  -- | The resource error message, or null if there was no error.

  -- Versions: 0+
  alterConfigsResourceResponseErrorMessage :: !(KafkaString)
,

  -- | The resource type.

  -- Versions: 0+
  alterConfigsResourceResponseResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  alterConfigsResourceResponseResourceName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterConfigsResourceResponse with version-aware field handling.
encodeAlterConfigsResourceResponse :: MonadPut m => E.ApiVersion -> AlterConfigsResourceResponse -> m ()
encodeAlterConfigsResourceResponse version amsg =
  do
    serialize (alterConfigsResourceResponseErrorCode amsg)
    if version >= 2 then serialize (toCompactString (alterConfigsResourceResponseErrorMessage amsg)) else serialize (alterConfigsResourceResponseErrorMessage amsg)
    serialize (alterConfigsResourceResponseResourceType amsg)
    if version >= 2 then serialize (toCompactString (alterConfigsResourceResponseResourceName amsg)) else serialize (alterConfigsResourceResponseResourceName amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterConfigsResourceResponse with version-aware field handling.
decodeAlterConfigsResourceResponse :: MonadGet m => E.ApiVersion -> m AlterConfigsResourceResponse
decodeAlterConfigsResourceResponse version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterConfigsResourceResponse
      {
      alterConfigsResourceResponseErrorCode = fielderrorcode
      ,
      alterConfigsResourceResponseErrorMessage = fielderrormessage
      ,
      alterConfigsResourceResponseResourceType = fieldresourcetype
      ,
      alterConfigsResourceResponseResourceName = fieldresourcename
      }



data AlterConfigsResponse = AlterConfigsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  alterConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each resource.

  -- Versions: 0+
  alterConfigsResponseResponses :: !(KafkaArray (AlterConfigsResourceResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterConfigsResponse.
maxAlterConfigsResponseVersion :: Int16
maxAlterConfigsResponseVersion = 2

-- | Encode AlterConfigsResponse with the given API version.
encodeAlterConfigsResponse :: MonadPut m => E.ApiVersion -> AlterConfigsResponse -> m ()
encodeAlterConfigsResponse version msg
  | version == 2 =
    do
      serialize (alterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterConfigsResourceResponse (case P.unKafkaArray (alterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (alterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterConfigsResourceResponse (case P.unKafkaArray (alterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterConfigsResponse with the given API version.
decodeAlterConfigsResponse :: MonadGet m => E.ApiVersion -> m AlterConfigsResponse
decodeAlterConfigsResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResourceResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterConfigsResponse
        {
        alterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterConfigsResponseResponses = fieldresponses
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResourceResponse
      pure AlterConfigsResponse
        {
        alterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterConfigsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAlterConfigsResponse' / 'decodeAlterConfigsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AlterConfigsResponse where
  wireCodec = Just (WC.serialShimCodec encodeAlterConfigsResponse decodeAlterConfigsResponse)
  {-# INLINE wireCodec #-}
