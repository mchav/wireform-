{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.IncrementalAlterConfigsResponse
Description : Kafka IncrementalAlterConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 44.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.IncrementalAlterConfigsResponse
  (
    IncrementalAlterConfigsResponse(..),
    AlterConfigsResourceResponse(..),
    encodeIncrementalAlterConfigsResponse,
    decodeIncrementalAlterConfigsResponse,
    maxIncrementalAlterConfigsResponseVersion
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
    if version >= 1 then serialize (toCompactString (alterConfigsResourceResponseErrorMessage amsg)) else serialize (alterConfigsResourceResponseErrorMessage amsg)
    serialize (alterConfigsResourceResponseResourceType amsg)
    if version >= 1 then serialize (toCompactString (alterConfigsResourceResponseResourceName amsg)) else serialize (alterConfigsResourceResponseResourceName amsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterConfigsResourceResponse with version-aware field handling.
decodeAlterConfigsResourceResponse :: MonadGet m => E.ApiVersion -> m AlterConfigsResourceResponse
decodeAlterConfigsResourceResponse version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
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



data IncrementalAlterConfigsResponse = IncrementalAlterConfigsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  incrementalAlterConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each resource.

  -- Versions: 0+
  incrementalAlterConfigsResponseResponses :: !(KafkaArray (AlterConfigsResourceResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for IncrementalAlterConfigsResponse.
maxIncrementalAlterConfigsResponseVersion :: Int16
maxIncrementalAlterConfigsResponseVersion = 1

-- | Encode IncrementalAlterConfigsResponse with the given API version.
encodeIncrementalAlterConfigsResponse :: MonadPut m => E.ApiVersion -> IncrementalAlterConfigsResponse -> m ()
encodeIncrementalAlterConfigsResponse version msg
  | version == 0 =
    do
      serialize (incrementalAlterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 1 encodeAlterConfigsResourceResponse (case P.unKafkaArray (incrementalAlterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (incrementalAlterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 1 encodeAlterConfigsResourceResponse (case P.unKafkaArray (incrementalAlterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode IncrementalAlterConfigsResponse with the given API version.
decodeIncrementalAlterConfigsResponse :: MonadGet m => E.ApiVersion -> m IncrementalAlterConfigsResponse
decodeIncrementalAlterConfigsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeAlterConfigsResourceResponse
      pure IncrementalAlterConfigsResponse
        {
        incrementalAlterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        incrementalAlterConfigsResponseResponses = fieldresponses
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeAlterConfigsResourceResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure IncrementalAlterConfigsResponse
        {
        incrementalAlterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        incrementalAlterConfigsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec IncrementalAlterConfigsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
