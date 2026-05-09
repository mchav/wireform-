{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClientQuotasRequest
Description : Kafka DescribeClientQuotasRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 48.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClientQuotasRequest
  (
    DescribeClientQuotasRequest(..),
    ComponentData(..),
    encodeDescribeClientQuotasRequest,
    decodeDescribeClientQuotasRequest,
    maxDescribeClientQuotasRequestVersion
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


-- | Filter components to apply to quota entities.
data ComponentData = ComponentData
  {

  -- | The entity type that the filter component applies to.

  -- Versions: 0+
  componentDataEntityType :: !(KafkaString)
,

  -- | How to match the entity {0 = exact name, 1 = default name, 2 = any specified name}.

  -- Versions: 0+
  componentDataMatchType :: !(Int8)
,

  -- | The string to match against, or null if unused for the match type.

  -- Versions: 0+
  componentDataMatch :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode ComponentData with version-aware field handling.
encodeComponentData :: MonadPut m => E.ApiVersion -> ComponentData -> m ()
encodeComponentData version cmsg =
  do
    if version >= 1 then serialize (toCompactString (componentDataEntityType cmsg)) else serialize (componentDataEntityType cmsg)
    serialize (componentDataMatchType cmsg)
    if version >= 1 then serialize (toCompactString (componentDataMatch cmsg)) else serialize (componentDataMatch cmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ComponentData with version-aware field handling.
decodeComponentData :: MonadGet m => E.ApiVersion -> m ComponentData
decodeComponentData version =
  do
    fieldentitytype <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldmatchtype <- deserialize
    fieldmatch <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ComponentData
      {
      componentDataEntityType = fieldentitytype
      ,
      componentDataMatchType = fieldmatchtype
      ,
      componentDataMatch = fieldmatch
      }



data DescribeClientQuotasRequest = DescribeClientQuotasRequest
  {

  -- | Filter components to apply to quota entities.

  -- Versions: 0+
  describeClientQuotasRequestComponents :: !(KafkaArray (ComponentData))
,

  -- | Whether the match is strict, i.e. should exclude entities with unspecified entity types.

  -- Versions: 0+
  describeClientQuotasRequestStrict :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClientQuotasRequest.
maxDescribeClientQuotasRequestVersion :: Int16
maxDescribeClientQuotasRequestVersion = 1

-- | Encode DescribeClientQuotasRequest with the given API version.
encodeDescribeClientQuotasRequest :: MonadPut m => E.ApiVersion -> DescribeClientQuotasRequest -> m ()
encodeDescribeClientQuotasRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 1 encodeComponentData (case P.unKafkaArray (describeClientQuotasRequestComponents msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeClientQuotasRequestStrict msg)


  | version == 1 =
    do
      E.encodeVersionedArray version 1 encodeComponentData (case P.unKafkaArray (describeClientQuotasRequestComponents msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeClientQuotasRequestStrict msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeClientQuotasRequest with the given API version.
decodeDescribeClientQuotasRequest :: MonadGet m => E.ApiVersion -> m DescribeClientQuotasRequest
decodeDescribeClientQuotasRequest version
  | version == 0 =
    do
      fieldcomponents <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeComponentData
      fieldstrict <- deserialize
      pure DescribeClientQuotasRequest
        {
        describeClientQuotasRequestComponents = fieldcomponents
        ,
        describeClientQuotasRequestStrict = fieldstrict
        }

  | version == 1 =
    do
      fieldcomponents <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeComponentData
      fieldstrict <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClientQuotasRequest
        {
        describeClientQuotasRequestComponents = fieldcomponents
        ,
        describeClientQuotasRequestStrict = fieldstrict
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeClientQuotasRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
