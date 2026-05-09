{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClientQuotasResponse
Description : Kafka DescribeClientQuotasResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 48.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClientQuotasResponse
  (
    DescribeClientQuotasResponse(..),
    EntryData(..),
    EntityData(..),
    ValueData(..),
    encodeDescribeClientQuotasResponse,
    decodeDescribeClientQuotasResponse,
    maxDescribeClientQuotasResponseVersion
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


-- | The quota entity description.
data EntityData = EntityData
  {

  -- | The entity type.

  -- Versions: 0+
  entityDataEntityType :: !(KafkaString)
,

  -- | The entity name, or null if the default.

  -- Versions: 0+
  entityDataEntityName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode EntityData with version-aware field handling.
encodeEntityData :: MonadPut m => E.ApiVersion -> EntityData -> m ()
encodeEntityData version emsg =
  do
    if version >= 1 then serialize (toCompactString (entityDataEntityType emsg)) else serialize (entityDataEntityType emsg)
    if version >= 1 then serialize (toCompactString (entityDataEntityName emsg)) else serialize (entityDataEntityName emsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EntityData with version-aware field handling.
decodeEntityData :: MonadGet m => E.ApiVersion -> m EntityData
decodeEntityData version =
  do
    fieldentitytype <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldentityname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EntityData
      {
      entityDataEntityType = fieldentitytype
      ,
      entityDataEntityName = fieldentityname
      }


-- | The quota values for the entity.
data ValueData = ValueData
  {

  -- | The quota configuration key.

  -- Versions: 0+
  valueDataKey :: !(KafkaString)
,

  -- | The quota configuration value.

  -- Versions: 0+
  valueDataValue :: !(Double)

  }
  deriving (Eq, Show, Generic)


-- | Encode ValueData with version-aware field handling.
encodeValueData :: MonadPut m => E.ApiVersion -> ValueData -> m ()
encodeValueData version vmsg =
  do
    if version >= 1 then serialize (toCompactString (valueDataKey vmsg)) else serialize (valueDataKey vmsg)
    serialize (valueDataValue vmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ValueData with version-aware field handling.
decodeValueData :: MonadGet m => E.ApiVersion -> m ValueData
decodeValueData version =
  do
    fieldkey <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ValueData
      {
      valueDataKey = fieldkey
      ,
      valueDataValue = fieldvalue
      }


-- | A result entry.
data EntryData = EntryData
  {

  -- | The quota entity description.

  -- Versions: 0+
  entryDataEntity :: !(KafkaArray (EntityData))
,

  -- | The quota values for the entity.

  -- Versions: 0+
  entryDataValues :: !(KafkaArray (ValueData))

  }
  deriving (Eq, Show, Generic)


-- | Encode EntryData with version-aware field handling.
encodeEntryData :: MonadPut m => E.ApiVersion -> EntryData -> m ()
encodeEntryData version emsg =
  do
    E.encodeVersionedArray version 1 encodeEntityData (case P.unKafkaArray (entryDataEntity emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 1 encodeValueData (case P.unKafkaArray (entryDataValues emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EntryData with version-aware field handling.
decodeEntryData :: MonadGet m => E.ApiVersion -> m EntryData
decodeEntryData version =
  do
    fieldentity <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntityData
    fieldvalues <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeValueData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EntryData
      {
      entryDataEntity = fieldentity
      ,
      entryDataValues = fieldvalues
      }



data DescribeClientQuotasResponse = DescribeClientQuotasResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeClientQuotasResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or `0` if the quota description succeeded.

  -- Versions: 0+
  describeClientQuotasResponseErrorCode :: !(Int16)
,

  -- | The error message, or `null` if the quota description succeeded.

  -- Versions: 0+
  describeClientQuotasResponseErrorMessage :: !(KafkaString)
,

  -- | A result entry.

  -- Versions: 0+
  describeClientQuotasResponseEntries :: !(KafkaArray (EntryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClientQuotasResponse.
maxDescribeClientQuotasResponseVersion :: Int16
maxDescribeClientQuotasResponseVersion = 1

-- | Encode DescribeClientQuotasResponse with the given API version.
encodeDescribeClientQuotasResponse :: MonadPut m => E.ApiVersion -> DescribeClientQuotasResponse -> m ()
encodeDescribeClientQuotasResponse version msg
  | version == 0 =
    do
      serialize (describeClientQuotasResponseThrottleTimeMs msg)
      serialize (describeClientQuotasResponseErrorCode msg)
      serialize (describeClientQuotasResponseErrorMessage msg)
      E.encodeVersionedNullableArray version 1 encodeEntryData (describeClientQuotasResponseEntries msg)


  | version == 1 =
    do
      serialize (describeClientQuotasResponseThrottleTimeMs msg)
      serialize (describeClientQuotasResponseErrorCode msg)
      serialize (toCompactString (describeClientQuotasResponseErrorMessage msg))
      E.encodeVersionedNullableArray version 1 encodeEntryData (describeClientQuotasResponseEntries msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeClientQuotasResponse with the given API version.
decodeDescribeClientQuotasResponse :: MonadGet m => E.ApiVersion -> m DescribeClientQuotasResponse
decodeDescribeClientQuotasResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldentries <- E.decodeVersionedNullableArray version 1 decodeEntryData
      pure DescribeClientQuotasResponse
        {
        describeClientQuotasResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeClientQuotasResponseErrorCode = fielderrorcode
        ,
        describeClientQuotasResponseErrorMessage = fielderrormessage
        ,
        describeClientQuotasResponseEntries = fieldentries
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      fieldentries <- E.decodeVersionedNullableArray version 1 decodeEntryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClientQuotasResponse
        {
        describeClientQuotasResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeClientQuotasResponseErrorCode = fielderrorcode
        ,
        describeClientQuotasResponseErrorMessage = fielderrormessage
        ,
        describeClientQuotasResponseEntries = fieldentries
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeClientQuotasResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
