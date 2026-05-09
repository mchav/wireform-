{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterClientQuotasResponse
Description : Kafka AlterClientQuotasResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 49.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterClientQuotasResponse
  (
    AlterClientQuotasResponse(..),
    EntryData(..),
    EntityData(..),
    encodeAlterClientQuotasResponse,
    decodeAlterClientQuotasResponse,
    maxAlterClientQuotasResponseVersion
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


-- | The quota entity to alter.
data EntityData = EntityData
  {

  -- | The entity type.

  -- Versions: 0+
  entityDataEntityType :: !(KafkaString)
,

  -- | The name of the entity, or null if the default.

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


-- | The quota configuration entries to alter.
data EntryData = EntryData
  {

  -- | The error code, or `0` if the quota alteration succeeded.

  -- Versions: 0+
  entryDataErrorCode :: !(Int16)
,

  -- | The error message, or `null` if the quota alteration succeeded.

  -- Versions: 0+
  entryDataErrorMessage :: !(KafkaString)
,

  -- | The quota entity to alter.

  -- Versions: 0+
  entryDataEntity :: !(KafkaArray (EntityData))

  }
  deriving (Eq, Show, Generic)


-- | Encode EntryData with version-aware field handling.
encodeEntryData :: MonadPut m => E.ApiVersion -> EntryData -> m ()
encodeEntryData version emsg =
  do
    serialize (entryDataErrorCode emsg)
    if version >= 1 then serialize (toCompactString (entryDataErrorMessage emsg)) else serialize (entryDataErrorMessage emsg)
    E.encodeVersionedArray version 1 encodeEntityData (case P.unKafkaArray (entryDataEntity emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EntryData with version-aware field handling.
decodeEntryData :: MonadGet m => E.ApiVersion -> m EntryData
decodeEntryData version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldentity <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntityData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EntryData
      {
      entryDataErrorCode = fielderrorcode
      ,
      entryDataErrorMessage = fielderrormessage
      ,
      entryDataEntity = fieldentity
      }



data AlterClientQuotasResponse = AlterClientQuotasResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterClientQuotasResponseThrottleTimeMs :: !(Int32)
,

  -- | The quota configuration entries to alter.

  -- Versions: 0+
  alterClientQuotasResponseEntries :: !(KafkaArray (EntryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterClientQuotasResponse.
maxAlterClientQuotasResponseVersion :: Int16
maxAlterClientQuotasResponseVersion = 1

-- | Encode AlterClientQuotasResponse with the given API version.
encodeAlterClientQuotasResponse :: MonadPut m => E.ApiVersion -> AlterClientQuotasResponse -> m ()
encodeAlterClientQuotasResponse version msg
  | version == 0 =
    do
      serialize (alterClientQuotasResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 1 encodeEntryData (case P.unKafkaArray (alterClientQuotasResponseEntries msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (alterClientQuotasResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 1 encodeEntryData (case P.unKafkaArray (alterClientQuotasResponseEntries msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterClientQuotasResponse with the given API version.
decodeAlterClientQuotasResponse :: MonadGet m => E.ApiVersion -> m AlterClientQuotasResponse
decodeAlterClientQuotasResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldentries <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntryData
      pure AlterClientQuotasResponse
        {
        alterClientQuotasResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterClientQuotasResponseEntries = fieldentries
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldentries <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterClientQuotasResponse
        {
        alterClientQuotasResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterClientQuotasResponseEntries = fieldentries
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAlterClientQuotasResponse' / 'decodeAlterClientQuotasResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AlterClientQuotasResponse where
  wireCodec = Just (WC.serialShimCodec encodeAlterClientQuotasResponse decodeAlterClientQuotasResponse)
  {-# INLINE wireCodec #-}
