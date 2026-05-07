{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterClientQuotasRequest
Description : Kafka AlterClientQuotasRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 49.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterClientQuotasRequest
  (
    AlterClientQuotasRequest(..),
    EntryData(..),
    EntityData(..),
    OpData(..),
    encodeAlterClientQuotasRequest,
    decodeAlterClientQuotasRequest,
    maxAlterClientQuotasRequestVersion
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


-- | An individual quota configuration entry to alter.
data OpData = OpData
  {

  -- | The quota configuration key.

  -- Versions: 0+
  opDataKey :: !(KafkaString)
,

  -- | The value to set, otherwise ignored if the value is to be removed.

  -- Versions: 0+
  opDataValue :: !(Double)
,

  -- | Whether the quota configuration value should be removed, otherwise set.

  -- Versions: 0+
  opDataRemove :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode OpData with version-aware field handling.
encodeOpData :: MonadPut m => E.ApiVersion -> OpData -> m ()
encodeOpData version omsg =
  do
    if version >= 1 then serialize (toCompactString (opDataKey omsg)) else serialize (opDataKey omsg)
    serialize (opDataValue omsg)
    serialize (opDataRemove omsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OpData with version-aware field handling.
decodeOpData :: MonadGet m => E.ApiVersion -> m OpData
decodeOpData version =
  do
    fieldkey <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- deserialize
    fieldremove <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OpData
      {
      opDataKey = fieldkey
      ,
      opDataValue = fieldvalue
      ,
      opDataRemove = fieldremove
      }


-- | The quota configuration entries to alter.
data EntryData = EntryData
  {

  -- | The quota entity to alter.

  -- Versions: 0+
  entryDataEntity :: !(KafkaArray (EntityData))
,

  -- | An individual quota configuration entry to alter.

  -- Versions: 0+
  entryDataOps :: !(KafkaArray (OpData))

  }
  deriving (Eq, Show, Generic)


-- | Encode EntryData with version-aware field handling.
encodeEntryData :: MonadPut m => E.ApiVersion -> EntryData -> m ()
encodeEntryData version emsg =
  do
    E.encodeVersionedArray version 1 encodeEntityData (case P.unKafkaArray (entryDataEntity emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 1 encodeOpData (case P.unKafkaArray (entryDataOps emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EntryData with version-aware field handling.
decodeEntryData :: MonadGet m => E.ApiVersion -> m EntryData
decodeEntryData version =
  do
    fieldentity <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntityData
    fieldops <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeOpData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EntryData
      {
      entryDataEntity = fieldentity
      ,
      entryDataOps = fieldops
      }



data AlterClientQuotasRequest = AlterClientQuotasRequest
  {

  -- | The quota configuration entries to alter.

  -- Versions: 0+
  alterClientQuotasRequestEntries :: !(KafkaArray (EntryData))
,

  -- | Whether the alteration should be validated, but not performed.

  -- Versions: 0+
  alterClientQuotasRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterClientQuotasRequest.
maxAlterClientQuotasRequestVersion :: Int16
maxAlterClientQuotasRequestVersion = 1

-- | Encode AlterClientQuotasRequest with the given API version.
encodeAlterClientQuotasRequest :: MonadPut m => E.ApiVersion -> AlterClientQuotasRequest -> m ()
encodeAlterClientQuotasRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 1 encodeEntryData (case P.unKafkaArray (alterClientQuotasRequestEntries msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (alterClientQuotasRequestValidateOnly msg)


  | version == 1 =
    do
      E.encodeVersionedArray version 1 encodeEntryData (case P.unKafkaArray (alterClientQuotasRequestEntries msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (alterClientQuotasRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterClientQuotasRequest with the given API version.
decodeAlterClientQuotasRequest :: MonadGet m => E.ApiVersion -> m AlterClientQuotasRequest
decodeAlterClientQuotasRequest version
  | version == 0 =
    do
      fieldentries <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntryData
      fieldvalidateonly <- deserialize
      pure AlterClientQuotasRequest
        {
        alterClientQuotasRequestEntries = fieldentries
        ,
        alterClientQuotasRequestValidateOnly = fieldvalidateonly
        }

  | version == 1 =
    do
      fieldentries <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeEntryData
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterClientQuotasRequest
        {
        alterClientQuotasRequestEntries = fieldentries
        ,
        alterClientQuotasRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version