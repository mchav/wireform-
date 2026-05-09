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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | KafkaMessage instance for AlterClientQuotasRequest.
instance KafkaMessage AlterClientQuotasRequest where
  messageApiKey = 49
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

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

-- | Worst-case wire size of a EntityData.
wireMaxSizeEntityData :: Int -> EntityData -> Int
wireMaxSizeEntityData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (entityDataEntityType msg))
  + WP.compactStringMaxSize (P.toCompactString (entityDataEntityName msg))
  + 1

-- | Direct-poke encoder for EntityData.
wirePokeEntityData :: Int -> Ptr Word8 -> EntityData -> IO (Ptr Word8)
wirePokeEntityData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (entityDataEntityType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (entityDataEntityName msg))
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for EntityData.
wirePeekEntityData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EntityData, Ptr Word8)
wirePeekEntityData version _fp _basePtr p0 endPtr = do
  (f0_entitytype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_entityname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (EntityData { entityDataEntityType = f0_entitytype, entityDataEntityName = f1_entityname }, pTagsEnd)

-- | Worst-case wire size of a OpData.
wireMaxSizeOpData :: Int -> OpData -> Int
wireMaxSizeOpData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (opDataKey msg))
  + 8
  + 1
  + 1

-- | Direct-poke encoder for OpData.
wirePokeOpData :: Int -> Ptr Word8 -> OpData -> IO (Ptr Word8)
wirePokeOpData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (opDataKey msg))
  p2 <- W.pokeFloat64BE p1 (opDataValue msg)
  p3 <- W.pokeWord8 p2 (if (opDataRemove msg) then 1 else 0)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OpData.
wirePeekOpData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OpData, Ptr Word8)
wirePeekOpData version _fp _basePtr p0 endPtr = do
  (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- W.peekFloat64BE p1 endPtr
  (f2_remove, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OpData { opDataKey = f0_key, opDataValue = f1_value, opDataRemove = f2_remove }, pTagsEnd)

-- | Worst-case wire size of a EntryData.
wireMaxSizeEntryData :: Int -> EntryData -> Int
wireMaxSizeEntryData _version msg =
  0
  + (5 + (case P.unKafkaArray (entryDataEntity msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntityData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (entryDataOps msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOpData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for EntryData.
wirePokeEntryData :: Int -> Ptr Word8 -> EntryData -> IO (Ptr Word8)
wirePokeEntryData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntityData version p x) p0 (entryDataEntity msg)
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeOpData version p x) p1 (entryDataOps msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for EntryData.
wirePeekEntryData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EntryData, Ptr Word8)
wirePeekEntryData version _fp _basePtr p0 endPtr = do
  (f0_entity, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntityData version _fp _basePtr p e) p0 endPtr
  (f1_ops, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekOpData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (EntryData { entryDataEntity = f0_entity, entryDataOps = f1_ops }, pTagsEnd)

-- | Worst-case wire size of a AlterClientQuotasRequest.
wireMaxSizeAlterClientQuotasRequest :: Int -> AlterClientQuotasRequest -> Int
wireMaxSizeAlterClientQuotasRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (alterClientQuotasRequestEntries msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntryData _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for AlterClientQuotasRequest.
wirePokeAlterClientQuotasRequest :: Int -> Ptr Word8 -> AlterClientQuotasRequest -> IO (Ptr Word8)
wirePokeAlterClientQuotasRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntryData version p x) p0 (alterClientQuotasRequestEntries msg)
    p2 <- W.pokeWord8 p1 (if (alterClientQuotasRequestValidateOnly msg) then 1 else 0)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntryData version p x) p0 (alterClientQuotasRequestEntries msg)
    p2 <- W.pokeWord8 p1 (if (alterClientQuotasRequestValidateOnly msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AlterClientQuotasRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterClientQuotasRequest.
wirePeekAlterClientQuotasRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterClientQuotasRequest, Ptr Word8)
wirePeekAlterClientQuotasRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_entries, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pure (AlterClientQuotasRequest { alterClientQuotasRequestEntries = f0_entries, alterClientQuotasRequestValidateOnly = f1_validateonly }, p2)
  | version == 1 = do
    (f0_entries, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterClientQuotasRequest { alterClientQuotasRequestEntries = f0_entries, alterClientQuotasRequestValidateOnly = f1_validateonly }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterClientQuotasRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AlterClientQuotasRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterClientQuotasRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterClientQuotasRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterClientQuotasRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}