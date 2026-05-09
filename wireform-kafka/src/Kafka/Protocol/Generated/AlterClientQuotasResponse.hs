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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
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

-- | KafkaMessage instance for AlterClientQuotasResponse.
instance KafkaMessage AlterClientQuotasResponse where
  messageApiKey = 49
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

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

-- | Worst-case wire size of a EntryData.
wireMaxSizeEntryData :: Int -> EntryData -> Int
wireMaxSizeEntryData _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (entryDataErrorMessage msg))
  + (5 + (case P.unKafkaArray (entryDataEntity msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntityData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for EntryData.
wirePokeEntryData :: Int -> Ptr Word8 -> EntryData -> IO (Ptr Word8)
wirePokeEntryData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (entryDataErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (entryDataErrorMessage msg))
  p3 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntityData version p x) p2 (entryDataEntity msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for EntryData.
wirePeekEntryData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EntryData, Ptr Word8)
wirePeekEntryData version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_entity, p3) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntityData version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (EntryData { entryDataErrorCode = f0_errorcode, entryDataErrorMessage = f1_errormessage, entryDataEntity = f2_entity }, pTagsEnd)

-- | Worst-case wire size of a AlterClientQuotasResponse.
wireMaxSizeAlterClientQuotasResponse :: Int -> AlterClientQuotasResponse -> Int
wireMaxSizeAlterClientQuotasResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (alterClientQuotasResponseEntries msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntryData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterClientQuotasResponse.
wirePokeAlterClientQuotasResponse :: Int -> Ptr Word8 -> AlterClientQuotasResponse -> IO (Ptr Word8)
wirePokeAlterClientQuotasResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterClientQuotasResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntryData version p x) p1 (alterClientQuotasResponseEntries msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterClientQuotasResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntryData version p x) p1 (alterClientQuotasResponseEntries msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AlterClientQuotasResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterClientQuotasResponse.
wirePeekAlterClientQuotasResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterClientQuotasResponse, Ptr Word8)
wirePeekAlterClientQuotasResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_entries, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p1 endPtr
    pure (AlterClientQuotasResponse { alterClientQuotasResponseThrottleTimeMs = f0_throttletimems, alterClientQuotasResponseEntries = f1_entries }, p2)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_entries, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterClientQuotasResponse { alterClientQuotasResponseThrottleTimeMs = f0_throttletimems, alterClientQuotasResponseEntries = f1_entries }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterClientQuotasResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterClientQuotasResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterClientQuotasResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterClientQuotasResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterClientQuotasResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}