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
    maxDescribeClientQuotasResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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

-- | KafkaMessage instance for DescribeClientQuotasResponse.
instance KafkaMessage DescribeClientQuotasResponse where
  messageApiKey = 48
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

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

-- | Worst-case wire size of a ValueData.
wireMaxSizeValueData :: Int -> ValueData -> Int
wireMaxSizeValueData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (valueDataKey msg))
  + 8
  + 1

-- | Direct-poke encoder for ValueData.
wirePokeValueData :: Int -> Ptr Word8 -> ValueData -> IO (Ptr Word8)
wirePokeValueData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (valueDataKey msg))
  p2 <- W.pokeFloat64BE p1 (valueDataValue msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ValueData.
wirePeekValueData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ValueData, Ptr Word8)
wirePeekValueData version _fp _basePtr p0 endPtr = do
  (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- W.peekFloat64BE p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ValueData { valueDataKey = f0_key, valueDataValue = f1_value }, pTagsEnd)

-- | Worst-case wire size of a EntryData.
wireMaxSizeEntryData :: Int -> EntryData -> Int
wireMaxSizeEntryData _version msg =
  0
  + (5 + (case P.unKafkaArray (entryDataEntity msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntityData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (entryDataValues msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeValueData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for EntryData.
wirePokeEntryData :: Int -> Ptr Word8 -> EntryData -> IO (Ptr Word8)
wirePokeEntryData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeEntityData version p x) p0 (entryDataEntity msg)
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeValueData version p x) p1 (entryDataValues msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for EntryData.
wirePeekEntryData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EntryData, Ptr Word8)
wirePeekEntryData version _fp _basePtr p0 endPtr = do
  (f0_entity, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekEntityData version _fp _basePtr p e) p0 endPtr
  (f1_values, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekValueData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (EntryData { entryDataEntity = f0_entity, entryDataValues = f1_values }, pTagsEnd)

-- | Worst-case wire size of a DescribeClientQuotasResponse.
wireMaxSizeDescribeClientQuotasResponse :: Int -> DescribeClientQuotasResponse -> Int
wireMaxSizeDescribeClientQuotasResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeClientQuotasResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (describeClientQuotasResponseEntries msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEntryData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeClientQuotasResponse.
wirePokeDescribeClientQuotasResponse :: Int -> Ptr Word8 -> DescribeClientQuotasResponse -> IO (Ptr Word8)
wirePokeDescribeClientQuotasResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeClientQuotasResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeClientQuotasResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (describeClientQuotasResponseErrorMessage msg))
    p4 <- WP.pokeVersionedNullableArray version 1 (\p x -> wirePokeEntryData version p x) p3 (describeClientQuotasResponseEntries msg)
    pure p4
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeClientQuotasResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeClientQuotasResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (describeClientQuotasResponseErrorMessage msg))
    p4 <- WP.pokeVersionedNullableArray version 1 (\p x -> wirePokeEntryData version p x) p3 (describeClientQuotasResponseEntries msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke DescribeClientQuotasResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeClientQuotasResponse.
wirePeekDescribeClientQuotasResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeClientQuotasResponse, Ptr Word8)
wirePeekDescribeClientQuotasResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_entries, p4) <- WP.peekVersionedNullableArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p3 endPtr
    pure (DescribeClientQuotasResponse { describeClientQuotasResponseThrottleTimeMs = f0_throttletimems, describeClientQuotasResponseErrorCode = f1_errorcode, describeClientQuotasResponseErrorMessage = f2_errormessage, describeClientQuotasResponseEntries = f3_entries }, p4)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_entries, p4) <- WP.peekVersionedNullableArray version 1 (\p e -> wirePeekEntryData version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DescribeClientQuotasResponse { describeClientQuotasResponseThrottleTimeMs = f0_throttletimems, describeClientQuotasResponseErrorCode = f1_errorcode, describeClientQuotasResponseErrorMessage = f2_errormessage, describeClientQuotasResponseEntries = f3_entries }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeClientQuotasResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeClientQuotasResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeClientQuotasResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeClientQuotasResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeClientQuotasResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}