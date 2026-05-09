{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateTopicsResponse
Description : Kafka CreateTopicsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 19.



Valid versions: 2-7
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateTopicsResponse
  (
    CreateTopicsResponse(..),
    CreatableTopicResult(..),
    CreatableTopicConfigs(..),
    encodeCreateTopicsResponse,
    decodeCreateTopicsResponse,
    maxCreateTopicsResponseVersion
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


-- | Configuration of the topic.
data CreatableTopicConfigs = CreatableTopicConfigs
  {

  -- | The configuration name.

  -- Versions: 5+
  creatableTopicConfigsName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 5+
  creatableTopicConfigsValue :: !(KafkaString)
,

  -- | True if the configuration is read-only.

  -- Versions: 5+
  creatableTopicConfigsReadOnly :: !(Bool)
,

  -- | The configuration source.

  -- Versions: 5+
  creatableTopicConfigsConfigSource :: !(Int8)
,

  -- | True if this configuration is sensitive.

  -- Versions: 5+
  creatableTopicConfigsIsSensitive :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableTopicConfigs with version-aware field handling.
encodeCreatableTopicConfigs :: MonadPut m => E.ApiVersion -> CreatableTopicConfigs -> m ()
encodeCreatableTopicConfigs version cmsg =
  do
    when (version >= 5) $
      if version >= 5 then serialize (toCompactString (creatableTopicConfigsName cmsg)) else serialize (creatableTopicConfigsName cmsg)
    when (version >= 5) $
      if version >= 5 then serialize (toCompactString (creatableTopicConfigsValue cmsg)) else serialize (creatableTopicConfigsValue cmsg)
    when (version >= 5) $
      serialize (creatableTopicConfigsReadOnly cmsg)
    when (version >= 5) $
      serialize (creatableTopicConfigsConfigSource cmsg)
    when (version >= 5) $
      serialize (creatableTopicConfigsIsSensitive cmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatableTopicConfigs with version-aware field handling.
decodeCreatableTopicConfigs :: MonadGet m => E.ApiVersion -> m CreatableTopicConfigs
decodeCreatableTopicConfigs version =
  do
    fieldname <- if version >= 5
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldvalue <- if version >= 5
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldreadonly <- if version >= 5
      then deserialize
      else pure (False)
    fieldconfigsource <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldissensitive <- if version >= 5
      then deserialize
      else pure (False)
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatableTopicConfigs
      {
      creatableTopicConfigsName = fieldname
      ,
      creatableTopicConfigsValue = fieldvalue
      ,
      creatableTopicConfigsReadOnly = fieldreadonly
      ,
      creatableTopicConfigsConfigSource = fieldconfigsource
      ,
      creatableTopicConfigsIsSensitive = fieldissensitive
      }


-- | Results for each topic we tried to create.
data CreatableTopicResult = CreatableTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  creatableTopicResultName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 7+
  creatableTopicResultTopicId :: !(KafkaUuid)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  creatableTopicResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 1+
  creatableTopicResultErrorMessage :: !(KafkaString)
,

  -- | Optional topic config error returned if configs are not returned in the response.

  -- Versions: 5+
  creatableTopicResultTopicConfigErrorCode :: !(Int16)
,

  -- | Number of partitions of the topic.

  -- Versions: 5+
  creatableTopicResultNumPartitions :: !(Int32)
,

  -- | Replication factor of the topic.

  -- Versions: 5+
  creatableTopicResultReplicationFactor :: !(Int16)
,

  -- | Configuration of the topic.

  -- Versions: 5+
  creatableTopicResultConfigs :: !(KafkaArray (CreatableTopicConfigs))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableTopicResult with version-aware field handling.
encodeCreatableTopicResult :: MonadPut m => E.ApiVersion -> CreatableTopicResult -> m ()
encodeCreatableTopicResult version cmsg =
  do
    if version >= 5 then serialize (toCompactString (creatableTopicResultName cmsg)) else serialize (creatableTopicResultName cmsg)
    when (version >= 7) $
      serialize (creatableTopicResultTopicId cmsg)
    serialize (creatableTopicResultErrorCode cmsg)
    when (version >= 1) $
      if version >= 5 then serialize (toCompactString (creatableTopicResultErrorMessage cmsg)) else serialize (creatableTopicResultErrorMessage cmsg)
    when (version >= 5) $
      serialize (creatableTopicResultNumPartitions cmsg)
    when (version >= 5) $
      serialize (creatableTopicResultReplicationFactor cmsg)
    when (version >= 5) $
      E.encodeVersionedNullableArray version 5 encodeCreatableTopicConfigs (creatableTopicResultConfigs cmsg)
    when (version >= 5) $ do
      let _entries = (if version >= 5 then [(0, Data.Bytes.Put.runPutS (serialize (creatableTopicResultTopicConfigErrorCode cmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode CreatableTopicResult with version-aware field handling.
decodeCreatableTopicResult :: MonadGet m => E.ApiVersion -> m CreatableTopicResult
decodeCreatableTopicResult version =
  do
    fieldname <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- if version >= 7
      then deserialize
      else pure (P.nullUuid)
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 1
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldnumpartitions <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldreplicationfactor <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldconfigs <- if version >= 5
      then E.decodeVersionedNullableArray version 5 decodeCreatableTopicConfigs
      else pure (P.KafkaArray P.Null)
    _taggedFields <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fieldtopicconfigerrorcode =
          if version >= 5
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                  Right _v -> _v
                  Left  _  -> (0)
              Nothing  -> (0)
            else (0)
    pure CreatableTopicResult
      {
      creatableTopicResultName = fieldname
      ,
      creatableTopicResultTopicId = fieldtopicid
      ,
      creatableTopicResultErrorCode = fielderrorcode
      ,
      creatableTopicResultErrorMessage = fielderrormessage
      ,
      creatableTopicResultTopicConfigErrorCode = fieldtopicconfigerrorcode
      ,
      creatableTopicResultNumPartitions = fieldnumpartitions
      ,
      creatableTopicResultReplicationFactor = fieldreplicationfactor
      ,
      creatableTopicResultConfigs = fieldconfigs
      }



data CreateTopicsResponse = CreateTopicsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  createTopicsResponseThrottleTimeMs :: !(Int32)
,

  -- | Results for each topic we tried to create.

  -- Versions: 0+
  createTopicsResponseTopics :: !(KafkaArray (CreatableTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateTopicsResponse.
maxCreateTopicsResponseVersion :: Int16
maxCreateTopicsResponseVersion = 7

-- | KafkaMessage instance for CreateTopicsResponse.
instance KafkaMessage CreateTopicsResponse where
  messageApiKey = 19
  messageMinVersion = 2
  messageMaxVersion = 7
  messageFlexibleVersion = Just 5

-- | Encode CreateTopicsResponse with the given API version.
encodeCreateTopicsResponse :: MonadPut m => E.ApiVersion -> CreateTopicsResponse -> m ()
encodeCreateTopicsResponse version msg
  | version >= 2 && version <= 4 =
    do
      serialize (createTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeCreatableTopicResult (case P.unKafkaArray (createTopicsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 5 && version <= 7 =
    do
      serialize (createTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeCreatableTopicResult (case P.unKafkaArray (createTopicsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateTopicsResponse with the given API version.
decodeCreateTopicsResponse :: MonadGet m => E.ApiVersion -> m CreateTopicsResponse
decodeCreateTopicsResponse version
  | version >= 2 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableTopicResult
      pure CreateTopicsResponse
        {
        createTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createTopicsResponseTopics = fieldtopics
        }

  | version >= 5 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateTopicsResponse
        {
        createTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createTopicsResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a CreatableTopicConfigs.
wireMaxSizeCreatableTopicConfigs :: Int -> CreatableTopicConfigs -> Int
wireMaxSizeCreatableTopicConfigs _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (creatableTopicConfigsName msg))
  + WP.compactStringMaxSize (P.toCompactString (creatableTopicConfigsValue msg))
  + 1
  + 1
  + 1
  + 1

-- | Direct-poke encoder for CreatableTopicConfigs.
wirePokeCreatableTopicConfigs :: Int -> Ptr Word8 -> CreatableTopicConfigs -> IO (Ptr Word8)
wirePokeCreatableTopicConfigs version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (creatableTopicConfigsName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (creatableTopicConfigsValue msg))
  p3 <- W.pokeWord8 p2 (if (creatableTopicConfigsReadOnly msg) then 1 else 0)
  p4 <- W.pokeWord8 p3 (fromIntegral (creatableTopicConfigsConfigSource msg))
  p5 <- W.pokeWord8 p4 (if (creatableTopicConfigsIsSensitive msg) then 1 else 0)
  if version >= 5 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for CreatableTopicConfigs.
wirePeekCreatableTopicConfigs :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableTopicConfigs, Ptr Word8)
wirePeekCreatableTopicConfigs version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_readonly, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
  (f3_configsource, p4) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr
  (f4_issensitive, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (CreatableTopicConfigs { creatableTopicConfigsName = f0_name, creatableTopicConfigsValue = f1_value, creatableTopicConfigsReadOnly = f2_readonly, creatableTopicConfigsConfigSource = f3_configsource, creatableTopicConfigsIsSensitive = f4_issensitive }, pTagsEnd)

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeCreateTopicsResponse' / 'decodeCreateTopicsResponse'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec CreateTopicsResponse where
  wireCodec = Just (WC.serialShimCodec encodeCreateTopicsResponse decodeCreateTopicsResponse)
  {-# INLINE wireCodec #-}