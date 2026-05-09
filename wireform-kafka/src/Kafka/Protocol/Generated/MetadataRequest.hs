{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.MetadataRequest
Description : Kafka MetadataRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 3.



Valid versions: 0-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.MetadataRequest
  (
    MetadataRequest(..),
    MetadataRequestTopic(..),
    encodeMetadataRequest,
    decodeMetadataRequest,
    maxMetadataRequestVersion
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


-- | The topics to fetch metadata for.
data MetadataRequestTopic = MetadataRequestTopic
  {

  -- | The topic id.

  -- Versions: 10+
  metadataRequestTopicTopicId :: !(KafkaUuid)
,

  -- | The topic name.

  -- Versions: 0+
  metadataRequestTopicName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataRequestTopic with version-aware field handling.
encodeMetadataRequestTopic :: MonadPut m => E.ApiVersion -> MetadataRequestTopic -> m ()
encodeMetadataRequestTopic version mmsg =
  do
    when (version >= 10) $
      serialize (metadataRequestTopicTopicId mmsg)
    if version >= 9 then serialize (toCompactString (metadataRequestTopicName mmsg)) else serialize (metadataRequestTopicName mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataRequestTopic with version-aware field handling.
decodeMetadataRequestTopic :: MonadGet m => E.ApiVersion -> m MetadataRequestTopic
decodeMetadataRequestTopic version =
  do
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldname <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataRequestTopic
      {
      metadataRequestTopicTopicId = fieldtopicid
      ,
      metadataRequestTopicName = fieldname
      }



data MetadataRequest = MetadataRequest
  {

  -- | The topics to fetch metadata for.

  -- Versions: 0+
  metadataRequestTopics :: !(KafkaArray (MetadataRequestTopic))
,

  -- | If this is true, the broker may auto-create topics that we requested which do not already exist, if 

  -- Versions: 4+
  metadataRequestAllowAutoTopicCreation :: !(Bool)
,

  -- | Whether to include cluster authorized operations.

  -- Versions: 8-10
  metadataRequestIncludeClusterAuthorizedOperations :: !(Bool)
,

  -- | Whether to include topic authorized operations.

  -- Versions: 8+
  metadataRequestIncludeTopicAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for MetadataRequest.
maxMetadataRequestVersion :: Int16
maxMetadataRequestVersion = 13

-- | KafkaMessage instance for MetadataRequest.
instance KafkaMessage MetadataRequest where
  messageApiKey = 3
  messageMinVersion = 0
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode MetadataRequest with the given API version.
encodeMetadataRequest :: MonadPut m => E.ApiVersion -> MetadataRequest -> m ()
encodeMetadataRequest version msg
  | version == 8 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeClusterAuthorizedOperations msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)


  | version >= 9 && version <= 10 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeClusterAuthorizedOperations msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 11 && version <= 13 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)
      serialize (metadataRequestIncludeTopicAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 3 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)


  | version >= 4 && version <= 7 =
    do
      E.encodeVersionedNullableArray version 9 encodeMetadataRequestTopic (metadataRequestTopics msg)
      serialize (metadataRequestAllowAutoTopicCreation msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode MetadataRequest with the given API version.
decodeMetadataRequest :: MonadGet m => E.ApiVersion -> m MetadataRequest
decodeMetadataRequest version
  | version == 8 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 9 && version <= 10 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 11 && version <= 13 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      fieldincludetopicauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = fieldincludetopicauthorizedoperations
        }

  | version >= 0 && version <= 3 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = True
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = False
        }

  | version >= 4 && version <= 7 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 9 decodeMetadataRequestTopic
      fieldallowautotopiccreation <- deserialize
      pure MetadataRequest
        {
        metadataRequestTopics = fieldtopics
        ,
        metadataRequestAllowAutoTopicCreation = fieldallowautotopiccreation
        ,
        metadataRequestIncludeClusterAuthorizedOperations = False
        ,
        metadataRequestIncludeTopicAuthorizedOperations = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a MetadataRequestTopic.
wireMaxSizeMetadataRequestTopic :: Int -> MetadataRequestTopic -> Int
wireMaxSizeMetadataRequestTopic _version msg =
  0
  + 16
  + WP.compactStringMaxSize (P.toCompactString (metadataRequestTopicName msg))
  + 1

-- | Direct-poke encoder for MetadataRequestTopic.
wirePokeMetadataRequestTopic :: Int -> Ptr Word8 -> MetadataRequestTopic -> IO (Ptr Word8)
wirePokeMetadataRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (metadataRequestTopicTopicId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (metadataRequestTopicName msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for MetadataRequestTopic.
wirePeekMetadataRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataRequestTopic, Ptr Word8)
wirePeekMetadataRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_name, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (MetadataRequestTopic { metadataRequestTopicTopicId = f0_topicid, metadataRequestTopicName = f1_name }, pTagsEnd)

-- | Worst-case wire size of a MetadataRequest.
wireMaxSizeMetadataRequest :: Int -> MetadataRequest -> Int
wireMaxSizeMetadataRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (metadataRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMetadataRequestTopic _version x ) v); P.Null -> 0 }))
  + 1
  + 1
  + 1
  + 1

-- | Direct-poke encoder for MetadataRequest.
wirePokeMetadataRequest :: Int -> Ptr Word8 -> MetadataRequest -> IO (Ptr Word8)
wirePokeMetadataRequest version basePtr msg
  | version == 8 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0)
    p3 <- W.pokeWord8 p2 (if (metadataRequestIncludeClusterAuthorizedOperations msg) then 1 else 0)
    p4 <- W.pokeWord8 p3 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0)
    pure p4
  | version >= 9 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0)
    p3 <- W.pokeWord8 p2 (if (metadataRequestIncludeClusterAuthorizedOperations msg) then 1 else 0)
    p4 <- W.pokeWord8 p3 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p4
  | version >= 11 && version <= 13 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0)
    p3 <- W.pokeWord8 p2 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p3
  | version >= 0 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    pure p1
  | version >= 4 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0)
    pure p2
  | otherwise = error $ "wirePoke MetadataRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for MetadataRequest.
wirePeekMetadataRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataRequest, Ptr Word8)
wirePeekMetadataRequest version _fp _basePtr p0 endPtr
  | version == 8 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    (f2_includeclusterauthorizedoperations, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    (f3_includetopicauthorizedoperations, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = f2_includeclusterauthorizedoperations, metadataRequestIncludeTopicAuthorizedOperations = f3_includetopicauthorizedoperations }, p4)
  | version >= 9 && version <= 10 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    (f2_includeclusterauthorizedoperations, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    (f3_includetopicauthorizedoperations, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = f2_includeclusterauthorizedoperations, metadataRequestIncludeTopicAuthorizedOperations = f3_includetopicauthorizedoperations }, pTagsEnd)
  | version >= 11 && version <= 13 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    (f2_includetopicauthorizedoperations, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = False, metadataRequestIncludeTopicAuthorizedOperations = f2_includetopicauthorizedoperations }, pTagsEnd)
  | version >= 0 && version <= 3 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = False, metadataRequestIncludeClusterAuthorizedOperations = False, metadataRequestIncludeTopicAuthorizedOperations = False }, p1)
  | version >= 4 && version <= 7 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = False, metadataRequestIncludeTopicAuthorizedOperations = False }, p2)
  | otherwise = error $ "wirePeek MetadataRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec MetadataRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeMetadataRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeMetadataRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekMetadataRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}