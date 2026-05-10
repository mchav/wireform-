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
    maxMetadataRequestVersion
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
  p1 <- (if version >= 10 then WP.pokeKafkaUuid p0 (metadataRequestTopicTopicId msg) else pure p0)
  p2 <- (if version >= 9 then WP.pokeCompactString p1 (P.toCompactString (metadataRequestTopicName msg)) else WP.pokeKafkaString p1 (metadataRequestTopicName msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for MetadataRequestTopic.
wirePeekMetadataRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataRequestTopic, Ptr Word8)
wirePeekMetadataRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- (if version >= 10 then WP.peekKafkaUuid p0 endPtr else pure (P.nullUuid, p0))
  (f1_name, p2) <- (if version >= 9 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (MetadataRequestTopic { metadataRequestTopicTopicId = f0_topicid, metadataRequestTopicName = f1_name }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultMetadataRequestTopic :: MetadataRequestTopic
defaultMetadataRequestTopic = MetadataRequestTopic { metadataRequestTopicTopicId = P.nullUuid, metadataRequestTopicName = P.KafkaString Null }

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
    p2 <- (if version >= 4 then W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0) else pure p1)
    p3 <- (if version >= 8 && version <= 10 then W.pokeWord8 p2 (if (metadataRequestIncludeClusterAuthorizedOperations msg) then 1 else 0) else pure p2)
    p4 <- (if version >= 8 then W.pokeWord8 p3 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0) else pure p3)
    pure p4
  | version >= 9 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- (if version >= 4 then W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0) else pure p1)
    p3 <- (if version >= 8 && version <= 10 then W.pokeWord8 p2 (if (metadataRequestIncludeClusterAuthorizedOperations msg) then 1 else 0) else pure p2)
    p4 <- (if version >= 8 then W.pokeWord8 p3 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | version >= 11 && version <= 13 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- (if version >= 4 then W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0) else pure p1)
    p3 <- (if version >= 8 then W.pokeWord8 p2 (if (metadataRequestIncludeTopicAuthorizedOperations msg) then 1 else 0) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | version >= 0 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    pure p1
  | version >= 4 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 9 (\p x -> wirePokeMetadataRequestTopic version p x) p0 (metadataRequestTopics msg)
    p2 <- (if version >= 4 then W.pokeWord8 p1 (if (metadataRequestAllowAutoTopicCreation msg) then 1 else 0) else pure p1)
    pure p2
  | otherwise = error $ "wirePoke MetadataRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for MetadataRequest.
wirePeekMetadataRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataRequest, Ptr Word8)
wirePeekMetadataRequest version _fp _basePtr p0 endPtr
  | version == 8 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (if version >= 4 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_includeclusterauthorizedoperations, p3) <- (if version >= 8 && version <= 10 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    (f3_includetopicauthorizedoperations, p4) <- (if version >= 8 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr else pure (False, p3))
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = f2_includeclusterauthorizedoperations, metadataRequestIncludeTopicAuthorizedOperations = f3_includetopicauthorizedoperations }, p4)
  | version >= 9 && version <= 10 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (if version >= 4 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_includeclusterauthorizedoperations, p3) <- (if version >= 8 && version <= 10 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    (f3_includetopicauthorizedoperations, p4) <- (if version >= 8 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr else pure (False, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = f2_includeclusterauthorizedoperations, metadataRequestIncludeTopicAuthorizedOperations = f3_includetopicauthorizedoperations }, pTagsEnd)
  | version >= 11 && version <= 13 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (if version >= 4 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_includetopicauthorizedoperations, p3) <- (if version >= 8 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = f1_allowautotopiccreation, metadataRequestIncludeClusterAuthorizedOperations = False, metadataRequestIncludeTopicAuthorizedOperations = f2_includetopicauthorizedoperations }, pTagsEnd)
  | version >= 0 && version <= 3 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    pure (MetadataRequest { metadataRequestTopics = f0_topics, metadataRequestAllowAutoTopicCreation = False, metadataRequestIncludeClusterAuthorizedOperations = False, metadataRequestIncludeTopicAuthorizedOperations = False }, p1)
  | version >= 4 && version <= 7 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 9 (\p e -> wirePeekMetadataRequestTopic version _fp _basePtr p e) p0 endPtr
    (f1_allowautotopiccreation, p2) <- (if version >= 4 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
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