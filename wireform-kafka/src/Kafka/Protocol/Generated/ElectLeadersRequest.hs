{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ElectLeadersRequest
Description : Kafka ElectLeadersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 43.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ElectLeadersRequest
  (
    ElectLeadersRequest(..),
    TopicPartitions(..),
    maxElectLeadersRequestVersion
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


-- | The topic partitions to elect leaders.
data TopicPartitions = TopicPartitions
  {

  -- | The name of a topic.

  -- Versions: 0+
  topicPartitionsTopic :: !(KafkaString)
,

  -- | The partitions of this topic whose leader should be elected.

  -- Versions: 0+
  topicPartitionsPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


data ElectLeadersRequest = ElectLeadersRequest
  {

  -- | Type of elections to conduct for the partition. A value of '0' elects the preferred replica. A value

  -- Versions: 1+
  electLeadersRequestElectionType :: !(Int8)
,

  -- | The topic partitions to elect leaders.

  -- Versions: 0+
  electLeadersRequestTopicPartitions :: !(KafkaArray (TopicPartitions))
,

  -- | The time in ms to wait for the election to complete.

  -- Versions: 0+
  electLeadersRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ElectLeadersRequest.
maxElectLeadersRequestVersion :: Int16
maxElectLeadersRequestVersion = 2

-- | KafkaMessage instance for ElectLeadersRequest.
instance KafkaMessage ElectLeadersRequest where
  messageApiKey = 43
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a TopicPartitions.
wireMaxSizeTopicPartitions :: Int -> TopicPartitions -> Int
wireMaxSizeTopicPartitions _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicPartitionsTopic msg))
  + (5 + (case P.unKafkaArray (topicPartitionsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicPartitions.
wirePokeTopicPartitions :: Int -> Ptr Word8 -> TopicPartitions -> IO (Ptr Word8)
wirePokeTopicPartitions version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (topicPartitionsTopic msg)) else WP.pokeKafkaString p0 (topicPartitionsTopic msg))
  p2 <- WP.pokeVersionedArray version 2 W.pokeInt32BE p1 (topicPartitionsPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicPartitions.
wirePeekTopicPartitions :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicPartitions, Ptr Word8)
wirePeekTopicPartitions version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicPartitions { topicPartitionsTopic = f0_topic, topicPartitionsPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicPartitions :: TopicPartitions
defaultTopicPartitions = TopicPartitions { topicPartitionsTopic = P.KafkaString Null, topicPartitionsPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ElectLeadersRequest.
wireMaxSizeElectLeadersRequest :: Int -> ElectLeadersRequest -> Int
wireMaxSizeElectLeadersRequest _version msg =
  0
  + 1
  + (5 + (case P.unKafkaArray (electLeadersRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicPartitions _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for ElectLeadersRequest.
wirePokeElectLeadersRequest :: Int -> Ptr Word8 -> ElectLeadersRequest -> IO (Ptr Word8)
wirePokeElectLeadersRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeTopicPartitions version p x) p0 (electLeadersRequestTopicPartitions msg)
    p2 <- W.pokeInt32BE p1 (electLeadersRequestTimeoutMs msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeWord8 p0 (fromIntegral (electLeadersRequestElectionType msg)) else pure p0)
    p2 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeTopicPartitions version p x) p1 (electLeadersRequestTopicPartitions msg)
    p3 <- W.pokeInt32BE p2 (electLeadersRequestTimeoutMs msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeWord8 p0 (fromIntegral (electLeadersRequestElectionType msg)) else pure p0)
    p2 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeTopicPartitions version p x) p1 (electLeadersRequestTopicPartitions msg)
    p3 <- W.pokeInt32BE p2 (electLeadersRequestTimeoutMs msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ElectLeadersRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ElectLeadersRequest.
wirePeekElectLeadersRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ElectLeadersRequest, Ptr Word8)
wirePeekElectLeadersRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_topicpartitions, p1) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekTopicPartitions version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pure (ElectLeadersRequest { electLeadersRequestElectionType = 0, electLeadersRequestTopicPartitions = f0_topicpartitions, electLeadersRequestTimeoutMs = f1_timeoutms }, p2)
  | version == 1 = do
    (f0_electiontype, p1) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr else pure (0, p0))
    (f1_topicpartitions, p2) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekTopicPartitions version _fp _basePtr p e) p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    pure (ElectLeadersRequest { electLeadersRequestElectionType = f0_electiontype, electLeadersRequestTopicPartitions = f1_topicpartitions, electLeadersRequestTimeoutMs = f2_timeoutms }, p3)
  | version == 2 = do
    (f0_electiontype, p1) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr else pure (0, p0))
    (f1_topicpartitions, p2) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekTopicPartitions version _fp _basePtr p e) p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ElectLeadersRequest { electLeadersRequestElectionType = f0_electiontype, electLeadersRequestTopicPartitions = f1_topicpartitions, electLeadersRequestTimeoutMs = f2_timeoutms }, pTagsEnd)
  | otherwise = error $ "wirePeek ElectLeadersRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ElectLeadersRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeElectLeadersRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeElectLeadersRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekElectLeadersRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}