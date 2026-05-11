{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateTopicsRequest
Description : Kafka CreateTopicsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 19.



Valid versions: 2-7
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateTopicsRequest
  (
    CreateTopicsRequest(..),
    CreatableTopic(..),
    CreatableReplicaAssignment(..),
    CreatableTopicConfig(..),
    maxCreateTopicsRequestVersion
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


-- | The manual partition assignment, or the empty array if we are using automatic assignment.
data CreatableReplicaAssignment = CreatableReplicaAssignment
  {

  -- | The partition index.

  -- Versions: 0+
  creatableReplicaAssignmentPartitionIndex :: !(Int32)
,

  -- | The brokers to place the partition on.

  -- Versions: 0+
  creatableReplicaAssignmentBrokerIds :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | The custom topic configurations to set.
data CreatableTopicConfig = CreatableTopicConfig
  {

  -- | The configuration name.

  -- Versions: 0+
  creatableTopicConfigName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 0+
  creatableTopicConfigValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The topics to create.
data CreatableTopic = CreatableTopic
  {

  -- | The topic name.

  -- Versions: 0+
  creatableTopicName :: !(KafkaString)
,

  -- | The number of partitions to create in the topic, or -1 if we are either specifying a manual partitio

  -- Versions: 0+
  creatableTopicNumPartitions :: !(Int32)
,

  -- | The number of replicas to create for each partition in the topic, or -1 if we are either specifying 

  -- Versions: 0+
  creatableTopicReplicationFactor :: !(Int16)
,

  -- | The manual partition assignment, or the empty array if we are using automatic assignment.

  -- Versions: 0+
  creatableTopicAssignments :: !(KafkaArray (CreatableReplicaAssignment))
,

  -- | The custom topic configurations to set.

  -- Versions: 0+
  creatableTopicConfigs :: !(KafkaArray (CreatableTopicConfig))

  }
  deriving (Eq, Show, Generic)


data CreateTopicsRequest = CreateTopicsRequest
  {

  -- | The topics to create.

  -- Versions: 0+
  createTopicsRequestTopics :: !(KafkaArray (CreatableTopic))
,

  -- | How long to wait in milliseconds before timing out the request.

  -- Versions: 0+
  createTopicsRequesttimeoutMs :: !(Int32)
,

  -- | If true, check that the topics can be created as specified, but don't create anything.

  -- Versions: 1+
  createTopicsRequestvalidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateTopicsRequest.
maxCreateTopicsRequestVersion :: Int16
maxCreateTopicsRequestVersion = 7

-- | KafkaMessage instance for CreateTopicsRequest.
instance KafkaMessage CreateTopicsRequest where
  messageApiKey = 19
  messageMinVersion = 2
  messageMaxVersion = 7
  messageFlexibleVersion = Just 5

-- | Worst-case wire size of a CreatableReplicaAssignment.
wireMaxSizeCreatableReplicaAssignment :: Int -> CreatableReplicaAssignment -> Int
wireMaxSizeCreatableReplicaAssignment _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (creatableReplicaAssignmentBrokerIds msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatableReplicaAssignment.
wirePokeCreatableReplicaAssignment :: Int -> Ptr Word8 -> CreatableReplicaAssignment -> IO (Ptr Word8)
wirePokeCreatableReplicaAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (creatableReplicaAssignmentPartitionIndex msg)
  p2 <- WP.pokeVersionedArray version 5 W.pokeInt32BE p1 (creatableReplicaAssignmentBrokerIds msg)
  if version >= 5 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for CreatableReplicaAssignment.
wirePeekCreatableReplicaAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableReplicaAssignment, Ptr Word8)
wirePeekCreatableReplicaAssignment version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_brokerids, p2) <- WP.peekVersionedArray version 5 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (CreatableReplicaAssignment { creatableReplicaAssignmentPartitionIndex = f0_partitionindex, creatableReplicaAssignmentBrokerIds = f1_brokerids }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCreatableReplicaAssignment :: CreatableReplicaAssignment
defaultCreatableReplicaAssignment = CreatableReplicaAssignment { creatableReplicaAssignmentPartitionIndex = 0, creatableReplicaAssignmentBrokerIds = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a CreatableTopicConfig.
wireMaxSizeCreatableTopicConfig :: Int -> CreatableTopicConfig -> Int
wireMaxSizeCreatableTopicConfig _version msg =
  0
  + WP.dualStringMaxSize (creatableTopicConfigName msg)
  + WP.dualStringMaxSize (creatableTopicConfigValue msg)
  + 1

-- | Direct-poke encoder for CreatableTopicConfig.
wirePokeCreatableTopicConfig :: Int -> Ptr Word8 -> CreatableTopicConfig -> IO (Ptr Word8)
wirePokeCreatableTopicConfig version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 5 then WP.pokeCompactString p0 (P.toCompactString (creatableTopicConfigName msg)) else WP.pokeKafkaString p0 (creatableTopicConfigName msg))
  p2 <- (if version >= 5 then WP.pokeCompactString p1 (P.toCompactString (creatableTopicConfigValue msg)) else WP.pokeKafkaString p1 (creatableTopicConfigValue msg))
  if version >= 5 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for CreatableTopicConfig.
wirePeekCreatableTopicConfig :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableTopicConfig, Ptr Word8)
wirePeekCreatableTopicConfig version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_value, p2) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (CreatableTopicConfig { creatableTopicConfigName = f0_name, creatableTopicConfigValue = f1_value }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCreatableTopicConfig :: CreatableTopicConfig
defaultCreatableTopicConfig = CreatableTopicConfig { creatableTopicConfigName = P.KafkaString Null, creatableTopicConfigValue = P.KafkaString Null }

-- | Worst-case wire size of a CreatableTopic.
wireMaxSizeCreatableTopic :: Int -> CreatableTopic -> Int
wireMaxSizeCreatableTopic _version msg =
  0
  + WP.dualStringMaxSize (creatableTopicName msg)
  + 4
  + 2
  + (5 + (case P.unKafkaArray (creatableTopicAssignments msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableReplicaAssignment _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (creatableTopicConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableTopicConfig _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatableTopic.
wirePokeCreatableTopic :: Int -> Ptr Word8 -> CreatableTopic -> IO (Ptr Word8)
wirePokeCreatableTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 5 then WP.pokeCompactString p0 (P.toCompactString (creatableTopicName msg)) else WP.pokeKafkaString p0 (creatableTopicName msg))
  p2 <- W.pokeInt32BE p1 (creatableTopicNumPartitions msg)
  p3 <- W.pokeInt16BE p2 (creatableTopicReplicationFactor msg)
  p4 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableReplicaAssignment version p x) p3 (creatableTopicAssignments msg)
  p5 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableTopicConfig version p x) p4 (creatableTopicConfigs msg)
  if version >= 5 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for CreatableTopic.
wirePeekCreatableTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableTopic, Ptr Word8)
wirePeekCreatableTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_numpartitions, p2) <- W.peekInt32BE p1 endPtr
  (f2_replicationfactor, p3) <- W.peekInt16BE p2 endPtr
  (f3_assignments, p4) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableReplicaAssignment version _fp _basePtr p e) p3 endPtr
  (f4_configs, p5) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableTopicConfig version _fp _basePtr p e) p4 endPtr
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (CreatableTopic { creatableTopicName = f0_name, creatableTopicNumPartitions = f1_numpartitions, creatableTopicReplicationFactor = f2_replicationfactor, creatableTopicAssignments = f3_assignments, creatableTopicConfigs = f4_configs }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCreatableTopic :: CreatableTopic
defaultCreatableTopic = CreatableTopic { creatableTopicName = P.KafkaString Null, creatableTopicNumPartitions = 0, creatableTopicReplicationFactor = 0, creatableTopicAssignments = P.mkKafkaArray V.empty, creatableTopicConfigs = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a CreateTopicsRequest.
wireMaxSizeCreateTopicsRequest :: Int -> CreateTopicsRequest -> Int
wireMaxSizeCreateTopicsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (createTopicsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1
  + 1

-- | Direct-poke encoder for CreateTopicsRequest.
wirePokeCreateTopicsRequest :: Int -> Ptr Word8 -> CreateTopicsRequest -> IO (Ptr Word8)
wirePokeCreateTopicsRequest version basePtr msg
  | version >= 2 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableTopic version p x) p0 (createTopicsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (createTopicsRequesttimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeWord8 p2 (if (createTopicsRequestvalidateOnly msg) then 1 else 0) else pure p2)
    pure p3
  | version >= 5 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableTopic version p x) p0 (createTopicsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (createTopicsRequesttimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeWord8 p2 (if (createTopicsRequestvalidateOnly msg) then 1 else 0) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke CreateTopicsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateTopicsRequest.
wirePeekCreateTopicsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateTopicsRequest, Ptr Word8)
wirePeekCreateTopicsRequest version _fp _basePtr p0 endPtr
  | version >= 2 && version <= 4 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_validateonly, p3) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pure (CreateTopicsRequest { createTopicsRequestTopics = f0_topics, createTopicsRequesttimeoutMs = f1_timeoutms, createTopicsRequestvalidateOnly = f2_validateonly }, p3)
  | version >= 5 && version <= 7 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_validateonly, p3) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (CreateTopicsRequest { createTopicsRequestTopics = f0_topics, createTopicsRequesttimeoutMs = f1_timeoutms, createTopicsRequestvalidateOnly = f2_validateonly }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateTopicsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec CreateTopicsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateTopicsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateTopicsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateTopicsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}