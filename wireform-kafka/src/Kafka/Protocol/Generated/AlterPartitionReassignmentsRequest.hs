{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
Description : Kafka AlterPartitionReassignmentsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 45.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
  (
    AlterPartitionReassignmentsRequest(..),
    ReassignableTopic(..),
    ReassignablePartition(..),
    maxAlterPartitionReassignmentsRequestVersion
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


-- | The partitions to reassign.
data ReassignablePartition = ReassignablePartition
  {

  -- | The partition index.

  -- Versions: 0+
  reassignablePartitionPartitionIndex :: !(Int32)
,

  -- | The replicas to place the partitions on, or null to cancel a pending reassignment for this partition

  -- Versions: 0+
  reassignablePartitionReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | The topics to reassign.
data ReassignableTopic = ReassignableTopic
  {

  -- | The topic name.

  -- Versions: 0+
  reassignableTopicName :: !(KafkaString)
,

  -- | The partitions to reassign.

  -- Versions: 0+
  reassignableTopicPartitions :: !(KafkaArray (ReassignablePartition))

  }
  deriving (Eq, Show, Generic)


data AlterPartitionReassignmentsRequest = AlterPartitionReassignmentsRequest
  {

  -- | The time in ms to wait for the request to complete.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTimeoutMs :: !(Int32)
,

  -- | The option indicating whether changing the replication factor of any given partition as part of this

  -- Versions: 1+
  alterPartitionReassignmentsRequestAllowReplicationFactorChange :: !(Bool)
,

  -- | The topics to reassign.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTopics :: !(KafkaArray (ReassignableTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionReassignmentsRequest.
maxAlterPartitionReassignmentsRequestVersion :: Int16
maxAlterPartitionReassignmentsRequestVersion = 1

-- | KafkaMessage instance for AlterPartitionReassignmentsRequest.
instance KafkaMessage AlterPartitionReassignmentsRequest where
  messageApiKey = 45
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a ReassignablePartition.
wireMaxSizeReassignablePartition :: Int -> ReassignablePartition -> Int
wireMaxSizeReassignablePartition _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (reassignablePartitionReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReassignablePartition.
wirePokeReassignablePartition :: Int -> Ptr Word8 -> ReassignablePartition -> IO (Ptr Word8)
wirePokeReassignablePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (reassignablePartitionPartitionIndex msg)
  p2 <- WP.pokeVersionedNullableArray version 0 W.pokeInt32BE p1 (reassignablePartitionReplicas msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReassignablePartition.
wirePeekReassignablePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignablePartition, Ptr Word8)
wirePeekReassignablePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicas, p2) <- WP.peekVersionedNullableArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReassignablePartition { reassignablePartitionPartitionIndex = f0_partitionindex, reassignablePartitionReplicas = f1_replicas }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReassignablePartition :: ReassignablePartition
defaultReassignablePartition = ReassignablePartition { reassignablePartitionPartitionIndex = 0, reassignablePartitionReplicas = P.KafkaArray P.Null }

-- | Worst-case wire size of a ReassignableTopic.
wireMaxSizeReassignableTopic :: Int -> ReassignableTopic -> Int
wireMaxSizeReassignableTopic _version msg =
  0
  + WP.dualStringMaxSize (reassignableTopicName msg)
  + (5 + (case P.unKafkaArray (reassignableTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignablePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReassignableTopic.
wirePokeReassignableTopic :: Int -> Ptr Word8 -> ReassignableTopic -> IO (Ptr Word8)
wirePokeReassignableTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (reassignableTopicName msg)) else WP.pokeKafkaString p0 (reassignableTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignablePartition version p x) p1 (reassignableTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReassignableTopic.
wirePeekReassignableTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignableTopic, Ptr Word8)
wirePeekReassignableTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignablePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReassignableTopic { reassignableTopicName = f0_name, reassignableTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReassignableTopic :: ReassignableTopic
defaultReassignableTopic = ReassignableTopic { reassignableTopicName = P.KafkaString Null, reassignableTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AlterPartitionReassignmentsRequest.
wireMaxSizeAlterPartitionReassignmentsRequest :: Int -> AlterPartitionReassignmentsRequest -> Int
wireMaxSizeAlterPartitionReassignmentsRequest _version msg =
  0
  + 4
  + 1
  + (5 + (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignableTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterPartitionReassignmentsRequest.
wirePokeAlterPartitionReassignmentsRequest :: Int -> Ptr Word8 -> AlterPartitionReassignmentsRequest -> IO (Ptr Word8)
wirePokeAlterPartitionReassignmentsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsRequestTimeoutMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopic version p x) p1 (alterPartitionReassignmentsRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsRequestTimeoutMs msg)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (if (alterPartitionReassignmentsRequestAllowReplicationFactorChange msg) then 1 else 0) else pure p1)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopic version p x) p2 (alterPartitionReassignmentsRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AlterPartitionReassignmentsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterPartitionReassignmentsRequest.
wirePeekAlterPartitionReassignmentsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterPartitionReassignmentsRequest, Ptr Word8)
wirePeekAlterPartitionReassignmentsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterPartitionReassignmentsRequest { alterPartitionReassignmentsRequestTimeoutMs = f0_timeoutms, alterPartitionReassignmentsRequestAllowReplicationFactorChange = False, alterPartitionReassignmentsRequestTopics = f1_topics }, pTagsEnd)
  | version == 1 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_allowreplicationfactorchange, p2) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopic version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AlterPartitionReassignmentsRequest { alterPartitionReassignmentsRequestTimeoutMs = f0_timeoutms, alterPartitionReassignmentsRequestAllowReplicationFactorChange = f1_allowreplicationfactorchange, alterPartitionReassignmentsRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterPartitionReassignmentsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterPartitionReassignmentsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterPartitionReassignmentsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterPartitionReassignmentsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterPartitionReassignmentsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}