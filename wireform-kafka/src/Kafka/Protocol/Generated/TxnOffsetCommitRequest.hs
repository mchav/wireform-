{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.TxnOffsetCommitRequest
Description : Kafka TxnOffsetCommitRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 28.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.TxnOffsetCommitRequest
  (
    TxnOffsetCommitRequest(..),
    TxnOffsetCommitRequestTopic(..),
    TxnOffsetCommitRequestPartition(..),
    maxTxnOffsetCommitRequestVersion
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


-- | The partitions inside the topic that we want to commit offsets for.
data TxnOffsetCommitRequestPartition = TxnOffsetCommitRequestPartition
  {

  -- | The index of the partition within the topic.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The message offset to be committed.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch of the last consumed record.

  -- Versions: 2+
  txnOffsetCommitRequestPartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | Any associated metadata the client wants to keep.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionCommittedMetadata :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Each topic that we want to commit offsets for.
data TxnOffsetCommitRequestTopic = TxnOffsetCommitRequestTopic
  {

  -- | The topic name.

  -- Versions: 0-5
  txnOffsetCommitRequestTopicName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 6+
  txnOffsetCommitRequestTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions inside the topic that we want to commit offsets for.

  -- Versions: 0+
  txnOffsetCommitRequestTopicPartitions :: !(KafkaArray (TxnOffsetCommitRequestPartition))

  }
  deriving (Eq, Show, Generic)


data TxnOffsetCommitRequest = TxnOffsetCommitRequest
  {

  -- | The ID of the transaction.

  -- Versions: 0+
  txnOffsetCommitRequestTransactionalId :: !(KafkaString)
,

  -- | The ID of the group.

  -- Versions: 0+
  txnOffsetCommitRequestGroupId :: !(KafkaString)
,

  -- | The current producer ID in use by the transactional ID.

  -- Versions: 0+
  txnOffsetCommitRequestProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer ID.

  -- Versions: 0+
  txnOffsetCommitRequestProducerEpoch :: !(Int16)
,

  -- | The generation of the group if using the classic group protocol or the member epoch if using the con

  -- Versions: 3+
  txnOffsetCommitRequestGenerationIdOrMemberEpoch :: !(Int32)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 3+
  txnOffsetCommitRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 3+
  txnOffsetCommitRequestGroupInstanceId :: !(KafkaString)
,

  -- | Each topic that we want to commit offsets for.

  -- Versions: 0+
  txnOffsetCommitRequestTopics :: !(KafkaArray (TxnOffsetCommitRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for TxnOffsetCommitRequest.
maxTxnOffsetCommitRequestVersion :: Int16
maxTxnOffsetCommitRequestVersion = 6

-- | KafkaMessage instance for TxnOffsetCommitRequest.
instance KafkaMessage TxnOffsetCommitRequest where
  messageApiKey = 28
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a TxnOffsetCommitRequestPartition.
wireMaxSizeTxnOffsetCommitRequestPartition :: Int -> TxnOffsetCommitRequestPartition -> Int
wireMaxSizeTxnOffsetCommitRequestPartition _version msg =
  0
  + 4
  + 8
  + 4
  + WP.dualStringMaxSize (txnOffsetCommitRequestPartitionCommittedMetadata msg)
  + 1

-- | Direct-poke encoder for TxnOffsetCommitRequestPartition.
wirePokeTxnOffsetCommitRequestPartition :: Int -> Ptr Word8 -> TxnOffsetCommitRequestPartition -> IO (Ptr Word8)
wirePokeTxnOffsetCommitRequestPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (txnOffsetCommitRequestPartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (txnOffsetCommitRequestPartitionCommittedOffset msg)
  p3 <- (if version >= 2 then W.pokeInt32BE p2 (txnOffsetCommitRequestPartitionCommittedLeaderEpoch msg) else pure p2)
  p4 <- (if version >= 3 then WP.pokeCompactString p3 (P.toCompactString (txnOffsetCommitRequestPartitionCommittedMetadata msg)) else WP.pokeKafkaString p3 (txnOffsetCommitRequestPartitionCommittedMetadata msg))
  if version >= 3 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for TxnOffsetCommitRequestPartition.
wirePeekTxnOffsetCommitRequestPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitRequestPartition, Ptr Word8)
wirePeekTxnOffsetCommitRequestPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_committedoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_committedleaderepoch, p3) <- (if version >= 2 then W.peekInt32BE p2 endPtr else pure (-1, p2))
  (f3_committedmetadata, p4) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (TxnOffsetCommitRequestPartition { txnOffsetCommitRequestPartitionPartitionIndex = f0_partitionindex, txnOffsetCommitRequestPartitionCommittedOffset = f1_committedoffset, txnOffsetCommitRequestPartitionCommittedLeaderEpoch = f2_committedleaderepoch, txnOffsetCommitRequestPartitionCommittedMetadata = f3_committedmetadata }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTxnOffsetCommitRequestPartition :: TxnOffsetCommitRequestPartition
defaultTxnOffsetCommitRequestPartition = TxnOffsetCommitRequestPartition { txnOffsetCommitRequestPartitionPartitionIndex = 0, txnOffsetCommitRequestPartitionCommittedOffset = 0, txnOffsetCommitRequestPartitionCommittedLeaderEpoch = -1, txnOffsetCommitRequestPartitionCommittedMetadata = P.KafkaString Null }

-- | Worst-case wire size of a TxnOffsetCommitRequestTopic.
wireMaxSizeTxnOffsetCommitRequestTopic :: Int -> TxnOffsetCommitRequestTopic -> Int
wireMaxSizeTxnOffsetCommitRequestTopic _version msg =
  0
  + WP.dualStringMaxSize (txnOffsetCommitRequestTopicName msg)
  + 16
  + (5 + (case P.unKafkaArray (txnOffsetCommitRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTxnOffsetCommitRequestPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TxnOffsetCommitRequestTopic.
wirePokeTxnOffsetCommitRequestTopic :: Int -> Ptr Word8 -> TxnOffsetCommitRequestTopic -> IO (Ptr Word8)
wirePokeTxnOffsetCommitRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 5 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (txnOffsetCommitRequestTopicName msg)) else WP.pokeKafkaString p0 (txnOffsetCommitRequestTopicName msg)) else pure p0)
  p2 <- (if version >= 6 then WP.pokeKafkaUuid p1 (txnOffsetCommitRequestTopicTopicId msg) else pure p1)
  p3 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitRequestPartition version p x) p2 (txnOffsetCommitRequestTopicPartitions msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TxnOffsetCommitRequestTopic.
wirePeekTxnOffsetCommitRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitRequestTopic, Ptr Word8)
wirePeekTxnOffsetCommitRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version <= 5 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 6 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitRequestPartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TxnOffsetCommitRequestTopic { txnOffsetCommitRequestTopicName = f0_name, txnOffsetCommitRequestTopicTopicId = f1_topicid, txnOffsetCommitRequestTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTxnOffsetCommitRequestTopic :: TxnOffsetCommitRequestTopic
defaultTxnOffsetCommitRequestTopic = TxnOffsetCommitRequestTopic { txnOffsetCommitRequestTopicName = P.KafkaString Null, txnOffsetCommitRequestTopicTopicId = P.nullUuid, txnOffsetCommitRequestTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a TxnOffsetCommitRequest.
wireMaxSizeTxnOffsetCommitRequest :: Int -> TxnOffsetCommitRequest -> Int
wireMaxSizeTxnOffsetCommitRequest _version msg =
  0
  + WP.dualStringMaxSize (txnOffsetCommitRequestTransactionalId msg)
  + WP.dualStringMaxSize (txnOffsetCommitRequestGroupId msg)
  + 8
  + 2
  + 4
  + WP.dualStringMaxSize (txnOffsetCommitRequestMemberId msg)
  + WP.dualStringMaxSize (txnOffsetCommitRequestGroupInstanceId msg)
  + (5 + (case P.unKafkaArray (txnOffsetCommitRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTxnOffsetCommitRequestTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TxnOffsetCommitRequest.
wirePokeTxnOffsetCommitRequest :: Int -> Ptr Word8 -> TxnOffsetCommitRequest -> IO (Ptr Word8)
wirePokeTxnOffsetCommitRequest version basePtr msg
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (txnOffsetCommitRequestTransactionalId msg)) else WP.pokeKafkaString p0 (txnOffsetCommitRequestTransactionalId msg))
    p2 <- (if version >= 3 then WP.pokeCompactString p1 (P.toCompactString (txnOffsetCommitRequestGroupId msg)) else WP.pokeKafkaString p1 (txnOffsetCommitRequestGroupId msg))
    p3 <- W.pokeInt64BE p2 (txnOffsetCommitRequestProducerId msg)
    p4 <- W.pokeInt16BE p3 (txnOffsetCommitRequestProducerEpoch msg)
    p5 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitRequestTopic version p x) p4 (txnOffsetCommitRequestTopics msg)
    pure p5
  | version >= 3 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (txnOffsetCommitRequestTransactionalId msg)) else WP.pokeKafkaString p0 (txnOffsetCommitRequestTransactionalId msg))
    p2 <- (if version >= 3 then WP.pokeCompactString p1 (P.toCompactString (txnOffsetCommitRequestGroupId msg)) else WP.pokeKafkaString p1 (txnOffsetCommitRequestGroupId msg))
    p3 <- W.pokeInt64BE p2 (txnOffsetCommitRequestProducerId msg)
    p4 <- W.pokeInt16BE p3 (txnOffsetCommitRequestProducerEpoch msg)
    p5 <- (if version >= 3 then W.pokeInt32BE p4 (txnOffsetCommitRequestGenerationIdOrMemberEpoch msg) else pure p4)
    p6 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p5 (P.toCompactString (txnOffsetCommitRequestMemberId msg)) else WP.pokeKafkaString p5 (txnOffsetCommitRequestMemberId msg)) else pure p5)
    p7 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p6 (P.toCompactString (txnOffsetCommitRequestGroupInstanceId msg)) else WP.pokeKafkaString p6 (txnOffsetCommitRequestGroupInstanceId msg)) else pure p6)
    p8 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitRequestTopic version p x) p7 (txnOffsetCommitRequestTopics msg)
    WP.pokeEmptyTaggedFields p8
  | otherwise = error $ "wirePoke TxnOffsetCommitRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for TxnOffsetCommitRequest.
wirePeekTxnOffsetCommitRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitRequest, Ptr Word8)
wirePeekTxnOffsetCommitRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 2 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_groupid, p2) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitRequestTopic version _fp _basePtr p e) p4 endPtr
    pure (TxnOffsetCommitRequest { txnOffsetCommitRequestTransactionalId = f0_transactionalid, txnOffsetCommitRequestGroupId = f1_groupid, txnOffsetCommitRequestProducerId = f2_producerid, txnOffsetCommitRequestProducerEpoch = f3_producerepoch, txnOffsetCommitRequestGenerationIdOrMemberEpoch = -1, txnOffsetCommitRequestMemberId = P.KafkaString Null, txnOffsetCommitRequestGroupInstanceId = P.KafkaString Null, txnOffsetCommitRequestTopics = f4_topics }, p5)
  | version >= 3 && version <= 6 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_groupid, p2) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    (f4_generationidormemberepoch, p5) <- (if version >= 3 then W.peekInt32BE p4 endPtr else pure (-1, p4))
    (f5_memberid, p6) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr) else pure (P.KafkaString Null, p5))
    (f6_groupinstanceid, p7) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr else WP.peekKafkaString p6 endPtr) else pure (P.KafkaString Null, p6))
    (f7_topics, p8) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitRequestTopic version _fp _basePtr p e) p7 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (TxnOffsetCommitRequest { txnOffsetCommitRequestTransactionalId = f0_transactionalid, txnOffsetCommitRequestGroupId = f1_groupid, txnOffsetCommitRequestProducerId = f2_producerid, txnOffsetCommitRequestProducerEpoch = f3_producerepoch, txnOffsetCommitRequestGenerationIdOrMemberEpoch = f4_generationidormemberepoch, txnOffsetCommitRequestMemberId = f5_memberid, txnOffsetCommitRequestGroupInstanceId = f6_groupinstanceid, txnOffsetCommitRequestTopics = f7_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek TxnOffsetCommitRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec TxnOffsetCommitRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeTxnOffsetCommitRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeTxnOffsetCommitRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekTxnOffsetCommitRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}