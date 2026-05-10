{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareAcknowledgeRequest
Description : Kafka ShareAcknowledgeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 79.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareAcknowledgeRequest
  (
    ShareAcknowledgeRequest(..),
    AcknowledgeTopic(..),
    AcknowledgePartition(..),
    AcknowledgementBatch(..),
    maxShareAcknowledgeRequestVersion
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


-- | Record batches to acknowledge.
data AcknowledgementBatch = AcknowledgementBatch
  {

  -- | First offset of batch of records to acknowledge.

  -- Versions: 0+
  acknowledgementBatchFirstOffset :: !(Int64)
,

  -- | Last offset (inclusive) of batch of records to acknowledge.

  -- Versions: 0+
  acknowledgementBatchLastOffset :: !(Int64)
,

  -- | Array of acknowledge types - 0:Gap,1:Accept,2:Release,3:Reject,4:Renew.

  -- Versions: 0+
  acknowledgementBatchAcknowledgeTypes :: !(KafkaArray (Int8))

  }
  deriving (Eq, Show, Generic)

-- | The partitions containing records to acknowledge.
data AcknowledgePartition = AcknowledgePartition
  {

  -- | The partition index.

  -- Versions: 0+
  acknowledgePartitionPartitionIndex :: !(Int32)
,

  -- | Record batches to acknowledge.

  -- Versions: 0+
  acknowledgePartitionAcknowledgementBatches :: !(KafkaArray (AcknowledgementBatch))

  }
  deriving (Eq, Show, Generic)

-- | The topics containing records to acknowledge.
data AcknowledgeTopic = AcknowledgeTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  acknowledgeTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions containing records to acknowledge.

  -- Versions: 0+
  acknowledgeTopicPartitions :: !(KafkaArray (AcknowledgePartition))

  }
  deriving (Eq, Show, Generic)


data ShareAcknowledgeRequest = ShareAcknowledgeRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  shareAcknowledgeRequestGroupId :: !(KafkaString)
,

  -- | The member ID.

  -- Versions: 0+
  shareAcknowledgeRequestMemberId :: !(KafkaString)
,

  -- | The current share session epoch: 0 to open a share session; -1 to close it; otherwise increments for

  -- Versions: 0+
  shareAcknowledgeRequestShareSessionEpoch :: !(Int32)
,

  -- | Whether Renew type acknowledgements present in AcknowledgementBatches.

  -- Versions: 2+
  shareAcknowledgeRequestIsRenewAck :: !(Bool)
,

  -- | The topics containing records to acknowledge.

  -- Versions: 0+
  shareAcknowledgeRequestTopics :: !(KafkaArray (AcknowledgeTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareAcknowledgeRequest.
maxShareAcknowledgeRequestVersion :: Int16
maxShareAcknowledgeRequestVersion = 2

-- | KafkaMessage instance for ShareAcknowledgeRequest.
instance KafkaMessage ShareAcknowledgeRequest where
  messageApiKey = 79
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a AcknowledgementBatch.
wireMaxSizeAcknowledgementBatch :: Int -> AcknowledgementBatch -> Int
wireMaxSizeAcknowledgementBatch _version msg =
  0
  + 8
  + 8
  + (5 + (case P.unKafkaArray (acknowledgementBatchAcknowledgeTypes msg) of { P.NotNull v -> sum (fmap (\x -> 1 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AcknowledgementBatch.
wirePokeAcknowledgementBatch :: Int -> Ptr Word8 -> AcknowledgementBatch -> IO (Ptr Word8)
wirePokeAcknowledgementBatch version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (acknowledgementBatchFirstOffset msg)
  p2 <- W.pokeInt64BE p1 (acknowledgementBatchLastOffset msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> W.pokeWord8 p (fromIntegral (x :: Int8))) p2 (acknowledgementBatchAcknowledgeTypes msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AcknowledgementBatch.
wirePeekAcknowledgementBatch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AcknowledgementBatch, Ptr Word8)
wirePeekAcknowledgementBatch version _fp _basePtr p0 endPtr = do
  (f0_firstoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_lastoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_acknowledgetypes, p3) <- WP.peekVersionedArray version 0 (\p e -> (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AcknowledgementBatch { acknowledgementBatchFirstOffset = f0_firstoffset, acknowledgementBatchLastOffset = f1_lastoffset, acknowledgementBatchAcknowledgeTypes = f2_acknowledgetypes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAcknowledgementBatch :: AcknowledgementBatch
defaultAcknowledgementBatch = AcknowledgementBatch { acknowledgementBatchFirstOffset = 0, acknowledgementBatchLastOffset = 0, acknowledgementBatchAcknowledgeTypes = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AcknowledgePartition.
wireMaxSizeAcknowledgePartition :: Int -> AcknowledgePartition -> Int
wireMaxSizeAcknowledgePartition _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (acknowledgePartitionAcknowledgementBatches msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAcknowledgementBatch _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AcknowledgePartition.
wirePokeAcknowledgePartition :: Int -> Ptr Word8 -> AcknowledgePartition -> IO (Ptr Word8)
wirePokeAcknowledgePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (acknowledgePartitionPartitionIndex msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcknowledgementBatch version p x) p1 (acknowledgePartitionAcknowledgementBatches msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AcknowledgePartition.
wirePeekAcknowledgePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AcknowledgePartition, Ptr Word8)
wirePeekAcknowledgePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_acknowledgementbatches, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcknowledgementBatch version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AcknowledgePartition { acknowledgePartitionPartitionIndex = f0_partitionindex, acknowledgePartitionAcknowledgementBatches = f1_acknowledgementbatches }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAcknowledgePartition :: AcknowledgePartition
defaultAcknowledgePartition = AcknowledgePartition { acknowledgePartitionPartitionIndex = 0, acknowledgePartitionAcknowledgementBatches = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AcknowledgeTopic.
wireMaxSizeAcknowledgeTopic :: Int -> AcknowledgeTopic -> Int
wireMaxSizeAcknowledgeTopic _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (acknowledgeTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAcknowledgePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AcknowledgeTopic.
wirePokeAcknowledgeTopic :: Int -> Ptr Word8 -> AcknowledgeTopic -> IO (Ptr Word8)
wirePokeAcknowledgeTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (acknowledgeTopicTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcknowledgePartition version p x) p1 (acknowledgeTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AcknowledgeTopic.
wirePeekAcknowledgeTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AcknowledgeTopic, Ptr Word8)
wirePeekAcknowledgeTopic version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcknowledgePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AcknowledgeTopic { acknowledgeTopicTopicId = f0_topicid, acknowledgeTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAcknowledgeTopic :: AcknowledgeTopic
defaultAcknowledgeTopic = AcknowledgeTopic { acknowledgeTopicTopicId = P.nullUuid, acknowledgeTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ShareAcknowledgeRequest.
wireMaxSizeShareAcknowledgeRequest :: Int -> ShareAcknowledgeRequest -> Int
wireMaxSizeShareAcknowledgeRequest _version msg =
  0
  + WP.dualStringMaxSize (shareAcknowledgeRequestGroupId msg)
  + WP.dualStringMaxSize (shareAcknowledgeRequestMemberId msg)
  + 4
  + 1
  + (5 + (case P.unKafkaArray (shareAcknowledgeRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAcknowledgeTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareAcknowledgeRequest.
wirePokeShareAcknowledgeRequest :: Int -> Ptr Word8 -> ShareAcknowledgeRequest -> IO (Ptr Word8)
wirePokeShareAcknowledgeRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (shareAcknowledgeRequestGroupId msg)) else WP.pokeKafkaString p0 (shareAcknowledgeRequestGroupId msg))
    p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (shareAcknowledgeRequestMemberId msg)) else WP.pokeKafkaString p1 (shareAcknowledgeRequestMemberId msg))
    p3 <- W.pokeInt32BE p2 (shareAcknowledgeRequestShareSessionEpoch msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcknowledgeTopic version p x) p3 (shareAcknowledgeRequestTopics msg)
    WP.pokeEmptyTaggedFields p4
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (shareAcknowledgeRequestGroupId msg)) else WP.pokeKafkaString p0 (shareAcknowledgeRequestGroupId msg))
    p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (shareAcknowledgeRequestMemberId msg)) else WP.pokeKafkaString p1 (shareAcknowledgeRequestMemberId msg))
    p3 <- W.pokeInt32BE p2 (shareAcknowledgeRequestShareSessionEpoch msg)
    p4 <- (if version >= 2 then W.pokeWord8 p3 (if (shareAcknowledgeRequestIsRenewAck msg) then 1 else 0) else pure p3)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcknowledgeTopic version p x) p4 (shareAcknowledgeRequestTopics msg)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke ShareAcknowledgeRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareAcknowledgeRequest.
wirePeekShareAcknowledgeRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareAcknowledgeRequest, Ptr Word8)
wirePeekShareAcknowledgeRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_groupid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_memberid, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_sharesessionepoch, p3) <- W.peekInt32BE p2 endPtr
    (f3_topics, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcknowledgeTopic version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ShareAcknowledgeRequest { shareAcknowledgeRequestGroupId = f0_groupid, shareAcknowledgeRequestMemberId = f1_memberid, shareAcknowledgeRequestShareSessionEpoch = f2_sharesessionepoch, shareAcknowledgeRequestIsRenewAck = False, shareAcknowledgeRequestTopics = f3_topics }, pTagsEnd)
  | version == 2 = do
    (f0_groupid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_memberid, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_sharesessionepoch, p3) <- W.peekInt32BE p2 endPtr
    (f3_isrenewack, p4) <- (if version >= 2 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr else pure (False, p3))
    (f4_topics, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcknowledgeTopic version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (ShareAcknowledgeRequest { shareAcknowledgeRequestGroupId = f0_groupid, shareAcknowledgeRequestMemberId = f1_memberid, shareAcknowledgeRequestShareSessionEpoch = f2_sharesessionepoch, shareAcknowledgeRequestIsRenewAck = f3_isrenewack, shareAcknowledgeRequestTopics = f4_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareAcknowledgeRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ShareAcknowledgeRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareAcknowledgeRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareAcknowledgeRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareAcknowledgeRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}