{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareFetchRequest
Description : Kafka ShareFetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 78.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareFetchRequest
  (
    ShareFetchRequest(..),
    FetchTopic(..),
    FetchPartition(..),
    AcknowledgementBatch(..),
    ForgottenTopic(..),
    maxShareFetchRequestVersion
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

  -- | Array of acknowledge types - 0:Gap,1:Accept,2:Release,3:Reject.

  -- Versions: 0+
  acknowledgementBatchAcknowledgeTypes :: !(KafkaArray (Int8))

  }
  deriving (Eq, Show, Generic)

-- | The partitions to fetch.
data FetchPartition = FetchPartition
  {

  -- | The partition index.

  -- Versions: 0+
  fetchPartitionPartitionIndex :: !(Int32)
,

  -- | The maximum bytes to fetch from this partition. 0 when only acknowledgement with no fetching is requ

  -- Versions: 0+
  fetchPartitionPartitionMaxBytes :: !(Int32)
,

  -- | Record batches to acknowledge.

  -- Versions: 0+
  fetchPartitionAcknowledgementBatches :: !(KafkaArray (AcknowledgementBatch))

  }
  deriving (Eq, Show, Generic)

-- | The topics to fetch.
data FetchTopic = FetchTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  fetchTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  fetchTopicPartitions :: !(KafkaArray (FetchPartition))

  }
  deriving (Eq, Show, Generic)

-- | The partitions to remove from this share session.
data ForgottenTopic = ForgottenTopic
  {

  -- | The unique topic ID.

  -- Versions: 0+
  forgottenTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions indexes to forget.

  -- Versions: 0+
  forgottenTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


data ShareFetchRequest = ShareFetchRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  shareFetchRequestGroupId :: !(KafkaString)
,

  -- | The member ID.

  -- Versions: 0+
  shareFetchRequestMemberId :: !(KafkaString)
,

  -- | The current share session epoch: 0 to open a share session; -1 to close it; otherwise increments for

  -- Versions: 0+
  shareFetchRequestShareSessionEpoch :: !(Int32)
,

  -- | The maximum time in milliseconds to wait for the response.

  -- Versions: 0+
  shareFetchRequestMaxWaitMs :: !(Int32)
,

  -- | The minimum bytes to accumulate in the response.

  -- Versions: 0+
  shareFetchRequestMinBytes :: !(Int32)
,

  -- | The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.

  -- Versions: 0+
  shareFetchRequestMaxBytes :: !(Int32)
,

  -- | The topics to fetch.

  -- Versions: 0+
  shareFetchRequestTopics :: !(KafkaArray (FetchTopic))
,

  -- | The partitions to remove from this share session.

  -- Versions: 0+
  shareFetchRequestForgottenTopicsData :: !(KafkaArray (ForgottenTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareFetchRequest.
maxShareFetchRequestVersion :: Int16
maxShareFetchRequestVersion = 0

-- | KafkaMessage instance for ShareFetchRequest.
instance KafkaMessage ShareFetchRequest where
  messageApiKey = 78
  messageMinVersion = 0
  messageMaxVersion = 0
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

-- | Worst-case wire size of a FetchPartition.
wireMaxSizeFetchPartition :: Int -> FetchPartition -> Int
wireMaxSizeFetchPartition _version msg =
  0
  + 4
  + 4
  + (5 + (case P.unKafkaArray (fetchPartitionAcknowledgementBatches msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAcknowledgementBatch _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FetchPartition.
wirePokeFetchPartition :: Int -> Ptr Word8 -> FetchPartition -> IO (Ptr Word8)
wirePokeFetchPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (fetchPartitionPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (fetchPartitionPartitionMaxBytes msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcknowledgementBatch version p x) p2 (fetchPartitionAcknowledgementBatches msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for FetchPartition.
wirePeekFetchPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchPartition, Ptr Word8)
wirePeekFetchPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_partitionmaxbytes, p2) <- W.peekInt32BE p1 endPtr
  (f2_acknowledgementbatches, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcknowledgementBatch version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (FetchPartition { fetchPartitionPartitionIndex = f0_partitionindex, fetchPartitionPartitionMaxBytes = f1_partitionmaxbytes, fetchPartitionAcknowledgementBatches = f2_acknowledgementbatches }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFetchPartition :: FetchPartition
defaultFetchPartition = FetchPartition { fetchPartitionPartitionIndex = 0, fetchPartitionPartitionMaxBytes = 0, fetchPartitionAcknowledgementBatches = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a FetchTopic.
wireMaxSizeFetchTopic :: Int -> FetchTopic -> Int
wireMaxSizeFetchTopic _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (fetchTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFetchPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FetchTopic.
wirePokeFetchTopic :: Int -> Ptr Word8 -> FetchTopic -> IO (Ptr Word8)
wirePokeFetchTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (fetchTopicTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFetchPartition version p x) p1 (fetchTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for FetchTopic.
wirePeekFetchTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchTopic, Ptr Word8)
wirePeekFetchTopic version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFetchPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (FetchTopic { fetchTopicTopicId = f0_topicid, fetchTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFetchTopic :: FetchTopic
defaultFetchTopic = FetchTopic { fetchTopicTopicId = P.nullUuid, fetchTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ForgottenTopic.
wireMaxSizeForgottenTopic :: Int -> ForgottenTopic -> Int
wireMaxSizeForgottenTopic _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (forgottenTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ForgottenTopic.
wirePokeForgottenTopic :: Int -> Ptr Word8 -> ForgottenTopic -> IO (Ptr Word8)
wirePokeForgottenTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (forgottenTopicTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (forgottenTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ForgottenTopic.
wirePeekForgottenTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ForgottenTopic, Ptr Word8)
wirePeekForgottenTopic version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ForgottenTopic { forgottenTopicTopicId = f0_topicid, forgottenTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultForgottenTopic :: ForgottenTopic
defaultForgottenTopic = ForgottenTopic { forgottenTopicTopicId = P.nullUuid, forgottenTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ShareFetchRequest.
wireMaxSizeShareFetchRequest :: Int -> ShareFetchRequest -> Int
wireMaxSizeShareFetchRequest _version msg =
  0
  + WP.dualStringMaxSize (shareFetchRequestGroupId msg)
  + WP.dualStringMaxSize (shareFetchRequestMemberId msg)
  + 4
  + 4
  + 4
  + 4
  + (5 + (case P.unKafkaArray (shareFetchRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFetchTopic _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (shareFetchRequestForgottenTopicsData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeForgottenTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareFetchRequest.
wirePokeShareFetchRequest :: Int -> Ptr Word8 -> ShareFetchRequest -> IO (Ptr Word8)
wirePokeShareFetchRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (shareFetchRequestGroupId msg)) else WP.pokeKafkaString p0 (shareFetchRequestGroupId msg))
    p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (shareFetchRequestMemberId msg)) else WP.pokeKafkaString p1 (shareFetchRequestMemberId msg))
    p3 <- W.pokeInt32BE p2 (shareFetchRequestShareSessionEpoch msg)
    p4 <- W.pokeInt32BE p3 (shareFetchRequestMaxWaitMs msg)
    p5 <- W.pokeInt32BE p4 (shareFetchRequestMinBytes msg)
    p6 <- W.pokeInt32BE p5 (shareFetchRequestMaxBytes msg)
    p7 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFetchTopic version p x) p6 (shareFetchRequestTopics msg)
    p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeForgottenTopic version p x) p7 (shareFetchRequestForgottenTopicsData msg)
    WP.pokeEmptyTaggedFields p8
  | otherwise = error $ "wirePoke ShareFetchRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareFetchRequest.
wirePeekShareFetchRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareFetchRequest, Ptr Word8)
wirePeekShareFetchRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_memberid, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_sharesessionepoch, p3) <- W.peekInt32BE p2 endPtr
    (f3_maxwaitms, p4) <- W.peekInt32BE p3 endPtr
    (f4_minbytes, p5) <- W.peekInt32BE p4 endPtr
    (f5_maxbytes, p6) <- W.peekInt32BE p5 endPtr
    (f6_topics, p7) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p6 endPtr
    (f7_forgottentopicsdata, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekForgottenTopic version _fp _basePtr p e) p7 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (ShareFetchRequest { shareFetchRequestGroupId = f0_groupid, shareFetchRequestMemberId = f1_memberid, shareFetchRequestShareSessionEpoch = f2_sharesessionepoch, shareFetchRequestMaxWaitMs = f3_maxwaitms, shareFetchRequestMinBytes = f4_minbytes, shareFetchRequestMaxBytes = f5_maxbytes, shareFetchRequestTopics = f6_topics, shareFetchRequestForgottenTopicsData = f7_forgottentopicsdata }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareFetchRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ShareFetchRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareFetchRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareFetchRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareFetchRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}