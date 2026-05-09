{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeProducersResponse
Description : Kafka DescribeProducersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 61.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeProducersResponse
  (
    DescribeProducersResponse(..),
    TopicResponse(..),
    PartitionResponse(..),
    ProducerState(..),
    encodeDescribeProducersResponse,
    decodeDescribeProducersResponse,
    maxDescribeProducersResponseVersion
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


-- | The active producers for the partition.
data ProducerState = ProducerState
  {

  -- | The producer id.

  -- Versions: 0+
  producerStateProducerId :: !(Int64)
,

  -- | The producer epoch.

  -- Versions: 0+
  producerStateProducerEpoch :: !(Int32)
,

  -- | The last sequence number sent by the producer.

  -- Versions: 0+
  producerStateLastSequence :: !(Int32)
,

  -- | The last timestamp sent by the producer.

  -- Versions: 0+
  producerStateLastTimestamp :: !(Int64)
,

  -- | The current epoch of the producer group.

  -- Versions: 0+
  producerStateCoordinatorEpoch :: !(Int32)
,

  -- | The current transaction start offset of the producer.

  -- Versions: 0+
  producerStateCurrentTxnStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode ProducerState with version-aware field handling.
encodeProducerState :: MonadPut m => E.ApiVersion -> ProducerState -> m ()
encodeProducerState version pmsg =
  do
    serialize (producerStateProducerId pmsg)
    serialize (producerStateProducerEpoch pmsg)
    serialize (producerStateLastSequence pmsg)
    serialize (producerStateLastTimestamp pmsg)
    serialize (producerStateCoordinatorEpoch pmsg)
    serialize (producerStateCurrentTxnStartOffset pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ProducerState with version-aware field handling.
decodeProducerState :: MonadGet m => E.ApiVersion -> m ProducerState
decodeProducerState version =
  do
    fieldproducerid <- deserialize
    fieldproducerepoch <- deserialize
    fieldlastsequence <- deserialize
    fieldlasttimestamp <- deserialize
    fieldcoordinatorepoch <- deserialize
    fieldcurrenttxnstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ProducerState
      {
      producerStateProducerId = fieldproducerid
      ,
      producerStateProducerEpoch = fieldproducerepoch
      ,
      producerStateLastSequence = fieldlastsequence
      ,
      producerStateLastTimestamp = fieldlasttimestamp
      ,
      producerStateCoordinatorEpoch = fieldcoordinatorepoch
      ,
      producerStateCurrentTxnStartOffset = fieldcurrenttxnstartoffset
      }


-- | Each partition in the response.
data PartitionResponse = PartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResponsePartitionIndex :: !(Int32)
,

  -- | The partition error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResponseErrorCode :: !(Int16)
,

  -- | The partition error message, which may be null if no additional details are available.

  -- Versions: 0+
  partitionResponseErrorMessage :: !(KafkaString)
,

  -- | The active producers for the partition.

  -- Versions: 0+
  partitionResponseActiveProducers :: !(KafkaArray (ProducerState))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResponse with version-aware field handling.
encodePartitionResponse :: MonadPut m => E.ApiVersion -> PartitionResponse -> m ()
encodePartitionResponse version pmsg =
  do
    serialize (partitionResponsePartitionIndex pmsg)
    serialize (partitionResponseErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResponseErrorMessage pmsg)) else serialize (partitionResponseErrorMessage pmsg)
    E.encodeVersionedArray version 0 encodeProducerState (case P.unKafkaArray (partitionResponseActiveProducers pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResponse with version-aware field handling.
decodePartitionResponse :: MonadGet m => E.ApiVersion -> m PartitionResponse
decodePartitionResponse version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldactiveproducers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeProducerState
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResponse
      {
      partitionResponsePartitionIndex = fieldpartitionindex
      ,
      partitionResponseErrorCode = fielderrorcode
      ,
      partitionResponseErrorMessage = fielderrormessage
      ,
      partitionResponseActiveProducers = fieldactiveproducers
      }


-- | Each topic in the response.
data TopicResponse = TopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  topicResponseName :: !(KafkaString)
,

  -- | Each partition in the response.

  -- Versions: 0+
  topicResponsePartitions :: !(KafkaArray (PartitionResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicResponse with version-aware field handling.
encodeTopicResponse :: MonadPut m => E.ApiVersion -> TopicResponse -> m ()
encodeTopicResponse version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicResponseName tmsg)) else serialize (topicResponseName tmsg)
    E.encodeVersionedArray version 0 encodePartitionResponse (case P.unKafkaArray (topicResponsePartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicResponse with version-aware field handling.
decodeTopicResponse :: MonadGet m => E.ApiVersion -> m TopicResponse
decodeTopicResponse version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResponse
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicResponse
      {
      topicResponseName = fieldname
      ,
      topicResponsePartitions = fieldpartitions
      }



data DescribeProducersResponse = DescribeProducersResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeProducersResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  describeProducersResponseTopics :: !(KafkaArray (TopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeProducersResponse.
maxDescribeProducersResponseVersion :: Int16
maxDescribeProducersResponseVersion = 0

-- | KafkaMessage instance for DescribeProducersResponse.
instance KafkaMessage DescribeProducersResponse where
  messageApiKey = 61
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode DescribeProducersResponse with the given API version.
encodeDescribeProducersResponse :: MonadPut m => E.ApiVersion -> DescribeProducersResponse -> m ()
encodeDescribeProducersResponse version msg
  | version == 0 =
    do
      serialize (describeProducersResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeTopicResponse (case P.unKafkaArray (describeProducersResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeProducersResponse with the given API version.
decodeDescribeProducersResponse :: MonadGet m => E.ApiVersion -> m DescribeProducersResponse
decodeDescribeProducersResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeProducersResponse
        {
        describeProducersResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeProducersResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ProducerState.
wireMaxSizeProducerState :: Int -> ProducerState -> Int
wireMaxSizeProducerState _version msg =
  0
  + 8
  + 4
  + 4
  + 8
  + 4
  + 8
  + 1

-- | Direct-poke encoder for ProducerState.
wirePokeProducerState :: Int -> Ptr Word8 -> ProducerState -> IO (Ptr Word8)
wirePokeProducerState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (producerStateProducerId msg)
  p2 <- W.pokeInt32BE p1 (producerStateProducerEpoch msg)
  p3 <- W.pokeInt32BE p2 (producerStateLastSequence msg)
  p4 <- W.pokeInt64BE p3 (producerStateLastTimestamp msg)
  p5 <- W.pokeInt32BE p4 (producerStateCoordinatorEpoch msg)
  p6 <- W.pokeInt64BE p5 (producerStateCurrentTxnStartOffset msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for ProducerState.
wirePeekProducerState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ProducerState, Ptr Word8)
wirePeekProducerState version _fp _basePtr p0 endPtr = do
  (f0_producerid, p1) <- W.peekInt64BE p0 endPtr
  (f1_producerepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_lastsequence, p3) <- W.peekInt32BE p2 endPtr
  (f3_lasttimestamp, p4) <- W.peekInt64BE p3 endPtr
  (f4_coordinatorepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_currenttxnstartoffset, p6) <- W.peekInt64BE p5 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (ProducerState { producerStateProducerId = f0_producerid, producerStateProducerEpoch = f1_producerepoch, producerStateLastSequence = f2_lastsequence, producerStateLastTimestamp = f3_lasttimestamp, producerStateCoordinatorEpoch = f4_coordinatorepoch, producerStateCurrentTxnStartOffset = f5_currenttxnstartoffset }, pTagsEnd)

-- | Worst-case wire size of a PartitionResponse.
wireMaxSizePartitionResponse :: Int -> PartitionResponse -> Int
wireMaxSizePartitionResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (partitionResponseActiveProducers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeProducerState _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionResponse.
wirePokePartitionResponse :: Int -> Ptr Word8 -> PartitionResponse -> IO (Ptr Word8)
wirePokePartitionResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResponsePartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionResponseErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionResponseErrorMessage msg))
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeProducerState version p x) p3 (partitionResponseActiveProducers msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionResponse.
wirePeekPartitionResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResponse, Ptr Word8)
wirePeekPartitionResponse version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_activeproducers, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekProducerState version _fp _basePtr p e) p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionResponse { partitionResponsePartitionIndex = f0_partitionindex, partitionResponseErrorCode = f1_errorcode, partitionResponseErrorMessage = f2_errormessage, partitionResponseActiveProducers = f3_activeproducers }, pTagsEnd)

-- | Worst-case wire size of a TopicResponse.
wireMaxSizeTopicResponse :: Int -> TopicResponse -> Int
wireMaxSizeTopicResponse _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicResponseName msg))
  + (5 + (case P.unKafkaArray (topicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicResponse.
wirePokeTopicResponse :: Int -> Ptr Word8 -> TopicResponse -> IO (Ptr Word8)
wirePokeTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicResponseName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResponse version p x) p1 (topicResponsePartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicResponse.
wirePeekTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicResponse, Ptr Word8)
wirePeekTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResponse version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicResponse { topicResponseName = f0_name, topicResponsePartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DescribeProducersResponse.
wireMaxSizeDescribeProducersResponse :: Int -> DescribeProducersResponse -> Int
wireMaxSizeDescribeProducersResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeProducersResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeProducersResponse.
wirePokeDescribeProducersResponse :: Int -> Ptr Word8 -> DescribeProducersResponse -> IO (Ptr Word8)
wirePokeDescribeProducersResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeProducersResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicResponse version p x) p1 (describeProducersResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DescribeProducersResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeProducersResponse.
wirePeekDescribeProducersResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeProducersResponse, Ptr Word8)
wirePeekDescribeProducersResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicResponse version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeProducersResponse { describeProducersResponseThrottleTimeMs = f0_throttletimems, describeProducersResponseTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeProducersResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeProducersResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeProducersResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeProducersResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeProducersResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}