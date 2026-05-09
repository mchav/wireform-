{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTransactionsResponse
Description : Kafka DescribeTransactionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 65.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTransactionsResponse
  (
    DescribeTransactionsResponse(..),
    TransactionState(..),
    TopicData(..),
    maxDescribeTransactionsResponseVersion
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


-- | The set of partitions included in the current transaction (if active). When a transaction is preparing to commit or abort, this will include only partitions which do not have markers.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopic :: !(KafkaString)
,

  -- | The partition ids included in the current transaction.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | The current state of the transaction.
data TransactionState = TransactionState
  {

  -- | The error code.

  -- Versions: 0+
  transactionStateErrorCode :: !(Int16)
,

  -- | The transactional id.

  -- Versions: 0+
  transactionStateTransactionalId :: !(KafkaString)
,

  -- | The current transaction state of the producer.

  -- Versions: 0+
  transactionStateTransactionState :: !(KafkaString)
,

  -- | The timeout in milliseconds for the transaction.

  -- Versions: 0+
  transactionStateTransactionTimeoutMs :: !(Int32)
,

  -- | The start time of the transaction in milliseconds.

  -- Versions: 0+
  transactionStateTransactionStartTimeMs :: !(Int64)
,

  -- | The current producer id associated with the transaction.

  -- Versions: 0+
  transactionStateProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer id.

  -- Versions: 0+
  transactionStateProducerEpoch :: !(Int16)
,

  -- | The set of partitions included in the current transaction (if active). When a transaction is prepari

  -- Versions: 0+
  transactionStateTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)


data DescribeTransactionsResponse = DescribeTransactionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeTransactionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The current state of the transaction.

  -- Versions: 0+
  describeTransactionsResponseTransactionStates :: !(KafkaArray (TransactionState))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTransactionsResponse.
maxDescribeTransactionsResponseVersion :: Int16
maxDescribeTransactionsResponseVersion = 0

-- | KafkaMessage instance for DescribeTransactionsResponse.
instance KafkaMessage DescribeTransactionsResponse where
  messageApiKey = 65
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicDataTopic msg))
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicDataTopic msg))
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopic = f0_topic, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a TransactionState.
wireMaxSizeTransactionState :: Int -> TransactionState -> Int
wireMaxSizeTransactionState _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (transactionStateTransactionalId msg))
  + WP.compactStringMaxSize (P.toCompactString (transactionStateTransactionState msg))
  + 4
  + 8
  + 8
  + 2
  + (5 + (case P.unKafkaArray (transactionStateTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TransactionState.
wirePokeTransactionState :: Int -> Ptr Word8 -> TransactionState -> IO (Ptr Word8)
wirePokeTransactionState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (transactionStateErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (transactionStateTransactionalId msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (transactionStateTransactionState msg))
  p4 <- W.pokeInt32BE p3 (transactionStateTransactionTimeoutMs msg)
  p5 <- W.pokeInt64BE p4 (transactionStateTransactionStartTimeMs msg)
  p6 <- W.pokeInt64BE p5 (transactionStateProducerId msg)
  p7 <- W.pokeInt16BE p6 (transactionStateProducerEpoch msg)
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p7 (transactionStateTopics msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for TransactionState.
wirePeekTransactionState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TransactionState, Ptr Word8)
wirePeekTransactionState version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_transactionalid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_transactionstate, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_transactiontimeoutms, p4) <- W.peekInt32BE p3 endPtr
  (f4_transactionstarttimems, p5) <- W.peekInt64BE p4 endPtr
  (f5_producerid, p6) <- W.peekInt64BE p5 endPtr
  (f6_producerepoch, p7) <- W.peekInt16BE p6 endPtr
  (f7_topics, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p7 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (TransactionState { transactionStateErrorCode = f0_errorcode, transactionStateTransactionalId = f1_transactionalid, transactionStateTransactionState = f2_transactionstate, transactionStateTransactionTimeoutMs = f3_transactiontimeoutms, transactionStateTransactionStartTimeMs = f4_transactionstarttimems, transactionStateProducerId = f5_producerid, transactionStateProducerEpoch = f6_producerepoch, transactionStateTopics = f7_topics }, pTagsEnd)

-- | Worst-case wire size of a DescribeTransactionsResponse.
wireMaxSizeDescribeTransactionsResponse :: Int -> DescribeTransactionsResponse -> Int
wireMaxSizeDescribeTransactionsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeTransactionsResponseTransactionStates msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTransactionState _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeTransactionsResponse.
wirePokeDescribeTransactionsResponse :: Int -> Ptr Word8 -> DescribeTransactionsResponse -> IO (Ptr Word8)
wirePokeDescribeTransactionsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeTransactionsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTransactionState version p x) p1 (describeTransactionsResponseTransactionStates msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DescribeTransactionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeTransactionsResponse.
wirePeekDescribeTransactionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTransactionsResponse, Ptr Word8)
wirePeekDescribeTransactionsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_transactionstates, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTransactionState version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeTransactionsResponse { describeTransactionsResponseThrottleTimeMs = f0_throttletimems, describeTransactionsResponseTransactionStates = f1_transactionstates }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeTransactionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeTransactionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeTransactionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeTransactionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeTransactionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}