{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.TxnOffsetCommitResponse
Description : Kafka TxnOffsetCommitResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 28.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.TxnOffsetCommitResponse
  (
    TxnOffsetCommitResponse(..),
    TxnOffsetCommitResponseTopic(..),
    TxnOffsetCommitResponsePartition(..),
    maxTxnOffsetCommitResponseVersion
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


-- | The responses for each partition in the topic.
data TxnOffsetCommitResponsePartition = TxnOffsetCommitResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  txnOffsetCommitResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  txnOffsetCommitResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The responses for each topic.
data TxnOffsetCommitResponseTopic = TxnOffsetCommitResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  txnOffsetCommitResponseTopicName :: !(KafkaString)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  txnOffsetCommitResponseTopicPartitions :: !(KafkaArray (TxnOffsetCommitResponsePartition))

  }
  deriving (Eq, Show, Generic)


data TxnOffsetCommitResponse = TxnOffsetCommitResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  txnOffsetCommitResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  txnOffsetCommitResponseTopics :: !(KafkaArray (TxnOffsetCommitResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for TxnOffsetCommitResponse.
maxTxnOffsetCommitResponseVersion :: Int16
maxTxnOffsetCommitResponseVersion = 5

-- | KafkaMessage instance for TxnOffsetCommitResponse.
instance KafkaMessage TxnOffsetCommitResponse where
  messageApiKey = 28
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a TxnOffsetCommitResponsePartition.
wireMaxSizeTxnOffsetCommitResponsePartition :: Int -> TxnOffsetCommitResponsePartition -> Int
wireMaxSizeTxnOffsetCommitResponsePartition _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for TxnOffsetCommitResponsePartition.
wirePokeTxnOffsetCommitResponsePartition :: Int -> Ptr Word8 -> TxnOffsetCommitResponsePartition -> IO (Ptr Word8)
wirePokeTxnOffsetCommitResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (txnOffsetCommitResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (txnOffsetCommitResponsePartitionErrorCode msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TxnOffsetCommitResponsePartition.
wirePeekTxnOffsetCommitResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitResponsePartition, Ptr Word8)
wirePeekTxnOffsetCommitResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TxnOffsetCommitResponsePartition { txnOffsetCommitResponsePartitionPartitionIndex = f0_partitionindex, txnOffsetCommitResponsePartitionErrorCode = f1_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTxnOffsetCommitResponsePartition :: TxnOffsetCommitResponsePartition
defaultTxnOffsetCommitResponsePartition = TxnOffsetCommitResponsePartition { txnOffsetCommitResponsePartitionPartitionIndex = 0, txnOffsetCommitResponsePartitionErrorCode = 0 }

-- | Worst-case wire size of a TxnOffsetCommitResponseTopic.
wireMaxSizeTxnOffsetCommitResponseTopic :: Int -> TxnOffsetCommitResponseTopic -> Int
wireMaxSizeTxnOffsetCommitResponseTopic _version msg =
  0
  + WP.dualStringMaxSize (txnOffsetCommitResponseTopicName msg)
  + (5 + (case P.unKafkaArray (txnOffsetCommitResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTxnOffsetCommitResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TxnOffsetCommitResponseTopic.
wirePokeTxnOffsetCommitResponseTopic :: Int -> Ptr Word8 -> TxnOffsetCommitResponseTopic -> IO (Ptr Word8)
wirePokeTxnOffsetCommitResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (txnOffsetCommitResponseTopicName msg)) else WP.pokeKafkaString p0 (txnOffsetCommitResponseTopicName msg))
  p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitResponsePartition version p x) p1 (txnOffsetCommitResponseTopicPartitions msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TxnOffsetCommitResponseTopic.
wirePeekTxnOffsetCommitResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitResponseTopic, Ptr Word8)
wirePeekTxnOffsetCommitResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitResponsePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TxnOffsetCommitResponseTopic { txnOffsetCommitResponseTopicName = f0_name, txnOffsetCommitResponseTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTxnOffsetCommitResponseTopic :: TxnOffsetCommitResponseTopic
defaultTxnOffsetCommitResponseTopic = TxnOffsetCommitResponseTopic { txnOffsetCommitResponseTopicName = P.KafkaString Null, txnOffsetCommitResponseTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a TxnOffsetCommitResponse.
wireMaxSizeTxnOffsetCommitResponse :: Int -> TxnOffsetCommitResponse -> Int
wireMaxSizeTxnOffsetCommitResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (txnOffsetCommitResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTxnOffsetCommitResponseTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TxnOffsetCommitResponse.
wirePokeTxnOffsetCommitResponse :: Int -> Ptr Word8 -> TxnOffsetCommitResponse -> IO (Ptr Word8)
wirePokeTxnOffsetCommitResponse version basePtr msg
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (txnOffsetCommitResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitResponseTopic version p x) p1 (txnOffsetCommitResponseTopics msg)
    pure p2
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (txnOffsetCommitResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeTxnOffsetCommitResponseTopic version p x) p1 (txnOffsetCommitResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke TxnOffsetCommitResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for TxnOffsetCommitResponse.
wirePeekTxnOffsetCommitResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TxnOffsetCommitResponse, Ptr Word8)
wirePeekTxnOffsetCommitResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pure (TxnOffsetCommitResponse { txnOffsetCommitResponseThrottleTimeMs = f0_throttletimems, txnOffsetCommitResponseTopics = f1_topics }, p2)
  | version >= 3 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekTxnOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (TxnOffsetCommitResponse { txnOffsetCommitResponseThrottleTimeMs = f0_throttletimems, txnOffsetCommitResponseTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek TxnOffsetCommitResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec TxnOffsetCommitResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeTxnOffsetCommitResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeTxnOffsetCommitResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekTxnOffsetCommitResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}