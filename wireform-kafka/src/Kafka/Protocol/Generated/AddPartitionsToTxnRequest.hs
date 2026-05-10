{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddPartitionsToTxnRequest
Description : Kafka AddPartitionsToTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 24.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddPartitionsToTxnRequest
  (
    AddPartitionsToTxnRequest(..),
    AddPartitionsToTxnTransaction(..),
    AddPartitionsToTxnTopic(..),
    maxAddPartitionsToTxnRequestVersion
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


data AddPartitionsToTxnTopic = AddPartitionsToTxnTopic
  {

  -- | The name of the topic.

  -- Versions: 0+
  addPartitionsToTxnTopicName :: !(KafkaString)
,

  -- | The partition indexes to add to the transaction.

  -- Versions: 0+
  addPartitionsToTxnTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | List of transactions to add partitions to.
data AddPartitionsToTxnTransaction = AddPartitionsToTxnTransaction
  {

  -- | The transactional id corresponding to the transaction.

  -- Versions: 4+
  addPartitionsToTxnTransactionTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 4+
  addPartitionsToTxnTransactionProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 4+
  addPartitionsToTxnTransactionProducerEpoch :: !(Int16)
,

  -- | Boolean to signify if we want to check if the partition is in the transaction rather than add it.

  -- Versions: 4+
  addPartitionsToTxnTransactionVerifyOnly :: !(Bool)
,

  -- | The partitions to add to the transaction.

  -- Versions: 4+
  addPartitionsToTxnTransactionTopics :: !(KafkaArray (AddPartitionsToTxnTopic))

  }
  deriving (Eq, Show, Generic)


data AddPartitionsToTxnRequest = AddPartitionsToTxnRequest
  {

  -- | List of transactions to add partitions to.

  -- Versions: 4+
  addPartitionsToTxnRequestTransactions :: !(KafkaArray (AddPartitionsToTxnTransaction))
,

  -- | The transactional id corresponding to the transaction.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowProducerEpoch :: !(Int16)
,

  -- | The partitions to add to the transaction.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowTopics :: !(KafkaArray (AddPartitionsToTxnTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddPartitionsToTxnRequest.
maxAddPartitionsToTxnRequestVersion :: Int16
maxAddPartitionsToTxnRequestVersion = 5

-- | KafkaMessage instance for AddPartitionsToTxnRequest.
instance KafkaMessage AddPartitionsToTxnRequest where
  messageApiKey = 24
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a AddPartitionsToTxnTopic.
wireMaxSizeAddPartitionsToTxnTopic :: Int -> AddPartitionsToTxnTopic -> Int
wireMaxSizeAddPartitionsToTxnTopic _version msg =
  0
  + WP.dualStringMaxSize (addPartitionsToTxnTopicName msg)
  + (5 + (case P.unKafkaArray (addPartitionsToTxnTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnTopic.
wirePokeAddPartitionsToTxnTopic :: Int -> Ptr Word8 -> AddPartitionsToTxnTopic -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnTopicName msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnTopicName msg))
  p2 <- WP.pokeVersionedArray version 3 W.pokeInt32BE p1 (addPartitionsToTxnTopicPartitions msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AddPartitionsToTxnTopic.
wirePeekAddPartitionsToTxnTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnTopic, Ptr Word8)
wirePeekAddPartitionsToTxnTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 3 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AddPartitionsToTxnTopic { addPartitionsToTxnTopicName = f0_name, addPartitionsToTxnTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAddPartitionsToTxnTopic :: AddPartitionsToTxnTopic
defaultAddPartitionsToTxnTopic = AddPartitionsToTxnTopic { addPartitionsToTxnTopicName = P.KafkaString Null, addPartitionsToTxnTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AddPartitionsToTxnTransaction.
wireMaxSizeAddPartitionsToTxnTransaction :: Int -> AddPartitionsToTxnTransaction -> Int
wireMaxSizeAddPartitionsToTxnTransaction _version msg =
  0
  + WP.dualStringMaxSize (addPartitionsToTxnTransactionTransactionalId msg)
  + 8
  + 2
  + 1
  + (5 + (case P.unKafkaArray (addPartitionsToTxnTransactionTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnTransaction.
wirePokeAddPartitionsToTxnTransaction :: Int -> Ptr Word8 -> AddPartitionsToTxnTransaction -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnTransaction version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnTransactionTransactionalId msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnTransactionTransactionalId msg)) else pure p0)
  p2 <- (if version >= 4 then W.pokeInt64BE p1 (addPartitionsToTxnTransactionProducerId msg) else pure p1)
  p3 <- (if version >= 4 then W.pokeInt16BE p2 (addPartitionsToTxnTransactionProducerEpoch msg) else pure p2)
  p4 <- (if version >= 4 then W.pokeWord8 p3 (if (addPartitionsToTxnTransactionVerifyOnly msg) then 1 else 0) else pure p3)
  p5 <- (if version >= 4 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopic version p x) p4 (addPartitionsToTxnTransactionTopics msg) else pure p4)
  if version >= 3 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for AddPartitionsToTxnTransaction.
wirePeekAddPartitionsToTxnTransaction :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnTransaction, Ptr Word8)
wirePeekAddPartitionsToTxnTransaction version _fp _basePtr p0 endPtr = do
  (f0_transactionalid, p1) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_producerid, p2) <- (if version >= 4 then W.peekInt64BE p1 endPtr else pure (0, p1))
  (f2_producerepoch, p3) <- (if version >= 4 then W.peekInt16BE p2 endPtr else pure (0, p2))
  (f3_verifyonly, p4) <- (if version >= 4 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr else pure (False, p3))
  (f4_topics, p5) <- (if version >= 4 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopic version _fp _basePtr p e) p4 endPtr else pure (P.mkKafkaArray V.empty, p4))
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (AddPartitionsToTxnTransaction { addPartitionsToTxnTransactionTransactionalId = f0_transactionalid, addPartitionsToTxnTransactionProducerId = f1_producerid, addPartitionsToTxnTransactionProducerEpoch = f2_producerepoch, addPartitionsToTxnTransactionVerifyOnly = f3_verifyonly, addPartitionsToTxnTransactionTopics = f4_topics }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAddPartitionsToTxnTransaction :: AddPartitionsToTxnTransaction
defaultAddPartitionsToTxnTransaction = AddPartitionsToTxnTransaction { addPartitionsToTxnTransactionTransactionalId = P.KafkaString Null, addPartitionsToTxnTransactionProducerId = 0, addPartitionsToTxnTransactionProducerEpoch = 0, addPartitionsToTxnTransactionVerifyOnly = False, addPartitionsToTxnTransactionTopics = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AddPartitionsToTxnRequest.
wireMaxSizeAddPartitionsToTxnRequest :: Int -> AddPartitionsToTxnRequest -> Int
wireMaxSizeAddPartitionsToTxnRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (addPartitionsToTxnRequestTransactions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnTransaction _version x ) v); P.Null -> 0 }))
  + WP.dualStringMaxSize (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)
  + 8
  + 2
  + (5 + (case P.unKafkaArray (addPartitionsToTxnRequestV3AndBelowTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnRequest.
wirePokeAddPartitionsToTxnRequest :: Int -> Ptr Word8 -> AddPartitionsToTxnRequest -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- (if version <= 3 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)) else pure p0)
    p2 <- (if version <= 3 then W.pokeInt64BE p1 (addPartitionsToTxnRequestV3AndBelowProducerId msg) else pure p1)
    p3 <- (if version <= 3 then W.pokeInt16BE p2 (addPartitionsToTxnRequestV3AndBelowProducerEpoch msg) else pure p2)
    p4 <- (if version <= 3 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopic version p x) p3 (addPartitionsToTxnRequestV3AndBelowTopics msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTransaction version p x) p0 (addPartitionsToTxnRequestTransactions msg) else pure p0)
    WP.pokeEmptyTaggedFields p1
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version <= 3 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)) else pure p0)
    p2 <- (if version <= 3 then W.pokeInt64BE p1 (addPartitionsToTxnRequestV3AndBelowProducerId msg) else pure p1)
    p3 <- (if version <= 3 then W.pokeInt16BE p2 (addPartitionsToTxnRequestV3AndBelowProducerEpoch msg) else pure p2)
    p4 <- (if version <= 3 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopic version p x) p3 (addPartitionsToTxnRequestV3AndBelowTopics msg) else pure p3)
    pure p4
  | otherwise = error $ "wirePoke AddPartitionsToTxnRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AddPartitionsToTxnRequest.
wirePeekAddPartitionsToTxnRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnRequest, Ptr Word8)
wirePeekAddPartitionsToTxnRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_v3andbelowtransactionalid, p1) <- (if version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_v3andbelowproducerid, p2) <- (if version <= 3 then W.peekInt64BE p1 endPtr else pure (0, p1))
    (f2_v3andbelowproducerepoch, p3) <- (if version <= 3 then W.peekInt16BE p2 endPtr else pure (0, p2))
    (f3_v3andbelowtopics, p4) <- (if version <= 3 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopic version _fp _basePtr p e) p3 endPtr else pure (P.mkKafkaArray V.empty, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AddPartitionsToTxnRequest { addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty, addPartitionsToTxnRequestV3AndBelowTransactionalId = f0_v3andbelowtransactionalid, addPartitionsToTxnRequestV3AndBelowProducerId = f1_v3andbelowproducerid, addPartitionsToTxnRequestV3AndBelowProducerEpoch = f2_v3andbelowproducerepoch, addPartitionsToTxnRequestV3AndBelowTopics = f3_v3andbelowtopics }, pTagsEnd)
  | version >= 4 && version <= 5 = do
    (f0_transactions, p1) <- (if version >= 4 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTransaction version _fp _basePtr p e) p0 endPtr else pure (P.mkKafkaArray V.empty, p0))
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (AddPartitionsToTxnRequest { addPartitionsToTxnRequestTransactions = f0_transactions, addPartitionsToTxnRequestV3AndBelowTransactionalId = P.KafkaString Null, addPartitionsToTxnRequestV3AndBelowProducerId = 0, addPartitionsToTxnRequestV3AndBelowProducerEpoch = 0, addPartitionsToTxnRequestV3AndBelowTopics = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_v3andbelowtransactionalid, p1) <- (if version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_v3andbelowproducerid, p2) <- (if version <= 3 then W.peekInt64BE p1 endPtr else pure (0, p1))
    (f2_v3andbelowproducerepoch, p3) <- (if version <= 3 then W.peekInt16BE p2 endPtr else pure (0, p2))
    (f3_v3andbelowtopics, p4) <- (if version <= 3 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopic version _fp _basePtr p e) p3 endPtr else pure (P.mkKafkaArray V.empty, p3))
    pure (AddPartitionsToTxnRequest { addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty, addPartitionsToTxnRequestV3AndBelowTransactionalId = f0_v3andbelowtransactionalid, addPartitionsToTxnRequestV3AndBelowProducerId = f1_v3andbelowproducerid, addPartitionsToTxnRequestV3AndBelowProducerEpoch = f2_v3andbelowproducerepoch, addPartitionsToTxnRequestV3AndBelowTopics = f3_v3andbelowtopics }, p4)
  | otherwise = error $ "wirePeek AddPartitionsToTxnRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AddPartitionsToTxnRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddPartitionsToTxnRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddPartitionsToTxnRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddPartitionsToTxnRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}