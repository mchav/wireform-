{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ProduceRequest
Description : Kafka ProduceRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 0.



Valid versions: 3-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ProduceRequest
  (
    ProduceRequest(..),
    TopicProduceData(..),
    PartitionProduceData(..),
    encodeProduceRequest,
    decodeProduceRequest,
    maxProduceRequestVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | Each partition to produce to.
data PartitionProduceData = PartitionProduceData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionProduceDataIndex :: !(Int32)
,

  -- | The record data to be produced.

  -- Versions: 0+
  partitionProduceDataRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionProduceData with version-aware field handling.
encodePartitionProduceData :: MonadPut m => E.ApiVersion -> PartitionProduceData -> m ()
encodePartitionProduceData version pmsg =
  do
    serialize (partitionProduceDataIndex pmsg)
    if version >= 9 then serialize (toCompactBytes (partitionProduceDataRecords pmsg)) else serialize (partitionProduceDataRecords pmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionProduceData with version-aware field handling.
decodePartitionProduceData :: MonadGet m => E.ApiVersion -> m PartitionProduceData
decodePartitionProduceData version =
  do
    fieldindex <- deserialize
    fieldrecords <- if version >= 9 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionProduceData
      {
      partitionProduceDataIndex = fieldindex
      ,
      partitionProduceDataRecords = fieldrecords
      }


-- | Each topic to produce to.
data TopicProduceData = TopicProduceData
  {

  -- | The topic name.

  -- Versions: 0-12
  topicProduceDataName :: !(KafkaString)
,

  -- | The unique topic ID

  -- Versions: 13+
  topicProduceDataTopicId :: !(KafkaUuid)
,

  -- | Each partition to produce to.

  -- Versions: 0+
  topicProduceDataPartitionData :: !(KafkaArray (PartitionProduceData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicProduceData with version-aware field handling.
encodeTopicProduceData :: MonadPut m => E.ApiVersion -> TopicProduceData -> m ()
encodeTopicProduceData version tmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 9 then serialize (toCompactString (topicProduceDataName tmsg)) else serialize (topicProduceDataName tmsg)
    when (version >= 13) $
      serialize (topicProduceDataTopicId tmsg)
    E.encodeVersionedArray version 9 encodePartitionProduceData (case P.unKafkaArray (topicProduceDataPartitionData tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicProduceData with version-aware field handling.
decodeTopicProduceData :: MonadGet m => E.ApiVersion -> m TopicProduceData
decodeTopicProduceData version =
  do
    fieldname <- if version >= 0 && version <= 12
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitiondata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodePartitionProduceData
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicProduceData
      {
      topicProduceDataName = fieldname
      ,
      topicProduceDataTopicId = fieldtopicid
      ,
      topicProduceDataPartitionData = fieldpartitiondata
      }



data ProduceRequest = ProduceRequest
  {

  -- | The transactional ID, or null if the producer is not transactional.

  -- Versions: 3+
  produceRequestTransactionalId :: !(KafkaString)
,

  -- | The number of acknowledgments the producer requires the leader to have received before considering a

  -- Versions: 0+
  produceRequestAcks :: !(Int16)
,

  -- | The timeout to await a response in milliseconds.

  -- Versions: 0+
  produceRequestTimeoutMs :: !(Int32)
,

  -- | Each topic to produce to.

  -- Versions: 0+
  produceRequestTopicData :: !(KafkaArray (TopicProduceData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ProduceRequest.
maxProduceRequestVersion :: Int16
maxProduceRequestVersion = 13

-- | KafkaMessage instance for ProduceRequest.
instance KafkaMessage ProduceRequest where
  messageApiKey = 0
  messageMinVersion = 3
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode ProduceRequest with the given API version.
encodeProduceRequest :: MonadPut m => E.ApiVersion -> ProduceRequest -> m ()
encodeProduceRequest version msg
  | version >= 9 && version <= 13 =
    do
      serialize (toCompactString (produceRequestTransactionalId msg))
      serialize (produceRequestAcks msg)
      serialize (produceRequestTimeoutMs msg)
      E.encodeVersionedArray version 9 encodeTopicProduceData (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 8 =
    do
      serialize (produceRequestTransactionalId msg)
      serialize (produceRequestAcks msg)
      serialize (produceRequestTimeoutMs msg)
      E.encodeVersionedArray version 9 encodeTopicProduceData (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ProduceRequest with the given API version.
decodeProduceRequest :: MonadGet m => E.ApiVersion -> m ProduceRequest
decodeProduceRequest version
  | version >= 9 && version <= 13 =
    do
      fieldtransactionalid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldacks <- deserialize
      fieldtimeoutms <- deserialize
      fieldtopicdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ProduceRequest
        {
        produceRequestTransactionalId = fieldtransactionalid
        ,
        produceRequestAcks = fieldacks
        ,
        produceRequestTimeoutMs = fieldtimeoutms
        ,
        produceRequestTopicData = fieldtopicdata
        }

  | version >= 3 && version <= 8 =
    do
      fieldtransactionalid <- deserialize
      fieldacks <- deserialize
      fieldtimeoutms <- deserialize
      fieldtopicdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceData
      pure ProduceRequest
        {
        produceRequestTransactionalId = fieldtransactionalid
        ,
        produceRequestAcks = fieldacks
        ,
        produceRequestTimeoutMs = fieldtimeoutms
        ,
        produceRequestTopicData = fieldtopicdata
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionProduceData.
wireMaxSizePartitionProduceData :: Int -> PartitionProduceData -> Int
wireMaxSizePartitionProduceData _version msg =
  0
  + 4
  + WP.compactBytesMaxSize (P.toCompactBytes (partitionProduceDataRecords msg))
  + 1

-- | Direct-poke encoder for PartitionProduceData.
wirePokePartitionProduceData :: Int -> Ptr Word8 -> PartitionProduceData -> IO (Ptr Word8)
wirePokePartitionProduceData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionProduceDataIndex msg)
  p2 <- WP.pokeCompactBytes p1 (P.toCompactBytes (partitionProduceDataRecords msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for PartitionProduceData.
wirePeekPartitionProduceData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionProduceData, Ptr Word8)
wirePeekPartitionProduceData version _fp _basePtr p0 endPtr = do
  (f0_index, p1) <- W.peekInt32BE p0 endPtr
  (f1_records, p2) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (PartitionProduceData { partitionProduceDataIndex = f0_index, partitionProduceDataRecords = f1_records }, pTagsEnd)

-- | Worst-case wire size of a TopicProduceData.
wireMaxSizeTopicProduceData :: Int -> TopicProduceData -> Int
wireMaxSizeTopicProduceData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicProduceDataName msg))
  + 16
  + (5 + (case P.unKafkaArray (topicProduceDataPartitionData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionProduceData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicProduceData.
wirePokeTopicProduceData :: Int -> Ptr Word8 -> TopicProduceData -> IO (Ptr Word8)
wirePokeTopicProduceData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicProduceDataName msg))
  p2 <- WP.pokeKafkaUuid p1 (topicProduceDataTopicId msg)
  p3 <- WP.pokeVersionedArray version 9 (\p x -> wirePokePartitionProduceData version p x) p2 (topicProduceDataPartitionData msg)
  if version >= 9 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TopicProduceData.
wirePeekTopicProduceData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicProduceData, Ptr Word8)
wirePeekTopicProduceData version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitiondata, p3) <- WP.peekVersionedArray version 9 (\p e -> wirePeekPartitionProduceData version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TopicProduceData { topicProduceDataName = f0_name, topicProduceDataTopicId = f1_topicid, topicProduceDataPartitionData = f2_partitiondata }, pTagsEnd)

-- | Worst-case wire size of a ProduceRequest.
wireMaxSizeProduceRequest :: Int -> ProduceRequest -> Int
wireMaxSizeProduceRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (produceRequestTransactionalId msg))
  + 2
  + 4
  + (5 + (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicProduceData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ProduceRequest.
wirePokeProduceRequest :: Int -> Ptr Word8 -> ProduceRequest -> IO (Ptr Word8)
wirePokeProduceRequest version basePtr msg
  | version >= 9 && version <= 13 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (produceRequestTransactionalId msg))
    p2 <- W.pokeInt16BE p1 (produceRequestAcks msg)
    p3 <- W.pokeInt32BE p2 (produceRequestTimeoutMs msg)
    p4 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceData version p x) p3 (produceRequestTopicData msg)
    WP.pokeEmptyTaggedFields p4
  | version >= 3 && version <= 8 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (produceRequestTransactionalId msg))
    p2 <- W.pokeInt16BE p1 (produceRequestAcks msg)
    p3 <- W.pokeInt32BE p2 (produceRequestTimeoutMs msg)
    p4 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceData version p x) p3 (produceRequestTopicData msg)
    pure p4
  | otherwise = error $ "wirePoke ProduceRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ProduceRequest.
wirePeekProduceRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ProduceRequest, Ptr Word8)
wirePeekProduceRequest version _fp _basePtr p0 endPtr
  | version >= 9 && version <= 13 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_acks, p2) <- W.peekInt16BE p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    (f3_topicdata, p4) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceData version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ProduceRequest { produceRequestTransactionalId = f0_transactionalid, produceRequestAcks = f1_acks, produceRequestTimeoutMs = f2_timeoutms, produceRequestTopicData = f3_topicdata }, pTagsEnd)
  | version >= 3 && version <= 8 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_acks, p2) <- W.peekInt16BE p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    (f3_topicdata, p4) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceData version _fp _basePtr p e) p3 endPtr
    pure (ProduceRequest { produceRequestTransactionalId = f0_transactionalid, produceRequestAcks = f1_acks, produceRequestTimeoutMs = f2_timeoutms, produceRequestTopicData = f3_topicdata }, p4)
  | otherwise = error $ "wirePeek ProduceRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ProduceRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeProduceRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeProduceRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekProduceRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}