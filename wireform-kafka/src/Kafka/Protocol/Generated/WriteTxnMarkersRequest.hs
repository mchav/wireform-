{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteTxnMarkersRequest
Description : Kafka WriteTxnMarkersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 27.



Valid versions: 1-2
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteTxnMarkersRequest
  (
    WriteTxnMarkersRequest(..),
    WritableTxnMarker(..),
    WritableTxnMarkerTopic(..),
    maxWriteTxnMarkersRequestVersion
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


-- | Each topic that we want to write transaction marker(s) for.
data WritableTxnMarkerTopic = WritableTxnMarkerTopic
  {

  -- | The topic name.

  -- Versions: 0+
  writableTxnMarkerTopicName :: !(KafkaString)
,

  -- | The indexes of the partitions to write transaction markers for.

  -- Versions: 0+
  writableTxnMarkerTopicPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | The transaction markers to be written.
data WritableTxnMarker = WritableTxnMarker
  {

  -- | The current producer ID.

  -- Versions: 0+
  writableTxnMarkerProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer ID.

  -- Versions: 0+
  writableTxnMarkerProducerEpoch :: !(Int16)
,

  -- | The result of the transaction to write to the partitions (false = ABORT, true = COMMIT).

  -- Versions: 0+
  writableTxnMarkerTransactionResult :: !(Bool)
,

  -- | Each topic that we want to write transaction marker(s) for.

  -- Versions: 0+
  writableTxnMarkerTopics :: !(KafkaArray (WritableTxnMarkerTopic))
,

  -- | Epoch associated with the transaction state partition hosted by this transaction coordinator.

  -- Versions: 0+
  writableTxnMarkerCoordinatorEpoch :: !(Int32)
,

  -- | Transaction version of the marker. Ex: 0/1 = legacy (TV0/TV1), 2 = TV2 etc.

  -- Versions: 2+
  writableTxnMarkerTransactionVersion :: !(Int8)

  }
  deriving (Eq, Show, Generic)


data WriteTxnMarkersRequest = WriteTxnMarkersRequest
  {

  -- | The transaction markers to be written.

  -- Versions: 0+
  writeTxnMarkersRequestMarkers :: !(KafkaArray (WritableTxnMarker))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteTxnMarkersRequest.
maxWriteTxnMarkersRequestVersion :: Int16
maxWriteTxnMarkersRequestVersion = 2

-- | KafkaMessage instance for WriteTxnMarkersRequest.
instance KafkaMessage WriteTxnMarkersRequest where
  messageApiKey = 27
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a WritableTxnMarkerTopic.
wireMaxSizeWritableTxnMarkerTopic :: Int -> WritableTxnMarkerTopic -> Int
wireMaxSizeWritableTxnMarkerTopic _version msg =
  0
  + WP.dualStringMaxSize (writableTxnMarkerTopicName msg)
  + (5 + (case P.unKafkaArray (writableTxnMarkerTopicPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WritableTxnMarkerTopic.
wirePokeWritableTxnMarkerTopic :: Int -> Ptr Word8 -> WritableTxnMarkerTopic -> IO (Ptr Word8)
wirePokeWritableTxnMarkerTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (writableTxnMarkerTopicName msg)) else WP.pokeKafkaString p0 (writableTxnMarkerTopicName msg))
  p2 <- WP.pokeVersionedArray version 1 W.pokeInt32BE p1 (writableTxnMarkerTopicPartitionIndexes msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WritableTxnMarkerTopic.
wirePeekWritableTxnMarkerTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WritableTxnMarkerTopic, Ptr Word8)
wirePeekWritableTxnMarkerTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitionindexes, p2) <- WP.peekVersionedArray version 1 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WritableTxnMarkerTopic { writableTxnMarkerTopicName = f0_name, writableTxnMarkerTopicPartitionIndexes = f1_partitionindexes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWritableTxnMarkerTopic :: WritableTxnMarkerTopic
defaultWritableTxnMarkerTopic = WritableTxnMarkerTopic { writableTxnMarkerTopicName = P.KafkaString Null, writableTxnMarkerTopicPartitionIndexes = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a WritableTxnMarker.
wireMaxSizeWritableTxnMarker :: Int -> WritableTxnMarker -> Int
wireMaxSizeWritableTxnMarker _version msg =
  0
  + 8
  + 2
  + 1
  + (5 + (case P.unKafkaArray (writableTxnMarkerTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWritableTxnMarkerTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1
  + 1

-- | Direct-poke encoder for WritableTxnMarker.
wirePokeWritableTxnMarker :: Int -> Ptr Word8 -> WritableTxnMarker -> IO (Ptr Word8)
wirePokeWritableTxnMarker version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (writableTxnMarkerProducerId msg)
  p2 <- W.pokeInt16BE p1 (writableTxnMarkerProducerEpoch msg)
  p3 <- W.pokeWord8 p2 (if (writableTxnMarkerTransactionResult msg) then 1 else 0)
  p4 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeWritableTxnMarkerTopic version p x) p3 (writableTxnMarkerTopics msg)
  p5 <- W.pokeInt32BE p4 (writableTxnMarkerCoordinatorEpoch msg)
  p6 <- (if version >= 2 then W.pokeWord8 p5 (fromIntegral (writableTxnMarkerTransactionVersion msg)) else pure p5)
  if version >= 1 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for WritableTxnMarker.
wirePeekWritableTxnMarker :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WritableTxnMarker, Ptr Word8)
wirePeekWritableTxnMarker version _fp _basePtr p0 endPtr = do
  (f0_producerid, p1) <- W.peekInt64BE p0 endPtr
  (f1_producerepoch, p2) <- W.peekInt16BE p1 endPtr
  (f2_transactionresult, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
  (f3_topics, p4) <- WP.peekVersionedArray version 1 (\p e -> wirePeekWritableTxnMarkerTopic version _fp _basePtr p e) p3 endPtr
  (f4_coordinatorepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_transactionversion, p6) <- (if version >= 2 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr else pure (0, p5))
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (WritableTxnMarker { writableTxnMarkerProducerId = f0_producerid, writableTxnMarkerProducerEpoch = f1_producerepoch, writableTxnMarkerTransactionResult = f2_transactionresult, writableTxnMarkerTopics = f3_topics, writableTxnMarkerCoordinatorEpoch = f4_coordinatorepoch, writableTxnMarkerTransactionVersion = f5_transactionversion }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWritableTxnMarker :: WritableTxnMarker
defaultWritableTxnMarker = WritableTxnMarker { writableTxnMarkerProducerId = 0, writableTxnMarkerProducerEpoch = 0, writableTxnMarkerTransactionResult = False, writableTxnMarkerTopics = P.mkKafkaArray V.empty, writableTxnMarkerCoordinatorEpoch = 0, writableTxnMarkerTransactionVersion = 0 }

-- | Worst-case wire size of a WriteTxnMarkersRequest.
wireMaxSizeWriteTxnMarkersRequest :: Int -> WriteTxnMarkersRequest -> Int
wireMaxSizeWriteTxnMarkersRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (writeTxnMarkersRequestMarkers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWritableTxnMarker _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteTxnMarkersRequest.
wirePokeWriteTxnMarkersRequest :: Int -> Ptr Word8 -> WriteTxnMarkersRequest -> IO (Ptr Word8)
wirePokeWriteTxnMarkersRequest version basePtr msg
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeWritableTxnMarker version p x) p0 (writeTxnMarkersRequestMarkers msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke WriteTxnMarkersRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for WriteTxnMarkersRequest.
wirePeekWriteTxnMarkersRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteTxnMarkersRequest, Ptr Word8)
wirePeekWriteTxnMarkersRequest version _fp _basePtr p0 endPtr
  | version >= 1 && version <= 2 = do
    (f0_markers, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekWritableTxnMarker version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (WriteTxnMarkersRequest { writeTxnMarkersRequestMarkers = f0_markers }, pTagsEnd)
  | otherwise = error $ "wirePeek WriteTxnMarkersRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec WriteTxnMarkersRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeWriteTxnMarkersRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeWriteTxnMarkersRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekWriteTxnMarkersRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}