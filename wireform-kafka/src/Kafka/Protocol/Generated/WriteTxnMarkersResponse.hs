{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteTxnMarkersResponse
Description : Kafka WriteTxnMarkersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 27.



Valid versions: 1-2
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteTxnMarkersResponse
  (
    WriteTxnMarkersResponse(..),
    WritableTxnMarkerResult(..),
    WritableTxnMarkerTopicResult(..),
    WritableTxnMarkerPartitionResult(..),
    maxWriteTxnMarkersResponseVersion
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


-- | The results by partition.
data WritableTxnMarkerPartitionResult = WritableTxnMarkerPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  writableTxnMarkerPartitionResultPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  writableTxnMarkerPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The results by topic.
data WritableTxnMarkerTopicResult = WritableTxnMarkerTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  writableTxnMarkerTopicResultName :: !(KafkaString)
,

  -- | The results by partition.

  -- Versions: 0+
  writableTxnMarkerTopicResultPartitions :: !(KafkaArray (WritableTxnMarkerPartitionResult))

  }
  deriving (Eq, Show, Generic)

-- | The results for writing makers.
data WritableTxnMarkerResult = WritableTxnMarkerResult
  {

  -- | The current producer ID in use by the transactional ID.

  -- Versions: 0+
  writableTxnMarkerResultProducerId :: !(Int64)
,

  -- | The results by topic.

  -- Versions: 0+
  writableTxnMarkerResultTopics :: !(KafkaArray (WritableTxnMarkerTopicResult))

  }
  deriving (Eq, Show, Generic)


data WriteTxnMarkersResponse = WriteTxnMarkersResponse
  {

  -- | The results for writing makers.

  -- Versions: 0+
  writeTxnMarkersResponseMarkers :: !(KafkaArray (WritableTxnMarkerResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteTxnMarkersResponse.
maxWriteTxnMarkersResponseVersion :: Int16
maxWriteTxnMarkersResponseVersion = 2

-- | KafkaMessage instance for WriteTxnMarkersResponse.
instance KafkaMessage WriteTxnMarkersResponse where
  messageApiKey = 27
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a WritableTxnMarkerPartitionResult.
wireMaxSizeWritableTxnMarkerPartitionResult :: Int -> WritableTxnMarkerPartitionResult -> Int
wireMaxSizeWritableTxnMarkerPartitionResult _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for WritableTxnMarkerPartitionResult.
wirePokeWritableTxnMarkerPartitionResult :: Int -> Ptr Word8 -> WritableTxnMarkerPartitionResult -> IO (Ptr Word8)
wirePokeWritableTxnMarkerPartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (writableTxnMarkerPartitionResultPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (writableTxnMarkerPartitionResultErrorCode msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WritableTxnMarkerPartitionResult.
wirePeekWritableTxnMarkerPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WritableTxnMarkerPartitionResult, Ptr Word8)
wirePeekWritableTxnMarkerPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WritableTxnMarkerPartitionResult { writableTxnMarkerPartitionResultPartitionIndex = f0_partitionindex, writableTxnMarkerPartitionResultErrorCode = f1_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWritableTxnMarkerPartitionResult :: WritableTxnMarkerPartitionResult
defaultWritableTxnMarkerPartitionResult = WritableTxnMarkerPartitionResult { writableTxnMarkerPartitionResultPartitionIndex = 0, writableTxnMarkerPartitionResultErrorCode = 0 }

-- | Worst-case wire size of a WritableTxnMarkerTopicResult.
wireMaxSizeWritableTxnMarkerTopicResult :: Int -> WritableTxnMarkerTopicResult -> Int
wireMaxSizeWritableTxnMarkerTopicResult _version msg =
  0
  + WP.dualStringMaxSize (writableTxnMarkerTopicResultName msg)
  + (5 + (case P.unKafkaArray (writableTxnMarkerTopicResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWritableTxnMarkerPartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WritableTxnMarkerTopicResult.
wirePokeWritableTxnMarkerTopicResult :: Int -> Ptr Word8 -> WritableTxnMarkerTopicResult -> IO (Ptr Word8)
wirePokeWritableTxnMarkerTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (writableTxnMarkerTopicResultName msg)) else WP.pokeKafkaString p0 (writableTxnMarkerTopicResultName msg))
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeWritableTxnMarkerPartitionResult version p x) p1 (writableTxnMarkerTopicResultPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WritableTxnMarkerTopicResult.
wirePeekWritableTxnMarkerTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WritableTxnMarkerTopicResult, Ptr Word8)
wirePeekWritableTxnMarkerTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekWritableTxnMarkerPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WritableTxnMarkerTopicResult { writableTxnMarkerTopicResultName = f0_name, writableTxnMarkerTopicResultPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWritableTxnMarkerTopicResult :: WritableTxnMarkerTopicResult
defaultWritableTxnMarkerTopicResult = WritableTxnMarkerTopicResult { writableTxnMarkerTopicResultName = P.KafkaString Null, writableTxnMarkerTopicResultPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a WritableTxnMarkerResult.
wireMaxSizeWritableTxnMarkerResult :: Int -> WritableTxnMarkerResult -> Int
wireMaxSizeWritableTxnMarkerResult _version msg =
  0
  + 8
  + (5 + (case P.unKafkaArray (writableTxnMarkerResultTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWritableTxnMarkerTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WritableTxnMarkerResult.
wirePokeWritableTxnMarkerResult :: Int -> Ptr Word8 -> WritableTxnMarkerResult -> IO (Ptr Word8)
wirePokeWritableTxnMarkerResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (writableTxnMarkerResultProducerId msg)
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeWritableTxnMarkerTopicResult version p x) p1 (writableTxnMarkerResultTopics msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WritableTxnMarkerResult.
wirePeekWritableTxnMarkerResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WritableTxnMarkerResult, Ptr Word8)
wirePeekWritableTxnMarkerResult version _fp _basePtr p0 endPtr = do
  (f0_producerid, p1) <- W.peekInt64BE p0 endPtr
  (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekWritableTxnMarkerTopicResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WritableTxnMarkerResult { writableTxnMarkerResultProducerId = f0_producerid, writableTxnMarkerResultTopics = f1_topics }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWritableTxnMarkerResult :: WritableTxnMarkerResult
defaultWritableTxnMarkerResult = WritableTxnMarkerResult { writableTxnMarkerResultProducerId = 0, writableTxnMarkerResultTopics = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a WriteTxnMarkersResponse.
wireMaxSizeWriteTxnMarkersResponse :: Int -> WriteTxnMarkersResponse -> Int
wireMaxSizeWriteTxnMarkersResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (writeTxnMarkersResponseMarkers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWritableTxnMarkerResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteTxnMarkersResponse.
wirePokeWriteTxnMarkersResponse :: Int -> Ptr Word8 -> WriteTxnMarkersResponse -> IO (Ptr Word8)
wirePokeWriteTxnMarkersResponse version basePtr msg
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeWritableTxnMarkerResult version p x) p0 (writeTxnMarkersResponseMarkers msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke WriteTxnMarkersResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for WriteTxnMarkersResponse.
wirePeekWriteTxnMarkersResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteTxnMarkersResponse, Ptr Word8)
wirePeekWriteTxnMarkersResponse version _fp _basePtr p0 endPtr
  | version >= 1 && version <= 2 = do
    (f0_markers, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekWritableTxnMarkerResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (WriteTxnMarkersResponse { writeTxnMarkersResponseMarkers = f0_markers }, pTagsEnd)
  | otherwise = error $ "wirePeek WriteTxnMarkersResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec WriteTxnMarkersResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeWriteTxnMarkersResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeWriteTxnMarkersResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekWriteTxnMarkersResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}