{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
Description : Kafka ReadShareGroupStateSummaryResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 87.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
  (
    ReadShareGroupStateSummaryResponse(..),
    ReadStateSummaryResult(..),
    PartitionResult(..),
    maxReadShareGroupStateSummaryResponseVersion
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


-- | The results for the partitions.
data PartitionResult = PartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResultPartition :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)
,

  -- | The state epoch of the share-partition.

  -- Versions: 0+
  partitionResultStateEpoch :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  partitionResultStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The read results.
data ReadStateSummaryResult = ReadStateSummaryResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateSummaryResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  readStateSummaryResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


data ReadShareGroupStateSummaryResponse = ReadShareGroupStateSummaryResponse
  {

  -- | The read results.

  -- Versions: 0+
  readShareGroupStateSummaryResponseResults :: !(KafkaArray (ReadStateSummaryResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateSummaryResponse.
maxReadShareGroupStateSummaryResponseVersion :: Int16
maxReadShareGroupStateSummaryResponseVersion = 0

-- | KafkaMessage instance for ReadShareGroupStateSummaryResponse.
instance KafkaMessage ReadShareGroupStateSummaryResponse where
  messageApiKey = 87
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionResult.
wireMaxSizePartitionResult :: Int -> PartitionResult -> Int
wireMaxSizePartitionResult _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (partitionResultErrorMessage msg)
  + 4
  + 8
  + 1

-- | Direct-poke encoder for PartitionResult.
wirePokePartitionResult :: Int -> Ptr Word8 -> PartitionResult -> IO (Ptr Word8)
wirePokePartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResultPartition msg)
  p2 <- W.pokeInt16BE p1 (partitionResultErrorCode msg)
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (partitionResultErrorMessage msg)) else WP.pokeKafkaString p2 (partitionResultErrorMessage msg))
  p4 <- W.pokeInt32BE p3 (partitionResultStateEpoch msg)
  p5 <- W.pokeInt64BE p4 (partitionResultStartOffset msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for PartitionResult.
wirePeekPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResult, Ptr Word8)
wirePeekPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  (f3_stateepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_startoffset, p5) <- W.peekInt64BE p4 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (PartitionResult { partitionResultPartition = f0_partition, partitionResultErrorCode = f1_errorcode, partitionResultErrorMessage = f2_errormessage, partitionResultStateEpoch = f3_stateepoch, partitionResultStartOffset = f4_startoffset }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionResult :: PartitionResult
defaultPartitionResult = PartitionResult { partitionResultPartition = 0, partitionResultErrorCode = 0, partitionResultErrorMessage = P.KafkaString Null, partitionResultStateEpoch = 0, partitionResultStartOffset = 0 }

-- | Worst-case wire size of a ReadStateSummaryResult.
wireMaxSizeReadStateSummaryResult :: Int -> ReadStateSummaryResult -> Int
wireMaxSizeReadStateSummaryResult _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (readStateSummaryResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadStateSummaryResult.
wirePokeReadStateSummaryResult :: Int -> Ptr Word8 -> ReadStateSummaryResult -> IO (Ptr Word8)
wirePokeReadStateSummaryResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (readStateSummaryResultTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResult version p x) p1 (readStateSummaryResultPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReadStateSummaryResult.
wirePeekReadStateSummaryResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadStateSummaryResult, Ptr Word8)
wirePeekReadStateSummaryResult version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReadStateSummaryResult { readStateSummaryResultTopicId = f0_topicid, readStateSummaryResultPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReadStateSummaryResult :: ReadStateSummaryResult
defaultReadStateSummaryResult = ReadStateSummaryResult { readStateSummaryResultTopicId = P.nullUuid, readStateSummaryResultPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ReadShareGroupStateSummaryResponse.
wireMaxSizeReadShareGroupStateSummaryResponse :: Int -> ReadShareGroupStateSummaryResponse -> Int
wireMaxSizeReadShareGroupStateSummaryResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (readShareGroupStateSummaryResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReadStateSummaryResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadShareGroupStateSummaryResponse.
wirePokeReadShareGroupStateSummaryResponse :: Int -> Ptr Word8 -> ReadShareGroupStateSummaryResponse -> IO (Ptr Word8)
wirePokeReadShareGroupStateSummaryResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReadStateSummaryResult version p x) p0 (readShareGroupStateSummaryResponseResults msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke ReadShareGroupStateSummaryResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ReadShareGroupStateSummaryResponse.
wirePeekReadShareGroupStateSummaryResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadShareGroupStateSummaryResponse, Ptr Word8)
wirePeekReadShareGroupStateSummaryResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_results, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReadStateSummaryResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ReadShareGroupStateSummaryResponse { readShareGroupStateSummaryResponseResults = f0_results }, pTagsEnd)
  | otherwise = error $ "wirePeek ReadShareGroupStateSummaryResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ReadShareGroupStateSummaryResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeReadShareGroupStateSummaryResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeReadShareGroupStateSummaryResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekReadShareGroupStateSummaryResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}