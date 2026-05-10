{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteShareGroupStateResponse
Description : Kafka WriteShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 85.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteShareGroupStateResponse
  (
    WriteShareGroupStateResponse(..),
    WriteStateResult(..),
    PartitionResult(..),
    maxWriteShareGroupStateResponseVersion
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

  }
  deriving (Eq, Show, Generic)

-- | The write results.
data WriteStateResult = WriteStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  writeStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  writeStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


data WriteShareGroupStateResponse = WriteShareGroupStateResponse
  {

  -- | The write results.

  -- Versions: 0+
  writeShareGroupStateResponseResults :: !(KafkaArray (WriteStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteShareGroupStateResponse.
maxWriteShareGroupStateResponseVersion :: Int16
maxWriteShareGroupStateResponseVersion = 1

-- | KafkaMessage instance for WriteShareGroupStateResponse.
instance KafkaMessage WriteShareGroupStateResponse where
  messageApiKey = 85
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionResult.
wireMaxSizePartitionResult :: Int -> PartitionResult -> Int
wireMaxSizePartitionResult _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for PartitionResult.
wirePokePartitionResult :: Int -> Ptr Word8 -> PartitionResult -> IO (Ptr Word8)
wirePokePartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResultPartition msg)
  p2 <- W.pokeInt16BE p1 (partitionResultErrorCode msg)
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (partitionResultErrorMessage msg)) else WP.pokeKafkaString p2 (partitionResultErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for PartitionResult.
wirePeekPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResult, Ptr Word8)
wirePeekPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (PartitionResult { partitionResultPartition = f0_partition, partitionResultErrorCode = f1_errorcode, partitionResultErrorMessage = f2_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionResult :: PartitionResult
defaultPartitionResult = PartitionResult { partitionResultPartition = 0, partitionResultErrorCode = 0, partitionResultErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a WriteStateResult.
wireMaxSizeWriteStateResult :: Int -> WriteStateResult -> Int
wireMaxSizeWriteStateResult _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (writeStateResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteStateResult.
wirePokeWriteStateResult :: Int -> Ptr Word8 -> WriteStateResult -> IO (Ptr Word8)
wirePokeWriteStateResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (writeStateResultTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResult version p x) p1 (writeStateResultPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WriteStateResult.
wirePeekWriteStateResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteStateResult, Ptr Word8)
wirePeekWriteStateResult version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WriteStateResult { writeStateResultTopicId = f0_topicid, writeStateResultPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultWriteStateResult :: WriteStateResult
defaultWriteStateResult = WriteStateResult { writeStateResultTopicId = P.nullUuid, writeStateResultPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a WriteShareGroupStateResponse.
wireMaxSizeWriteShareGroupStateResponse :: Int -> WriteShareGroupStateResponse -> Int
wireMaxSizeWriteShareGroupStateResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (writeShareGroupStateResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWriteStateResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteShareGroupStateResponse.
wirePokeWriteShareGroupStateResponse :: Int -> Ptr Word8 -> WriteShareGroupStateResponse -> IO (Ptr Word8)
wirePokeWriteShareGroupStateResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeWriteStateResult version p x) p0 (writeShareGroupStateResponseResults msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke WriteShareGroupStateResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for WriteShareGroupStateResponse.
wirePeekWriteShareGroupStateResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteShareGroupStateResponse, Ptr Word8)
wirePeekWriteShareGroupStateResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_results, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekWriteStateResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (WriteShareGroupStateResponse { writeShareGroupStateResponseResults = f0_results }, pTagsEnd)
  | otherwise = error $ "wirePeek WriteShareGroupStateResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec WriteShareGroupStateResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeWriteShareGroupStateResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeWriteShareGroupStateResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekWriteShareGroupStateResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}