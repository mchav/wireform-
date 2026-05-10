{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupStateResponse
Description : Kafka DeleteShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 86.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupStateResponse
  (
    DeleteShareGroupStateResponse(..),
    DeleteStateResult(..),
    PartitionResult(..),
    maxDeleteShareGroupStateResponseVersion
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

-- | The delete results.
data DeleteStateResult = DeleteStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  deleteStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  deleteStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


data DeleteShareGroupStateResponse = DeleteShareGroupStateResponse
  {

  -- | The delete results.

  -- Versions: 0+
  deleteShareGroupStateResponseResults :: !(KafkaArray (DeleteStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupStateResponse.
maxDeleteShareGroupStateResponseVersion :: Int16
maxDeleteShareGroupStateResponseVersion = 0

-- | KafkaMessage instance for DeleteShareGroupStateResponse.
instance KafkaMessage DeleteShareGroupStateResponse where
  messageApiKey = 86
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

-- | Worst-case wire size of a DeleteStateResult.
wireMaxSizeDeleteStateResult :: Int -> DeleteStateResult -> Int
wireMaxSizeDeleteStateResult _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (deleteStateResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteStateResult.
wirePokeDeleteStateResult :: Int -> Ptr Word8 -> DeleteStateResult -> IO (Ptr Word8)
wirePokeDeleteStateResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (deleteStateResultTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResult version p x) p1 (deleteStateResultPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteStateResult.
wirePeekDeleteStateResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteStateResult, Ptr Word8)
wirePeekDeleteStateResult version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteStateResult { deleteStateResultTopicId = f0_topicid, deleteStateResultPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeleteStateResult :: DeleteStateResult
defaultDeleteStateResult = DeleteStateResult { deleteStateResultTopicId = P.nullUuid, deleteStateResultPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DeleteShareGroupStateResponse.
wireMaxSizeDeleteShareGroupStateResponse :: Int -> DeleteShareGroupStateResponse -> Int
wireMaxSizeDeleteShareGroupStateResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (deleteShareGroupStateResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteStateResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteShareGroupStateResponse.
wirePokeDeleteShareGroupStateResponse :: Int -> Ptr Word8 -> DeleteShareGroupStateResponse -> IO (Ptr Word8)
wirePokeDeleteShareGroupStateResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDeleteStateResult version p x) p0 (deleteShareGroupStateResponseResults msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DeleteShareGroupStateResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteShareGroupStateResponse.
wirePeekDeleteShareGroupStateResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupStateResponse, Ptr Word8)
wirePeekDeleteShareGroupStateResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_results, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDeleteStateResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DeleteShareGroupStateResponse { deleteShareGroupStateResponseResults = f0_results }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteShareGroupStateResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteShareGroupStateResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteShareGroupStateResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteShareGroupStateResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteShareGroupStateResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}