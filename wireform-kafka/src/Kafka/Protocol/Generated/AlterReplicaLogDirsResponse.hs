{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterReplicaLogDirsResponse
Description : Kafka AlterReplicaLogDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 34.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterReplicaLogDirsResponse
  (
    AlterReplicaLogDirsResponse(..),
    AlterReplicaLogDirTopicResult(..),
    AlterReplicaLogDirPartitionResult(..),
    maxAlterReplicaLogDirsResponseVersion
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


-- | The results for each partition.
data AlterReplicaLogDirPartitionResult = AlterReplicaLogDirPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  alterReplicaLogDirPartitionResultPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  alterReplicaLogDirPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The results for each topic.
data AlterReplicaLogDirTopicResult = AlterReplicaLogDirTopicResult
  {

  -- | The name of the topic.

  -- Versions: 0+
  alterReplicaLogDirTopicResultTopicName :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  alterReplicaLogDirTopicResultPartitions :: !(KafkaArray (AlterReplicaLogDirPartitionResult))

  }
  deriving (Eq, Show, Generic)


data AlterReplicaLogDirsResponse = AlterReplicaLogDirsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  alterReplicaLogDirsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each topic.

  -- Versions: 0+
  alterReplicaLogDirsResponseResults :: !(KafkaArray (AlterReplicaLogDirTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterReplicaLogDirsResponse.
maxAlterReplicaLogDirsResponseVersion :: Int16
maxAlterReplicaLogDirsResponseVersion = 2

-- | KafkaMessage instance for AlterReplicaLogDirsResponse.
instance KafkaMessage AlterReplicaLogDirsResponse where
  messageApiKey = 34
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a AlterReplicaLogDirPartitionResult.
wireMaxSizeAlterReplicaLogDirPartitionResult :: Int -> AlterReplicaLogDirPartitionResult -> Int
wireMaxSizeAlterReplicaLogDirPartitionResult _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for AlterReplicaLogDirPartitionResult.
wirePokeAlterReplicaLogDirPartitionResult :: Int -> Ptr Word8 -> AlterReplicaLogDirPartitionResult -> IO (Ptr Word8)
wirePokeAlterReplicaLogDirPartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (alterReplicaLogDirPartitionResultPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (alterReplicaLogDirPartitionResultErrorCode msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterReplicaLogDirPartitionResult.
wirePeekAlterReplicaLogDirPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDirPartitionResult, Ptr Word8)
wirePeekAlterReplicaLogDirPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterReplicaLogDirPartitionResult { alterReplicaLogDirPartitionResultPartitionIndex = f0_partitionindex, alterReplicaLogDirPartitionResultErrorCode = f1_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterReplicaLogDirPartitionResult :: AlterReplicaLogDirPartitionResult
defaultAlterReplicaLogDirPartitionResult = AlterReplicaLogDirPartitionResult { alterReplicaLogDirPartitionResultPartitionIndex = 0, alterReplicaLogDirPartitionResultErrorCode = 0 }

-- | Worst-case wire size of a AlterReplicaLogDirTopicResult.
wireMaxSizeAlterReplicaLogDirTopicResult :: Int -> AlterReplicaLogDirTopicResult -> Int
wireMaxSizeAlterReplicaLogDirTopicResult _version msg =
  0
  + WP.dualStringMaxSize (alterReplicaLogDirTopicResultTopicName msg)
  + (5 + (case P.unKafkaArray (alterReplicaLogDirTopicResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterReplicaLogDirPartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterReplicaLogDirTopicResult.
wirePokeAlterReplicaLogDirTopicResult :: Int -> Ptr Word8 -> AlterReplicaLogDirTopicResult -> IO (Ptr Word8)
wirePokeAlterReplicaLogDirTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (alterReplicaLogDirTopicResultTopicName msg)) else WP.pokeKafkaString p0 (alterReplicaLogDirTopicResultTopicName msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDirPartitionResult version p x) p1 (alterReplicaLogDirTopicResultPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterReplicaLogDirTopicResult.
wirePeekAlterReplicaLogDirTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDirTopicResult, Ptr Word8)
wirePeekAlterReplicaLogDirTopicResult version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDirPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterReplicaLogDirTopicResult { alterReplicaLogDirTopicResultTopicName = f0_topicname, alterReplicaLogDirTopicResultPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterReplicaLogDirTopicResult :: AlterReplicaLogDirTopicResult
defaultAlterReplicaLogDirTopicResult = AlterReplicaLogDirTopicResult { alterReplicaLogDirTopicResultTopicName = P.KafkaString Null, alterReplicaLogDirTopicResultPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AlterReplicaLogDirsResponse.
wireMaxSizeAlterReplicaLogDirsResponse :: Int -> AlterReplicaLogDirsResponse -> Int
wireMaxSizeAlterReplicaLogDirsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (alterReplicaLogDirsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterReplicaLogDirTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterReplicaLogDirsResponse.
wirePokeAlterReplicaLogDirsResponse :: Int -> Ptr Word8 -> AlterReplicaLogDirsResponse -> IO (Ptr Word8)
wirePokeAlterReplicaLogDirsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterReplicaLogDirsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDirTopicResult version p x) p1 (alterReplicaLogDirsResponseResults msg)
    pure p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterReplicaLogDirsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDirTopicResult version p x) p1 (alterReplicaLogDirsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AlterReplicaLogDirsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterReplicaLogDirsResponse.
wirePeekAlterReplicaLogDirsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDirsResponse, Ptr Word8)
wirePeekAlterReplicaLogDirsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDirTopicResult version _fp _basePtr p e) p1 endPtr
    pure (AlterReplicaLogDirsResponse { alterReplicaLogDirsResponseThrottleTimeMs = f0_throttletimems, alterReplicaLogDirsResponseResults = f1_results }, p2)
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDirTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterReplicaLogDirsResponse { alterReplicaLogDirsResponseThrottleTimeMs = f0_throttletimems, alterReplicaLogDirsResponseResults = f1_results }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterReplicaLogDirsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec AlterReplicaLogDirsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterReplicaLogDirsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterReplicaLogDirsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterReplicaLogDirsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}