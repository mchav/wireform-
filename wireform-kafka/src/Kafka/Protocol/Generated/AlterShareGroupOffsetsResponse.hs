{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterShareGroupOffsetsResponse
Description : Kafka AlterShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 91.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterShareGroupOffsetsResponse
  (
    AlterShareGroupOffsetsResponse(..),
    AlterShareGroupOffsetsResponseTopic(..),
    AlterShareGroupOffsetsResponsePartition(..),
    maxAlterShareGroupOffsetsResponseVersion
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



data AlterShareGroupOffsetsResponsePartition = AlterShareGroupOffsetsResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The results for each topic.
data AlterShareGroupOffsetsResponseTopic = AlterShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,


  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicPartitions :: !(KafkaArray (AlterShareGroupOffsetsResponsePartition))

  }
  deriving (Eq, Show, Generic)


data AlterShareGroupOffsetsResponse = AlterShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponseErrorMessage :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  alterShareGroupOffsetsResponseResponses :: !(KafkaArray (AlterShareGroupOffsetsResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterShareGroupOffsetsResponse.
maxAlterShareGroupOffsetsResponseVersion :: Int16
maxAlterShareGroupOffsetsResponseVersion = 0

-- | KafkaMessage instance for AlterShareGroupOffsetsResponse.
instance KafkaMessage AlterShareGroupOffsetsResponse where
  messageApiKey = 91
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a AlterShareGroupOffsetsResponsePartition.
wireMaxSizeAlterShareGroupOffsetsResponsePartition :: Int -> AlterShareGroupOffsetsResponsePartition -> Int
wireMaxSizeAlterShareGroupOffsetsResponsePartition _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (alterShareGroupOffsetsResponsePartitionErrorMessage msg))
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsResponsePartition.
wirePokeAlterShareGroupOffsetsResponsePartition :: Int -> Ptr Word8 -> AlterShareGroupOffsetsResponsePartition -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (alterShareGroupOffsetsResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (alterShareGroupOffsetsResponsePartitionErrorCode msg)
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (alterShareGroupOffsetsResponsePartitionErrorMessage msg)) else WP.pokeKafkaString p2 (alterShareGroupOffsetsResponsePartitionErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterShareGroupOffsetsResponsePartition.
wirePeekAlterShareGroupOffsetsResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponsePartition, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterShareGroupOffsetsResponsePartition { alterShareGroupOffsetsResponsePartitionPartitionIndex = f0_partitionindex, alterShareGroupOffsetsResponsePartitionErrorCode = f1_errorcode, alterShareGroupOffsetsResponsePartitionErrorMessage = f2_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterShareGroupOffsetsResponsePartition :: AlterShareGroupOffsetsResponsePartition
defaultAlterShareGroupOffsetsResponsePartition = AlterShareGroupOffsetsResponsePartition { alterShareGroupOffsetsResponsePartitionPartitionIndex = 0, alterShareGroupOffsetsResponsePartitionErrorCode = 0, alterShareGroupOffsetsResponsePartitionErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a AlterShareGroupOffsetsResponseTopic.
wireMaxSizeAlterShareGroupOffsetsResponseTopic :: Int -> AlterShareGroupOffsetsResponseTopic -> Int
wireMaxSizeAlterShareGroupOffsetsResponseTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterShareGroupOffsetsResponseTopicTopicName msg))
  + 16
  + (5 + (case P.unKafkaArray (alterShareGroupOffsetsResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterShareGroupOffsetsResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsResponseTopic.
wirePokeAlterShareGroupOffsetsResponseTopic :: Int -> Ptr Word8 -> AlterShareGroupOffsetsResponseTopic -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (alterShareGroupOffsetsResponseTopicTopicName msg)) else WP.pokeKafkaString p0 (alterShareGroupOffsetsResponseTopicTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (alterShareGroupOffsetsResponseTopicTopicId msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsResponsePartition version p x) p2 (alterShareGroupOffsetsResponseTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterShareGroupOffsetsResponseTopic.
wirePeekAlterShareGroupOffsetsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponseTopic, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsResponsePartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterShareGroupOffsetsResponseTopic { alterShareGroupOffsetsResponseTopicTopicName = f0_topicname, alterShareGroupOffsetsResponseTopicTopicId = f1_topicid, alterShareGroupOffsetsResponseTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterShareGroupOffsetsResponseTopic :: AlterShareGroupOffsetsResponseTopic
defaultAlterShareGroupOffsetsResponseTopic = AlterShareGroupOffsetsResponseTopic { alterShareGroupOffsetsResponseTopicTopicName = P.KafkaString Null, alterShareGroupOffsetsResponseTopicTopicId = P.nullUuid, alterShareGroupOffsetsResponseTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AlterShareGroupOffsetsResponse.
wireMaxSizeAlterShareGroupOffsetsResponse :: Int -> AlterShareGroupOffsetsResponse -> Int
wireMaxSizeAlterShareGroupOffsetsResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (alterShareGroupOffsetsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (alterShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterShareGroupOffsetsResponseTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsResponse.
wirePokeAlterShareGroupOffsetsResponse :: Int -> Ptr Word8 -> AlterShareGroupOffsetsResponse -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterShareGroupOffsetsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (alterShareGroupOffsetsResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (alterShareGroupOffsetsResponseErrorMessage msg)) else WP.pokeKafkaString p2 (alterShareGroupOffsetsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsResponseTopic version p x) p3 (alterShareGroupOffsetsResponseResponses msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke AlterShareGroupOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterShareGroupOffsetsResponse.
wirePeekAlterShareGroupOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponse, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsResponseTopic version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AlterShareGroupOffsetsResponse { alterShareGroupOffsetsResponseThrottleTimeMs = f0_throttletimems, alterShareGroupOffsetsResponseErrorCode = f1_errorcode, alterShareGroupOffsetsResponseErrorMessage = f2_errormessage, alterShareGroupOffsetsResponseResponses = f3_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterShareGroupOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterShareGroupOffsetsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterShareGroupOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterShareGroupOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterShareGroupOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}