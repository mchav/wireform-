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
    encodeAlterShareGroupOffsetsResponse,
    decodeAlterShareGroupOffsetsResponse,
    maxAlterShareGroupOffsetsResponseVersion
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


-- | Encode AlterShareGroupOffsetsResponsePartition with version-aware field handling.
encodeAlterShareGroupOffsetsResponsePartition :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponsePartition -> m ()
encodeAlterShareGroupOffsetsResponsePartition version amsg =
  do
    serialize (alterShareGroupOffsetsResponsePartitionPartitionIndex amsg)
    serialize (alterShareGroupOffsetsResponsePartitionErrorCode amsg)
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsResponsePartitionErrorMessage amsg)) else serialize (alterShareGroupOffsetsResponsePartitionErrorMessage amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsResponsePartition with version-aware field handling.
decodeAlterShareGroupOffsetsResponsePartition :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponsePartition
decodeAlterShareGroupOffsetsResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsResponsePartition
      {
      alterShareGroupOffsetsResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      alterShareGroupOffsetsResponsePartitionErrorCode = fielderrorcode
      ,
      alterShareGroupOffsetsResponsePartitionErrorMessage = fielderrormessage
      }


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


-- | Encode AlterShareGroupOffsetsResponseTopic with version-aware field handling.
encodeAlterShareGroupOffsetsResponseTopic :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponseTopic -> m ()
encodeAlterShareGroupOffsetsResponseTopic version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsResponseTopicTopicName amsg)) else serialize (alterShareGroupOffsetsResponseTopicTopicName amsg)
    serialize (alterShareGroupOffsetsResponseTopicTopicId amsg)
    E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsResponsePartition (case P.unKafkaArray (alterShareGroupOffsetsResponseTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsResponseTopic with version-aware field handling.
decodeAlterShareGroupOffsetsResponseTopic :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponseTopic
decodeAlterShareGroupOffsetsResponseTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsResponsePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsResponseTopic
      {
      alterShareGroupOffsetsResponseTopicTopicName = fieldtopicname
      ,
      alterShareGroupOffsetsResponseTopicTopicId = fieldtopicid
      ,
      alterShareGroupOffsetsResponseTopicPartitions = fieldpartitions
      }



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

-- | Encode AlterShareGroupOffsetsResponse with the given API version.
encodeAlterShareGroupOffsetsResponse :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponse -> m ()
encodeAlterShareGroupOffsetsResponse version msg
  | version == 0 =
    do
      serialize (alterShareGroupOffsetsResponseThrottleTimeMs msg)
      serialize (alterShareGroupOffsetsResponseErrorCode msg)
      serialize (toCompactString (alterShareGroupOffsetsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsResponseTopic (case P.unKafkaArray (alterShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterShareGroupOffsetsResponse with the given API version.
decodeAlterShareGroupOffsetsResponse :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponse
decodeAlterShareGroupOffsetsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterShareGroupOffsetsResponse
        {
        alterShareGroupOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterShareGroupOffsetsResponseErrorCode = fielderrorcode
        ,
        alterShareGroupOffsetsResponseErrorMessage = fielderrormessage
        ,
        alterShareGroupOffsetsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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
  p3 <- WP.pokeCompactString p2 (P.toCompactString (alterShareGroupOffsetsResponsePartitionErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterShareGroupOffsetsResponsePartition.
wirePeekAlterShareGroupOffsetsResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponsePartition, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterShareGroupOffsetsResponsePartition { alterShareGroupOffsetsResponsePartitionPartitionIndex = f0_partitionindex, alterShareGroupOffsetsResponsePartitionErrorCode = f1_errorcode, alterShareGroupOffsetsResponsePartitionErrorMessage = f2_errormessage }, pTagsEnd)

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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterShareGroupOffsetsResponseTopicTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (alterShareGroupOffsetsResponseTopicTopicId msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsResponsePartition version p x) p2 (alterShareGroupOffsetsResponseTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterShareGroupOffsetsResponseTopic.
wirePeekAlterShareGroupOffsetsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponseTopic, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsResponsePartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterShareGroupOffsetsResponseTopic { alterShareGroupOffsetsResponseTopicTopicName = f0_topicname, alterShareGroupOffsetsResponseTopicTopicId = f1_topicid, alterShareGroupOffsetsResponseTopicPartitions = f2_partitions }, pTagsEnd)

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
    p3 <- WP.pokeCompactString p2 (P.toCompactString (alterShareGroupOffsetsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsResponseTopic version p x) p3 (alterShareGroupOffsetsResponseResponses msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke AlterShareGroupOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterShareGroupOffsetsResponse.
wirePeekAlterShareGroupOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsResponse, Ptr Word8)
wirePeekAlterShareGroupOffsetsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsResponseTopic version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AlterShareGroupOffsetsResponse { alterShareGroupOffsetsResponseThrottleTimeMs = f0_throttletimems, alterShareGroupOffsetsResponseErrorCode = f1_errorcode, alterShareGroupOffsetsResponseErrorMessage = f2_errormessage, alterShareGroupOffsetsResponseResponses = f3_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterShareGroupOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AlterShareGroupOffsetsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterShareGroupOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterShareGroupOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterShareGroupOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}