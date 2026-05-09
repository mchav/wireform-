{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
Description : Kafka AlterPartitionReassignmentsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 45.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
  (
    AlterPartitionReassignmentsRequest(..),
    ReassignableTopic(..),
    ReassignablePartition(..),
    encodeAlterPartitionReassignmentsRequest,
    decodeAlterPartitionReassignmentsRequest,
    maxAlterPartitionReassignmentsRequestVersion
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


-- | The partitions to reassign.
data ReassignablePartition = ReassignablePartition
  {

  -- | The partition index.

  -- Versions: 0+
  reassignablePartitionPartitionIndex :: !(Int32)
,

  -- | The replicas to place the partitions on, or null to cancel a pending reassignment for this partition

  -- Versions: 0+
  reassignablePartitionReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignablePartition with version-aware field handling.
encodeReassignablePartition :: MonadPut m => E.ApiVersion -> ReassignablePartition -> m ()
encodeReassignablePartition version rmsg =
  do
    serialize (reassignablePartitionPartitionIndex rmsg)
    E.encodeVersionedNullableArray version 0 (\_ x -> serialize x) (reassignablePartitionReplicas rmsg) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignablePartition with version-aware field handling.
decodeReassignablePartition :: MonadGet m => E.ApiVersion -> m ReassignablePartition
decodeReassignablePartition version =
  do
    fieldpartitionindex <- deserialize
    fieldreplicas <- E.decodeVersionedNullableArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignablePartition
      {
      reassignablePartitionPartitionIndex = fieldpartitionindex
      ,
      reassignablePartitionReplicas = fieldreplicas
      }


-- | The topics to reassign.
data ReassignableTopic = ReassignableTopic
  {

  -- | The topic name.

  -- Versions: 0+
  reassignableTopicName :: !(KafkaString)
,

  -- | The partitions to reassign.

  -- Versions: 0+
  reassignableTopicPartitions :: !(KafkaArray (ReassignablePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignableTopic with version-aware field handling.
encodeReassignableTopic :: MonadPut m => E.ApiVersion -> ReassignableTopic -> m ()
encodeReassignableTopic version rmsg =
  do
    if version >= 0 then serialize (toCompactString (reassignableTopicName rmsg)) else serialize (reassignableTopicName rmsg)
    E.encodeVersionedArray version 0 encodeReassignablePartition (case P.unKafkaArray (reassignableTopicPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignableTopic with version-aware field handling.
decodeReassignableTopic :: MonadGet m => E.ApiVersion -> m ReassignableTopic
decodeReassignableTopic version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignablePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignableTopic
      {
      reassignableTopicName = fieldname
      ,
      reassignableTopicPartitions = fieldpartitions
      }



data AlterPartitionReassignmentsRequest = AlterPartitionReassignmentsRequest
  {

  -- | The time in ms to wait for the request to complete.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTimeoutMs :: !(Int32)
,

  -- | The option indicating whether changing the replication factor of any given partition as part of this

  -- Versions: 1+
  alterPartitionReassignmentsRequestAllowReplicationFactorChange :: !(Bool)
,

  -- | The topics to reassign.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTopics :: !(KafkaArray (ReassignableTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionReassignmentsRequest.
maxAlterPartitionReassignmentsRequestVersion :: Int16
maxAlterPartitionReassignmentsRequestVersion = 1

-- | KafkaMessage instance for AlterPartitionReassignmentsRequest.
instance KafkaMessage AlterPartitionReassignmentsRequest where
  messageApiKey = 45
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode AlterPartitionReassignmentsRequest with the given API version.
encodeAlterPartitionReassignmentsRequest :: MonadPut m => E.ApiVersion -> AlterPartitionReassignmentsRequest -> m ()
encodeAlterPartitionReassignmentsRequest version msg
  | version == 0 =
    do
      serialize (alterPartitionReassignmentsRequestTimeoutMs msg)
      E.encodeVersionedArray version 0 encodeReassignableTopic (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (alterPartitionReassignmentsRequestTimeoutMs msg)
      serialize (alterPartitionReassignmentsRequestAllowReplicationFactorChange msg)
      E.encodeVersionedArray version 0 encodeReassignableTopic (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionReassignmentsRequest with the given API version.
decodeAlterPartitionReassignmentsRequest :: MonadGet m => E.ApiVersion -> m AlterPartitionReassignmentsRequest
decodeAlterPartitionReassignmentsRequest version
  | version == 0 =
    do
      fieldtimeoutms <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsRequest
        {
        alterPartitionReassignmentsRequestTimeoutMs = fieldtimeoutms
        ,
        alterPartitionReassignmentsRequestAllowReplicationFactorChange = True
        ,
        alterPartitionReassignmentsRequestTopics = fieldtopics
        }

  | version == 1 =
    do
      fieldtimeoutms <- deserialize
      fieldallowreplicationfactorchange <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsRequest
        {
        alterPartitionReassignmentsRequestTimeoutMs = fieldtimeoutms
        ,
        alterPartitionReassignmentsRequestAllowReplicationFactorChange = fieldallowreplicationfactorchange
        ,
        alterPartitionReassignmentsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ReassignablePartition.
wireMaxSizeReassignablePartition :: Int -> ReassignablePartition -> Int
wireMaxSizeReassignablePartition _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (reassignablePartitionReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReassignablePartition.
wirePokeReassignablePartition :: Int -> Ptr Word8 -> ReassignablePartition -> IO (Ptr Word8)
wirePokeReassignablePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (reassignablePartitionPartitionIndex msg)
  p2 <- WP.pokeVersionedNullableArray version 0 W.pokeInt32BE p1 (reassignablePartitionReplicas msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReassignablePartition.
wirePeekReassignablePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignablePartition, Ptr Word8)
wirePeekReassignablePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicas, p2) <- WP.peekVersionedNullableArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReassignablePartition { reassignablePartitionPartitionIndex = f0_partitionindex, reassignablePartitionReplicas = f1_replicas }, pTagsEnd)

-- | Worst-case wire size of a ReassignableTopic.
wireMaxSizeReassignableTopic :: Int -> ReassignableTopic -> Int
wireMaxSizeReassignableTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (reassignableTopicName msg))
  + (5 + (case P.unKafkaArray (reassignableTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignablePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReassignableTopic.
wirePokeReassignableTopic :: Int -> Ptr Word8 -> ReassignableTopic -> IO (Ptr Word8)
wirePokeReassignableTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (reassignableTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignablePartition version p x) p1 (reassignableTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReassignableTopic.
wirePeekReassignableTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignableTopic, Ptr Word8)
wirePeekReassignableTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignablePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReassignableTopic { reassignableTopicName = f0_name, reassignableTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a AlterPartitionReassignmentsRequest.
wireMaxSizeAlterPartitionReassignmentsRequest :: Int -> AlterPartitionReassignmentsRequest -> Int
wireMaxSizeAlterPartitionReassignmentsRequest _version msg =
  0
  + 4
  + 1
  + (5 + (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignableTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterPartitionReassignmentsRequest.
wirePokeAlterPartitionReassignmentsRequest :: Int -> Ptr Word8 -> AlterPartitionReassignmentsRequest -> IO (Ptr Word8)
wirePokeAlterPartitionReassignmentsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsRequestTimeoutMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopic version p x) p1 (alterPartitionReassignmentsRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsRequestTimeoutMs msg)
    p2 <- W.pokeWord8 p1 (if (alterPartitionReassignmentsRequestAllowReplicationFactorChange msg) then 1 else 0)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopic version p x) p2 (alterPartitionReassignmentsRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AlterPartitionReassignmentsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterPartitionReassignmentsRequest.
wirePeekAlterPartitionReassignmentsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterPartitionReassignmentsRequest, Ptr Word8)
wirePeekAlterPartitionReassignmentsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterPartitionReassignmentsRequest { alterPartitionReassignmentsRequestTimeoutMs = f0_timeoutms, alterPartitionReassignmentsRequestAllowReplicationFactorChange = False, alterPartitionReassignmentsRequestTopics = f1_topics }, pTagsEnd)
  | version == 1 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_allowreplicationfactorchange, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopic version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AlterPartitionReassignmentsRequest { alterPartitionReassignmentsRequestTimeoutMs = f0_timeoutms, alterPartitionReassignmentsRequestAllowReplicationFactorChange = f1_allowreplicationfactorchange, alterPartitionReassignmentsRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterPartitionReassignmentsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterPartitionReassignmentsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterPartitionReassignmentsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterPartitionReassignmentsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterPartitionReassignmentsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}