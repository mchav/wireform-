{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndQuorumEpochRequest
Description : Kafka EndQuorumEpochRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 54.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndQuorumEpochRequest
  (
    EndQuorumEpochRequest(..),
    TopicData(..),
    PartitionData(..),
    ReplicaInfo(..),
    LeaderEndpoint(..),
    encodeEndQuorumEpochRequest,
    decodeEndQuorumEpochRequest,
    maxEndQuorumEpochRequestVersion
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


-- | A sorted list of preferred candidates to start the election.
data ReplicaInfo = ReplicaInfo
  {

  -- | The ID of the candidate replica.

  -- Versions: 1+
  replicaInfoCandidateId :: !(Int32)
,

  -- | The directory ID of the candidate replica.

  -- Versions: 1+
  replicaInfoCandidateDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReplicaInfo with version-aware field handling.
encodeReplicaInfo :: MonadPut m => E.ApiVersion -> ReplicaInfo -> m ()
encodeReplicaInfo version rmsg =
  do
    when (version >= 1) $
      serialize (replicaInfoCandidateId rmsg)
    when (version >= 1) $
      serialize (replicaInfoCandidateDirectoryId rmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReplicaInfo with version-aware field handling.
decodeReplicaInfo :: MonadGet m => E.ApiVersion -> m ReplicaInfo
decodeReplicaInfo version =
  do
    fieldcandidateid <- if version >= 1
      then deserialize
      else pure (0)
    fieldcandidatedirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReplicaInfo
      {
      replicaInfoCandidateId = fieldcandidateid
      ,
      replicaInfoCandidateDirectoryId = fieldcandidatedirectoryid
      }


-- | The partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The current leader ID that is resigning.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The current epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | A sorted list of preferred successors to start the election.

  -- Versions: 0
  partitionDataPreferredSuccessors :: !(KafkaArray (Int32))
,

  -- | A sorted list of preferred candidates to start the election.

  -- Versions: 1+
  partitionDataPreferredCandidates :: !(KafkaArray (ReplicaInfo))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version == 0) $
      E.encodeVersionedArray version 1 (\_ x -> serialize x) (case P.unKafkaArray (partitionDataPreferredSuccessors pmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 1) $
      E.encodeVersionedArray version 1 encodeReplicaInfo (case P.unKafkaArray (partitionDataPreferredCandidates pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldpreferredsuccessors <- if version == 0
      then P.mkKafkaArray <$> E.decodeVersionedArray version 1 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    fieldpreferredcandidates <- if version >= 1
      then P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeReplicaInfo
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataPreferredSuccessors = fieldpreferredsuccessors
      ,
      partitionDataPreferredCandidates = fieldpreferredcandidates
      }


-- | The topics.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 1 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 1 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodePartitionData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }


-- | Endpoints for the leader.
data LeaderEndpoint = LeaderEndpoint
  {

  -- | The name of the endpoint.

  -- Versions: 1+
  leaderEndpointName :: !(KafkaString)
,

  -- | The node's hostname.

  -- Versions: 1+
  leaderEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 1+
  leaderEndpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderEndpoint with version-aware field handling.
encodeLeaderEndpoint :: MonadPut m => E.ApiVersion -> LeaderEndpoint -> m ()
encodeLeaderEndpoint version lmsg =
  do
    when (version >= 1) $
      if version >= 1 then serialize (toCompactString (leaderEndpointName lmsg)) else serialize (leaderEndpointName lmsg)
    when (version >= 1) $
      if version >= 1 then serialize (toCompactString (leaderEndpointHost lmsg)) else serialize (leaderEndpointHost lmsg)
    when (version >= 1) $
      serialize (leaderEndpointPort lmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderEndpoint with version-aware field handling.
decodeLeaderEndpoint :: MonadGet m => E.ApiVersion -> m LeaderEndpoint
decodeLeaderEndpoint version =
  do
    fieldname <- if version >= 1
      then if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldhost <- if version >= 1
      then if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderEndpoint
      {
      leaderEndpointName = fieldname
      ,
      leaderEndpointHost = fieldhost
      ,
      leaderEndpointPort = fieldport
      }



data EndQuorumEpochRequest = EndQuorumEpochRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  endQuorumEpochRequestClusterId :: !(KafkaString)
,

  -- | The topics.

  -- Versions: 0+
  endQuorumEpochRequestTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for the leader.

  -- Versions: 1+
  endQuorumEpochRequestLeaderEndpoints :: !(KafkaArray (LeaderEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndQuorumEpochRequest.
maxEndQuorumEpochRequestVersion :: Int16
maxEndQuorumEpochRequestVersion = 1

-- | KafkaMessage instance for EndQuorumEpochRequest.
instance KafkaMessage EndQuorumEpochRequest where
  messageApiKey = 54
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Encode EndQuorumEpochRequest with the given API version.
encodeEndQuorumEpochRequest :: MonadPut m => E.ApiVersion -> EndQuorumEpochRequest -> m ()
encodeEndQuorumEpochRequest version msg
  | version == 0 =
    do
      serialize (endQuorumEpochRequestClusterId msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (toCompactString (endQuorumEpochRequestClusterId msg))
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 1 encodeLeaderEndpoint (case P.unKafkaArray (endQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndQuorumEpochRequest with the given API version.
decodeEndQuorumEpochRequest :: MonadGet m => E.ApiVersion -> m EndQuorumEpochRequest
decodeEndQuorumEpochRequest version
  | version == 0 =
    do
      fieldclusterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      pure EndQuorumEpochRequest
        {
        endQuorumEpochRequestClusterId = fieldclusterid
        ,
        endQuorumEpochRequestTopics = fieldtopics
        ,
        endQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fieldclusterid <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      fieldleaderendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeLeaderEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndQuorumEpochRequest
        {
        endQuorumEpochRequestClusterId = fieldclusterid
        ,
        endQuorumEpochRequestTopics = fieldtopics
        ,
        endQuorumEpochRequestLeaderEndpoints = fieldleaderendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ReplicaInfo.
wireMaxSizeReplicaInfo :: Int -> ReplicaInfo -> Int
wireMaxSizeReplicaInfo _version msg =
  0
  + 4
  + 16
  + 1

-- | Direct-poke encoder for ReplicaInfo.
wirePokeReplicaInfo :: Int -> Ptr Word8 -> ReplicaInfo -> IO (Ptr Word8)
wirePokeReplicaInfo version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (replicaInfoCandidateId msg)
  p2 <- WP.pokeKafkaUuid p1 (replicaInfoCandidateDirectoryId msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReplicaInfo.
wirePeekReplicaInfo :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaInfo, Ptr Word8)
wirePeekReplicaInfo version _fp _basePtr p0 endPtr = do
  (f0_candidateid, p1) <- W.peekInt32BE p0 endPtr
  (f1_candidatedirectoryid, p2) <- WP.peekKafkaUuid p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReplicaInfo { replicaInfoCandidateId = f0_candidateid, replicaInfoCandidateDirectoryId = f1_candidatedirectoryid }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 4
  + (5 + (case P.unKafkaArray (partitionDataPreferredSuccessors msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (partitionDataPreferredCandidates msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReplicaInfo _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (partitionDataLeaderId msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderEpoch msg)
  p4 <- WP.pokeVersionedArray version 1 W.pokeInt32BE p3 (partitionDataPreferredSuccessors msg)
  p5 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeReplicaInfo version p x) p4 (partitionDataPreferredCandidates msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderid, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_preferredsuccessors, p4) <- WP.peekVersionedArray version 1 W.peekInt32BE p3 endPtr
  (f4_preferredcandidates, p5) <- WP.peekVersionedArray version 1 (\p e -> wirePeekReplicaInfo version _fp _basePtr p e) p4 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataLeaderId = f1_leaderid, partitionDataLeaderEpoch = f2_leaderepoch, partitionDataPreferredSuccessors = f3_preferredsuccessors, partitionDataPreferredCandidates = f4_preferredcandidates }, pTagsEnd)

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicDataTopicName msg))
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a LeaderEndpoint.
wireMaxSizeLeaderEndpoint :: Int -> LeaderEndpoint -> Int
wireMaxSizeLeaderEndpoint _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (leaderEndpointName msg))
  + WP.compactStringMaxSize (P.toCompactString (leaderEndpointHost msg))
  + 2
  + 1

-- | Direct-poke encoder for LeaderEndpoint.
wirePokeLeaderEndpoint :: Int -> Ptr Word8 -> LeaderEndpoint -> IO (Ptr Word8)
wirePokeLeaderEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (leaderEndpointName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (leaderEndpointHost msg))
  p3 <- W.pokeWord16BE p2 (leaderEndpointPort msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for LeaderEndpoint.
wirePeekLeaderEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderEndpoint, Ptr Word8)
wirePeekLeaderEndpoint version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (LeaderEndpoint { leaderEndpointName = f0_name, leaderEndpointHost = f1_host, leaderEndpointPort = f2_port }, pTagsEnd)

-- | Worst-case wire size of a EndQuorumEpochRequest.
wireMaxSizeEndQuorumEpochRequest :: Int -> EndQuorumEpochRequest -> Int
wireMaxSizeEndQuorumEpochRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (endQuorumEpochRequestClusterId msg))
  + (5 + (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (endQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeLeaderEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for EndQuorumEpochRequest.
wirePokeEndQuorumEpochRequest :: Int -> Ptr Word8 -> EndQuorumEpochRequest -> IO (Ptr Word8)
wirePokeEndQuorumEpochRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (endQuorumEpochRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochRequestTopics msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (endQuorumEpochRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochRequestTopics msg)
    p3 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeLeaderEndpoint version p x) p2 (endQuorumEpochRequestLeaderEndpoints msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke EndQuorumEpochRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for EndQuorumEpochRequest.
wirePeekEndQuorumEpochRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndQuorumEpochRequest, Ptr Word8)
wirePeekEndQuorumEpochRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pure (EndQuorumEpochRequest { endQuorumEpochRequestClusterId = f0_clusterid, endQuorumEpochRequestTopics = f1_topics, endQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty }, p2)
  | version == 1 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    (f2_leaderendpoints, p3) <- WP.peekVersionedArray version 1 (\p e -> wirePeekLeaderEndpoint version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (EndQuorumEpochRequest { endQuorumEpochRequestClusterId = f0_clusterid, endQuorumEpochRequestTopics = f1_topics, endQuorumEpochRequestLeaderEndpoints = f2_leaderendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek EndQuorumEpochRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec EndQuorumEpochRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndQuorumEpochRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndQuorumEpochRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndQuorumEpochRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}