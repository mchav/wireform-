{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareFetchResponse
Description : Kafka ShareFetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 78.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareFetchResponse
  (
    ShareFetchResponse(..),
    ShareFetchableTopicResponse(..),
    PartitionData(..),
    LeaderIdAndEpoch(..),
    AcquiredRecords(..),
    NodeEndpoint(..),
    encodeShareFetchResponse,
    decodeShareFetchResponse,
    maxShareFetchResponseVersion
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


-- | The current leader of the partition.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    serialize (leaderIdAndEpochLeaderId lmsg)
    serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | The acquired records.
data AcquiredRecords = AcquiredRecords
  {

  -- | The earliest offset in this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsFirstOffset :: !(Int64)
,

  -- | The last offset of this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsLastOffset :: !(Int64)
,

  -- | The delivery count of this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsDeliveryCount :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode AcquiredRecords with version-aware field handling.
encodeAcquiredRecords :: MonadPut m => E.ApiVersion -> AcquiredRecords -> m ()
encodeAcquiredRecords version amsg =
  do
    serialize (acquiredRecordsFirstOffset amsg)
    serialize (acquiredRecordsLastOffset amsg)
    serialize (acquiredRecordsDeliveryCount amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AcquiredRecords with version-aware field handling.
decodeAcquiredRecords :: MonadGet m => E.ApiVersion -> m AcquiredRecords
decodeAcquiredRecords version =
  do
    fieldfirstoffset <- deserialize
    fieldlastoffset <- deserialize
    fielddeliverycount <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AcquiredRecords
      {
      acquiredRecordsFirstOffset = fieldfirstoffset
      ,
      acquiredRecordsLastOffset = fieldlastoffset
      ,
      acquiredRecordsDeliveryCount = fielddeliverycount
      }


-- | The topic partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The fetch error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The fetch error message, or null if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorMessage :: !(KafkaString)
,

  -- | The acknowledge error code, or 0 if there was no acknowledge error.

  -- Versions: 0+
  partitionDataAcknowledgeErrorCode :: !(Int16)
,

  -- | The acknowledge error message, or null if there was no acknowledge error.

  -- Versions: 0+
  partitionDataAcknowledgeErrorMessage :: !(KafkaString)
,

  -- | The current leader of the partition.

  -- Versions: 0+
  partitionDataCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | The record data.

  -- Versions: 0+
  partitionDataRecords :: !(KafkaBytes)
,

  -- | The acquired records.

  -- Versions: 0+
  partitionDataAcquiredRecords :: !(KafkaArray (AcquiredRecords))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionDataErrorMessage pmsg)) else serialize (partitionDataErrorMessage pmsg)
    serialize (partitionDataAcknowledgeErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionDataAcknowledgeErrorMessage pmsg)) else serialize (partitionDataAcknowledgeErrorMessage pmsg)
    encodeLeaderIdAndEpoch version (partitionDataCurrentLeader pmsg)
    if version >= 0 then serialize (toCompactBytes (partitionDataRecords pmsg)) else serialize (partitionDataRecords pmsg)
    E.encodeVersionedArray version 0 encodeAcquiredRecords (case P.unKafkaArray (partitionDataAcquiredRecords pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldacknowledgeerrorcode <- deserialize
    fieldacknowledgeerrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldcurrentleader <- decodeLeaderIdAndEpoch version
    fieldrecords <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
    fieldacquiredrecords <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcquiredRecords
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataErrorMessage = fielderrormessage
      ,
      partitionDataAcknowledgeErrorCode = fieldacknowledgeerrorcode
      ,
      partitionDataAcknowledgeErrorMessage = fieldacknowledgeerrormessage
      ,
      partitionDataCurrentLeader = fieldcurrentleader
      ,
      partitionDataRecords = fieldrecords
      ,
      partitionDataAcquiredRecords = fieldacquiredrecords
      }


-- | The response topics.
data ShareFetchableTopicResponse = ShareFetchableTopicResponse
  {

  -- | The unique topic ID.

  -- Versions: 0+
  shareFetchableTopicResponseTopicId :: !(KafkaUuid)
,

  -- | The topic partitions.

  -- Versions: 0+
  shareFetchableTopicResponsePartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ShareFetchableTopicResponse with version-aware field handling.
encodeShareFetchableTopicResponse :: MonadPut m => E.ApiVersion -> ShareFetchableTopicResponse -> m ()
encodeShareFetchableTopicResponse version smsg =
  do
    serialize (shareFetchableTopicResponseTopicId smsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (shareFetchableTopicResponsePartitions smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ShareFetchableTopicResponse with version-aware field handling.
decodeShareFetchableTopicResponse :: MonadGet m => E.ApiVersion -> m ShareFetchableTopicResponse
decodeShareFetchableTopicResponse version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ShareFetchableTopicResponse
      {
      shareFetchableTopicResponseTopicId = fieldtopicid
      ,
      shareFetchableTopicResponsePartitions = fieldpartitions
      }


-- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 0+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 0+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 0+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 0+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    serialize (nodeEndpointNodeId nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    serialize (nodeEndpointPort nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure NodeEndpoint
      {
      nodeEndpointNodeId = fieldnodeid
      ,
      nodeEndpointHost = fieldhost
      ,
      nodeEndpointPort = fieldport
      ,
      nodeEndpointRack = fieldrack
      }



data ShareFetchResponse = ShareFetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  shareFetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level response error code.

  -- Versions: 0+
  shareFetchResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  shareFetchResponseErrorMessage :: !(KafkaString)
,

  -- | The time in milliseconds for which the acquired records are locked.

  -- Versions: 1+
  shareFetchResponseAcquisitionLockTimeoutMs :: !(Int32)
,

  -- | The response topics.

  -- Versions: 0+
  shareFetchResponseResponses :: !(KafkaArray (ShareFetchableTopicResponse))
,

  -- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.

  -- Versions: 0+
  shareFetchResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareFetchResponse.
maxShareFetchResponseVersion :: Int16
maxShareFetchResponseVersion = 2

-- | KafkaMessage instance for ShareFetchResponse.
instance KafkaMessage ShareFetchResponse where
  messageApiKey = 78
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Encode ShareFetchResponse with the given API version.
encodeShareFetchResponse :: MonadPut m => E.ApiVersion -> ShareFetchResponse -> m ()
encodeShareFetchResponse version msg
  | version >= 1 && version <= 2 =
    do
      serialize (shareFetchResponseThrottleTimeMs msg)
      serialize (shareFetchResponseErrorCode msg)
      serialize (toCompactString (shareFetchResponseErrorMessage msg))
      serialize (shareFetchResponseAcquisitionLockTimeoutMs msg)
      E.encodeVersionedArray version 0 encodeShareFetchableTopicResponse (case P.unKafkaArray (shareFetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNodeEndpoint (case P.unKafkaArray (shareFetchResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareFetchResponse with the given API version.
decodeShareFetchResponse :: MonadGet m => E.ApiVersion -> m ShareFetchResponse
decodeShareFetchResponse version
  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldacquisitionlocktimeoutms <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeShareFetchableTopicResponse
      fieldnodeendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeNodeEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareFetchResponse
        {
        shareFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        shareFetchResponseErrorCode = fielderrorcode
        ,
        shareFetchResponseErrorMessage = fielderrormessage
        ,
        shareFetchResponseAcquisitionLockTimeoutMs = fieldacquisitionlocktimeoutms
        ,
        shareFetchResponseResponses = fieldresponses
        ,
        shareFetchResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a LeaderIdAndEpoch.
wireMaxSizeLeaderIdAndEpoch :: Int -> LeaderIdAndEpoch -> Int
wireMaxSizeLeaderIdAndEpoch _version msg =
  0
  + 4
  + 4
  + 1

-- | Direct-poke encoder for LeaderIdAndEpoch.
wirePokeLeaderIdAndEpoch :: Int -> Ptr Word8 -> LeaderIdAndEpoch -> IO (Ptr Word8)
wirePokeLeaderIdAndEpoch version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (leaderIdAndEpochLeaderId msg)
  p2 <- W.pokeInt32BE p1 (leaderIdAndEpochLeaderEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for LeaderIdAndEpoch.
wirePeekLeaderIdAndEpoch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderIdAndEpoch, Ptr Word8)
wirePeekLeaderIdAndEpoch version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = f0_leaderid, leaderIdAndEpochLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a AcquiredRecords.
wireMaxSizeAcquiredRecords :: Int -> AcquiredRecords -> Int
wireMaxSizeAcquiredRecords _version msg =
  0
  + 8
  + 8
  + 2
  + 1

-- | Direct-poke encoder for AcquiredRecords.
wirePokeAcquiredRecords :: Int -> Ptr Word8 -> AcquiredRecords -> IO (Ptr Word8)
wirePokeAcquiredRecords version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (acquiredRecordsFirstOffset msg)
  p2 <- W.pokeInt64BE p1 (acquiredRecordsLastOffset msg)
  p3 <- W.pokeInt16BE p2 (acquiredRecordsDeliveryCount msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AcquiredRecords.
wirePeekAcquiredRecords :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AcquiredRecords, Ptr Word8)
wirePeekAcquiredRecords version _fp _basePtr p0 endPtr = do
  (f0_firstoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_lastoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_deliverycount, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AcquiredRecords { acquiredRecordsFirstOffset = f0_firstoffset, acquiredRecordsLastOffset = f1_lastoffset, acquiredRecordsDeliveryCount = f2_deliverycount }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionDataErrorMessage msg))
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionDataAcknowledgeErrorMessage msg))
  + wireMaxSizeLeaderIdAndEpoch _version (partitionDataCurrentLeader msg)
  + WP.compactBytesMaxSize (P.toCompactBytes (partitionDataRecords msg))
  + (5 + (case P.unKafkaArray (partitionDataAcquiredRecords msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAcquiredRecords _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionDataErrorMessage msg))
  p4 <- W.pokeInt16BE p3 (partitionDataAcknowledgeErrorCode msg)
  p5 <- WP.pokeCompactString p4 (P.toCompactString (partitionDataAcknowledgeErrorMessage msg))
  p6 <- wirePokeLeaderIdAndEpoch version p5 (partitionDataCurrentLeader msg)
  p7 <- WP.pokeCompactBytes p6 (P.toCompactBytes (partitionDataRecords msg))
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAcquiredRecords version p x) p7 (partitionDataAcquiredRecords msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_acknowledgeerrorcode, p4) <- W.peekInt16BE p3 endPtr
  (f4_acknowledgeerrormessage, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
  (f5_currentleader, p6) <- wirePeekLeaderIdAndEpoch version _fp _basePtr p5 endPtr
  (f6_records, p7) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p6 endPtr
  (f7_acquiredrecords, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAcquiredRecords version _fp _basePtr p e) p7 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataErrorMessage = f2_errormessage, partitionDataAcknowledgeErrorCode = f3_acknowledgeerrorcode, partitionDataAcknowledgeErrorMessage = f4_acknowledgeerrormessage, partitionDataCurrentLeader = f5_currentleader, partitionDataRecords = f6_records, partitionDataAcquiredRecords = f7_acquiredrecords }, pTagsEnd)

-- | Worst-case wire size of a ShareFetchableTopicResponse.
wireMaxSizeShareFetchableTopicResponse :: Int -> ShareFetchableTopicResponse -> Int
wireMaxSizeShareFetchableTopicResponse _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (shareFetchableTopicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareFetchableTopicResponse.
wirePokeShareFetchableTopicResponse :: Int -> Ptr Word8 -> ShareFetchableTopicResponse -> IO (Ptr Word8)
wirePokeShareFetchableTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (shareFetchableTopicResponseTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (shareFetchableTopicResponsePartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ShareFetchableTopicResponse.
wirePeekShareFetchableTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareFetchableTopicResponse, Ptr Word8)
wirePeekShareFetchableTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ShareFetchableTopicResponse { shareFetchableTopicResponseTopicId = f0_topicid, shareFetchableTopicResponsePartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (nodeEndpointHost msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (nodeEndpointRack msg))
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (nodeEndpointNodeId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg))
  p3 <- W.pokeInt32BE p2 (nodeEndpointPort msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (nodeEndpointRack msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port, nodeEndpointRack = f3_rack }, pTagsEnd)

-- | Worst-case wire size of a ShareFetchResponse.
wireMaxSizeShareFetchResponse :: Int -> ShareFetchResponse -> Int
wireMaxSizeShareFetchResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (shareFetchResponseErrorMessage msg))
  + 4
  + (5 + (case P.unKafkaArray (shareFetchResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeShareFetchableTopicResponse _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (shareFetchResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareFetchResponse.
wirePokeShareFetchResponse :: Int -> Ptr Word8 -> ShareFetchResponse -> IO (Ptr Word8)
wirePokeShareFetchResponse version basePtr msg
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (shareFetchResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (shareFetchResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (shareFetchResponseErrorMessage msg))
    p4 <- W.pokeInt32BE p3 (shareFetchResponseAcquisitionLockTimeoutMs msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeShareFetchableTopicResponse version p x) p4 (shareFetchResponseResponses msg)
    p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeNodeEndpoint version p x) p5 (shareFetchResponseNodeEndpoints msg)
    WP.pokeEmptyTaggedFields p6
  | otherwise = error $ "wirePoke ShareFetchResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareFetchResponse.
wirePeekShareFetchResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareFetchResponse, Ptr Word8)
wirePeekShareFetchResponse version _fp _basePtr p0 endPtr
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_acquisitionlocktimeoutms, p4) <- W.peekInt32BE p3 endPtr
    (f4_responses, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekShareFetchableTopicResponse version _fp _basePtr p e) p4 endPtr
    (f5_nodeendpoints, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekNodeEndpoint version _fp _basePtr p e) p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (ShareFetchResponse { shareFetchResponseThrottleTimeMs = f0_throttletimems, shareFetchResponseErrorCode = f1_errorcode, shareFetchResponseErrorMessage = f2_errormessage, shareFetchResponseAcquisitionLockTimeoutMs = f3_acquisitionlocktimeoutms, shareFetchResponseResponses = f4_responses, shareFetchResponseNodeEndpoints = f5_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareFetchResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ShareFetchResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareFetchResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareFetchResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareFetchResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}