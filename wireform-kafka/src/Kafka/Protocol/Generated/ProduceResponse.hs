{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ProduceResponse
Description : Kafka ProduceResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 0.



Valid versions: 3-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ProduceResponse
  (
    ProduceResponse(..),
    TopicProduceResponse(..),
    PartitionProduceResponse(..),
    BatchIndexAndErrorMessage(..),
    LeaderIdAndEpoch(..),
    NodeEndpoint(..),
    encodeProduceResponse,
    decodeProduceResponse,
    maxProduceResponseVersion
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


-- | The batch indices of records that caused the batch to be dropped.
data BatchIndexAndErrorMessage = BatchIndexAndErrorMessage
  {

  -- | The batch index of the record that caused the batch to be dropped.

  -- Versions: 8+
  batchIndexAndErrorMessageBatchIndex :: !(Int32)
,

  -- | The error message of the record that caused the batch to be dropped.

  -- Versions: 8+
  batchIndexAndErrorMessageBatchIndexErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode BatchIndexAndErrorMessage with version-aware field handling.
encodeBatchIndexAndErrorMessage :: MonadPut m => E.ApiVersion -> BatchIndexAndErrorMessage -> m ()
encodeBatchIndexAndErrorMessage version bmsg =
  do
    when (version >= 8) $
      serialize (batchIndexAndErrorMessageBatchIndex bmsg)
    when (version >= 8) $
      if version >= 9 then serialize (toCompactString (batchIndexAndErrorMessageBatchIndexErrorMessage bmsg)) else serialize (batchIndexAndErrorMessageBatchIndexErrorMessage bmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode BatchIndexAndErrorMessage with version-aware field handling.
decodeBatchIndexAndErrorMessage :: MonadGet m => E.ApiVersion -> m BatchIndexAndErrorMessage
decodeBatchIndexAndErrorMessage version =
  do
    fieldbatchindex <- if version >= 8
      then deserialize
      else pure (0)
    fieldbatchindexerrormessage <- if version >= 8
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure BatchIndexAndErrorMessage
      {
      batchIndexAndErrorMessageBatchIndex = fieldbatchindex
      ,
      batchIndexAndErrorMessageBatchIndexErrorMessage = fieldbatchindexerrormessage
      }


-- | The leader broker that the producer should use for future requests.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 10+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 10+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    when (version >= 10) $
      serialize (leaderIdAndEpochLeaderId lmsg)
    when (version >= 10) $
      serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- if version >= 10
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- if version >= 10
      then deserialize
      else pure ((-1))
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | Each partition that we produced to within the topic.
data PartitionProduceResponse = PartitionProduceResponse
  {

  -- | The partition index.

  -- Versions: 0+
  partitionProduceResponseIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionProduceResponseErrorCode :: !(Int16)
,

  -- | The base offset.

  -- Versions: 0+
  partitionProduceResponseBaseOffset :: !(Int64)
,

  -- | The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, 

  -- Versions: 2+
  partitionProduceResponseLogAppendTimeMs :: !(Int64)
,

  -- | The log start offset.

  -- Versions: 5+
  partitionProduceResponseLogStartOffset :: !(Int64)
,

  -- | The batch indices of records that caused the batch to be dropped.

  -- Versions: 8+
  partitionProduceResponseRecordErrors :: !(KafkaArray (BatchIndexAndErrorMessage))
,

  -- | The global error message summarizing the common root cause of the records that caused the batch to b

  -- Versions: 8+
  partitionProduceResponseErrorMessage :: !(KafkaString)
,

  -- | The leader broker that the producer should use for future requests.

  -- Versions: 10+
  partitionProduceResponseCurrentLeader :: !(LeaderIdAndEpoch)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionProduceResponse with version-aware field handling.
encodePartitionProduceResponse :: MonadPut m => E.ApiVersion -> PartitionProduceResponse -> m ()
encodePartitionProduceResponse version pmsg =
  do
    serialize (partitionProduceResponseIndex pmsg)
    serialize (partitionProduceResponseErrorCode pmsg)
    serialize (partitionProduceResponseBaseOffset pmsg)
    when (version >= 2) $
      serialize (partitionProduceResponseLogAppendTimeMs pmsg)
    when (version >= 5) $
      serialize (partitionProduceResponseLogStartOffset pmsg)
    when (version >= 8) $
      E.encodeVersionedArray version 9 encodeBatchIndexAndErrorMessage (case P.unKafkaArray (partitionProduceResponseRecordErrors pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      if version >= 9 then serialize (toCompactString (partitionProduceResponseErrorMessage pmsg)) else serialize (partitionProduceResponseErrorMessage pmsg)
    when (version >= 9) $ do
      let _entries = (if version >= 10 then [(0, Data.Bytes.Put.runPutS (encodeLeaderIdAndEpoch version (partitionProduceResponseCurrentLeader pmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode PartitionProduceResponse with version-aware field handling.
decodePartitionProduceResponse :: MonadGet m => E.ApiVersion -> m PartitionProduceResponse
decodePartitionProduceResponse version =
  do
    fieldindex <- deserialize
    fielderrorcode <- deserialize
    fieldbaseoffset <- deserialize
    fieldlogappendtimems <- if version >= 2
      then deserialize
      else pure ((-1))
    fieldlogstartoffset <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldrecorderrors <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeBatchIndexAndErrorMessage
      else pure (P.mkKafkaArray V.empty)
    fielderrormessage <- if version >= 8
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _taggedFields <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fieldcurrentleader =
          if version >= 10
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (decodeLeaderIdAndEpoch version) _bs of
                  Right _v -> _v
                  Left  _  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
              Nothing  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
            else (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
    pure PartitionProduceResponse
      {
      partitionProduceResponseIndex = fieldindex
      ,
      partitionProduceResponseErrorCode = fielderrorcode
      ,
      partitionProduceResponseBaseOffset = fieldbaseoffset
      ,
      partitionProduceResponseLogAppendTimeMs = fieldlogappendtimems
      ,
      partitionProduceResponseLogStartOffset = fieldlogstartoffset
      ,
      partitionProduceResponseRecordErrors = fieldrecorderrors
      ,
      partitionProduceResponseErrorMessage = fielderrormessage
      ,
      partitionProduceResponseCurrentLeader = fieldcurrentleader
      }


-- | Each produce response.
data TopicProduceResponse = TopicProduceResponse
  {

  -- | The topic name.

  -- Versions: 0-12
  topicProduceResponseName :: !(KafkaString)
,

  -- | The unique topic ID

  -- Versions: 13+
  topicProduceResponseTopicId :: !(KafkaUuid)
,

  -- | Each partition that we produced to within the topic.

  -- Versions: 0+
  topicProduceResponsePartitionResponses :: !(KafkaArray (PartitionProduceResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicProduceResponse with version-aware field handling.
encodeTopicProduceResponse :: MonadPut m => E.ApiVersion -> TopicProduceResponse -> m ()
encodeTopicProduceResponse version tmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 9 then serialize (toCompactString (topicProduceResponseName tmsg)) else serialize (topicProduceResponseName tmsg)
    when (version >= 13) $
      serialize (topicProduceResponseTopicId tmsg)
    E.encodeVersionedArray version 9 encodePartitionProduceResponse (case P.unKafkaArray (topicProduceResponsePartitionResponses tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicProduceResponse with version-aware field handling.
decodeTopicProduceResponse :: MonadGet m => E.ApiVersion -> m TopicProduceResponse
decodeTopicProduceResponse version =
  do
    fieldname <- if version >= 0 && version <= 12
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitionresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodePartitionProduceResponse
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicProduceResponse
      {
      topicProduceResponseName = fieldname
      ,
      topicProduceResponseTopicId = fieldtopicid
      ,
      topicProduceResponsePartitionResponses = fieldpartitionresponses
      }


-- | Endpoints for all current-leaders enumerated in PartitionProduceResponses, with errors NOT_LEADER_OR_FOLLOWER.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 10+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 10+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 10+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 10+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    when (version >= 10) $
      serialize (nodeEndpointNodeId nmsg)
    when (version >= 10) $
      if version >= 9 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    when (version >= 10) $
      serialize (nodeEndpointPort nmsg)
    when (version >= 10) $
      if version >= 9 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- if version >= 10
      then deserialize
      else pure (0)
    fieldhost <- if version >= 10
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 10
      then deserialize
      else pure (0)
    fieldrack <- if version >= 10
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
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



data ProduceResponse = ProduceResponse
  {

  -- | Each produce response.

  -- Versions: 0+
  produceResponseResponses :: !(KafkaArray (TopicProduceResponse))
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  produceResponseThrottleTimeMs :: !(Int32)
,

  -- | Endpoints for all current-leaders enumerated in PartitionProduceResponses, with errors NOT_LEADER_OR

  -- Versions: 10+
  produceResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ProduceResponse.
maxProduceResponseVersion :: Int16
maxProduceResponseVersion = 13

-- | KafkaMessage instance for ProduceResponse.
instance KafkaMessage ProduceResponse where
  messageApiKey = 0
  messageMinVersion = 3
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode ProduceResponse with the given API version.
encodeProduceResponse :: MonadPut m => E.ApiVersion -> ProduceResponse -> m ()
encodeProduceResponse version msg
  | version == 9 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)
      do
        let _entries = (if version >= 10 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (produceResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries

  | version >= 10 && version <= 13 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)
      do
        let _entries = (if version >= 10 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (produceResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries

  | version >= 3 && version <= 8 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ProduceResponse with the given API version.
decodeProduceResponse :: MonadGet m => E.ApiVersion -> m ProduceResponse
decodeProduceResponse version
  | version == 9 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 10
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = fieldnodeendpoints
        }

  | version >= 10 && version <= 13 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 10
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = fieldnodeendpoints
        }

  | version >= 3 && version <= 8 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a BatchIndexAndErrorMessage.
wireMaxSizeBatchIndexAndErrorMessage :: Int -> BatchIndexAndErrorMessage -> Int
wireMaxSizeBatchIndexAndErrorMessage _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (batchIndexAndErrorMessageBatchIndexErrorMessage msg))
  + 1

-- | Direct-poke encoder for BatchIndexAndErrorMessage.
wirePokeBatchIndexAndErrorMessage :: Int -> Ptr Word8 -> BatchIndexAndErrorMessage -> IO (Ptr Word8)
wirePokeBatchIndexAndErrorMessage version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (batchIndexAndErrorMessageBatchIndex msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (batchIndexAndErrorMessageBatchIndexErrorMessage msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for BatchIndexAndErrorMessage.
wirePeekBatchIndexAndErrorMessage :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BatchIndexAndErrorMessage, Ptr Word8)
wirePeekBatchIndexAndErrorMessage version _fp _basePtr p0 endPtr = do
  (f0_batchindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_batchindexerrormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (BatchIndexAndErrorMessage { batchIndexAndErrorMessageBatchIndex = f0_batchindex, batchIndexAndErrorMessageBatchIndexErrorMessage = f1_batchindexerrormessage }, pTagsEnd)

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
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for LeaderIdAndEpoch.
wirePeekLeaderIdAndEpoch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderIdAndEpoch, Ptr Word8)
wirePeekLeaderIdAndEpoch version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = f0_leaderid, leaderIdAndEpochLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a PartitionProduceResponse.
wireMaxSizePartitionProduceResponse :: Int -> PartitionProduceResponse -> Int
wireMaxSizePartitionProduceResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 8
  + 8
  + (5 + (case P.unKafkaArray (partitionProduceResponseRecordErrors msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeBatchIndexAndErrorMessage _version x ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (partitionProduceResponseErrorMessage msg))
  + wireMaxSizeLeaderIdAndEpoch _version (partitionProduceResponseCurrentLeader msg)
  + 1

-- | Direct-poke encoder for PartitionProduceResponse.
wirePokePartitionProduceResponse :: Int -> Ptr Word8 -> PartitionProduceResponse -> IO (Ptr Word8)
wirePokePartitionProduceResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionProduceResponseIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionProduceResponseErrorCode msg)
  p3 <- W.pokeInt64BE p2 (partitionProduceResponseBaseOffset msg)
  p4 <- W.pokeInt64BE p3 (partitionProduceResponseLogAppendTimeMs msg)
  p5 <- W.pokeInt64BE p4 (partitionProduceResponseLogStartOffset msg)
  p6 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeBatchIndexAndErrorMessage version p x) p5 (partitionProduceResponseRecordErrors msg)
  p7 <- WP.pokeCompactString p6 (P.toCompactString (partitionProduceResponseErrorMessage msg))
  if version >= 9 then do
    let !_taggedEntries = (if version >= 10 then [(0, W.runWirePokeWith (wireMaxSizeLeaderIdAndEpoch version (partitionProduceResponseCurrentLeader msg)) (\p -> wirePokeLeaderIdAndEpoch version p (partitionProduceResponseCurrentLeader msg)))] else [])
    WP.pokeTaggedFieldEntries p7 _taggedEntries
  else pure p7

-- | Direct-poke decoder for PartitionProduceResponse.
wirePeekPartitionProduceResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionProduceResponse, Ptr Word8)
wirePeekPartitionProduceResponse version _fp _basePtr p0 endPtr = do
  (f0_index, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_baseoffset, p3) <- W.peekInt64BE p2 endPtr
  (f3_logappendtimems, p4) <- W.peekInt64BE p3 endPtr
  (f4_logstartoffset, p5) <- W.peekInt64BE p4 endPtr
  (f5_recorderrors, p6) <- WP.peekVersionedArray version 9 (\p e -> wirePeekBatchIndexAndErrorMessage version _fp _basePtr p e) p5 endPtr
  (f6_errormessage, p7) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr
  (_taggedMap, pTagsEnd) <- if version >= 9 then WP.peekTaggedFieldsMap p7 endPtr else pure (Data.Map.Strict.empty, p7)
  let !_tag_currentleader = if version >= 10 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekLeaderIdAndEpoch version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> undefined :: LeaderIdAndEpoch}; Nothing -> undefined :: LeaderIdAndEpoch} else undefined :: LeaderIdAndEpoch
  pure (PartitionProduceResponse { partitionProduceResponseIndex = f0_index, partitionProduceResponseErrorCode = f1_errorcode, partitionProduceResponseBaseOffset = f2_baseoffset, partitionProduceResponseLogAppendTimeMs = f3_logappendtimems, partitionProduceResponseLogStartOffset = f4_logstartoffset, partitionProduceResponseRecordErrors = f5_recorderrors, partitionProduceResponseErrorMessage = f6_errormessage, partitionProduceResponseCurrentLeader = _tag_currentleader }, pTagsEnd)

-- | Worst-case wire size of a TopicProduceResponse.
wireMaxSizeTopicProduceResponse :: Int -> TopicProduceResponse -> Int
wireMaxSizeTopicProduceResponse _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicProduceResponseName msg))
  + 16
  + (5 + (case P.unKafkaArray (topicProduceResponsePartitionResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionProduceResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicProduceResponse.
wirePokeTopicProduceResponse :: Int -> Ptr Word8 -> TopicProduceResponse -> IO (Ptr Word8)
wirePokeTopicProduceResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicProduceResponseName msg))
  p2 <- WP.pokeKafkaUuid p1 (topicProduceResponseTopicId msg)
  p3 <- WP.pokeVersionedArray version 9 (\p x -> wirePokePartitionProduceResponse version p x) p2 (topicProduceResponsePartitionResponses msg)
  if version >= 9 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TopicProduceResponse.
wirePeekTopicProduceResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicProduceResponse, Ptr Word8)
wirePeekTopicProduceResponse version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitionresponses, p3) <- WP.peekVersionedArray version 9 (\p e -> wirePeekPartitionProduceResponse version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TopicProduceResponse { topicProduceResponseName = f0_name, topicProduceResponseTopicId = f1_topicid, topicProduceResponsePartitionResponses = f2_partitionresponses }, pTagsEnd)

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
  if version >= 9 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port, nodeEndpointRack = f3_rack }, pTagsEnd)

-- | Worst-case wire size of a ProduceResponse.
wireMaxSizeProduceResponse :: Int -> ProduceResponse -> Int
wireMaxSizeProduceResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicProduceResponse _version x ) v); P.Null -> 0 }))
  + 4
  + (5 + (case P.unKafkaArray (produceResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ProduceResponse.
wirePokeProduceResponse :: Int -> Ptr Word8 -> ProduceResponse -> IO (Ptr Word8)
wirePokeProduceResponse version basePtr msg
  | version == 9 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceResponse version p x) p0 (produceResponseResponses msg)
    p2 <- W.pokeInt32BE p1 (produceResponseThrottleTimeMs msg)
    let !_taggedEntries = (if version >= 10 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (produceResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (produceResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p2 _taggedEntries
  | version >= 10 && version <= 13 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceResponse version p x) p0 (produceResponseResponses msg)
    p2 <- W.pokeInt32BE p1 (produceResponseThrottleTimeMs msg)
    let !_taggedEntries = (if version >= 10 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (produceResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (produceResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p2 _taggedEntries
  | version >= 3 && version <= 8 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceResponse version p x) p0 (produceResponseResponses msg)
    p2 <- W.pokeInt32BE p1 (produceResponseThrottleTimeMs msg)
    pure p2
  | otherwise = error $ "wirePoke ProduceResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ProduceResponse.
wirePeekProduceResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ProduceResponse, Ptr Word8)
wirePeekProduceResponse version _fp _basePtr p0 endPtr
  | version == 9 = do
    (f0_responses, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceResponse version _fp _basePtr p e) p0 endPtr
    (f1_throttletimems, p2) <- W.peekInt32BE p1 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p2 endPtr
    let !_tag_nodeendpoints = if version >= 10 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (ProduceResponse { produceResponseResponses = f0_responses, produceResponseThrottleTimeMs = f1_throttletimems, produceResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version >= 10 && version <= 13 = do
    (f0_responses, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceResponse version _fp _basePtr p e) p0 endPtr
    (f1_throttletimems, p2) <- W.peekInt32BE p1 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p2 endPtr
    let !_tag_nodeendpoints = if version >= 10 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (ProduceResponse { produceResponseResponses = f0_responses, produceResponseThrottleTimeMs = f1_throttletimems, produceResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version >= 3 && version <= 8 = do
    (f0_responses, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceResponse version _fp _basePtr p e) p0 endPtr
    (f1_throttletimems, p2) <- W.peekInt32BE p1 endPtr
    pure (ProduceResponse { produceResponseResponses = f0_responses, produceResponseThrottleTimeMs = f1_throttletimems, produceResponseNodeEndpoints = P.mkKafkaArray V.empty }, p2)
  | otherwise = error $ "wirePeek ProduceResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ProduceResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeProduceResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeProduceResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekProduceResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}