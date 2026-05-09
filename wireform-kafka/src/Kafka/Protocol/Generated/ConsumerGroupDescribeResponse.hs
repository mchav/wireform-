{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ConsumerGroupDescribeResponse
Description : Kafka ConsumerGroupDescribeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 69.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ConsumerGroupDescribeResponse
  (
    ConsumerGroupDescribeResponse(..),
    DescribedGroup(..),
    Member(..),
    Assignment(..),
    TopicPartitions(..),
    encodeConsumerGroupDescribeResponse,
    decodeConsumerGroupDescribeResponse,
    maxConsumerGroupDescribeResponseVersion
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


data TopicPartitions = TopicPartitions
  {

  -- | The topic ID.

  -- Versions: 0+
  topicPartitionsTopicId :: !(KafkaUuid)
,

  -- | The topic name.

  -- Versions: 0+
  topicPartitionsTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  topicPartitionsPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartitions with version-aware field handling.
encodeTopicPartitions :: MonadPut m => E.ApiVersion -> TopicPartitions -> m ()
encodeTopicPartitions version tmsg =
  do
    serialize (topicPartitionsTopicId tmsg)
    if version >= 0 then serialize (toCompactString (topicPartitionsTopicName tmsg)) else serialize (topicPartitionsTopicName tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicPartitionsPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicPartitions with version-aware field handling.
decodeTopicPartitions :: MonadGet m => E.ApiVersion -> m TopicPartitions
decodeTopicPartitions version =
  do
    fieldtopicid <- deserialize
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicPartitions
      {
      topicPartitionsTopicId = fieldtopicid
      ,
      topicPartitionsTopicName = fieldtopicname
      ,
      topicPartitionsPartitions = fieldpartitions
      }



data Assignment = Assignment
  {

  -- | The assigned topic-partitions to the member.

  -- Versions: 0+
  assignmentTopicPartitions :: !(KafkaArray (TopicPartitions))

  }
  deriving (Eq, Show, Generic)


-- | Encode Assignment with version-aware field handling.
encodeAssignment :: MonadPut m => E.ApiVersion -> Assignment -> m ()
encodeAssignment version amsg =
  do
    E.encodeVersionedArray version 0 encodeTopicPartitions (case P.unKafkaArray (assignmentTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Assignment with version-aware field handling.
decodeAssignment :: MonadGet m => E.ApiVersion -> m Assignment
decodeAssignment version =
  do
    fieldtopicpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicPartitions
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Assignment
      {
      assignmentTopicPartitions = fieldtopicpartitions
      }


-- | The members.
data Member = Member
  {

  -- | The member ID.

  -- Versions: 0+
  memberMemberId :: !(KafkaString)
,

  -- | The member instance ID.

  -- Versions: 0+
  memberInstanceId :: !(KafkaString)
,

  -- | The member rack ID.

  -- Versions: 0+
  memberRackId :: !(KafkaString)
,

  -- | The current member epoch.

  -- Versions: 0+
  memberMemberEpoch :: !(Int32)
,

  -- | The client ID.

  -- Versions: 0+
  memberClientId :: !(KafkaString)
,

  -- | The client host.

  -- Versions: 0+
  memberClientHost :: !(KafkaString)
,

  -- | The subscribed topic names.

  -- Versions: 0+
  memberSubscribedTopicNames :: !(KafkaArray (KafkaString))
,

  -- | the subscribed topic regex otherwise or null of not provided.

  -- Versions: 0+
  memberSubscribedTopicRegex :: !(KafkaString)
,

  -- | The current assignment.

  -- Versions: 0+
  memberAssignment :: !(Assignment)
,

  -- | The target assignment.

  -- Versions: 0+
  memberTargetAssignment :: !(Assignment)
,

  -- | -1 for unknown. 0 for classic member. +1 for consumer member.

  -- Versions: 1+
  memberMemberType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode Member with version-aware field handling.
encodeMember :: MonadPut m => E.ApiVersion -> Member -> m ()
encodeMember version mmsg =
  do
    if version >= 0 then serialize (toCompactString (memberMemberId mmsg)) else serialize (memberMemberId mmsg)
    if version >= 0 then serialize (toCompactString (memberInstanceId mmsg)) else serialize (memberInstanceId mmsg)
    if version >= 0 then serialize (toCompactString (memberRackId mmsg)) else serialize (memberRackId mmsg)
    serialize (memberMemberEpoch mmsg)
    if version >= 0 then serialize (toCompactString (memberClientId mmsg)) else serialize (memberClientId mmsg)
    if version >= 0 then serialize (toCompactString (memberClientHost mmsg)) else serialize (memberClientHost mmsg)
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (memberSubscribedTopicNames mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    if version >= 0 then serialize (toCompactString (memberSubscribedTopicRegex mmsg)) else serialize (memberSubscribedTopicRegex mmsg)
    encodeAssignment version (memberAssignment mmsg)
    encodeAssignment version (memberTargetAssignment mmsg)
    when (version >= 1) $
      serialize (memberMemberType mmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Member with version-aware field handling.
decodeMember :: MonadGet m => E.ApiVersion -> m Member
decodeMember version =
  do
    fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldinstanceid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldrackid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldmemberepoch <- deserialize
    fieldclientid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldclienthost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldsubscribedtopicnames <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldsubscribedtopicregex <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldassignment <- decodeAssignment version
    fieldtargetassignment <- decodeAssignment version
    fieldmembertype <- if version >= 1
      then deserialize
      else pure ((-1))
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Member
      {
      memberMemberId = fieldmemberid
      ,
      memberInstanceId = fieldinstanceid
      ,
      memberRackId = fieldrackid
      ,
      memberMemberEpoch = fieldmemberepoch
      ,
      memberClientId = fieldclientid
      ,
      memberClientHost = fieldclienthost
      ,
      memberSubscribedTopicNames = fieldsubscribedtopicnames
      ,
      memberSubscribedTopicRegex = fieldsubscribedtopicregex
      ,
      memberAssignment = fieldassignment
      ,
      memberTargetAssignment = fieldtargetassignment
      ,
      memberMemberType = fieldmembertype
      }


-- | Each described group.
data DescribedGroup = DescribedGroup
  {

  -- | The describe error, or 0 if there was no error.

  -- Versions: 0+
  describedGroupErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  describedGroupErrorMessage :: !(KafkaString)
,

  -- | The group ID string.

  -- Versions: 0+
  describedGroupGroupId :: !(KafkaString)
,

  -- | The group state string, or the empty string.

  -- Versions: 0+
  describedGroupGroupState :: !(KafkaString)
,

  -- | The group epoch.

  -- Versions: 0+
  describedGroupGroupEpoch :: !(Int32)
,

  -- | The assignment epoch.

  -- Versions: 0+
  describedGroupAssignmentEpoch :: !(Int32)
,

  -- | The selected assignor.

  -- Versions: 0+
  describedGroupAssignorName :: !(KafkaString)
,

  -- | The members.

  -- Versions: 0+
  describedGroupMembers :: !(KafkaArray (Member))
,

  -- | 32-bit bitfield to represent authorized operations for this group.

  -- Versions: 0+
  describedGroupAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedGroup with version-aware field handling.
encodeDescribedGroup :: MonadPut m => E.ApiVersion -> DescribedGroup -> m ()
encodeDescribedGroup version dmsg =
  do
    serialize (describedGroupErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupErrorMessage dmsg)) else serialize (describedGroupErrorMessage dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupGroupId dmsg)) else serialize (describedGroupGroupId dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupGroupState dmsg)) else serialize (describedGroupGroupState dmsg)
    serialize (describedGroupGroupEpoch dmsg)
    serialize (describedGroupAssignmentEpoch dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupAssignorName dmsg)) else serialize (describedGroupAssignorName dmsg)
    E.encodeVersionedArray version 0 encodeMember (case P.unKafkaArray (describedGroupMembers dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    serialize (describedGroupAuthorizedOperations dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroup with version-aware field handling.
decodeDescribedGroup :: MonadGet m => E.ApiVersion -> m DescribedGroup
decodeDescribedGroup version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupstate <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupepoch <- deserialize
    fieldassignmentepoch <- deserialize
    fieldassignorname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeMember
    fieldauthorizedoperations <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedGroup
      {
      describedGroupErrorCode = fielderrorcode
      ,
      describedGroupErrorMessage = fielderrormessage
      ,
      describedGroupGroupId = fieldgroupid
      ,
      describedGroupGroupState = fieldgroupstate
      ,
      describedGroupGroupEpoch = fieldgroupepoch
      ,
      describedGroupAssignmentEpoch = fieldassignmentepoch
      ,
      describedGroupAssignorName = fieldassignorname
      ,
      describedGroupMembers = fieldmembers
      ,
      describedGroupAuthorizedOperations = fieldauthorizedoperations
      }



data ConsumerGroupDescribeResponse = ConsumerGroupDescribeResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  consumerGroupDescribeResponseThrottleTimeMs :: !(Int32)
,

  -- | Each described group.

  -- Versions: 0+
  consumerGroupDescribeResponseGroups :: !(KafkaArray (DescribedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ConsumerGroupDescribeResponse.
maxConsumerGroupDescribeResponseVersion :: Int16
maxConsumerGroupDescribeResponseVersion = 1

-- | KafkaMessage instance for ConsumerGroupDescribeResponse.
instance KafkaMessage ConsumerGroupDescribeResponse where
  messageApiKey = 69
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode ConsumerGroupDescribeResponse with the given API version.
encodeConsumerGroupDescribeResponse :: MonadPut m => E.ApiVersion -> ConsumerGroupDescribeResponse -> m ()
encodeConsumerGroupDescribeResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (consumerGroupDescribeResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeDescribedGroup (case P.unKafkaArray (consumerGroupDescribeResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ConsumerGroupDescribeResponse with the given API version.
decodeConsumerGroupDescribeResponse :: MonadGet m => E.ApiVersion -> m ConsumerGroupDescribeResponse
decodeConsumerGroupDescribeResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribedGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ConsumerGroupDescribeResponse
        {
        consumerGroupDescribeResponseThrottleTimeMs = fieldthrottletimems
        ,
        consumerGroupDescribeResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a TopicPartitions.
wireMaxSizeTopicPartitions :: Int -> TopicPartitions -> Int
wireMaxSizeTopicPartitions _version msg =
  0
  + 16
  + WP.compactStringMaxSize (P.toCompactString (topicPartitionsTopicName msg))
  + (5 + (case P.unKafkaArray (topicPartitionsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicPartitions.
wirePokeTopicPartitions :: Int -> Ptr Word8 -> TopicPartitions -> IO (Ptr Word8)
wirePokeTopicPartitions version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (topicPartitionsTopicId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (topicPartitionsTopicName msg))
  p3 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p2 (topicPartitionsPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TopicPartitions.
wirePeekTopicPartitions :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicPartitions, Ptr Word8)
wirePeekTopicPartitions version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_topicname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 0 W.peekInt32BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TopicPartitions { topicPartitionsTopicId = f0_topicid, topicPartitionsTopicName = f1_topicname, topicPartitionsPartitions = f2_partitions }, pTagsEnd)

-- | Worst-case wire size of a Assignment.
wireMaxSizeAssignment :: Int -> Assignment -> Int
wireMaxSizeAssignment _version msg =
  0
  + (5 + (case P.unKafkaArray (assignmentTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicPartitions _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Assignment.
wirePokeAssignment :: Int -> Ptr Word8 -> Assignment -> IO (Ptr Word8)
wirePokeAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicPartitions version p x) p0 (assignmentTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for Assignment.
wirePeekAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Assignment, Ptr Word8)
wirePeekAssignment version _fp _basePtr p0 endPtr = do
  (f0_topicpartitions, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicPartitions version _fp _basePtr p e) p0 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (Assignment { assignmentTopicPartitions = f0_topicpartitions }, pTagsEnd)

-- | Worst-case wire size of a Member.
wireMaxSizeMember :: Int -> Member -> Int
wireMaxSizeMember _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (memberMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberRackId msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (memberClientId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberClientHost msg))
  + (5 + (case P.unKafkaArray (memberSubscribedTopicNames msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (memberSubscribedTopicRegex msg))
  + wireMaxSizeAssignment _version (memberAssignment msg)
  + wireMaxSizeAssignment _version (memberTargetAssignment msg)
  + 1
  + 1

-- | Direct-poke encoder for Member.
wirePokeMember :: Int -> Ptr Word8 -> Member -> IO (Ptr Word8)
wirePokeMember version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (memberMemberId msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (memberInstanceId msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (memberRackId msg))
  p4 <- W.pokeInt32BE p3 (memberMemberEpoch msg)
  p5 <- WP.pokeCompactString p4 (P.toCompactString (memberClientId msg))
  p6 <- WP.pokeCompactString p5 (P.toCompactString (memberClientHost msg))
  p7 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p6 (memberSubscribedTopicNames msg)
  p8 <- WP.pokeCompactString p7 (P.toCompactString (memberSubscribedTopicRegex msg))
  p9 <- wirePokeAssignment version p8 (memberAssignment msg)
  p10 <- wirePokeAssignment version p9 (memberTargetAssignment msg)
  p11 <- W.pokeWord8 p10 (fromIntegral (memberMemberType msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p11 else pure p11

-- | Direct-poke decoder for Member.
wirePeekMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Member, Ptr Word8)
wirePeekMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_instanceid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_rackid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_memberepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_clientid, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
  (f5_clienthost, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
  (f6_subscribedtopicnames, p7) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p6 endPtr
  (f7_subscribedtopicregex, p8) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr
  (f8_assignment, p9) <- wirePeekAssignment version _fp _basePtr p8 endPtr
  (f9_targetassignment, p10) <- wirePeekAssignment version _fp _basePtr p9 endPtr
  (f10_membertype, p11) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p10 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p11 endPtr else pure p11
  pure (Member { memberMemberId = f0_memberid, memberInstanceId = f1_instanceid, memberRackId = f2_rackid, memberMemberEpoch = f3_memberepoch, memberClientId = f4_clientid, memberClientHost = f5_clienthost, memberSubscribedTopicNames = f6_subscribedtopicnames, memberSubscribedTopicRegex = f7_subscribedtopicregex, memberAssignment = f8_assignment, memberTargetAssignment = f9_targetassignment, memberMemberType = f10_membertype }, pTagsEnd)

-- | Worst-case wire size of a DescribedGroup.
wireMaxSizeDescribedGroup :: Int -> DescribedGroup -> Int
wireMaxSizeDescribedGroup _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describedGroupErrorMessage msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupState msg))
  + 4
  + 4
  + WP.compactStringMaxSize (P.toCompactString (describedGroupAssignorName msg))
  + (5 + (case P.unKafkaArray (describedGroupMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMember _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribedGroup.
wirePokeDescribedGroup :: Int -> Ptr Word8 -> DescribedGroup -> IO (Ptr Word8)
wirePokeDescribedGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describedGroupErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedGroupErrorMessage msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describedGroupGroupId msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describedGroupGroupState msg))
  p5 <- W.pokeInt32BE p4 (describedGroupGroupEpoch msg)
  p6 <- W.pokeInt32BE p5 (describedGroupAssignmentEpoch msg)
  p7 <- WP.pokeCompactString p6 (P.toCompactString (describedGroupAssignorName msg))
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeMember version p x) p7 (describedGroupMembers msg)
  p9 <- W.pokeInt32BE p8 (describedGroupAuthorizedOperations msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p9 else pure p9

-- | Direct-poke decoder for DescribedGroup.
wirePeekDescribedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroup, Ptr Word8)
wirePeekDescribedGroup version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_groupid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_groupstate, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_groupepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_assignmentepoch, p6) <- W.peekInt32BE p5 endPtr
  (f6_assignorname, p7) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr
  (f7_members, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekMember version _fp _basePtr p e) p7 endPtr
  (f8_authorizedoperations, p9) <- W.peekInt32BE p8 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p9 endPtr else pure p9
  pure (DescribedGroup { describedGroupErrorCode = f0_errorcode, describedGroupErrorMessage = f1_errormessage, describedGroupGroupId = f2_groupid, describedGroupGroupState = f3_groupstate, describedGroupGroupEpoch = f4_groupepoch, describedGroupAssignmentEpoch = f5_assignmentepoch, describedGroupAssignorName = f6_assignorname, describedGroupMembers = f7_members, describedGroupAuthorizedOperations = f8_authorizedoperations }, pTagsEnd)

-- | Worst-case wire size of a ConsumerGroupDescribeResponse.
wireMaxSizeConsumerGroupDescribeResponse :: Int -> ConsumerGroupDescribeResponse -> Int
wireMaxSizeConsumerGroupDescribeResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (consumerGroupDescribeResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ConsumerGroupDescribeResponse.
wirePokeConsumerGroupDescribeResponse :: Int -> Ptr Word8 -> ConsumerGroupDescribeResponse -> IO (Ptr Word8)
wirePokeConsumerGroupDescribeResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (consumerGroupDescribeResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribedGroup version p x) p1 (consumerGroupDescribeResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ConsumerGroupDescribeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ConsumerGroupDescribeResponse.
wirePeekConsumerGroupDescribeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ConsumerGroupDescribeResponse, Ptr Word8)
wirePeekConsumerGroupDescribeResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ConsumerGroupDescribeResponse { consumerGroupDescribeResponseThrottleTimeMs = f0_throttletimems, consumerGroupDescribeResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek ConsumerGroupDescribeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ConsumerGroupDescribeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeConsumerGroupDescribeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeConsumerGroupDescribeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekConsumerGroupDescribeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}