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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC


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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ConsumerGroupDescribeResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
