{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareGroupDescribeResponse
Description : Kafka ShareGroupDescribeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 77.



Valid versions: 1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareGroupDescribeResponse
  (
    ShareGroupDescribeResponse(..),
    DescribedGroup(..),
    Member(..),
    Assignment(..),
    TopicPartitions(..),
    maxShareGroupDescribeResponseVersion
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


data Assignment = Assignment
  {

  -- | The assigned topic-partitions to the member.

  -- Versions: 0+
  assignmentTopicPartitions :: !(KafkaArray (TopicPartitions))

  }
  deriving (Eq, Show, Generic)

-- | The members.
data Member = Member
  {

  -- | The member ID.

  -- Versions: 0+
  memberMemberId :: !(KafkaString)
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

  -- | The current assignment.

  -- Versions: 0+
  memberAssignment :: !(Assignment)

  }
  deriving (Eq, Show, Generic)

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


data ShareGroupDescribeResponse = ShareGroupDescribeResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  shareGroupDescribeResponseThrottleTimeMs :: !(Int32)
,

  -- | Each described group.

  -- Versions: 0+
  shareGroupDescribeResponseGroups :: !(KafkaArray (DescribedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareGroupDescribeResponse.
maxShareGroupDescribeResponseVersion :: Int16
maxShareGroupDescribeResponseVersion = 1

-- | KafkaMessage instance for ShareGroupDescribeResponse.
instance KafkaMessage ShareGroupDescribeResponse where
  messageApiKey = 77
  messageMinVersion = 1
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a TopicPartitions.
wireMaxSizeTopicPartitions :: Int -> TopicPartitions -> Int
wireMaxSizeTopicPartitions _version msg =
  0
  + 16
  + WP.dualStringMaxSize (topicPartitionsTopicName msg)
  + (5 + (case P.unKafkaArray (topicPartitionsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicPartitions.
wirePokeTopicPartitions :: Int -> Ptr Word8 -> TopicPartitions -> IO (Ptr Word8)
wirePokeTopicPartitions version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (topicPartitionsTopicId msg)
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (topicPartitionsTopicName msg)) else WP.pokeKafkaString p1 (topicPartitionsTopicName msg))
  p3 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p2 (topicPartitionsPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TopicPartitions.
wirePeekTopicPartitions :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicPartitions, Ptr Word8)
wirePeekTopicPartitions version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_topicname, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_partitions, p3) <- WP.peekVersionedArray version 0 W.peekInt32BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TopicPartitions { topicPartitionsTopicId = f0_topicid, topicPartitionsTopicName = f1_topicname, topicPartitionsPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicPartitions :: TopicPartitions
defaultTopicPartitions = TopicPartitions { topicPartitionsTopicId = P.nullUuid, topicPartitionsTopicName = P.KafkaString Null, topicPartitionsPartitions = P.mkKafkaArray V.empty }

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

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAssignment :: Assignment
defaultAssignment = Assignment { assignmentTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a Member.
wireMaxSizeMember :: Int -> Member -> Int
wireMaxSizeMember _version msg =
  0
  + WP.dualStringMaxSize (memberMemberId msg)
  + WP.dualStringMaxSize (memberRackId msg)
  + 4
  + WP.dualStringMaxSize (memberClientId msg)
  + WP.dualStringMaxSize (memberClientHost msg)
  + (5 + (case P.unKafkaArray (memberSubscribedTopicNames msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + wireMaxSizeAssignment _version (memberAssignment msg)
  + 1

-- | Direct-poke encoder for Member.
wirePokeMember :: Int -> Ptr Word8 -> Member -> IO (Ptr Word8)
wirePokeMember version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (memberMemberId msg)) else WP.pokeKafkaString p0 (memberMemberId msg))
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (memberRackId msg)) else WP.pokeKafkaString p1 (memberRackId msg))
  p3 <- W.pokeInt32BE p2 (memberMemberEpoch msg)
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (memberClientId msg)) else WP.pokeKafkaString p3 (memberClientId msg))
  p5 <- (if version >= 0 then WP.pokeCompactString p4 (P.toCompactString (memberClientHost msg)) else WP.pokeKafkaString p4 (memberClientHost msg))
  p6 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p5 (memberSubscribedTopicNames msg)
  p7 <- wirePokeAssignment version p6 (memberAssignment msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for Member.
wirePeekMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Member, Ptr Word8)
wirePeekMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_rackid, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_memberepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_clientid, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_clienthost, p5) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
  (f5_subscribedtopicnames, p6) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p5 endPtr
  (f6_assignment, p7) <- wirePeekAssignment version _fp _basePtr p6 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (Member { memberMemberId = f0_memberid, memberRackId = f1_rackid, memberMemberEpoch = f2_memberepoch, memberClientId = f3_clientid, memberClientHost = f4_clienthost, memberSubscribedTopicNames = f5_subscribedtopicnames, memberAssignment = f6_assignment }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultMember :: Member
defaultMember = Member { memberMemberId = P.KafkaString Null, memberRackId = P.KafkaString Null, memberMemberEpoch = 0, memberClientId = P.KafkaString Null, memberClientHost = P.KafkaString Null, memberSubscribedTopicNames = P.mkKafkaArray V.empty, memberAssignment = defaultAssignment }

-- | Worst-case wire size of a DescribedGroup.
wireMaxSizeDescribedGroup :: Int -> DescribedGroup -> Int
wireMaxSizeDescribedGroup _version msg =
  0
  + 2
  + WP.dualStringMaxSize (describedGroupErrorMessage msg)
  + WP.dualStringMaxSize (describedGroupGroupId msg)
  + WP.dualStringMaxSize (describedGroupGroupState msg)
  + 4
  + 4
  + WP.dualStringMaxSize (describedGroupAssignorName msg)
  + (5 + (case P.unKafkaArray (describedGroupMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMember _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribedGroup.
wirePokeDescribedGroup :: Int -> Ptr Word8 -> DescribedGroup -> IO (Ptr Word8)
wirePokeDescribedGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describedGroupErrorCode msg)
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (describedGroupErrorMessage msg)) else WP.pokeKafkaString p1 (describedGroupErrorMessage msg))
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (describedGroupGroupId msg)) else WP.pokeKafkaString p2 (describedGroupGroupId msg))
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (describedGroupGroupState msg)) else WP.pokeKafkaString p3 (describedGroupGroupState msg))
  p5 <- W.pokeInt32BE p4 (describedGroupGroupEpoch msg)
  p6 <- W.pokeInt32BE p5 (describedGroupAssignmentEpoch msg)
  p7 <- (if version >= 0 then WP.pokeCompactString p6 (P.toCompactString (describedGroupAssignorName msg)) else WP.pokeKafkaString p6 (describedGroupAssignorName msg))
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeMember version p x) p7 (describedGroupMembers msg)
  p9 <- W.pokeInt32BE p8 (describedGroupAuthorizedOperations msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p9 else pure p9

-- | Direct-poke decoder for DescribedGroup.
wirePeekDescribedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroup, Ptr Word8)
wirePeekDescribedGroup version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_groupid, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  (f3_groupstate, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_groupepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_assignmentepoch, p6) <- W.peekInt32BE p5 endPtr
  (f6_assignorname, p7) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr else WP.peekKafkaString p6 endPtr)
  (f7_members, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekMember version _fp _basePtr p e) p7 endPtr
  (f8_authorizedoperations, p9) <- W.peekInt32BE p8 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p9 endPtr else pure p9
  pure (DescribedGroup { describedGroupErrorCode = f0_errorcode, describedGroupErrorMessage = f1_errormessage, describedGroupGroupId = f2_groupid, describedGroupGroupState = f3_groupstate, describedGroupGroupEpoch = f4_groupepoch, describedGroupAssignmentEpoch = f5_assignmentepoch, describedGroupAssignorName = f6_assignorname, describedGroupMembers = f7_members, describedGroupAuthorizedOperations = f8_authorizedoperations }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribedGroup :: DescribedGroup
defaultDescribedGroup = DescribedGroup { describedGroupErrorCode = 0, describedGroupErrorMessage = P.KafkaString Null, describedGroupGroupId = P.KafkaString Null, describedGroupGroupState = P.KafkaString Null, describedGroupGroupEpoch = 0, describedGroupAssignmentEpoch = 0, describedGroupAssignorName = P.KafkaString Null, describedGroupMembers = P.mkKafkaArray V.empty, describedGroupAuthorizedOperations = 0 }

-- | Worst-case wire size of a ShareGroupDescribeResponse.
wireMaxSizeShareGroupDescribeResponse :: Int -> ShareGroupDescribeResponse -> Int
wireMaxSizeShareGroupDescribeResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (shareGroupDescribeResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareGroupDescribeResponse.
wirePokeShareGroupDescribeResponse :: Int -> Ptr Word8 -> ShareGroupDescribeResponse -> IO (Ptr Word8)
wirePokeShareGroupDescribeResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (shareGroupDescribeResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribedGroup version p x) p1 (shareGroupDescribeResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ShareGroupDescribeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareGroupDescribeResponse.
wirePeekShareGroupDescribeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareGroupDescribeResponse, Ptr Word8)
wirePeekShareGroupDescribeResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ShareGroupDescribeResponse { shareGroupDescribeResponseThrottleTimeMs = f0_throttletimems, shareGroupDescribeResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareGroupDescribeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ShareGroupDescribeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareGroupDescribeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareGroupDescribeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareGroupDescribeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}