{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeGroupsResponse
Description : Kafka DescribeGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 15.



Valid versions: 0-6
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeGroupsResponse
  (
    DescribeGroupsResponse(..),
    DescribedGroup(..),
    DescribedGroupMember(..),
    encodeDescribeGroupsResponse,
    decodeDescribeGroupsResponse,
    maxDescribeGroupsResponseVersion
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


-- | The group members.
data DescribedGroupMember = DescribedGroupMember
  {

  -- | The member id.

  -- Versions: 0+
  describedGroupMemberMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 4+
  describedGroupMemberGroupInstanceId :: !(KafkaString)
,

  -- | The client ID used in the member's latest join group request.

  -- Versions: 0+
  describedGroupMemberClientId :: !(KafkaString)
,

  -- | The client host.

  -- Versions: 0+
  describedGroupMemberClientHost :: !(KafkaString)
,

  -- | The metadata corresponding to the current group protocol in use.

  -- Versions: 0+
  describedGroupMemberMemberMetadata :: !(KafkaBytes)
,

  -- | The current assignment provided by the group leader.

  -- Versions: 0+
  describedGroupMemberMemberAssignment :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedGroupMember with version-aware field handling.
encodeDescribedGroupMember :: MonadPut m => E.ApiVersion -> DescribedGroupMember -> m ()
encodeDescribedGroupMember version dmsg =
  do
    if version >= 5 then serialize (toCompactString (describedGroupMemberMemberId dmsg)) else serialize (describedGroupMemberMemberId dmsg)
    when (version >= 4) $
      if version >= 5 then serialize (toCompactString (describedGroupMemberGroupInstanceId dmsg)) else serialize (describedGroupMemberGroupInstanceId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupMemberClientId dmsg)) else serialize (describedGroupMemberClientId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupMemberClientHost dmsg)) else serialize (describedGroupMemberClientHost dmsg)
    if version >= 5 then serialize (toCompactBytes (describedGroupMemberMemberMetadata dmsg)) else serialize (describedGroupMemberMemberMetadata dmsg)
    if version >= 5 then serialize (toCompactBytes (describedGroupMemberMemberAssignment dmsg)) else serialize (describedGroupMemberMemberAssignment dmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroupMember with version-aware field handling.
decodeDescribedGroupMember :: MonadGet m => E.ApiVersion -> m DescribedGroupMember
decodeDescribedGroupMember version =
  do
    fieldmemberid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupinstanceid <- if version >= 4
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldclientid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldclienthost <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldmembermetadata <- if version >= 5 then P.fromCompactBytes <$> deserialize else deserialize
    fieldmemberassignment <- if version >= 5 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedGroupMember
      {
      describedGroupMemberMemberId = fieldmemberid
      ,
      describedGroupMemberGroupInstanceId = fieldgroupinstanceid
      ,
      describedGroupMemberClientId = fieldclientid
      ,
      describedGroupMemberClientHost = fieldclienthost
      ,
      describedGroupMemberMemberMetadata = fieldmembermetadata
      ,
      describedGroupMemberMemberAssignment = fieldmemberassignment
      }


-- | Each described group.
data DescribedGroup = DescribedGroup
  {

  -- | The describe error, or 0 if there was no error.

  -- Versions: 0+
  describedGroupErrorCode :: !(Int16)
,

  -- | The describe error message, or null if there was no error.

  -- Versions: 6+
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

  -- | The group protocol type, or the empty string.

  -- Versions: 0+
  describedGroupProtocolType :: !(KafkaString)
,

  -- | The group protocol data, or the empty string.

  -- Versions: 0+
  describedGroupProtocolData :: !(KafkaString)
,

  -- | The group members.

  -- Versions: 0+
  describedGroupMembers :: !(KafkaArray (DescribedGroupMember))
,

  -- | 32-bit bitfield to represent authorized operations for this group.

  -- Versions: 3+
  describedGroupAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedGroup with version-aware field handling.
encodeDescribedGroup :: MonadPut m => E.ApiVersion -> DescribedGroup -> m ()
encodeDescribedGroup version dmsg =
  do
    serialize (describedGroupErrorCode dmsg)
    when (version >= 6) $
      if version >= 5 then serialize (toCompactString (describedGroupErrorMessage dmsg)) else serialize (describedGroupErrorMessage dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupGroupId dmsg)) else serialize (describedGroupGroupId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupGroupState dmsg)) else serialize (describedGroupGroupState dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupProtocolType dmsg)) else serialize (describedGroupProtocolType dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupProtocolData dmsg)) else serialize (describedGroupProtocolData dmsg)
    E.encodeVersionedArray version 5 encodeDescribedGroupMember (case P.unKafkaArray (describedGroupMembers dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $
      serialize (describedGroupAuthorizedOperations dmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroup with version-aware field handling.
decodeDescribedGroup :: MonadGet m => E.ApiVersion -> m DescribedGroup
decodeDescribedGroup version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 6
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgroupid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupstate <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldprotocoltype <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldprotocoldata <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroupMember
    fieldauthorizedoperations <- if version >= 3
      then deserialize
      else pure ((-2147483648))
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
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
      describedGroupProtocolType = fieldprotocoltype
      ,
      describedGroupProtocolData = fieldprotocoldata
      ,
      describedGroupMembers = fieldmembers
      ,
      describedGroupAuthorizedOperations = fieldauthorizedoperations
      }



data DescribeGroupsResponse = DescribeGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  describeGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each described group.

  -- Versions: 0+
  describeGroupsResponseGroups :: !(KafkaArray (DescribedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeGroupsResponse.
maxDescribeGroupsResponseVersion :: Int16
maxDescribeGroupsResponseVersion = 6

-- | Encode DescribeGroupsResponse with the given API version.
encodeDescribeGroupsResponse :: MonadPut m => E.ApiVersion -> DescribeGroupsResponse -> m ()
encodeDescribeGroupsResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 5 && version <= 6 =
    do
      serialize (describeGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 4 =
    do
      serialize (describeGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeGroupsResponse with the given API version.
decodeDescribeGroupsResponse :: MonadGet m => E.ApiVersion -> m DescribeGroupsResponse
decodeDescribeGroupsResponse version
  | version == 0 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = 0
        ,
        describeGroupsResponseGroups = fieldgroups
        }

  | version >= 5 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeGroupsResponseGroups = fieldgroups
        }

  | version >= 1 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeGroupsResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeGroupsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
