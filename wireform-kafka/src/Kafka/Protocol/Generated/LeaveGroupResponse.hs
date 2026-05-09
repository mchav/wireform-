{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaveGroupResponse
Description : Kafka LeaveGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 13.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaveGroupResponse
  (
    LeaveGroupResponse(..),
    MemberResponse(..),
    encodeLeaveGroupResponse,
    decodeLeaveGroupResponse,
    maxLeaveGroupResponseVersion
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


-- | List of leaving member responses.
data MemberResponse = MemberResponse
  {

  -- | The member ID to remove from the group.

  -- Versions: 3+
  memberResponseMemberId :: !(KafkaString)
,

  -- | The group instance ID to remove from the group.

  -- Versions: 3+
  memberResponseGroupInstanceId :: !(KafkaString)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 3+
  memberResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode MemberResponse with version-aware field handling.
encodeMemberResponse :: MonadPut m => E.ApiVersion -> MemberResponse -> m ()
encodeMemberResponse version mmsg =
  do
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberResponseMemberId mmsg)) else serialize (memberResponseMemberId mmsg)
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberResponseGroupInstanceId mmsg)) else serialize (memberResponseGroupInstanceId mmsg)
    when (version >= 3) $
      serialize (memberResponseErrorCode mmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MemberResponse with version-aware field handling.
decodeMemberResponse :: MonadGet m => E.ApiVersion -> m MemberResponse
decodeMemberResponse version =
  do
    fieldmemberid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgroupinstanceid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MemberResponse
      {
      memberResponseMemberId = fieldmemberid
      ,
      memberResponseGroupInstanceId = fieldgroupinstanceid
      ,
      memberResponseErrorCode = fielderrorcode
      }



data LeaveGroupResponse = LeaveGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  leaveGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  leaveGroupResponseErrorCode :: !(Int16)
,

  -- | List of leaving member responses.

  -- Versions: 3+
  leaveGroupResponseMembers :: !(KafkaArray (MemberResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaveGroupResponse.
maxLeaveGroupResponseVersion :: Int16
maxLeaveGroupResponseVersion = 5

-- | Encode LeaveGroupResponse with the given API version.
encodeLeaveGroupResponse :: MonadPut m => E.ApiVersion -> LeaveGroupResponse -> m ()
encodeLeaveGroupResponse version msg
  | version == 0 =
    do
      serialize (leaveGroupResponseErrorCode msg)


  | version == 3 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)
      E.encodeVersionedArray version 4 encodeMemberResponse (case P.unKafkaArray (leaveGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 1 && version <= 2 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)


  | version >= 4 && version <= 5 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)
      E.encodeVersionedArray version 4 encodeMemberResponse (case P.unKafkaArray (leaveGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode LeaveGroupResponse with the given API version.
decodeLeaveGroupResponse :: MonadGet m => E.ApiVersion -> m LeaveGroupResponse
decodeLeaveGroupResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = 0
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberResponse
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = fieldmembers
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = fieldmembers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version