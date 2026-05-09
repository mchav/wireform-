{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.JoinGroupResponse
Description : Kafka JoinGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 11.



Valid versions: 0-9
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.JoinGroupResponse
  (
    JoinGroupResponse(..),
    JoinGroupResponseMember(..),
    encodeJoinGroupResponse,
    decodeJoinGroupResponse,
    maxJoinGroupResponseVersion
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
data JoinGroupResponseMember = JoinGroupResponseMember
  {

  -- | The group member ID.

  -- Versions: 0+
  joinGroupResponseMemberMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 5+
  joinGroupResponseMemberGroupInstanceId :: !(KafkaString)
,

  -- | The group member metadata.

  -- Versions: 0+
  joinGroupResponseMemberMetadata :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode JoinGroupResponseMember with version-aware field handling.
encodeJoinGroupResponseMember :: MonadPut m => E.ApiVersion -> JoinGroupResponseMember -> m ()
encodeJoinGroupResponseMember version jmsg =
  do
    if version >= 6 then serialize (toCompactString (joinGroupResponseMemberMemberId jmsg)) else serialize (joinGroupResponseMemberMemberId jmsg)
    when (version >= 5) $
      if version >= 6 then serialize (toCompactString (joinGroupResponseMemberGroupInstanceId jmsg)) else serialize (joinGroupResponseMemberGroupInstanceId jmsg)
    if version >= 6 then serialize (toCompactBytes (joinGroupResponseMemberMetadata jmsg)) else serialize (joinGroupResponseMemberMetadata jmsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode JoinGroupResponseMember with version-aware field handling.
decodeJoinGroupResponseMember :: MonadGet m => E.ApiVersion -> m JoinGroupResponseMember
decodeJoinGroupResponseMember version =
  do
    fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupinstanceid <- if version >= 5
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmetadata <- if version >= 6 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure JoinGroupResponseMember
      {
      joinGroupResponseMemberMemberId = fieldmemberid
      ,
      joinGroupResponseMemberGroupInstanceId = fieldgroupinstanceid
      ,
      joinGroupResponseMemberMetadata = fieldmetadata
      }



data JoinGroupResponse = JoinGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  joinGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  joinGroupResponseErrorCode :: !(Int16)
,

  -- | The generation ID of the group.

  -- Versions: 0+
  joinGroupResponseGenerationId :: !(Int32)
,

  -- | The group protocol name.

  -- Versions: 7+
  joinGroupResponseProtocolType :: !(KafkaString)
,

  -- | The group protocol selected by the coordinator.

  -- Versions: 0+
  joinGroupResponseProtocolName :: !(KafkaString)
,

  -- | The leader of the group.

  -- Versions: 0+
  joinGroupResponseLeader :: !(KafkaString)
,

  -- | True if the leader must skip running the assignment.

  -- Versions: 9+
  joinGroupResponseSkipAssignment :: !(Bool)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 0+
  joinGroupResponseMemberId :: !(KafkaString)
,

  -- | The group members.

  -- Versions: 0+
  joinGroupResponseMembers :: !(KafkaArray (JoinGroupResponseMember))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for JoinGroupResponse.
maxJoinGroupResponseVersion :: Int16
maxJoinGroupResponseVersion = 9

-- | Encode JoinGroupResponse with the given API version.
encodeJoinGroupResponse :: MonadPut m => E.ApiVersion -> JoinGroupResponse -> m ()
encodeJoinGroupResponse version msg
  | version == 6 =
    do
      serialize (joinGroupResponseThrottleTimeMs msg)
      serialize (joinGroupResponseErrorCode msg)
      serialize (joinGroupResponseGenerationId msg)
      serialize (toCompactString (joinGroupResponseProtocolName msg))
      serialize (toCompactString (joinGroupResponseLeader msg))
      serialize (toCompactString (joinGroupResponseMemberId msg))
      E.encodeVersionedArray version 6 encodeJoinGroupResponseMember (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 9 =
    do
      serialize (joinGroupResponseThrottleTimeMs msg)
      serialize (joinGroupResponseErrorCode msg)
      serialize (joinGroupResponseGenerationId msg)
      serialize (toCompactString (joinGroupResponseProtocolType msg))
      serialize (toCompactString (joinGroupResponseProtocolName msg))
      serialize (toCompactString (joinGroupResponseLeader msg))
      serialize (joinGroupResponseSkipAssignment msg)
      serialize (toCompactString (joinGroupResponseMemberId msg))
      E.encodeVersionedArray version 6 encodeJoinGroupResponseMember (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (joinGroupResponseErrorCode msg)
      serialize (joinGroupResponseGenerationId msg)
      serialize (joinGroupResponseProtocolName msg)
      serialize (joinGroupResponseLeader msg)
      serialize (joinGroupResponseMemberId msg)
      E.encodeVersionedArray version 6 encodeJoinGroupResponseMember (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 7 && version <= 8 =
    do
      serialize (joinGroupResponseThrottleTimeMs msg)
      serialize (joinGroupResponseErrorCode msg)
      serialize (joinGroupResponseGenerationId msg)
      serialize (toCompactString (joinGroupResponseProtocolType msg))
      serialize (toCompactString (joinGroupResponseProtocolName msg))
      serialize (toCompactString (joinGroupResponseLeader msg))
      serialize (toCompactString (joinGroupResponseMemberId msg))
      E.encodeVersionedArray version 6 encodeJoinGroupResponseMember (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 2 && version <= 5 =
    do
      serialize (joinGroupResponseThrottleTimeMs msg)
      serialize (joinGroupResponseErrorCode msg)
      serialize (joinGroupResponseGenerationId msg)
      serialize (joinGroupResponseProtocolName msg)
      serialize (joinGroupResponseLeader msg)
      serialize (joinGroupResponseMemberId msg)
      E.encodeVersionedArray version 6 encodeJoinGroupResponseMember (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode JoinGroupResponse with the given API version.
decodeJoinGroupResponse :: MonadGet m => E.ApiVersion -> m JoinGroupResponse
decodeJoinGroupResponse version
  | version == 6 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgenerationid <- deserialize
      fieldprotocolname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldleader <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupResponseMember
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure JoinGroupResponse
        {
        joinGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        joinGroupResponseErrorCode = fielderrorcode
        ,
        joinGroupResponseGenerationId = fieldgenerationid
        ,
        joinGroupResponseProtocolType = P.KafkaString Null
        ,
        joinGroupResponseProtocolName = fieldprotocolname
        ,
        joinGroupResponseLeader = fieldleader
        ,
        joinGroupResponseSkipAssignment = False
        ,
        joinGroupResponseMemberId = fieldmemberid
        ,
        joinGroupResponseMembers = fieldmembers
        }

  | version == 9 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgenerationid <- deserialize
      fieldprotocoltype <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocolname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldleader <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldskipassignment <- deserialize
      fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupResponseMember
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure JoinGroupResponse
        {
        joinGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        joinGroupResponseErrorCode = fielderrorcode
        ,
        joinGroupResponseGenerationId = fieldgenerationid
        ,
        joinGroupResponseProtocolType = fieldprotocoltype
        ,
        joinGroupResponseProtocolName = fieldprotocolname
        ,
        joinGroupResponseLeader = fieldleader
        ,
        joinGroupResponseSkipAssignment = fieldskipassignment
        ,
        joinGroupResponseMemberId = fieldmemberid
        ,
        joinGroupResponseMembers = fieldmembers
        }

  | version >= 0 && version <= 1 =
    do
      fielderrorcode <- deserialize
      fieldgenerationid <- deserialize
      fieldprotocolname <- deserialize
      fieldleader <- deserialize
      fieldmemberid <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupResponseMember
      pure JoinGroupResponse
        {
        joinGroupResponseThrottleTimeMs = 0
        ,
        joinGroupResponseErrorCode = fielderrorcode
        ,
        joinGroupResponseGenerationId = fieldgenerationid
        ,
        joinGroupResponseProtocolType = P.KafkaString Null
        ,
        joinGroupResponseProtocolName = fieldprotocolname
        ,
        joinGroupResponseLeader = fieldleader
        ,
        joinGroupResponseSkipAssignment = False
        ,
        joinGroupResponseMemberId = fieldmemberid
        ,
        joinGroupResponseMembers = fieldmembers
        }

  | version >= 7 && version <= 8 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgenerationid <- deserialize
      fieldprotocoltype <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocolname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldleader <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupResponseMember
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure JoinGroupResponse
        {
        joinGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        joinGroupResponseErrorCode = fielderrorcode
        ,
        joinGroupResponseGenerationId = fieldgenerationid
        ,
        joinGroupResponseProtocolType = fieldprotocoltype
        ,
        joinGroupResponseProtocolName = fieldprotocolname
        ,
        joinGroupResponseLeader = fieldleader
        ,
        joinGroupResponseSkipAssignment = False
        ,
        joinGroupResponseMemberId = fieldmemberid
        ,
        joinGroupResponseMembers = fieldmembers
        }

  | version >= 2 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgenerationid <- deserialize
      fieldprotocolname <- deserialize
      fieldleader <- deserialize
      fieldmemberid <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupResponseMember
      pure JoinGroupResponse
        {
        joinGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        joinGroupResponseErrorCode = fielderrorcode
        ,
        joinGroupResponseGenerationId = fieldgenerationid
        ,
        joinGroupResponseProtocolType = P.KafkaString Null
        ,
        joinGroupResponseProtocolName = fieldprotocolname
        ,
        joinGroupResponseLeader = fieldleader
        ,
        joinGroupResponseSkipAssignment = False
        ,
        joinGroupResponseMemberId = fieldmemberid
        ,
        joinGroupResponseMembers = fieldmembers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec JoinGroupResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
