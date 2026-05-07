{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.JoinGroupRequest
Description : Kafka JoinGroupRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 11.



Valid versions: 0-9
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.JoinGroupRequest
  (
    JoinGroupRequest(..),
    JoinGroupRequestProtocol(..),
    encodeJoinGroupRequest,
    decodeJoinGroupRequest,
    maxJoinGroupRequestVersion
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


-- | The list of protocols that the member supports.
data JoinGroupRequestProtocol = JoinGroupRequestProtocol
  {

  -- | The protocol name.

  -- Versions: 0+
  joinGroupRequestProtocolName :: !(KafkaString)
,

  -- | The protocol metadata.

  -- Versions: 0+
  joinGroupRequestProtocolMetadata :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode JoinGroupRequestProtocol with version-aware field handling.
encodeJoinGroupRequestProtocol :: MonadPut m => E.ApiVersion -> JoinGroupRequestProtocol -> m ()
encodeJoinGroupRequestProtocol version jmsg =
  do
    if version >= 6 then serialize (toCompactString (joinGroupRequestProtocolName jmsg)) else serialize (joinGroupRequestProtocolName jmsg)
    if version >= 6 then serialize (toCompactBytes (joinGroupRequestProtocolMetadata jmsg)) else serialize (joinGroupRequestProtocolMetadata jmsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode JoinGroupRequestProtocol with version-aware field handling.
decodeJoinGroupRequestProtocol :: MonadGet m => E.ApiVersion -> m JoinGroupRequestProtocol
decodeJoinGroupRequestProtocol version =
  do
    fieldname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
    fieldmetadata <- if version >= 6 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure JoinGroupRequestProtocol
      {
      joinGroupRequestProtocolName = fieldname
      ,
      joinGroupRequestProtocolMetadata = fieldmetadata
      }



data JoinGroupRequest = JoinGroupRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  joinGroupRequestGroupId :: !(KafkaString)
,

  -- | The coordinator considers the consumer dead if it receives no heartbeat after this timeout in millis

  -- Versions: 0+
  joinGroupRequestSessionTimeoutMs :: !(Int32)
,

  -- | The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebal

  -- Versions: 1+
  joinGroupRequestRebalanceTimeoutMs :: !(Int32)
,

  -- | The member id assigned by the group coordinator.

  -- Versions: 0+
  joinGroupRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 5+
  joinGroupRequestGroupInstanceId :: !(KafkaString)
,

  -- | The unique name the for class of protocols implemented by the group we want to join.

  -- Versions: 0+
  joinGroupRequestProtocolType :: !(KafkaString)
,

  -- | The list of protocols that the member supports.

  -- Versions: 0+
  joinGroupRequestProtocols :: !(KafkaArray (JoinGroupRequestProtocol))
,

  -- | The reason why the member (re-)joins the group.

  -- Versions: 8+
  joinGroupRequestReason :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for JoinGroupRequest.
maxJoinGroupRequestVersion :: Int16
maxJoinGroupRequestVersion = 9

-- | Encode JoinGroupRequest with the given API version.
encodeJoinGroupRequest :: MonadPut m => E.ApiVersion -> JoinGroupRequest -> m ()
encodeJoinGroupRequest version msg
  | version == 0 =
    do
      serialize (joinGroupRequestGroupId msg)
      serialize (joinGroupRequestSessionTimeoutMs msg)
      serialize (joinGroupRequestMemberId msg)
      serialize (joinGroupRequestProtocolType msg)
      E.encodeVersionedArray version 6 encodeJoinGroupRequestProtocol (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 5 =
    do
      serialize (joinGroupRequestGroupId msg)
      serialize (joinGroupRequestSessionTimeoutMs msg)
      serialize (joinGroupRequestRebalanceTimeoutMs msg)
      serialize (joinGroupRequestMemberId msg)
      serialize (joinGroupRequestGroupInstanceId msg)
      serialize (joinGroupRequestProtocolType msg)
      E.encodeVersionedArray version 6 encodeJoinGroupRequestProtocol (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 6 && version <= 7 =
    do
      serialize (toCompactString (joinGroupRequestGroupId msg))
      serialize (joinGroupRequestSessionTimeoutMs msg)
      serialize (joinGroupRequestRebalanceTimeoutMs msg)
      serialize (toCompactString (joinGroupRequestMemberId msg))
      serialize (toCompactString (joinGroupRequestGroupInstanceId msg))
      serialize (toCompactString (joinGroupRequestProtocolType msg))
      E.encodeVersionedArray version 6 encodeJoinGroupRequestProtocol (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 8 && version <= 9 =
    do
      serialize (toCompactString (joinGroupRequestGroupId msg))
      serialize (joinGroupRequestSessionTimeoutMs msg)
      serialize (joinGroupRequestRebalanceTimeoutMs msg)
      serialize (toCompactString (joinGroupRequestMemberId msg))
      serialize (toCompactString (joinGroupRequestGroupInstanceId msg))
      serialize (toCompactString (joinGroupRequestProtocolType msg))
      E.encodeVersionedArray version 6 encodeJoinGroupRequestProtocol (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (joinGroupRequestReason msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 4 =
    do
      serialize (joinGroupRequestGroupId msg)
      serialize (joinGroupRequestSessionTimeoutMs msg)
      serialize (joinGroupRequestRebalanceTimeoutMs msg)
      serialize (joinGroupRequestMemberId msg)
      serialize (joinGroupRequestProtocolType msg)
      E.encodeVersionedArray version 6 encodeJoinGroupRequestProtocol (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode JoinGroupRequest with the given API version.
decodeJoinGroupRequest :: MonadGet m => E.ApiVersion -> m JoinGroupRequest
decodeJoinGroupRequest version
  | version == 0 =
    do
      fieldgroupid <- deserialize
      fieldsessiontimeoutms <- deserialize
      fieldmemberid <- deserialize
      fieldprotocoltype <- deserialize
      fieldprotocols <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupRequestProtocol
      pure JoinGroupRequest
        {
        joinGroupRequestGroupId = fieldgroupid
        ,
        joinGroupRequestSessionTimeoutMs = fieldsessiontimeoutms
        ,
        joinGroupRequestRebalanceTimeoutMs = (-1)
        ,
        joinGroupRequestMemberId = fieldmemberid
        ,
        joinGroupRequestGroupInstanceId = P.KafkaString Null
        ,
        joinGroupRequestProtocolType = fieldprotocoltype
        ,
        joinGroupRequestProtocols = fieldprotocols
        ,
        joinGroupRequestReason = P.KafkaString Null
        }

  | version == 5 =
    do
      fieldgroupid <- deserialize
      fieldsessiontimeoutms <- deserialize
      fieldrebalancetimeoutms <- deserialize
      fieldmemberid <- deserialize
      fieldgroupinstanceid <- deserialize
      fieldprotocoltype <- deserialize
      fieldprotocols <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupRequestProtocol
      pure JoinGroupRequest
        {
        joinGroupRequestGroupId = fieldgroupid
        ,
        joinGroupRequestSessionTimeoutMs = fieldsessiontimeoutms
        ,
        joinGroupRequestRebalanceTimeoutMs = fieldrebalancetimeoutms
        ,
        joinGroupRequestMemberId = fieldmemberid
        ,
        joinGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        joinGroupRequestProtocolType = fieldprotocoltype
        ,
        joinGroupRequestProtocols = fieldprotocols
        ,
        joinGroupRequestReason = P.KafkaString Null
        }

  | version >= 6 && version <= 7 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldsessiontimeoutms <- deserialize
      fieldrebalancetimeoutms <- deserialize
      fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocoltype <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocols <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupRequestProtocol
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure JoinGroupRequest
        {
        joinGroupRequestGroupId = fieldgroupid
        ,
        joinGroupRequestSessionTimeoutMs = fieldsessiontimeoutms
        ,
        joinGroupRequestRebalanceTimeoutMs = fieldrebalancetimeoutms
        ,
        joinGroupRequestMemberId = fieldmemberid
        ,
        joinGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        joinGroupRequestProtocolType = fieldprotocoltype
        ,
        joinGroupRequestProtocols = fieldprotocols
        ,
        joinGroupRequestReason = P.KafkaString Null
        }

  | version >= 8 && version <= 9 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldsessiontimeoutms <- deserialize
      fieldrebalancetimeoutms <- deserialize
      fieldmemberid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocoltype <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocols <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupRequestProtocol
      fieldreason <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure JoinGroupRequest
        {
        joinGroupRequestGroupId = fieldgroupid
        ,
        joinGroupRequestSessionTimeoutMs = fieldsessiontimeoutms
        ,
        joinGroupRequestRebalanceTimeoutMs = fieldrebalancetimeoutms
        ,
        joinGroupRequestMemberId = fieldmemberid
        ,
        joinGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        joinGroupRequestProtocolType = fieldprotocoltype
        ,
        joinGroupRequestProtocols = fieldprotocols
        ,
        joinGroupRequestReason = fieldreason
        }

  | version >= 1 && version <= 4 =
    do
      fieldgroupid <- deserialize
      fieldsessiontimeoutms <- deserialize
      fieldrebalancetimeoutms <- deserialize
      fieldmemberid <- deserialize
      fieldprotocoltype <- deserialize
      fieldprotocols <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeJoinGroupRequestProtocol
      pure JoinGroupRequest
        {
        joinGroupRequestGroupId = fieldgroupid
        ,
        joinGroupRequestSessionTimeoutMs = fieldsessiontimeoutms
        ,
        joinGroupRequestRebalanceTimeoutMs = fieldrebalancetimeoutms
        ,
        joinGroupRequestMemberId = fieldmemberid
        ,
        joinGroupRequestGroupInstanceId = P.KafkaString Null
        ,
        joinGroupRequestProtocolType = fieldprotocoltype
        ,
        joinGroupRequestProtocols = fieldprotocols
        ,
        joinGroupRequestReason = P.KafkaString Null
        }
  | otherwise = fail $ "Unsupported version: " ++ show version