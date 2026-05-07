{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SyncGroupResponse
Description : Kafka SyncGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 14.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SyncGroupResponse
  (
    SyncGroupResponse(..),
    encodeSyncGroupResponse,
    decodeSyncGroupResponse,
    maxSyncGroupResponseVersion
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




data SyncGroupResponse = SyncGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  syncGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  syncGroupResponseErrorCode :: !(Int16)
,

  -- | The group protocol type.

  -- Versions: 5+
  syncGroupResponseProtocolType :: !(KafkaString)
,

  -- | The group protocol name.

  -- Versions: 5+
  syncGroupResponseProtocolName :: !(KafkaString)
,

  -- | The member assignment.

  -- Versions: 0+
  syncGroupResponseAssignment :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SyncGroupResponse.
maxSyncGroupResponseVersion :: Int16
maxSyncGroupResponseVersion = 5

-- | Encode SyncGroupResponse with the given API version.
encodeSyncGroupResponse :: MonadPut m => E.ApiVersion -> SyncGroupResponse -> m ()
encodeSyncGroupResponse version msg
  | version == 0 =
    do
      serialize (syncGroupResponseErrorCode msg)
      serialize (syncGroupResponseAssignment msg)


  | version == 4 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (toCompactBytes (syncGroupResponseAssignment msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 5 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (toCompactString (syncGroupResponseProtocolType msg))
      serialize (toCompactString (syncGroupResponseProtocolName msg))
      serialize (toCompactBytes (syncGroupResponseAssignment msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (syncGroupResponseAssignment msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SyncGroupResponse with the given API version.
decodeSyncGroupResponse :: MonadGet m => E.ApiVersion -> m SyncGroupResponse
decodeSyncGroupResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldassignment <- deserialize
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = 0
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldassignment <- if version >= 4 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version == 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldprotocoltype <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocolname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldassignment <- if version >= 4 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = fieldprotocoltype
        ,
        syncGroupResponseProtocolName = fieldprotocolname
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldassignment <- deserialize
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }
  | otherwise = fail $ "Unsupported version: " ++ show version