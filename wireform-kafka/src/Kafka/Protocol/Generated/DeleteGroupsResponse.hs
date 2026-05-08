{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteGroupsResponse
Description : Kafka DeleteGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 42.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteGroupsResponse
  (
    DeleteGroupsResponse(..),
    DeletableGroupResult(..),
    encodeDeleteGroupsResponse,
    decodeDeleteGroupsResponse,
    maxDeleteGroupsResponseVersion
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


-- | The deletion results.
data DeletableGroupResult = DeletableGroupResult
  {

  -- | The group id.

  -- Versions: 0+
  deletableGroupResultGroupId :: !(KafkaString)
,

  -- | The deletion error, or 0 if the deletion succeeded.

  -- Versions: 0+
  deletableGroupResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeletableGroupResult with version-aware field handling.
encodeDeletableGroupResult :: MonadPut m => E.ApiVersion -> DeletableGroupResult -> m ()
encodeDeletableGroupResult version dmsg =
  do
    if version >= 2 then serialize (toCompactString (deletableGroupResultGroupId dmsg)) else serialize (deletableGroupResultGroupId dmsg)
    serialize (deletableGroupResultErrorCode dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeletableGroupResult with version-aware field handling.
decodeDeletableGroupResult :: MonadGet m => E.ApiVersion -> m DeletableGroupResult
decodeDeletableGroupResult version =
  do
    fieldgroupid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeletableGroupResult
      {
      deletableGroupResultGroupId = fieldgroupid
      ,
      deletableGroupResultErrorCode = fielderrorcode
      }



data DeleteGroupsResponse = DeleteGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | The deletion results.

  -- Versions: 0+
  deleteGroupsResponseResults :: !(KafkaArray (DeletableGroupResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteGroupsResponse.
maxDeleteGroupsResponseVersion :: Int16
maxDeleteGroupsResponseVersion = 2

-- | Encode DeleteGroupsResponse with the given API version.
encodeDeleteGroupsResponse :: MonadPut m => E.ApiVersion -> DeleteGroupsResponse -> m ()
encodeDeleteGroupsResponse version msg
  | version == 2 =
    do
      serialize (deleteGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeletableGroupResult (case P.unKafkaArray (deleteGroupsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (deleteGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeletableGroupResult (case P.unKafkaArray (deleteGroupsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteGroupsResponse with the given API version.
decodeDeleteGroupsResponse :: MonadGet m => E.ApiVersion -> m DeleteGroupsResponse
decodeDeleteGroupsResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeletableGroupResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteGroupsResponse
        {
        deleteGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteGroupsResponseResults = fieldresults
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeletableGroupResult
      pure DeleteGroupsResponse
        {
        deleteGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteGroupsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version