{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ElectLeadersResponse
Description : Kafka ElectLeadersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 43.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ElectLeadersResponse
  (
    ElectLeadersResponse(..),
    ReplicaElectionResult(..),
    PartitionResult(..),
    encodeElectLeadersResponse,
    decodeElectLeadersResponse,
    maxElectLeadersResponseVersion
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


-- | The results for each partition.
data PartitionResult = PartitionResult
  {

  -- | The partition id.

  -- Versions: 0+
  partitionResultPartitionId :: !(Int32)
,

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartitionId pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 2 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartitionid <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartitionId = fieldpartitionid
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      }


-- | The election results, or an empty array if the requester did not have permission and the request asks for all partitions.
data ReplicaElectionResult = ReplicaElectionResult
  {

  -- | The topic name.

  -- Versions: 0+
  replicaElectionResultTopic :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  replicaElectionResultPartitionResult :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReplicaElectionResult with version-aware field handling.
encodeReplicaElectionResult :: MonadPut m => E.ApiVersion -> ReplicaElectionResult -> m ()
encodeReplicaElectionResult version rmsg =
  do
    if version >= 2 then serialize (toCompactString (replicaElectionResultTopic rmsg)) else serialize (replicaElectionResultTopic rmsg)
    E.encodeVersionedArray version 2 encodePartitionResult (case P.unKafkaArray (replicaElectionResultPartitionResult rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReplicaElectionResult with version-aware field handling.
decodeReplicaElectionResult :: MonadGet m => E.ApiVersion -> m ReplicaElectionResult
decodeReplicaElectionResult version =
  do
    fieldtopic <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionresult <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodePartitionResult
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReplicaElectionResult
      {
      replicaElectionResultTopic = fieldtopic
      ,
      replicaElectionResultPartitionResult = fieldpartitionresult
      }



data ElectLeadersResponse = ElectLeadersResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  electLeadersResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 1+
  electLeadersResponseErrorCode :: !(Int16)
,

  -- | The election results, or an empty array if the requester did not have permission and the request ask

  -- Versions: 0+
  electLeadersResponseReplicaElectionResults :: !(KafkaArray (ReplicaElectionResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ElectLeadersResponse.
maxElectLeadersResponseVersion :: Int16
maxElectLeadersResponseVersion = 2

-- | Encode ElectLeadersResponse with the given API version.
encodeElectLeadersResponse :: MonadPut m => E.ApiVersion -> ElectLeadersResponse -> m ()
encodeElectLeadersResponse version msg
  | version == 0 =
    do
      serialize (electLeadersResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeReplicaElectionResult (case P.unKafkaArray (electLeadersResponseReplicaElectionResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (electLeadersResponseThrottleTimeMs msg)
      serialize (electLeadersResponseErrorCode msg)
      E.encodeVersionedArray version 2 encodeReplicaElectionResult (case P.unKafkaArray (electLeadersResponseReplicaElectionResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      serialize (electLeadersResponseThrottleTimeMs msg)
      serialize (electLeadersResponseErrorCode msg)
      E.encodeVersionedArray version 2 encodeReplicaElectionResult (case P.unKafkaArray (electLeadersResponseReplicaElectionResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ElectLeadersResponse with the given API version.
decodeElectLeadersResponse :: MonadGet m => E.ApiVersion -> m ElectLeadersResponse
decodeElectLeadersResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldreplicaelectionresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeReplicaElectionResult
      pure ElectLeadersResponse
        {
        electLeadersResponseThrottleTimeMs = fieldthrottletimems
        ,
        electLeadersResponseErrorCode = 0
        ,
        electLeadersResponseReplicaElectionResults = fieldreplicaelectionresults
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldreplicaelectionresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeReplicaElectionResult
      pure ElectLeadersResponse
        {
        electLeadersResponseThrottleTimeMs = fieldthrottletimems
        ,
        electLeadersResponseErrorCode = fielderrorcode
        ,
        electLeadersResponseReplicaElectionResults = fieldreplicaelectionresults
        }

  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldreplicaelectionresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeReplicaElectionResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ElectLeadersResponse
        {
        electLeadersResponseThrottleTimeMs = fieldthrottletimems
        ,
        electLeadersResponseErrorCode = fielderrorcode
        ,
        electLeadersResponseReplicaElectionResults = fieldreplicaelectionresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ElectLeadersResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
