{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitializeShareGroupStateResponse
Description : Kafka InitializeShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 83.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitializeShareGroupStateResponse
  (
    InitializeShareGroupStateResponse(..),
    InitializeStateResult(..),
    PartitionResult(..),
    encodeInitializeShareGroupStateResponse,
    decodeInitializeShareGroupStateResponse,
    maxInitializeShareGroupStateResponseVersion
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


-- | The results for the partitions.
data PartitionResult = PartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResultPartition :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartition pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartition <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartition = fieldpartition
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      }


-- | The initialization results.
data InitializeStateResult = InitializeStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  initializeStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  initializeStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode InitializeStateResult with version-aware field handling.
encodeInitializeStateResult :: MonadPut m => E.ApiVersion -> InitializeStateResult -> m ()
encodeInitializeStateResult version imsg =
  do
    serialize (initializeStateResultTopicId imsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (initializeStateResultPartitions imsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode InitializeStateResult with version-aware field handling.
decodeInitializeStateResult :: MonadGet m => E.ApiVersion -> m InitializeStateResult
decodeInitializeStateResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure InitializeStateResult
      {
      initializeStateResultTopicId = fieldtopicid
      ,
      initializeStateResultPartitions = fieldpartitions
      }



data InitializeShareGroupStateResponse = InitializeShareGroupStateResponse
  {

  -- | The initialization results.

  -- Versions: 0+
  initializeShareGroupStateResponseResults :: !(KafkaArray (InitializeStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitializeShareGroupStateResponse.
maxInitializeShareGroupStateResponseVersion :: Int16
maxInitializeShareGroupStateResponseVersion = 0

-- | Encode InitializeShareGroupStateResponse with the given API version.
encodeInitializeShareGroupStateResponse :: MonadPut m => E.ApiVersion -> InitializeShareGroupStateResponse -> m ()
encodeInitializeShareGroupStateResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeInitializeStateResult (case P.unKafkaArray (initializeShareGroupStateResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode InitializeShareGroupStateResponse with the given API version.
decodeInitializeShareGroupStateResponse :: MonadGet m => E.ApiVersion -> m InitializeShareGroupStateResponse
decodeInitializeShareGroupStateResponse version
  | version == 0 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeInitializeStateResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitializeShareGroupStateResponse
        {
        initializeShareGroupStateResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec InitializeShareGroupStateResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
