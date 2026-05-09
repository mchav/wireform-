{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddPartitionsToTxnResponse
Description : Kafka AddPartitionsToTxnResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 24.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddPartitionsToTxnResponse
  (
    AddPartitionsToTxnResponse(..),
    AddPartitionsToTxnResult(..),
    AddPartitionsToTxnTopicResult(..),
    AddPartitionsToTxnPartitionResult(..),
    encodeAddPartitionsToTxnResponse,
    decodeAddPartitionsToTxnResponse,
    maxAddPartitionsToTxnResponseVersion
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


data AddPartitionsToTxnTopicResult = AddPartitionsToTxnTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  addPartitionsToTxnTopicResultName :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  addPartitionsToTxnTopicResultResultsByPartition :: !(KafkaArray (AddPartitionsToTxnPartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode AddPartitionsToTxnTopicResult with version-aware field handling.
encodeAddPartitionsToTxnTopicResult :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnTopicResult -> m ()
encodeAddPartitionsToTxnTopicResult version amsg =
  do
    if version >= 3 then serialize (toCompactString (addPartitionsToTxnTopicResultName amsg)) else serialize (addPartitionsToTxnTopicResultName amsg)
    E.encodeVersionedArray version 3 encodeAddPartitionsToTxnPartitionResult (case P.unKafkaArray (addPartitionsToTxnTopicResultResultsByPartition amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AddPartitionsToTxnTopicResult with version-aware field handling.
decodeAddPartitionsToTxnTopicResult :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnTopicResult
decodeAddPartitionsToTxnTopicResult version =
  do
    fieldname <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldresultsbypartition <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnPartitionResult
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AddPartitionsToTxnTopicResult
      {
      addPartitionsToTxnTopicResultName = fieldname
      ,
      addPartitionsToTxnTopicResultResultsByPartition = fieldresultsbypartition
      }



data AddPartitionsToTxnPartitionResult = AddPartitionsToTxnPartitionResult
  {

  -- | The partition indexes.

  -- Versions: 0+
  addPartitionsToTxnPartitionResultPartitionIndex :: !(Int32)
,

  -- | The response error code.

  -- Versions: 0+
  addPartitionsToTxnPartitionResultPartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode AddPartitionsToTxnPartitionResult with version-aware field handling.
encodeAddPartitionsToTxnPartitionResult :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnPartitionResult -> m ()
encodeAddPartitionsToTxnPartitionResult version amsg =
  do
    serialize (addPartitionsToTxnPartitionResultPartitionIndex amsg)
    serialize (addPartitionsToTxnPartitionResultPartitionErrorCode amsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AddPartitionsToTxnPartitionResult with version-aware field handling.
decodeAddPartitionsToTxnPartitionResult :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnPartitionResult
decodeAddPartitionsToTxnPartitionResult version =
  do
    fieldpartitionindex <- deserialize
    fieldpartitionerrorcode <- deserialize
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AddPartitionsToTxnPartitionResult
      {
      addPartitionsToTxnPartitionResultPartitionIndex = fieldpartitionindex
      ,
      addPartitionsToTxnPartitionResultPartitionErrorCode = fieldpartitionerrorcode
      }


-- | Results categorized by transactional ID.
data AddPartitionsToTxnResult = AddPartitionsToTxnResult
  {

  -- | The transactional id corresponding to the transaction.

  -- Versions: 4+
  addPartitionsToTxnResultTransactionalId :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 4+
  addPartitionsToTxnResultTopicResults :: !(KafkaArray (AddPartitionsToTxnTopicResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode AddPartitionsToTxnResult with version-aware field handling.
encodeAddPartitionsToTxnResult :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnResult -> m ()
encodeAddPartitionsToTxnResult version amsg =
  do
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (addPartitionsToTxnResultTransactionalId amsg)) else serialize (addPartitionsToTxnResultTransactionalId amsg)
    when (version >= 4) $
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopicResult (case P.unKafkaArray (addPartitionsToTxnResultTopicResults amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AddPartitionsToTxnResult with version-aware field handling.
decodeAddPartitionsToTxnResult :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnResult
decodeAddPartitionsToTxnResult version =
  do
    fieldtransactionalid <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicresults <- if version >= 4
      then P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopicResult
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AddPartitionsToTxnResult
      {
      addPartitionsToTxnResultTransactionalId = fieldtransactionalid
      ,
      addPartitionsToTxnResultTopicResults = fieldtopicresults
      }



data AddPartitionsToTxnResponse = AddPartitionsToTxnResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  addPartitionsToTxnResponseThrottleTimeMs :: !(Int32)
,

  -- | The response top level error code.

  -- Versions: 4+
  addPartitionsToTxnResponseErrorCode :: !(Int16)
,

  -- | Results categorized by transactional ID.

  -- Versions: 4+
  addPartitionsToTxnResponseResultsByTransaction :: !(KafkaArray (AddPartitionsToTxnResult))
,

  -- | The results for each topic.

  -- Versions: 0-3
  addPartitionsToTxnResponseResultsByTopicV3AndBelow :: !(KafkaArray (AddPartitionsToTxnTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddPartitionsToTxnResponse.
maxAddPartitionsToTxnResponseVersion :: Int16
maxAddPartitionsToTxnResponseVersion = 5

-- | Encode AddPartitionsToTxnResponse with the given API version.
encodeAddPartitionsToTxnResponse :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnResponse -> m ()
encodeAddPartitionsToTxnResponse version msg
  | version == 3 =
    do
      serialize (addPartitionsToTxnResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopicResult (case P.unKafkaArray (addPartitionsToTxnResponseResultsByTopicV3AndBelow msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 4 && version <= 5 =
    do
      serialize (addPartitionsToTxnResponseThrottleTimeMs msg)
      serialize (addPartitionsToTxnResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnResult (case P.unKafkaArray (addPartitionsToTxnResponseResultsByTransaction msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (addPartitionsToTxnResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopicResult (case P.unKafkaArray (addPartitionsToTxnResponseResultsByTopicV3AndBelow msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddPartitionsToTxnResponse with the given API version.
decodeAddPartitionsToTxnResponse :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnResponse
decodeAddPartitionsToTxnResponse version
  | version == 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresultsbytopicv3andbelow <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddPartitionsToTxnResponse
        {
        addPartitionsToTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        addPartitionsToTxnResponseErrorCode = 0
        ,
        addPartitionsToTxnResponseResultsByTransaction = P.mkKafkaArray V.empty
        ,
        addPartitionsToTxnResponseResultsByTopicV3AndBelow = fieldresultsbytopicv3andbelow
        }

  | version >= 4 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldresultsbytransaction <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddPartitionsToTxnResponse
        {
        addPartitionsToTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        addPartitionsToTxnResponseErrorCode = fielderrorcode
        ,
        addPartitionsToTxnResponseResultsByTransaction = fieldresultsbytransaction
        ,
        addPartitionsToTxnResponseResultsByTopicV3AndBelow = P.mkKafkaArray V.empty
        }

  | version >= 0 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresultsbytopicv3andbelow <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopicResult
      pure AddPartitionsToTxnResponse
        {
        addPartitionsToTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        addPartitionsToTxnResponseErrorCode = 0
        ,
        addPartitionsToTxnResponseResultsByTransaction = P.mkKafkaArray V.empty
        ,
        addPartitionsToTxnResponseResultsByTopicV3AndBelow = fieldresultsbytopicv3andbelow
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAddPartitionsToTxnResponse' / 'decodeAddPartitionsToTxnResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AddPartitionsToTxnResponse where
  wireCodec = Just (WC.serialShimCodec encodeAddPartitionsToTxnResponse decodeAddPartitionsToTxnResponse)
  {-# INLINE wireCodec #-}
