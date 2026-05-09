{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
Description : Kafka ReadShareGroupStateSummaryResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 87.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
  (
    ReadShareGroupStateSummaryResponse(..),
    ReadStateSummaryResult(..),
    PartitionResult(..),
    encodeReadShareGroupStateSummaryResponse,
    decodeReadShareGroupStateSummaryResponse,
    maxReadShareGroupStateSummaryResponseVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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
,

  -- | The state epoch of the share-partition.

  -- Versions: 0+
  partitionResultStateEpoch :: !(Int32)
,

  -- | The leader epoch of the share-partition.

  -- Versions: 0+
  partitionResultLeaderEpoch :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  partitionResultStartOffset :: !(Int64)
,

  -- | The number of offsets greater than or equal to share-partition start offset for which delivery has b

  -- Versions: 1+
  partitionResultDeliveryCompleteCount :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartition pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    serialize (partitionResultStateEpoch pmsg)
    serialize (partitionResultLeaderEpoch pmsg)
    serialize (partitionResultStartOffset pmsg)
    when (version >= 1) $
      serialize (partitionResultDeliveryCompleteCount pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartition <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldstateepoch <- deserialize
    fieldleaderepoch <- deserialize
    fieldstartoffset <- deserialize
    fielddeliverycompletecount <- if version >= 1
      then deserialize
      else pure ((-1))
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartition = fieldpartition
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      ,
      partitionResultStateEpoch = fieldstateepoch
      ,
      partitionResultLeaderEpoch = fieldleaderepoch
      ,
      partitionResultStartOffset = fieldstartoffset
      ,
      partitionResultDeliveryCompleteCount = fielddeliverycompletecount
      }


-- | The read results.
data ReadStateSummaryResult = ReadStateSummaryResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateSummaryResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  readStateSummaryResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateSummaryResult with version-aware field handling.
encodeReadStateSummaryResult :: MonadPut m => E.ApiVersion -> ReadStateSummaryResult -> m ()
encodeReadStateSummaryResult version rmsg =
  do
    serialize (readStateSummaryResultTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (readStateSummaryResultPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateSummaryResult with version-aware field handling.
decodeReadStateSummaryResult :: MonadGet m => E.ApiVersion -> m ReadStateSummaryResult
decodeReadStateSummaryResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateSummaryResult
      {
      readStateSummaryResultTopicId = fieldtopicid
      ,
      readStateSummaryResultPartitions = fieldpartitions
      }



data ReadShareGroupStateSummaryResponse = ReadShareGroupStateSummaryResponse
  {

  -- | The read results.

  -- Versions: 0+
  readShareGroupStateSummaryResponseResults :: !(KafkaArray (ReadStateSummaryResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateSummaryResponse.
maxReadShareGroupStateSummaryResponseVersion :: Int16
maxReadShareGroupStateSummaryResponseVersion = 1

-- | KafkaMessage instance for ReadShareGroupStateSummaryResponse.
instance KafkaMessage ReadShareGroupStateSummaryResponse where
  messageApiKey = 87
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode ReadShareGroupStateSummaryResponse with the given API version.
encodeReadShareGroupStateSummaryResponse :: MonadPut m => E.ApiVersion -> ReadShareGroupStateSummaryResponse -> m ()
encodeReadShareGroupStateSummaryResponse version msg
  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 0 encodeReadStateSummaryResult (case P.unKafkaArray (readShareGroupStateSummaryResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateSummaryResponse with the given API version.
decodeReadShareGroupStateSummaryResponse :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateSummaryResponse
decodeReadShareGroupStateSummaryResponse version
  | version >= 0 && version <= 1 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateSummaryResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateSummaryResponse
        {
        readShareGroupStateSummaryResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionResult.
wireMaxSizePartitionResult :: Int -> PartitionResult -> Int
wireMaxSizePartitionResult _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionResultErrorMessage msg))
  + 4
  + 4
  + 8
  + 4
  + 1

-- | Direct-poke encoder for PartitionResult.
wirePokePartitionResult :: Int -> Ptr Word8 -> PartitionResult -> IO (Ptr Word8)
wirePokePartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResultPartition msg)
  p2 <- W.pokeInt16BE p1 (partitionResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionResultErrorMessage msg))
  p4 <- W.pokeInt32BE p3 (partitionResultStateEpoch msg)
  p5 <- W.pokeInt32BE p4 (partitionResultLeaderEpoch msg)
  p6 <- W.pokeInt64BE p5 (partitionResultStartOffset msg)
  p7 <- W.pokeInt32BE p6 (partitionResultDeliveryCompleteCount msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for PartitionResult.
wirePeekPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResult, Ptr Word8)
wirePeekPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_stateepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_leaderepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_startoffset, p6) <- W.peekInt64BE p5 endPtr
  (f6_deliverycompletecount, p7) <- W.peekInt32BE p6 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (PartitionResult { partitionResultPartition = f0_partition, partitionResultErrorCode = f1_errorcode, partitionResultErrorMessage = f2_errormessage, partitionResultStateEpoch = f3_stateepoch, partitionResultLeaderEpoch = f4_leaderepoch, partitionResultStartOffset = f5_startoffset, partitionResultDeliveryCompleteCount = f6_deliverycompletecount }, pTagsEnd)

-- | Worst-case wire size of a ReadStateSummaryResult.
wireMaxSizeReadStateSummaryResult :: Int -> ReadStateSummaryResult -> Int
wireMaxSizeReadStateSummaryResult _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (readStateSummaryResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadStateSummaryResult.
wirePokeReadStateSummaryResult :: Int -> Ptr Word8 -> ReadStateSummaryResult -> IO (Ptr Word8)
wirePokeReadStateSummaryResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (readStateSummaryResultTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResult version p x) p1 (readStateSummaryResultPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReadStateSummaryResult.
wirePeekReadStateSummaryResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadStateSummaryResult, Ptr Word8)
wirePeekReadStateSummaryResult version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReadStateSummaryResult { readStateSummaryResultTopicId = f0_topicid, readStateSummaryResultPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ReadShareGroupStateSummaryResponse.
wireMaxSizeReadShareGroupStateSummaryResponse :: Int -> ReadShareGroupStateSummaryResponse -> Int
wireMaxSizeReadShareGroupStateSummaryResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (readShareGroupStateSummaryResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReadStateSummaryResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadShareGroupStateSummaryResponse.
wirePokeReadShareGroupStateSummaryResponse :: Int -> Ptr Word8 -> ReadShareGroupStateSummaryResponse -> IO (Ptr Word8)
wirePokeReadShareGroupStateSummaryResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReadStateSummaryResult version p x) p0 (readShareGroupStateSummaryResponseResults msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke ReadShareGroupStateSummaryResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ReadShareGroupStateSummaryResponse.
wirePeekReadShareGroupStateSummaryResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadShareGroupStateSummaryResponse, Ptr Word8)
wirePeekReadShareGroupStateSummaryResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_results, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReadStateSummaryResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ReadShareGroupStateSummaryResponse { readShareGroupStateSummaryResponseResults = f0_results }, pTagsEnd)
  | otherwise = error $ "wirePeek ReadShareGroupStateSummaryResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ReadShareGroupStateSummaryResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeReadShareGroupStateSummaryResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeReadShareGroupStateSummaryResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekReadShareGroupStateSummaryResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}