{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse
Description : Kafka AlterPartitionReassignmentsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 45.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse
  (
    AlterPartitionReassignmentsResponse(..),
    ReassignableTopicResponse(..),
    ReassignablePartitionResponse(..),
    encodeAlterPartitionReassignmentsResponse,
    decodeAlterPartitionReassignmentsResponse,
    maxAlterPartitionReassignmentsResponseVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The responses to partitions to reassign.
data ReassignablePartitionResponse = ReassignablePartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  reassignablePartitionResponsePartitionIndex :: !(Int32)
,

  -- | The error code for this partition, or 0 if there was no error.

  -- Versions: 0+
  reassignablePartitionResponseErrorCode :: !(Int16)
,

  -- | The error message for this partition, or null if there was no error.

  -- Versions: 0+
  reassignablePartitionResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignablePartitionResponse with version-aware field handling.
encodeReassignablePartitionResponse :: MonadPut m => E.ApiVersion -> ReassignablePartitionResponse -> m ()
encodeReassignablePartitionResponse version rmsg =
  do
    serialize (reassignablePartitionResponsePartitionIndex rmsg)
    serialize (reassignablePartitionResponseErrorCode rmsg)
    if version >= 0 then serialize (toCompactString (reassignablePartitionResponseErrorMessage rmsg)) else serialize (reassignablePartitionResponseErrorMessage rmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignablePartitionResponse with version-aware field handling.
decodeReassignablePartitionResponse :: MonadGet m => E.ApiVersion -> m ReassignablePartitionResponse
decodeReassignablePartitionResponse version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignablePartitionResponse
      {
      reassignablePartitionResponsePartitionIndex = fieldpartitionindex
      ,
      reassignablePartitionResponseErrorCode = fielderrorcode
      ,
      reassignablePartitionResponseErrorMessage = fielderrormessage
      }


-- | The responses to topics to reassign.
data ReassignableTopicResponse = ReassignableTopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  reassignableTopicResponseName :: !(KafkaString)
,

  -- | The responses to partitions to reassign.

  -- Versions: 0+
  reassignableTopicResponsePartitions :: !(KafkaArray (ReassignablePartitionResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignableTopicResponse with version-aware field handling.
encodeReassignableTopicResponse :: MonadPut m => E.ApiVersion -> ReassignableTopicResponse -> m ()
encodeReassignableTopicResponse version rmsg =
  do
    if version >= 0 then serialize (toCompactString (reassignableTopicResponseName rmsg)) else serialize (reassignableTopicResponseName rmsg)
    E.encodeVersionedArray version 0 encodeReassignablePartitionResponse (case P.unKafkaArray (reassignableTopicResponsePartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignableTopicResponse with version-aware field handling.
decodeReassignableTopicResponse :: MonadGet m => E.ApiVersion -> m ReassignableTopicResponse
decodeReassignableTopicResponse version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignablePartitionResponse
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignableTopicResponse
      {
      reassignableTopicResponseName = fieldname
      ,
      reassignableTopicResponsePartitions = fieldpartitions
      }



data AlterPartitionReassignmentsResponse = AlterPartitionReassignmentsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterPartitionReassignmentsResponseThrottleTimeMs :: !(Int32)
,

  -- | The option indicating whether changing the replication factor of any given partition as part of the 

  -- Versions: 1+
  alterPartitionReassignmentsResponseAllowReplicationFactorChange :: !(Bool)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  alterPartitionReassignmentsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  alterPartitionReassignmentsResponseErrorMessage :: !(KafkaString)
,

  -- | The responses to topics to reassign.

  -- Versions: 0+
  alterPartitionReassignmentsResponseResponses :: !(KafkaArray (ReassignableTopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionReassignmentsResponse.
maxAlterPartitionReassignmentsResponseVersion :: Int16
maxAlterPartitionReassignmentsResponseVersion = 1

-- | KafkaMessage instance for AlterPartitionReassignmentsResponse.
instance KafkaMessage AlterPartitionReassignmentsResponse where
  messageApiKey = 45
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode AlterPartitionReassignmentsResponse with the given API version.
encodeAlterPartitionReassignmentsResponse :: MonadPut m => E.ApiVersion -> AlterPartitionReassignmentsResponse -> m ()
encodeAlterPartitionReassignmentsResponse version msg
  | version == 0 =
    do
      serialize (alterPartitionReassignmentsResponseThrottleTimeMs msg)
      serialize (alterPartitionReassignmentsResponseErrorCode msg)
      serialize (toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeReassignableTopicResponse (case P.unKafkaArray (alterPartitionReassignmentsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (alterPartitionReassignmentsResponseThrottleTimeMs msg)
      serialize (alterPartitionReassignmentsResponseAllowReplicationFactorChange msg)
      serialize (alterPartitionReassignmentsResponseErrorCode msg)
      serialize (toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeReassignableTopicResponse (case P.unKafkaArray (alterPartitionReassignmentsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionReassignmentsResponse with the given API version.
decodeAlterPartitionReassignmentsResponse :: MonadGet m => E.ApiVersion -> m AlterPartitionReassignmentsResponse
decodeAlterPartitionReassignmentsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsResponse
        {
        alterPartitionReassignmentsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterPartitionReassignmentsResponseAllowReplicationFactorChange = True
        ,
        alterPartitionReassignmentsResponseErrorCode = fielderrorcode
        ,
        alterPartitionReassignmentsResponseErrorMessage = fielderrormessage
        ,
        alterPartitionReassignmentsResponseResponses = fieldresponses
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldallowreplicationfactorchange <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsResponse
        {
        alterPartitionReassignmentsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterPartitionReassignmentsResponseAllowReplicationFactorChange = fieldallowreplicationfactorchange
        ,
        alterPartitionReassignmentsResponseErrorCode = fielderrorcode
        ,
        alterPartitionReassignmentsResponseErrorMessage = fielderrormessage
        ,
        alterPartitionReassignmentsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ReassignablePartitionResponse.
wireMaxSizeReassignablePartitionResponse :: Int -> ReassignablePartitionResponse -> Int
wireMaxSizeReassignablePartitionResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (reassignablePartitionResponseErrorMessage msg))
  + 1

-- | Direct-poke encoder for ReassignablePartitionResponse.
wirePokeReassignablePartitionResponse :: Int -> Ptr Word8 -> ReassignablePartitionResponse -> IO (Ptr Word8)
wirePokeReassignablePartitionResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (reassignablePartitionResponsePartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (reassignablePartitionResponseErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (reassignablePartitionResponseErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ReassignablePartitionResponse.
wirePeekReassignablePartitionResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignablePartitionResponse, Ptr Word8)
wirePeekReassignablePartitionResponse version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ReassignablePartitionResponse { reassignablePartitionResponsePartitionIndex = f0_partitionindex, reassignablePartitionResponseErrorCode = f1_errorcode, reassignablePartitionResponseErrorMessage = f2_errormessage }, pTagsEnd)

-- | Worst-case wire size of a ReassignableTopicResponse.
wireMaxSizeReassignableTopicResponse :: Int -> ReassignableTopicResponse -> Int
wireMaxSizeReassignableTopicResponse _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (reassignableTopicResponseName msg))
  + (5 + (case P.unKafkaArray (reassignableTopicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignablePartitionResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReassignableTopicResponse.
wirePokeReassignableTopicResponse :: Int -> Ptr Word8 -> ReassignableTopicResponse -> IO (Ptr Word8)
wirePokeReassignableTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (reassignableTopicResponseName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignablePartitionResponse version p x) p1 (reassignableTopicResponsePartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReassignableTopicResponse.
wirePeekReassignableTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReassignableTopicResponse, Ptr Word8)
wirePeekReassignableTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignablePartitionResponse version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReassignableTopicResponse { reassignableTopicResponseName = f0_name, reassignableTopicResponsePartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a AlterPartitionReassignmentsResponse.
wireMaxSizeAlterPartitionReassignmentsResponse :: Int -> AlterPartitionReassignmentsResponse -> Int
wireMaxSizeAlterPartitionReassignmentsResponse _version msg =
  0
  + 4
  + 1
  + 2
  + WP.compactStringMaxSize (P.toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (alterPartitionReassignmentsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReassignableTopicResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterPartitionReassignmentsResponse.
wirePokeAlterPartitionReassignmentsResponse :: Int -> Ptr Word8 -> AlterPartitionReassignmentsResponse -> IO (Ptr Word8)
wirePokeAlterPartitionReassignmentsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (alterPartitionReassignmentsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopicResponse version p x) p3 (alterPartitionReassignmentsResponseResponses msg)
    WP.pokeEmptyTaggedFields p4
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionReassignmentsResponseThrottleTimeMs msg)
    p2 <- W.pokeWord8 p1 (if (alterPartitionReassignmentsResponseAllowReplicationFactorChange msg) then 1 else 0)
    p3 <- W.pokeInt16BE p2 (alterPartitionReassignmentsResponseErrorCode msg)
    p4 <- WP.pokeCompactString p3 (P.toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReassignableTopicResponse version p x) p4 (alterPartitionReassignmentsResponseResponses msg)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke AlterPartitionReassignmentsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterPartitionReassignmentsResponse.
wirePeekAlterPartitionReassignmentsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterPartitionReassignmentsResponse, Ptr Word8)
wirePeekAlterPartitionReassignmentsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopicResponse version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AlterPartitionReassignmentsResponse { alterPartitionReassignmentsResponseThrottleTimeMs = f0_throttletimems, alterPartitionReassignmentsResponseAllowReplicationFactorChange = False, alterPartitionReassignmentsResponseErrorCode = f1_errorcode, alterPartitionReassignmentsResponseErrorMessage = f2_errormessage, alterPartitionReassignmentsResponseResponses = f3_responses }, pTagsEnd)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_allowreplicationfactorchange, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
    (f3_errormessage, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_responses, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReassignableTopicResponse version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (AlterPartitionReassignmentsResponse { alterPartitionReassignmentsResponseThrottleTimeMs = f0_throttletimems, alterPartitionReassignmentsResponseAllowReplicationFactorChange = f1_allowreplicationfactorchange, alterPartitionReassignmentsResponseErrorCode = f2_errorcode, alterPartitionReassignmentsResponseErrorMessage = f3_errormessage, alterPartitionReassignmentsResponseResponses = f4_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterPartitionReassignmentsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterPartitionReassignmentsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterPartitionReassignmentsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterPartitionReassignmentsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterPartitionReassignmentsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}