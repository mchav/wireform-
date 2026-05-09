{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupOffsetsResponse
Description : Kafka DeleteShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 92.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupOffsetsResponse
  (
    DeleteShareGroupOffsetsResponse(..),
    DeleteShareGroupOffsetsResponseTopic(..),
    encodeDeleteShareGroupOffsetsResponse,
    decodeDeleteShareGroupOffsetsResponse,
    maxDeleteShareGroupOffsetsResponseVersion
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


-- | The results for each topic.
data DeleteShareGroupOffsetsResponseTopic = DeleteShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,

  -- | The topic-level error code, or 0 if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicErrorCode :: !(Int16)
,

  -- | The topic-level error message, or null if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseTopicErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteShareGroupOffsetsResponseTopic with version-aware field handling.
encodeDeleteShareGroupOffsetsResponseTopic :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsResponseTopic -> m ()
encodeDeleteShareGroupOffsetsResponseTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsResponseTopicTopicName dmsg)) else serialize (deleteShareGroupOffsetsResponseTopicTopicName dmsg)
    serialize (deleteShareGroupOffsetsResponseTopicTopicId dmsg)
    serialize (deleteShareGroupOffsetsResponseTopicErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsResponseTopicErrorMessage dmsg)) else serialize (deleteShareGroupOffsetsResponseTopicErrorMessage dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteShareGroupOffsetsResponseTopic with version-aware field handling.
decodeDeleteShareGroupOffsetsResponseTopic :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsResponseTopic
decodeDeleteShareGroupOffsetsResponseTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteShareGroupOffsetsResponseTopic
      {
      deleteShareGroupOffsetsResponseTopicTopicName = fieldtopicname
      ,
      deleteShareGroupOffsetsResponseTopicTopicId = fieldtopicid
      ,
      deleteShareGroupOffsetsResponseTopicErrorCode = fielderrorcode
      ,
      deleteShareGroupOffsetsResponseTopicErrorMessage = fielderrormessage
      }



data DeleteShareGroupOffsetsResponse = DeleteShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseErrorMessage :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  deleteShareGroupOffsetsResponseResponses :: !(KafkaArray (DeleteShareGroupOffsetsResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupOffsetsResponse.
maxDeleteShareGroupOffsetsResponseVersion :: Int16
maxDeleteShareGroupOffsetsResponseVersion = 0

-- | KafkaMessage instance for DeleteShareGroupOffsetsResponse.
instance KafkaMessage DeleteShareGroupOffsetsResponse where
  messageApiKey = 92
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode DeleteShareGroupOffsetsResponse with the given API version.
encodeDeleteShareGroupOffsetsResponse :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsResponse -> m ()
encodeDeleteShareGroupOffsetsResponse version msg
  | version == 0 =
    do
      serialize (deleteShareGroupOffsetsResponseThrottleTimeMs msg)
      serialize (deleteShareGroupOffsetsResponseErrorCode msg)
      serialize (toCompactString (deleteShareGroupOffsetsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeDeleteShareGroupOffsetsResponseTopic (case P.unKafkaArray (deleteShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupOffsetsResponse with the given API version.
decodeDeleteShareGroupOffsetsResponse :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsResponse
decodeDeleteShareGroupOffsetsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteShareGroupOffsetsResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupOffsetsResponse
        {
        deleteShareGroupOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteShareGroupOffsetsResponseErrorCode = fielderrorcode
        ,
        deleteShareGroupOffsetsResponseErrorMessage = fielderrormessage
        ,
        deleteShareGroupOffsetsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DeleteShareGroupOffsetsResponseTopic.
wireMaxSizeDeleteShareGroupOffsetsResponseTopic :: Int -> DeleteShareGroupOffsetsResponseTopic -> Int
wireMaxSizeDeleteShareGroupOffsetsResponseTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupOffsetsResponseTopicTopicName msg))
  + 16
  + 2
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupOffsetsResponseTopicErrorMessage msg))
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsResponseTopic.
wirePokeDeleteShareGroupOffsetsResponseTopic :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsResponseTopic -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteShareGroupOffsetsResponseTopicTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (deleteShareGroupOffsetsResponseTopicTopicId msg)
  p3 <- W.pokeInt16BE p2 (deleteShareGroupOffsetsResponseTopicErrorCode msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (deleteShareGroupOffsetsResponseTopicErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DeleteShareGroupOffsetsResponseTopic.
wirePeekDeleteShareGroupOffsetsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsResponseTopic, Ptr Word8)
wirePeekDeleteShareGroupOffsetsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DeleteShareGroupOffsetsResponseTopic { deleteShareGroupOffsetsResponseTopicTopicName = f0_topicname, deleteShareGroupOffsetsResponseTopicTopicId = f1_topicid, deleteShareGroupOffsetsResponseTopicErrorCode = f2_errorcode, deleteShareGroupOffsetsResponseTopicErrorMessage = f3_errormessage }, pTagsEnd)

-- | Worst-case wire size of a DeleteShareGroupOffsetsResponse.
wireMaxSizeDeleteShareGroupOffsetsResponse :: Int -> DeleteShareGroupOffsetsResponse -> Int
wireMaxSizeDeleteShareGroupOffsetsResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupOffsetsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (deleteShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteShareGroupOffsetsResponseTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsResponse.
wirePokeDeleteShareGroupOffsetsResponse :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsResponse -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteShareGroupOffsetsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (deleteShareGroupOffsetsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (deleteShareGroupOffsetsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDeleteShareGroupOffsetsResponseTopic version p x) p3 (deleteShareGroupOffsetsResponseResponses msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke DeleteShareGroupOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteShareGroupOffsetsResponse.
wirePeekDeleteShareGroupOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsResponse, Ptr Word8)
wirePeekDeleteShareGroupOffsetsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDeleteShareGroupOffsetsResponseTopic version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DeleteShareGroupOffsetsResponse { deleteShareGroupOffsetsResponseThrottleTimeMs = f0_throttletimems, deleteShareGroupOffsetsResponseErrorCode = f1_errorcode, deleteShareGroupOffsetsResponseErrorMessage = f2_errormessage, deleteShareGroupOffsetsResponseResponses = f3_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteShareGroupOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DeleteShareGroupOffsetsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteShareGroupOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteShareGroupOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteShareGroupOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}