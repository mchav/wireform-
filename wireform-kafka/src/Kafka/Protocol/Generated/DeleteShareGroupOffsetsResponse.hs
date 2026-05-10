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
    maxDeleteShareGroupOffsetsResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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

-- | Worst-case wire size of a DeleteShareGroupOffsetsResponseTopic.
wireMaxSizeDeleteShareGroupOffsetsResponseTopic :: Int -> DeleteShareGroupOffsetsResponseTopic -> Int
wireMaxSizeDeleteShareGroupOffsetsResponseTopic _version msg =
  0
  + WP.dualStringMaxSize (deleteShareGroupOffsetsResponseTopicTopicName msg)
  + 16
  + 2
  + WP.dualStringMaxSize (deleteShareGroupOffsetsResponseTopicErrorMessage msg)
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsResponseTopic.
wirePokeDeleteShareGroupOffsetsResponseTopic :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsResponseTopic -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (deleteShareGroupOffsetsResponseTopicTopicName msg)) else WP.pokeKafkaString p0 (deleteShareGroupOffsetsResponseTopicTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (deleteShareGroupOffsetsResponseTopicTopicId msg)
  p3 <- W.pokeInt16BE p2 (deleteShareGroupOffsetsResponseTopicErrorCode msg)
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (deleteShareGroupOffsetsResponseTopicErrorMessage msg)) else WP.pokeKafkaString p3 (deleteShareGroupOffsetsResponseTopicErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DeleteShareGroupOffsetsResponseTopic.
wirePeekDeleteShareGroupOffsetsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsResponseTopic, Ptr Word8)
wirePeekDeleteShareGroupOffsetsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DeleteShareGroupOffsetsResponseTopic { deleteShareGroupOffsetsResponseTopicTopicName = f0_topicname, deleteShareGroupOffsetsResponseTopicTopicId = f1_topicid, deleteShareGroupOffsetsResponseTopicErrorCode = f2_errorcode, deleteShareGroupOffsetsResponseTopicErrorMessage = f3_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeleteShareGroupOffsetsResponseTopic :: DeleteShareGroupOffsetsResponseTopic
defaultDeleteShareGroupOffsetsResponseTopic = DeleteShareGroupOffsetsResponseTopic { deleteShareGroupOffsetsResponseTopicTopicName = P.KafkaString Null, deleteShareGroupOffsetsResponseTopicTopicId = P.nullUuid, deleteShareGroupOffsetsResponseTopicErrorCode = 0, deleteShareGroupOffsetsResponseTopicErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a DeleteShareGroupOffsetsResponse.
wireMaxSizeDeleteShareGroupOffsetsResponse :: Int -> DeleteShareGroupOffsetsResponse -> Int
wireMaxSizeDeleteShareGroupOffsetsResponse _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (deleteShareGroupOffsetsResponseErrorMessage msg)
  + (5 + (case P.unKafkaArray (deleteShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteShareGroupOffsetsResponseTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsResponse.
wirePokeDeleteShareGroupOffsetsResponse :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsResponse -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteShareGroupOffsetsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (deleteShareGroupOffsetsResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (deleteShareGroupOffsetsResponseErrorMessage msg)) else WP.pokeKafkaString p2 (deleteShareGroupOffsetsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDeleteShareGroupOffsetsResponseTopic version p x) p3 (deleteShareGroupOffsetsResponseResponses msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke DeleteShareGroupOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteShareGroupOffsetsResponse.
wirePeekDeleteShareGroupOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsResponse, Ptr Word8)
wirePeekDeleteShareGroupOffsetsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDeleteShareGroupOffsetsResponseTopic version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DeleteShareGroupOffsetsResponse { deleteShareGroupOffsetsResponseThrottleTimeMs = f0_throttletimems, deleteShareGroupOffsetsResponseErrorCode = f1_errorcode, deleteShareGroupOffsetsResponseErrorMessage = f2_errormessage, deleteShareGroupOffsetsResponseResponses = f3_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteShareGroupOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteShareGroupOffsetsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteShareGroupOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteShareGroupOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteShareGroupOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}