{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteTopicsRequest
Description : Kafka DeleteTopicsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 20.



Valid versions: 1-6
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteTopicsRequest
  (
    DeleteTopicsRequest(..),
    DeleteTopicState(..),
    maxDeleteTopicsRequestVersion
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


-- | The name or topic ID of the topic.
data DeleteTopicState = DeleteTopicState
  {

  -- | The topic name.

  -- Versions: 6+
  deleteTopicStateName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 6+
  deleteTopicStateTopicId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


data DeleteTopicsRequest = DeleteTopicsRequest
  {

  -- | The name or topic ID of the topic.

  -- Versions: 6+
  deleteTopicsRequestTopics :: !(KafkaArray (DeleteTopicState))
,

  -- | The names of the topics to delete.

  -- Versions: 0-5
  deleteTopicsRequestTopicNames :: !(KafkaArray (KafkaString))
,

  -- | The length of time in milliseconds to wait for the deletions to complete.

  -- Versions: 0+
  deleteTopicsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteTopicsRequest.
maxDeleteTopicsRequestVersion :: Int16
maxDeleteTopicsRequestVersion = 6

-- | KafkaMessage instance for DeleteTopicsRequest.
instance KafkaMessage DeleteTopicsRequest where
  messageApiKey = 20
  messageMinVersion = 1
  messageMaxVersion = 6
  messageFlexibleVersion = Just 4

-- | Worst-case wire size of a DeleteTopicState.
wireMaxSizeDeleteTopicState :: Int -> DeleteTopicState -> Int
wireMaxSizeDeleteTopicState _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteTopicStateName msg))
  + 16
  + 1

-- | Direct-poke encoder for DeleteTopicState.
wirePokeDeleteTopicState :: Int -> Ptr Word8 -> DeleteTopicState -> IO (Ptr Word8)
wirePokeDeleteTopicState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteTopicStateName msg))
  p2 <- WP.pokeKafkaUuid p1 (deleteTopicStateTopicId msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteTopicState.
wirePeekDeleteTopicState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteTopicState, Ptr Word8)
wirePeekDeleteTopicState version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteTopicState { deleteTopicStateName = f0_name, deleteTopicStateTopicId = f1_topicid }, pTagsEnd)

-- | Worst-case wire size of a DeleteTopicsRequest.
wireMaxSizeDeleteTopicsRequest :: Int -> DeleteTopicsRequest -> Int
wireMaxSizeDeleteTopicsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (deleteTopicsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteTopicState _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (deleteTopicsRequestTopicNames msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DeleteTopicsRequest.
wirePokeDeleteTopicsRequest :: Int -> Ptr Word8 -> DeleteTopicsRequest -> IO (Ptr Word8)
wirePokeDeleteTopicsRequest version basePtr msg
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDeleteTopicState version p x) p0 (deleteTopicsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (deleteTopicsRequestTimeoutMs msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p s -> if version >= 4 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (deleteTopicsRequestTopicNames msg)
    p2 <- W.pokeInt32BE p1 (deleteTopicsRequestTimeoutMs msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p s -> if version >= 4 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (deleteTopicsRequestTopicNames msg)
    p2 <- W.pokeInt32BE p1 (deleteTopicsRequestTimeoutMs msg)
    pure p2
  | otherwise = error $ "wirePoke DeleteTopicsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteTopicsRequest.
wirePeekDeleteTopicsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteTopicsRequest, Ptr Word8)
wirePeekDeleteTopicsRequest version _fp _basePtr p0 endPtr
  | version == 6 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDeleteTopicState version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteTopicsRequest { deleteTopicsRequestTopics = f0_topics, deleteTopicsRequestTopicNames = P.mkKafkaArray V.empty, deleteTopicsRequestTimeoutMs = f1_timeoutms }, pTagsEnd)
  | version >= 4 && version <= 5 = do
    (f0_topicnames, p1) <- WP.peekVersionedArray version 4 (\p e -> if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteTopicsRequest { deleteTopicsRequestTopics = P.mkKafkaArray V.empty, deleteTopicsRequestTopicNames = f0_topicnames, deleteTopicsRequestTimeoutMs = f1_timeoutms }, pTagsEnd)
  | version >= 1 && version <= 3 = do
    (f0_topicnames, p1) <- WP.peekVersionedArray version 4 (\p e -> if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pure (DeleteTopicsRequest { deleteTopicsRequestTopics = P.mkKafkaArray V.empty, deleteTopicsRequestTopicNames = f0_topicnames, deleteTopicsRequestTimeoutMs = f1_timeoutms }, p2)
  | otherwise = error $ "wirePeek DeleteTopicsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteTopicsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteTopicsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteTopicsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteTopicsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}