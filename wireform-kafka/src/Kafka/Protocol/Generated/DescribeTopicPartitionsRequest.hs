{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTopicPartitionsRequest
Description : Kafka DescribeTopicPartitionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 75.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTopicPartitionsRequest
  (
    DescribeTopicPartitionsRequest(..),
    TopicRequest(..),
    Cursor(..),
    maxDescribeTopicPartitionsRequestVersion
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


-- | The topics to fetch details for.
data TopicRequest = TopicRequest
  {

  -- | The topic name.

  -- Versions: 0+
  topicRequestName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The first topic and partition index to fetch details for.
data Cursor = Cursor
  {

  -- | The name for the first topic to process.

  -- Versions: 0+
  cursorTopicName :: !(KafkaString)
,

  -- | The partition index to start with.

  -- Versions: 0+
  cursorPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


data DescribeTopicPartitionsRequest = DescribeTopicPartitionsRequest
  {

  -- | The topics to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsRequestTopics :: !(KafkaArray (TopicRequest))
,

  -- | The maximum number of partitions included in the response.

  -- Versions: 0+
  describeTopicPartitionsRequestResponsePartitionLimit :: !(Int32)
,

  -- | The first topic and partition index to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsRequestCursor :: !(Nullable (Cursor))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTopicPartitionsRequest.
maxDescribeTopicPartitionsRequestVersion :: Int16
maxDescribeTopicPartitionsRequestVersion = 0

-- | KafkaMessage instance for DescribeTopicPartitionsRequest.
instance KafkaMessage DescribeTopicPartitionsRequest where
  messageApiKey = 75
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a TopicRequest.
wireMaxSizeTopicRequest :: Int -> TopicRequest -> Int
wireMaxSizeTopicRequest _version msg =
  0
  + WP.dualStringMaxSize (topicRequestName msg)
  + 1

-- | Direct-poke encoder for TopicRequest.
wirePokeTopicRequest :: Int -> Ptr Word8 -> TopicRequest -> IO (Ptr Word8)
wirePokeTopicRequest version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (topicRequestName msg)) else WP.pokeKafkaString p0 (topicRequestName msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for TopicRequest.
wirePeekTopicRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicRequest, Ptr Word8)
wirePeekTopicRequest version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (TopicRequest { topicRequestName = f0_name }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicRequest :: TopicRequest
defaultTopicRequest = TopicRequest { topicRequestName = P.KafkaString Null }

-- | Worst-case wire size of a Cursor.
wireMaxSizeCursor :: Int -> Cursor -> Int
wireMaxSizeCursor _version msg =
  0
  + WP.dualStringMaxSize (cursorTopicName msg)
  + 4
  + 1

-- | Direct-poke encoder for Cursor.
wirePokeCursor :: Int -> Ptr Word8 -> Cursor -> IO (Ptr Word8)
wirePokeCursor version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (cursorTopicName msg)) else WP.pokeKafkaString p0 (cursorTopicName msg))
  p2 <- W.pokeInt32BE p1 (cursorPartitionIndex msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Cursor.
wirePeekCursor :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Cursor, Ptr Word8)
wirePeekCursor version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitionindex, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Cursor { cursorTopicName = f0_topicname, cursorPartitionIndex = f1_partitionindex }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCursor :: Cursor
defaultCursor = Cursor { cursorTopicName = P.KafkaString Null, cursorPartitionIndex = 0 }

-- | Worst-case wire size of a DescribeTopicPartitionsRequest.
wireMaxSizeDescribeTopicPartitionsRequest :: Int -> DescribeTopicPartitionsRequest -> Int
wireMaxSizeDescribeTopicPartitionsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeTopicPartitionsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicRequest _version x ) v); P.Null -> 0 }))
  + 4
  + (case (describeTopicPartitionsRequestCursor msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeCursor _version s })
  + 1

-- | Direct-poke encoder for DescribeTopicPartitionsRequest.
wirePokeDescribeTopicPartitionsRequest :: Int -> Ptr Word8 -> DescribeTopicPartitionsRequest -> IO (Ptr Word8)
wirePokeDescribeTopicPartitionsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicRequest version p x) p0 (describeTopicPartitionsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (describeTopicPartitionsRequestResponsePartitionLimit msg)
    p3 <- (case (describeTopicPartitionsRequestCursor msg) of { P.Null -> W.pokeWord8 p2 0; P.NotNull s -> W.pokeWord8 p2 1 >>= \p' -> wirePokeCursor version p' s })
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DescribeTopicPartitionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeTopicPartitionsRequest.
wirePeekDescribeTopicPartitionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTopicPartitionsRequest, Ptr Word8)
wirePeekDescribeTopicPartitionsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicRequest version _fp _basePtr p e) p0 endPtr
    (f1_responsepartitionlimit, p2) <- W.peekInt32BE p1 endPtr
    (f2_cursor, p3) <- (do { (flag, pAfterFlag) <- W.peekWord8 p2 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekCursor version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeTopicPartitionsRequest { describeTopicPartitionsRequestTopics = f0_topics, describeTopicPartitionsRequestResponsePartitionLimit = f1_responsepartitionlimit, describeTopicPartitionsRequestCursor = f2_cursor }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeTopicPartitionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DescribeTopicPartitionsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeTopicPartitionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeTopicPartitionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeTopicPartitionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}