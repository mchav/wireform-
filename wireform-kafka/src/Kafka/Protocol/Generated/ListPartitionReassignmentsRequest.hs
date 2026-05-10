{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListPartitionReassignmentsRequest
Description : Kafka ListPartitionReassignmentsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 46.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListPartitionReassignmentsRequest
  (
    ListPartitionReassignmentsRequest(..),
    ListPartitionReassignmentsTopics(..),
    maxListPartitionReassignmentsRequestVersion
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


-- | The topics to list partition reassignments for, or null to list everything.
data ListPartitionReassignmentsTopics = ListPartitionReassignmentsTopics
  {

  -- | The topic name.

  -- Versions: 0+
  listPartitionReassignmentsTopicsName :: !(KafkaString)
,

  -- | The partitions to list partition reassignments for.

  -- Versions: 0+
  listPartitionReassignmentsTopicsPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


data ListPartitionReassignmentsRequest = ListPartitionReassignmentsRequest
  {

  -- | The time in ms to wait for the request to complete.

  -- Versions: 0+
  listPartitionReassignmentsRequestTimeoutMs :: !(Int32)
,

  -- | The topics to list partition reassignments for, or null to list everything.

  -- Versions: 0+
  listPartitionReassignmentsRequestTopics :: !(KafkaArray (ListPartitionReassignmentsTopics))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListPartitionReassignmentsRequest.
maxListPartitionReassignmentsRequestVersion :: Int16
maxListPartitionReassignmentsRequestVersion = 0

-- | KafkaMessage instance for ListPartitionReassignmentsRequest.
instance KafkaMessage ListPartitionReassignmentsRequest where
  messageApiKey = 46
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a ListPartitionReassignmentsTopics.
wireMaxSizeListPartitionReassignmentsTopics :: Int -> ListPartitionReassignmentsTopics -> Int
wireMaxSizeListPartitionReassignmentsTopics _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listPartitionReassignmentsTopicsName msg))
  + (5 + (case P.unKafkaArray (listPartitionReassignmentsTopicsPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListPartitionReassignmentsTopics.
wirePokeListPartitionReassignmentsTopics :: Int -> Ptr Word8 -> ListPartitionReassignmentsTopics -> IO (Ptr Word8)
wirePokeListPartitionReassignmentsTopics version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (listPartitionReassignmentsTopicsName msg)) else WP.pokeKafkaString p0 (listPartitionReassignmentsTopicsName msg))
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (listPartitionReassignmentsTopicsPartitionIndexes msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ListPartitionReassignmentsTopics.
wirePeekListPartitionReassignmentsTopics :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListPartitionReassignmentsTopics, Ptr Word8)
wirePeekListPartitionReassignmentsTopics version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitionindexes, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ListPartitionReassignmentsTopics { listPartitionReassignmentsTopicsName = f0_name, listPartitionReassignmentsTopicsPartitionIndexes = f1_partitionindexes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListPartitionReassignmentsTopics :: ListPartitionReassignmentsTopics
defaultListPartitionReassignmentsTopics = ListPartitionReassignmentsTopics { listPartitionReassignmentsTopicsName = P.KafkaString Null, listPartitionReassignmentsTopicsPartitionIndexes = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ListPartitionReassignmentsRequest.
wireMaxSizeListPartitionReassignmentsRequest :: Int -> ListPartitionReassignmentsRequest -> Int
wireMaxSizeListPartitionReassignmentsRequest _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (listPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListPartitionReassignmentsTopics _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListPartitionReassignmentsRequest.
wirePokeListPartitionReassignmentsRequest :: Int -> Ptr Word8 -> ListPartitionReassignmentsRequest -> IO (Ptr Word8)
wirePokeListPartitionReassignmentsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listPartitionReassignmentsRequestTimeoutMs msg)
    p2 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeListPartitionReassignmentsTopics version p x) p1 (listPartitionReassignmentsRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ListPartitionReassignmentsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListPartitionReassignmentsRequest.
wirePeekListPartitionReassignmentsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListPartitionReassignmentsRequest, Ptr Word8)
wirePeekListPartitionReassignmentsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekListPartitionReassignmentsTopics version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ListPartitionReassignmentsRequest { listPartitionReassignmentsRequestTimeoutMs = f0_timeoutms, listPartitionReassignmentsRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ListPartitionReassignmentsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListPartitionReassignmentsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListPartitionReassignmentsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListPartitionReassignmentsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListPartitionReassignmentsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}