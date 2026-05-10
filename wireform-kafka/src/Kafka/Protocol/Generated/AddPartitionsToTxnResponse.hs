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
    maxAddPartitionsToTxnResponseVersion
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

-- | KafkaMessage instance for AddPartitionsToTxnResponse.
instance KafkaMessage AddPartitionsToTxnResponse where
  messageApiKey = 24
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a AddPartitionsToTxnTopicResult.
wireMaxSizeAddPartitionsToTxnTopicResult :: Int -> AddPartitionsToTxnTopicResult -> Int
wireMaxSizeAddPartitionsToTxnTopicResult _version msg =
  0
  + WP.dualStringMaxSize (addPartitionsToTxnTopicResultName msg)
  + (5 + (case P.unKafkaArray (addPartitionsToTxnTopicResultResultsByPartition msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnPartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnTopicResult.
wirePokeAddPartitionsToTxnTopicResult :: Int -> Ptr Word8 -> AddPartitionsToTxnTopicResult -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnTopicResultName msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnTopicResultName msg))
  p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnPartitionResult version p x) p1 (addPartitionsToTxnTopicResultResultsByPartition msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AddPartitionsToTxnTopicResult.
wirePeekAddPartitionsToTxnTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnTopicResult, Ptr Word8)
wirePeekAddPartitionsToTxnTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_resultsbypartition, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AddPartitionsToTxnTopicResult { addPartitionsToTxnTopicResultName = f0_name, addPartitionsToTxnTopicResultResultsByPartition = f1_resultsbypartition }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAddPartitionsToTxnTopicResult :: AddPartitionsToTxnTopicResult
defaultAddPartitionsToTxnTopicResult = AddPartitionsToTxnTopicResult { addPartitionsToTxnTopicResultName = P.KafkaString Null, addPartitionsToTxnTopicResultResultsByPartition = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AddPartitionsToTxnPartitionResult.
wireMaxSizeAddPartitionsToTxnPartitionResult :: Int -> AddPartitionsToTxnPartitionResult -> Int
wireMaxSizeAddPartitionsToTxnPartitionResult _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnPartitionResult.
wirePokeAddPartitionsToTxnPartitionResult :: Int -> Ptr Word8 -> AddPartitionsToTxnPartitionResult -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnPartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (addPartitionsToTxnPartitionResultPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (addPartitionsToTxnPartitionResultPartitionErrorCode msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AddPartitionsToTxnPartitionResult.
wirePeekAddPartitionsToTxnPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnPartitionResult, Ptr Word8)
wirePeekAddPartitionsToTxnPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_partitionerrorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AddPartitionsToTxnPartitionResult { addPartitionsToTxnPartitionResultPartitionIndex = f0_partitionindex, addPartitionsToTxnPartitionResultPartitionErrorCode = f1_partitionerrorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAddPartitionsToTxnPartitionResult :: AddPartitionsToTxnPartitionResult
defaultAddPartitionsToTxnPartitionResult = AddPartitionsToTxnPartitionResult { addPartitionsToTxnPartitionResultPartitionIndex = 0, addPartitionsToTxnPartitionResultPartitionErrorCode = 0 }

-- | Worst-case wire size of a AddPartitionsToTxnResult.
wireMaxSizeAddPartitionsToTxnResult :: Int -> AddPartitionsToTxnResult -> Int
wireMaxSizeAddPartitionsToTxnResult _version msg =
  0
  + WP.dualStringMaxSize (addPartitionsToTxnResultTransactionalId msg)
  + (5 + (case P.unKafkaArray (addPartitionsToTxnResultTopicResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnResult.
wirePokeAddPartitionsToTxnResult :: Int -> Ptr Word8 -> AddPartitionsToTxnResult -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addPartitionsToTxnResultTransactionalId msg)) else WP.pokeKafkaString p0 (addPartitionsToTxnResultTransactionalId msg)) else pure p0)
  p2 <- (if version >= 4 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopicResult version p x) p1 (addPartitionsToTxnResultTopicResults msg) else pure p1)
  if version >= 3 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AddPartitionsToTxnResult.
wirePeekAddPartitionsToTxnResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnResult, Ptr Word8)
wirePeekAddPartitionsToTxnResult version _fp _basePtr p0 endPtr = do
  (f0_transactionalid, p1) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicresults, p2) <- (if version >= 4 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopicResult version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AddPartitionsToTxnResult { addPartitionsToTxnResultTransactionalId = f0_transactionalid, addPartitionsToTxnResultTopicResults = f1_topicresults }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAddPartitionsToTxnResult :: AddPartitionsToTxnResult
defaultAddPartitionsToTxnResult = AddPartitionsToTxnResult { addPartitionsToTxnResultTransactionalId = P.KafkaString Null, addPartitionsToTxnResultTopicResults = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AddPartitionsToTxnResponse.
wireMaxSizeAddPartitionsToTxnResponse :: Int -> AddPartitionsToTxnResponse -> Int
wireMaxSizeAddPartitionsToTxnResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (addPartitionsToTxnResponseResultsByTransaction msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnResult _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (addPartitionsToTxnResponseResultsByTopicV3AndBelow msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAddPartitionsToTxnTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AddPartitionsToTxnResponse.
wirePokeAddPartitionsToTxnResponse :: Int -> Ptr Word8 -> AddPartitionsToTxnResponse -> IO (Ptr Word8)
wirePokeAddPartitionsToTxnResponse version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addPartitionsToTxnResponseThrottleTimeMs msg)
    p2 <- (if version <= 3 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopicResult version p x) p1 (addPartitionsToTxnResponseResultsByTopicV3AndBelow msg) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addPartitionsToTxnResponseThrottleTimeMs msg)
    p2 <- (if version >= 4 then W.pokeInt16BE p1 (addPartitionsToTxnResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 4 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnResult version p x) p2 (addPartitionsToTxnResponseResultsByTransaction msg) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addPartitionsToTxnResponseThrottleTimeMs msg)
    p2 <- (if version <= 3 then WP.pokeVersionedArray version 3 (\p x -> wirePokeAddPartitionsToTxnTopicResult version p x) p1 (addPartitionsToTxnResponseResultsByTopicV3AndBelow msg) else pure p1)
    pure p2
  | otherwise = error $ "wirePoke AddPartitionsToTxnResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AddPartitionsToTxnResponse.
wirePeekAddPartitionsToTxnResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddPartitionsToTxnResponse, Ptr Word8)
wirePeekAddPartitionsToTxnResponse version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_resultsbytopicv3andbelow, p2) <- (if version <= 3 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopicResult version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AddPartitionsToTxnResponse { addPartitionsToTxnResponseThrottleTimeMs = f0_throttletimems, addPartitionsToTxnResponseErrorCode = 0, addPartitionsToTxnResponseResultsByTransaction = P.mkKafkaArray V.empty, addPartitionsToTxnResponseResultsByTopicV3AndBelow = f1_resultsbytopicv3andbelow }, pTagsEnd)
  | version >= 4 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- (if version >= 4 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_resultsbytransaction, p3) <- (if version >= 4 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnResult version _fp _basePtr p e) p2 endPtr else pure (P.mkKafkaArray V.empty, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AddPartitionsToTxnResponse { addPartitionsToTxnResponseThrottleTimeMs = f0_throttletimems, addPartitionsToTxnResponseErrorCode = f1_errorcode, addPartitionsToTxnResponseResultsByTransaction = f2_resultsbytransaction, addPartitionsToTxnResponseResultsByTopicV3AndBelow = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_resultsbytopicv3andbelow, p2) <- (if version <= 3 then WP.peekVersionedArray version 3 (\p e -> wirePeekAddPartitionsToTxnTopicResult version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pure (AddPartitionsToTxnResponse { addPartitionsToTxnResponseThrottleTimeMs = f0_throttletimems, addPartitionsToTxnResponseErrorCode = 0, addPartitionsToTxnResponseResultsByTransaction = P.mkKafkaArray V.empty, addPartitionsToTxnResponseResultsByTopicV3AndBelow = f1_resultsbytopicv3andbelow }, p2)
  | otherwise = error $ "wirePeek AddPartitionsToTxnResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AddPartitionsToTxnResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddPartitionsToTxnResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddPartitionsToTxnResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddPartitionsToTxnResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}