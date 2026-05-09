{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StreamsGroupDescribeRequest
Description : Kafka StreamsGroupDescribeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 89.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StreamsGroupDescribeRequest
  (
    StreamsGroupDescribeRequest(..),
    encodeStreamsGroupDescribeRequest,
    decodeStreamsGroupDescribeRequest,
    maxStreamsGroupDescribeRequestVersion
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




data StreamsGroupDescribeRequest = StreamsGroupDescribeRequest
  {

  -- | The ids of the groups to describe

  -- Versions: 0+
  streamsGroupDescribeRequestGroupIds :: !(KafkaArray (KafkaString))
,

  -- | Whether to include authorized operations.

  -- Versions: 0+
  streamsGroupDescribeRequestIncludeAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StreamsGroupDescribeRequest.
maxStreamsGroupDescribeRequestVersion :: Int16
maxStreamsGroupDescribeRequestVersion = 0

-- | KafkaMessage instance for StreamsGroupDescribeRequest.
instance KafkaMessage StreamsGroupDescribeRequest where
  messageApiKey = 89
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode StreamsGroupDescribeRequest with the given API version.
encodeStreamsGroupDescribeRequest :: MonadPut m => E.ApiVersion -> StreamsGroupDescribeRequest -> m ()
encodeStreamsGroupDescribeRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (streamsGroupDescribeRequestGroupIds msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (streamsGroupDescribeRequestIncludeAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode StreamsGroupDescribeRequest with the given API version.
decodeStreamsGroupDescribeRequest :: MonadGet m => E.ApiVersion -> m StreamsGroupDescribeRequest
decodeStreamsGroupDescribeRequest version
  | version == 0 =
    do
      fieldgroupids <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldincludeauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure StreamsGroupDescribeRequest
        {
        streamsGroupDescribeRequestGroupIds = fieldgroupids
        ,
        streamsGroupDescribeRequestIncludeAuthorizedOperations = fieldincludeauthorizedoperations
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a StreamsGroupDescribeRequest.
wireMaxSizeStreamsGroupDescribeRequest :: Int -> StreamsGroupDescribeRequest -> Int
wireMaxSizeStreamsGroupDescribeRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (streamsGroupDescribeRequestGroupIds msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for StreamsGroupDescribeRequest.
wirePokeStreamsGroupDescribeRequest :: Int -> Ptr Word8 -> StreamsGroupDescribeRequest -> IO (Ptr Word8)
wirePokeStreamsGroupDescribeRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (streamsGroupDescribeRequestGroupIds msg)
    p2 <- W.pokeWord8 p1 (if (streamsGroupDescribeRequestIncludeAuthorizedOperations msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke StreamsGroupDescribeRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for StreamsGroupDescribeRequest.
wirePeekStreamsGroupDescribeRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StreamsGroupDescribeRequest, Ptr Word8)
wirePeekStreamsGroupDescribeRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupids, p1) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_includeauthorizedoperations, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (StreamsGroupDescribeRequest { streamsGroupDescribeRequestGroupIds = f0_groupids, streamsGroupDescribeRequestIncludeAuthorizedOperations = f1_includeauthorizedoperations }, pTagsEnd)
  | otherwise = error $ "wirePeek StreamsGroupDescribeRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec StreamsGroupDescribeRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeStreamsGroupDescribeRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeStreamsGroupDescribeRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekStreamsGroupDescribeRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}