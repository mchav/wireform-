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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec StreamsGroupDescribeRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
