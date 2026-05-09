{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTransactionsRequest
Description : Kafka DescribeTransactionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 65.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTransactionsRequest
  (
    DescribeTransactionsRequest(..),
    encodeDescribeTransactionsRequest,
    decodeDescribeTransactionsRequest,
    maxDescribeTransactionsRequestVersion
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




data DescribeTransactionsRequest = DescribeTransactionsRequest
  {

  -- | Array of transactionalIds to include in describe results. If empty, then no results will be returned

  -- Versions: 0+
  describeTransactionsRequestTransactionalIds :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTransactionsRequest.
maxDescribeTransactionsRequestVersion :: Int16
maxDescribeTransactionsRequestVersion = 0

-- | Encode DescribeTransactionsRequest with the given API version.
encodeDescribeTransactionsRequest :: MonadPut m => E.ApiVersion -> DescribeTransactionsRequest -> m ()
encodeDescribeTransactionsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (describeTransactionsRequestTransactionalIds msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeTransactionsRequest with the given API version.
decodeDescribeTransactionsRequest :: MonadGet m => E.ApiVersion -> m DescribeTransactionsRequest
decodeDescribeTransactionsRequest version
  | version == 0 =
    do
      fieldtransactionalids <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeTransactionsRequest
        {
        describeTransactionsRequestTransactionalIds = fieldtransactionalids
        }
  | otherwise = fail $ "Unsupported version: " ++ show version