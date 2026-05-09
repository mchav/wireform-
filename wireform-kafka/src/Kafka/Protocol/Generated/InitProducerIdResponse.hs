{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitProducerIdResponse
Description : Kafka InitProducerIdResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 22.



Valid versions: 0-6
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitProducerIdResponse
  (
    InitProducerIdResponse(..),
    encodeInitProducerIdResponse,
    decodeInitProducerIdResponse,
    maxInitProducerIdResponseVersion
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




data InitProducerIdResponse = InitProducerIdResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  initProducerIdResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  initProducerIdResponseErrorCode :: !(Int16)
,

  -- | The current producer id.

  -- Versions: 0+
  initProducerIdResponseProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer id.

  -- Versions: 0+
  initProducerIdResponseProducerEpoch :: !(Int16)
,

  -- | The producer id for ongoing transaction when KeepPreparedTxn is used, -1 if there is no transaction 

  -- Versions: 6+
  initProducerIdResponseOngoingTxnProducerId :: !(Int64)
,

  -- | The epoch associated with the  producer id for ongoing transaction when KeepPreparedTxn is used, -1 

  -- Versions: 6+
  initProducerIdResponseOngoingTxnProducerEpoch :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitProducerIdResponse.
maxInitProducerIdResponseVersion :: Int16
maxInitProducerIdResponseVersion = 6

-- | Encode InitProducerIdResponse with the given API version.
encodeInitProducerIdResponse :: MonadPut m => E.ApiVersion -> InitProducerIdResponse -> m ()
encodeInitProducerIdResponse version msg
  | version == 6 =
    do
      serialize (initProducerIdResponseThrottleTimeMs msg)
      serialize (initProducerIdResponseErrorCode msg)
      serialize (initProducerIdResponseProducerId msg)
      serialize (initProducerIdResponseProducerEpoch msg)
      serialize (initProducerIdResponseOngoingTxnProducerId msg)
      serialize (initProducerIdResponseOngoingTxnProducerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (initProducerIdResponseThrottleTimeMs msg)
      serialize (initProducerIdResponseErrorCode msg)
      serialize (initProducerIdResponseProducerId msg)
      serialize (initProducerIdResponseProducerEpoch msg)


  | version >= 2 && version <= 5 =
    do
      serialize (initProducerIdResponseThrottleTimeMs msg)
      serialize (initProducerIdResponseErrorCode msg)
      serialize (initProducerIdResponseProducerId msg)
      serialize (initProducerIdResponseProducerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode InitProducerIdResponse with the given API version.
decodeInitProducerIdResponse :: MonadGet m => E.ApiVersion -> m InitProducerIdResponse
decodeInitProducerIdResponse version
  | version == 6 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldongoingtxnproducerid <- deserialize
      fieldongoingtxnproducerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitProducerIdResponse
        {
        initProducerIdResponseThrottleTimeMs = fieldthrottletimems
        ,
        initProducerIdResponseErrorCode = fielderrorcode
        ,
        initProducerIdResponseProducerId = fieldproducerid
        ,
        initProducerIdResponseProducerEpoch = fieldproducerepoch
        ,
        initProducerIdResponseOngoingTxnProducerId = fieldongoingtxnproducerid
        ,
        initProducerIdResponseOngoingTxnProducerEpoch = fieldongoingtxnproducerepoch
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      pure InitProducerIdResponse
        {
        initProducerIdResponseThrottleTimeMs = fieldthrottletimems
        ,
        initProducerIdResponseErrorCode = fielderrorcode
        ,
        initProducerIdResponseProducerId = fieldproducerid
        ,
        initProducerIdResponseProducerEpoch = fieldproducerepoch
        ,
        initProducerIdResponseOngoingTxnProducerId = (-1)
        ,
        initProducerIdResponseOngoingTxnProducerEpoch = (-1)
        }

  | version >= 2 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitProducerIdResponse
        {
        initProducerIdResponseThrottleTimeMs = fieldthrottletimems
        ,
        initProducerIdResponseErrorCode = fielderrorcode
        ,
        initProducerIdResponseProducerId = fieldproducerid
        ,
        initProducerIdResponseProducerEpoch = fieldproducerepoch
        ,
        initProducerIdResponseOngoingTxnProducerId = (-1)
        ,
        initProducerIdResponseOngoingTxnProducerEpoch = (-1)
        }
  | otherwise = fail $ "Unsupported version: " ++ show version