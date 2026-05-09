{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StopReplicaRequest
Description : Kafka StopReplicaRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 5.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StopReplicaRequest
  (
    StopReplicaRequest(..),
    encodeStopReplicaRequest,
    decodeStopReplicaRequest,
    maxStopReplicaRequestVersion
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




data StopReplicaRequest = StopReplicaRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StopReplicaRequest.
maxStopReplicaRequestVersion :: Int16
maxStopReplicaRequestVersion = -1 -- No valid versions

-- | Encode StopReplicaRequest with the given API version.
encodeStopReplicaRequest :: MonadPut m => E.ApiVersion -> StopReplicaRequest -> m ()
encodeStopReplicaRequest version msg
  = error "No valid versions"


-- | Decode StopReplicaRequest with the given API version.
decodeStopReplicaRequest :: MonadGet m => E.ApiVersion -> m StopReplicaRequest
decodeStopReplicaRequest version
  = fail "No valid versions"
