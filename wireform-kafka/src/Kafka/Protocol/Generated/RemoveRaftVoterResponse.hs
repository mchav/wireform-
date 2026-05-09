{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RemoveRaftVoterResponse
Description : Kafka RemoveRaftVoterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 81.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RemoveRaftVoterResponse
  (
    RemoveRaftVoterResponse(..),
    encodeRemoveRaftVoterResponse,
    decodeRemoveRaftVoterResponse,
    maxRemoveRaftVoterResponseVersion
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




data RemoveRaftVoterResponse = RemoveRaftVoterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  removeRaftVoterResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  removeRaftVoterResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  removeRaftVoterResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RemoveRaftVoterResponse.
maxRemoveRaftVoterResponseVersion :: Int16
maxRemoveRaftVoterResponseVersion = 0

-- | Encode RemoveRaftVoterResponse with the given API version.
encodeRemoveRaftVoterResponse :: MonadPut m => E.ApiVersion -> RemoveRaftVoterResponse -> m ()
encodeRemoveRaftVoterResponse version msg
  | version == 0 =
    do
      serialize (removeRaftVoterResponseThrottleTimeMs msg)
      serialize (removeRaftVoterResponseErrorCode msg)
      serialize (toCompactString (removeRaftVoterResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RemoveRaftVoterResponse with the given API version.
decodeRemoveRaftVoterResponse :: MonadGet m => E.ApiVersion -> m RemoveRaftVoterResponse
decodeRemoveRaftVoterResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RemoveRaftVoterResponse
        {
        removeRaftVoterResponseThrottleTimeMs = fieldthrottletimems
        ,
        removeRaftVoterResponseErrorCode = fielderrorcode
        ,
        removeRaftVoterResponseErrorMessage = fielderrormessage
        }
  | otherwise = fail $ "Unsupported version: " ++ show version