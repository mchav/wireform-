{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControlledShutdownResponse
Description : Kafka ControlledShutdownResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 7.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControlledShutdownResponse
  (
    ControlledShutdownResponse(..),
    encodeControlledShutdownResponse,
    decodeControlledShutdownResponse,
    maxControlledShutdownResponseVersion
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




data ControlledShutdownResponse = ControlledShutdownResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControlledShutdownResponse.
maxControlledShutdownResponseVersion :: Int16
maxControlledShutdownResponseVersion = -1 -- No valid versions

-- | Encode ControlledShutdownResponse with the given API version.
encodeControlledShutdownResponse :: MonadPut m => E.ApiVersion -> ControlledShutdownResponse -> m ()
encodeControlledShutdownResponse version msg
  = error "No valid versions"


-- | Decode ControlledShutdownResponse with the given API version.
decodeControlledShutdownResponse :: MonadGet m => E.ApiVersion -> m ControlledShutdownResponse
decodeControlledShutdownResponse version
  = fail "No valid versions"
