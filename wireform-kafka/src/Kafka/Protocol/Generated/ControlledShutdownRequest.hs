{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControlledShutdownRequest
Description : Kafka ControlledShutdownRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 7.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControlledShutdownRequest
  (
    ControlledShutdownRequest(..),
    encodeControlledShutdownRequest,
    decodeControlledShutdownRequest,
    maxControlledShutdownRequestVersion
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




data ControlledShutdownRequest = ControlledShutdownRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControlledShutdownRequest.
maxControlledShutdownRequestVersion :: Int16
maxControlledShutdownRequestVersion = -1 -- No valid versions

-- | Encode ControlledShutdownRequest with the given API version.
encodeControlledShutdownRequest :: MonadPut m => E.ApiVersion -> ControlledShutdownRequest -> m ()
encodeControlledShutdownRequest version msg
  = error "No valid versions"


-- | Decode ControlledShutdownRequest with the given API version.
decodeControlledShutdownRequest :: MonadGet m => E.ApiVersion -> m ControlledShutdownRequest
decodeControlledShutdownRequest version
  = fail "No valid versions"
