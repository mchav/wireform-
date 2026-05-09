{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateMetadataResponse
Description : Kafka UpdateMetadataResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 6.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateMetadataResponse
  (
    UpdateMetadataResponse(..),
    encodeUpdateMetadataResponse,
    decodeUpdateMetadataResponse,
    maxUpdateMetadataResponseVersion
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




data UpdateMetadataResponse = UpdateMetadataResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateMetadataResponse.
maxUpdateMetadataResponseVersion :: Int16
maxUpdateMetadataResponseVersion = -1 -- No valid versions

-- | Encode UpdateMetadataResponse with the given API version.
encodeUpdateMetadataResponse :: MonadPut m => E.ApiVersion -> UpdateMetadataResponse -> m ()
encodeUpdateMetadataResponse version msg
  = error "No valid versions"


-- | Decode UpdateMetadataResponse with the given API version.
decodeUpdateMetadataResponse :: MonadGet m => E.ApiVersion -> m UpdateMetadataResponse
decodeUpdateMetadataResponse version
  = fail "No valid versions"
