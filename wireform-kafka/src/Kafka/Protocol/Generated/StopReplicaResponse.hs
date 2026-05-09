{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StopReplicaResponse
Description : Kafka StopReplicaResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 5.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StopReplicaResponse
  (
    StopReplicaResponse(..),
    encodeStopReplicaResponse,
    decodeStopReplicaResponse,
    maxStopReplicaResponseVersion
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




data StopReplicaResponse = StopReplicaResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StopReplicaResponse.
maxStopReplicaResponseVersion :: Int16
maxStopReplicaResponseVersion = -1 -- No valid versions

-- | Encode StopReplicaResponse with the given API version.
encodeStopReplicaResponse :: MonadPut m => E.ApiVersion -> StopReplicaResponse -> m ()
encodeStopReplicaResponse version msg
  = error "No valid versions"


-- | Decode StopReplicaResponse with the given API version.
decodeStopReplicaResponse :: MonadGet m => E.ApiVersion -> m StopReplicaResponse
decodeStopReplicaResponse version
  = fail "No valid versions"

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec StopReplicaResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
