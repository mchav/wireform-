{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddRaftVoterResponse
Description : Kafka AddRaftVoterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 80.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddRaftVoterResponse
  (
    AddRaftVoterResponse(..),
    encodeAddRaftVoterResponse,
    decodeAddRaftVoterResponse,
    maxAddRaftVoterResponseVersion
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




data AddRaftVoterResponse = AddRaftVoterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  addRaftVoterResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  addRaftVoterResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  addRaftVoterResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddRaftVoterResponse.
maxAddRaftVoterResponseVersion :: Int16
maxAddRaftVoterResponseVersion = 1

-- | Encode AddRaftVoterResponse with the given API version.
encodeAddRaftVoterResponse :: MonadPut m => E.ApiVersion -> AddRaftVoterResponse -> m ()
encodeAddRaftVoterResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (addRaftVoterResponseThrottleTimeMs msg)
      serialize (addRaftVoterResponseErrorCode msg)
      serialize (toCompactString (addRaftVoterResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddRaftVoterResponse with the given API version.
decodeAddRaftVoterResponse :: MonadGet m => E.ApiVersion -> m AddRaftVoterResponse
decodeAddRaftVoterResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddRaftVoterResponse
        {
        addRaftVoterResponseThrottleTimeMs = fieldthrottletimems
        ,
        addRaftVoterResponseErrorCode = fielderrorcode
        ,
        addRaftVoterResponseErrorMessage = fielderrormessage
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAddRaftVoterResponse' / 'decodeAddRaftVoterResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AddRaftVoterResponse where
  wireCodec = Just (WC.serialShimCodec encodeAddRaftVoterResponse decodeAddRaftVoterResponse)
  {-# INLINE wireCodec #-}
