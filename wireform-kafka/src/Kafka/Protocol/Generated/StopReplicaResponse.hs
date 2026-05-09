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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data StopReplicaResponse = StopReplicaResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StopReplicaResponse.
maxStopReplicaResponseVersion :: Int16
maxStopReplicaResponseVersion = -1 -- No valid versions

-- | KafkaMessage instance for StopReplicaResponse.
instance KafkaMessage StopReplicaResponse where
  messageApiKey = 5
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode StopReplicaResponse with the given API version.
encodeStopReplicaResponse :: MonadPut m => E.ApiVersion -> StopReplicaResponse -> m ()
encodeStopReplicaResponse version msg
  = error "No valid versions"


-- | Decode StopReplicaResponse with the given API version.
decodeStopReplicaResponse :: MonadGet m => E.ApiVersion -> m StopReplicaResponse
decodeStopReplicaResponse version
  = fail "No valid versions"



-- | Worst-case wire size of a StopReplicaResponse.
wireMaxSizeStopReplicaResponse :: Int -> StopReplicaResponse -> Int
wireMaxSizeStopReplicaResponse _version msg =
  0



wirePokeStopReplicaResponse :: Int -> Ptr Word8 -> StopReplicaResponse -> IO (Ptr Word8)
wirePokeStopReplicaResponse _version _basePtr _msg =
  error "wirePoke StopReplicaResponse: no valid versions"

wirePeekStopReplicaResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StopReplicaResponse, Ptr Word8)
wirePeekStopReplicaResponse _version _fp _basePtr _p _endPtr =
  error "wirePeek StopReplicaResponse: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec StopReplicaResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeStopReplicaResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeStopReplicaResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekStopReplicaResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}