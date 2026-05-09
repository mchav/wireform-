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
    maxStopReplicaRequestVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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




data StopReplicaRequest = StopReplicaRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StopReplicaRequest.
maxStopReplicaRequestVersion :: Int16
maxStopReplicaRequestVersion = -1 -- No valid versions

-- | KafkaMessage instance for StopReplicaRequest.
instance KafkaMessage StopReplicaRequest where
  messageApiKey = 5
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a StopReplicaRequest.
wireMaxSizeStopReplicaRequest :: Int -> StopReplicaRequest -> Int
wireMaxSizeStopReplicaRequest _version msg =
  0



wirePokeStopReplicaRequest :: Int -> Ptr Word8 -> StopReplicaRequest -> IO (Ptr Word8)
wirePokeStopReplicaRequest _version _basePtr _msg =
  error "wirePoke StopReplicaRequest: no valid versions"

wirePeekStopReplicaRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StopReplicaRequest, Ptr Word8)
wirePeekStopReplicaRequest _version _fp _basePtr _p _endPtr =
  error "wirePeek StopReplicaRequest: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec StopReplicaRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeStopReplicaRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeStopReplicaRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekStopReplicaRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}