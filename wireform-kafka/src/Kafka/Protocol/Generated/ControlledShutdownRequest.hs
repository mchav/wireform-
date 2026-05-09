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
    maxControlledShutdownRequestVersion
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




data ControlledShutdownRequest = ControlledShutdownRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControlledShutdownRequest.
maxControlledShutdownRequestVersion :: Int16
maxControlledShutdownRequestVersion = -1 -- No valid versions

-- | KafkaMessage instance for ControlledShutdownRequest.
instance KafkaMessage ControlledShutdownRequest where
  messageApiKey = 7
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a ControlledShutdownRequest.
wireMaxSizeControlledShutdownRequest :: Int -> ControlledShutdownRequest -> Int
wireMaxSizeControlledShutdownRequest _version msg =
  0



wirePokeControlledShutdownRequest :: Int -> Ptr Word8 -> ControlledShutdownRequest -> IO (Ptr Word8)
wirePokeControlledShutdownRequest _version _basePtr _msg =
  error "wirePoke ControlledShutdownRequest: no valid versions"

wirePeekControlledShutdownRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ControlledShutdownRequest, Ptr Word8)
wirePeekControlledShutdownRequest _version _fp _basePtr _p _endPtr =
  error "wirePeek ControlledShutdownRequest: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ControlledShutdownRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeControlledShutdownRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeControlledShutdownRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekControlledShutdownRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}