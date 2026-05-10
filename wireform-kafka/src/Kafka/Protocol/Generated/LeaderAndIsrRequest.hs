{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaderAndIsrRequest
Description : Kafka LeaderAndIsrRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 4.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaderAndIsrRequest
  (
    LeaderAndIsrRequest(..),
    maxLeaderAndIsrRequestVersion
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




data LeaderAndIsrRequest = LeaderAndIsrRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderAndIsrRequest.
maxLeaderAndIsrRequestVersion :: Int16
maxLeaderAndIsrRequestVersion = -1 -- No valid versions

-- | KafkaMessage instance for LeaderAndIsrRequest.
instance KafkaMessage LeaderAndIsrRequest where
  messageApiKey = 4
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a LeaderAndIsrRequest.
wireMaxSizeLeaderAndIsrRequest :: Int -> LeaderAndIsrRequest -> Int
wireMaxSizeLeaderAndIsrRequest _version msg =
  0



wirePokeLeaderAndIsrRequest :: Int -> Ptr Word8 -> LeaderAndIsrRequest -> IO (Ptr Word8)
wirePokeLeaderAndIsrRequest _version _basePtr _msg =
  error "wirePoke LeaderAndIsrRequest: no valid versions"

wirePeekLeaderAndIsrRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderAndIsrRequest, Ptr Word8)
wirePeekLeaderAndIsrRequest _version _fp _basePtr _p _endPtr =
  error "wirePeek LeaderAndIsrRequest: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec LeaderAndIsrRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaderAndIsrRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaderAndIsrRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaderAndIsrRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}