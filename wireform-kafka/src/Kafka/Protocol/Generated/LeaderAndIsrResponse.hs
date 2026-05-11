{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaderAndIsrResponse
Description : Kafka LeaderAndIsrResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 4.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaderAndIsrResponse
  (
    LeaderAndIsrResponse(..),
    maxLeaderAndIsrResponseVersion
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




data LeaderAndIsrResponse = LeaderAndIsrResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderAndIsrResponse.
maxLeaderAndIsrResponseVersion :: Int16
maxLeaderAndIsrResponseVersion = -1 -- No valid versions

-- | KafkaMessage instance for LeaderAndIsrResponse.
instance KafkaMessage LeaderAndIsrResponse where
  messageApiKey = 4
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a LeaderAndIsrResponse.
wireMaxSizeLeaderAndIsrResponse :: Int -> LeaderAndIsrResponse -> Int
wireMaxSizeLeaderAndIsrResponse _version msg =
  0



wirePokeLeaderAndIsrResponse :: Int -> Ptr Word8 -> LeaderAndIsrResponse -> IO (Ptr Word8)
wirePokeLeaderAndIsrResponse _version _basePtr _msg =
  error "wirePoke LeaderAndIsrResponse: no valid versions"

wirePeekLeaderAndIsrResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderAndIsrResponse, Ptr Word8)
wirePeekLeaderAndIsrResponse _version _fp _basePtr _p _endPtr =
  error "wirePeek LeaderAndIsrResponse: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec LeaderAndIsrResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaderAndIsrResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaderAndIsrResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaderAndIsrResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}