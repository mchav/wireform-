{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UnregisterBrokerRequest
Description : Kafka UnregisterBrokerRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 64.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UnregisterBrokerRequest
  (
    UnregisterBrokerRequest(..),
    maxUnregisterBrokerRequestVersion
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




data UnregisterBrokerRequest = UnregisterBrokerRequest
  {

  -- | The broker ID to unregister.

  -- Versions: 0+
  unregisterBrokerRequestBrokerId :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UnregisterBrokerRequest.
maxUnregisterBrokerRequestVersion :: Int16
maxUnregisterBrokerRequestVersion = 0

-- | KafkaMessage instance for UnregisterBrokerRequest.
instance KafkaMessage UnregisterBrokerRequest where
  messageApiKey = 64
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a UnregisterBrokerRequest.
wireMaxSizeUnregisterBrokerRequest :: Int -> UnregisterBrokerRequest -> Int
wireMaxSizeUnregisterBrokerRequest _version msg =
  0
  + 4
  + 1

-- | Direct-poke encoder for UnregisterBrokerRequest.
wirePokeUnregisterBrokerRequest :: Int -> Ptr Word8 -> UnregisterBrokerRequest -> IO (Ptr Word8)
wirePokeUnregisterBrokerRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (unregisterBrokerRequestBrokerId msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke UnregisterBrokerRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for UnregisterBrokerRequest.
wirePeekUnregisterBrokerRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UnregisterBrokerRequest, Ptr Word8)
wirePeekUnregisterBrokerRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (UnregisterBrokerRequest { unregisterBrokerRequestBrokerId = f0_brokerid }, pTagsEnd)
  | otherwise = error $ "wirePeek UnregisterBrokerRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec UnregisterBrokerRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUnregisterBrokerRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUnregisterBrokerRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUnregisterBrokerRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}