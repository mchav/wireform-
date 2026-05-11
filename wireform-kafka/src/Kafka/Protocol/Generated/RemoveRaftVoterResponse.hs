{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RemoveRaftVoterResponse
Description : Kafka RemoveRaftVoterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 81.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RemoveRaftVoterResponse
  (
    RemoveRaftVoterResponse(..),
    maxRemoveRaftVoterResponseVersion
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




data RemoveRaftVoterResponse = RemoveRaftVoterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  removeRaftVoterResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  removeRaftVoterResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  removeRaftVoterResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RemoveRaftVoterResponse.
maxRemoveRaftVoterResponseVersion :: Int16
maxRemoveRaftVoterResponseVersion = 0

-- | KafkaMessage instance for RemoveRaftVoterResponse.
instance KafkaMessage RemoveRaftVoterResponse where
  messageApiKey = 81
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a RemoveRaftVoterResponse.
wireMaxSizeRemoveRaftVoterResponse :: Int -> RemoveRaftVoterResponse -> Int
wireMaxSizeRemoveRaftVoterResponse _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (removeRaftVoterResponseErrorMessage msg)
  + 1

-- | Direct-poke encoder for RemoveRaftVoterResponse.
wirePokeRemoveRaftVoterResponse :: Int -> Ptr Word8 -> RemoveRaftVoterResponse -> IO (Ptr Word8)
wirePokeRemoveRaftVoterResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (removeRaftVoterResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (removeRaftVoterResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (removeRaftVoterResponseErrorMessage msg)) else WP.pokeKafkaString p2 (removeRaftVoterResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke RemoveRaftVoterResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for RemoveRaftVoterResponse.
wirePeekRemoveRaftVoterResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RemoveRaftVoterResponse, Ptr Word8)
wirePeekRemoveRaftVoterResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (RemoveRaftVoterResponse { removeRaftVoterResponseThrottleTimeMs = f0_throttletimems, removeRaftVoterResponseErrorCode = f1_errorcode, removeRaftVoterResponseErrorMessage = f2_errormessage }, pTagsEnd)
  | otherwise = error $ "wirePeek RemoveRaftVoterResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec RemoveRaftVoterResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRemoveRaftVoterResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRemoveRaftVoterResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRemoveRaftVoterResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}