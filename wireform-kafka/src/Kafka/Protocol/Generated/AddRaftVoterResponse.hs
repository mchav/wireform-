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
    maxAddRaftVoterResponseVersion
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

-- | KafkaMessage instance for AddRaftVoterResponse.
instance KafkaMessage AddRaftVoterResponse where
  messageApiKey = 80
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a AddRaftVoterResponse.
wireMaxSizeAddRaftVoterResponse :: Int -> AddRaftVoterResponse -> Int
wireMaxSizeAddRaftVoterResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (addRaftVoterResponseErrorMessage msg))
  + 1

-- | Direct-poke encoder for AddRaftVoterResponse.
wirePokeAddRaftVoterResponse :: Int -> Ptr Word8 -> AddRaftVoterResponse -> IO (Ptr Word8)
wirePokeAddRaftVoterResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addRaftVoterResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (addRaftVoterResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (addRaftVoterResponseErrorMessage msg)) else WP.pokeKafkaString p2 (addRaftVoterResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AddRaftVoterResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AddRaftVoterResponse.
wirePeekAddRaftVoterResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddRaftVoterResponse, Ptr Word8)
wirePeekAddRaftVoterResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AddRaftVoterResponse { addRaftVoterResponseThrottleTimeMs = f0_throttletimems, addRaftVoterResponseErrorCode = f1_errorcode, addRaftVoterResponseErrorMessage = f2_errormessage }, pTagsEnd)
  | otherwise = error $ "wirePeek AddRaftVoterResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AddRaftVoterResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddRaftVoterResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddRaftVoterResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddRaftVoterResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}