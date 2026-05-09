{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DefaultPrincipalData
Description : Kafka DefaultPrincipalData message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DefaultPrincipalData
  (
    DefaultPrincipalData(..),
    maxDefaultPrincipalDataVersion
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




data DefaultPrincipalData = DefaultPrincipalData
  {

  -- | The principal type.

  -- Versions: 0+
  defaultPrincipalDataType :: !(KafkaString)
,

  -- | The principal name.

  -- Versions: 0+
  defaultPrincipalDataName :: !(KafkaString)
,

  -- | Whether the principal was authenticated by a delegation token on the forwarding broker.

  -- Versions: 0+
  defaultPrincipalDataTokenAuthenticated :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DefaultPrincipalData.
maxDefaultPrincipalDataVersion :: Int16
maxDefaultPrincipalDataVersion = 0




-- | Worst-case wire size of a DefaultPrincipalData.
wireMaxSizeDefaultPrincipalData :: Int -> DefaultPrincipalData -> Int
wireMaxSizeDefaultPrincipalData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (defaultPrincipalDataType msg))
  + WP.compactStringMaxSize (P.toCompactString (defaultPrincipalDataName msg))
  + 1
  + 1

-- | Direct-poke encoder for DefaultPrincipalData.
wirePokeDefaultPrincipalData :: Int -> Ptr Word8 -> DefaultPrincipalData -> IO (Ptr Word8)
wirePokeDefaultPrincipalData version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (defaultPrincipalDataType msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (defaultPrincipalDataName msg))
    p3 <- W.pokeWord8 p2 (if (defaultPrincipalDataTokenAuthenticated msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DefaultPrincipalData : unsupported version: " ++ show version

-- | Direct-poke decoder for DefaultPrincipalData.
wirePeekDefaultPrincipalData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DefaultPrincipalData, Ptr Word8)
wirePeekDefaultPrincipalData version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_type, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_name, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_tokenauthenticated, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DefaultPrincipalData { defaultPrincipalDataType = f0_type, defaultPrincipalDataName = f1_name, defaultPrincipalDataTokenAuthenticated = f2_tokenauthenticated }, pTagsEnd)
  | otherwise = error $ "wirePeek DefaultPrincipalData : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DefaultPrincipalData where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDefaultPrincipalData (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDefaultPrincipalData (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDefaultPrincipalData (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}