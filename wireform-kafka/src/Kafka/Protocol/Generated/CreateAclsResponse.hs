{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateAclsResponse
Description : Kafka CreateAclsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 30.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateAclsResponse
  (
    CreateAclsResponse(..),
    AclCreationResult(..),
    maxCreateAclsResponseVersion
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


-- | The results for each ACL creation.
data AclCreationResult = AclCreationResult
  {

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  aclCreationResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  aclCreationResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data CreateAclsResponse = CreateAclsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  createAclsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each ACL creation.

  -- Versions: 0+
  createAclsResponseResults :: !(KafkaArray (AclCreationResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateAclsResponse.
maxCreateAclsResponseVersion :: Int16
maxCreateAclsResponseVersion = 3

-- | KafkaMessage instance for CreateAclsResponse.
instance KafkaMessage CreateAclsResponse where
  messageApiKey = 30
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a AclCreationResult.
wireMaxSizeAclCreationResult :: Int -> AclCreationResult -> Int
wireMaxSizeAclCreationResult _version msg =
  0
  + 2
  + WP.dualStringMaxSize (aclCreationResultErrorMessage msg)
  + 1

-- | Direct-poke encoder for AclCreationResult.
wirePokeAclCreationResult :: Int -> Ptr Word8 -> AclCreationResult -> IO (Ptr Word8)
wirePokeAclCreationResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (aclCreationResultErrorCode msg)
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (aclCreationResultErrorMessage msg)) else WP.pokeKafkaString p1 (aclCreationResultErrorMessage msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AclCreationResult.
wirePeekAclCreationResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AclCreationResult, Ptr Word8)
wirePeekAclCreationResult version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AclCreationResult { aclCreationResultErrorCode = f0_errorcode, aclCreationResultErrorMessage = f1_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAclCreationResult :: AclCreationResult
defaultAclCreationResult = AclCreationResult { aclCreationResultErrorCode = 0, aclCreationResultErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a CreateAclsResponse.
wireMaxSizeCreateAclsResponse :: Int -> CreateAclsResponse -> Int
wireMaxSizeCreateAclsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (createAclsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAclCreationResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreateAclsResponse.
wirePokeCreateAclsResponse :: Int -> Ptr Word8 -> CreateAclsResponse -> IO (Ptr Word8)
wirePokeCreateAclsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (createAclsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAclCreationResult version p x) p1 (createAclsResponseResults msg)
    pure p2
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (createAclsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAclCreationResult version p x) p1 (createAclsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke CreateAclsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateAclsResponse.
wirePeekCreateAclsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateAclsResponse, Ptr Word8)
wirePeekCreateAclsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAclCreationResult version _fp _basePtr p e) p1 endPtr
    pure (CreateAclsResponse { createAclsResponseThrottleTimeMs = f0_throttletimems, createAclsResponseResults = f1_results }, p2)
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAclCreationResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (CreateAclsResponse { createAclsResponseThrottleTimeMs = f0_throttletimems, createAclsResponseResults = f1_results }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateAclsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec CreateAclsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateAclsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateAclsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateAclsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}