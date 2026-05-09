{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeDelegationTokenRequest
Description : Kafka DescribeDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 41.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeDelegationTokenRequest
  (
    DescribeDelegationTokenRequest(..),
    DescribeDelegationTokenOwner(..),
    encodeDescribeDelegationTokenRequest,
    decodeDescribeDelegationTokenRequest,
    maxDescribeDelegationTokenRequestVersion
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
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | Each owner that we want to describe delegation tokens for, or null to describe all tokens.
data DescribeDelegationTokenOwner = DescribeDelegationTokenOwner
  {

  -- | The owner principal type.

  -- Versions: 0+
  describeDelegationTokenOwnerPrincipalType :: !(KafkaString)
,

  -- | The owner principal name.

  -- Versions: 0+
  describeDelegationTokenOwnerPrincipalName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeDelegationTokenOwner with version-aware field handling.
encodeDescribeDelegationTokenOwner :: MonadPut m => E.ApiVersion -> DescribeDelegationTokenOwner -> m ()
encodeDescribeDelegationTokenOwner version dmsg =
  do
    if version >= 2 then serialize (toCompactString (describeDelegationTokenOwnerPrincipalType dmsg)) else serialize (describeDelegationTokenOwnerPrincipalType dmsg)
    if version >= 2 then serialize (toCompactString (describeDelegationTokenOwnerPrincipalName dmsg)) else serialize (describeDelegationTokenOwnerPrincipalName dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeDelegationTokenOwner with version-aware field handling.
decodeDescribeDelegationTokenOwner :: MonadGet m => E.ApiVersion -> m DescribeDelegationTokenOwner
decodeDescribeDelegationTokenOwner version =
  do
    fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeDelegationTokenOwner
      {
      describeDelegationTokenOwnerPrincipalType = fieldprincipaltype
      ,
      describeDelegationTokenOwnerPrincipalName = fieldprincipalname
      }



data DescribeDelegationTokenRequest = DescribeDelegationTokenRequest
  {

  -- | Each owner that we want to describe delegation tokens for, or null to describe all tokens.

  -- Versions: 0+
  describeDelegationTokenRequestOwners :: !(KafkaArray (DescribeDelegationTokenOwner))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeDelegationTokenRequest.
maxDescribeDelegationTokenRequestVersion :: Int16
maxDescribeDelegationTokenRequestVersion = 3

-- | KafkaMessage instance for DescribeDelegationTokenRequest.
instance KafkaMessage DescribeDelegationTokenRequest where
  messageApiKey = 41
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Encode DescribeDelegationTokenRequest with the given API version.
encodeDescribeDelegationTokenRequest :: MonadPut m => E.ApiVersion -> DescribeDelegationTokenRequest -> m ()
encodeDescribeDelegationTokenRequest version msg
  | version == 1 =
    do
      E.encodeVersionedNullableArray version 2 encodeDescribeDelegationTokenOwner (describeDelegationTokenRequestOwners msg)


  | version >= 2 && version <= 3 =
    do
      E.encodeVersionedNullableArray version 2 encodeDescribeDelegationTokenOwner (describeDelegationTokenRequestOwners msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeDelegationTokenRequest with the given API version.
decodeDescribeDelegationTokenRequest :: MonadGet m => E.ApiVersion -> m DescribeDelegationTokenRequest
decodeDescribeDelegationTokenRequest version
  | version == 1 =
    do
      fieldowners <- E.decodeVersionedNullableArray version 2 decodeDescribeDelegationTokenOwner
      pure DescribeDelegationTokenRequest
        {
        describeDelegationTokenRequestOwners = fieldowners
        }

  | version >= 2 && version <= 3 =
    do
      fieldowners <- E.decodeVersionedNullableArray version 2 decodeDescribeDelegationTokenOwner
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeDelegationTokenRequest
        {
        describeDelegationTokenRequestOwners = fieldowners
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DescribeDelegationTokenOwner.
wireMaxSizeDescribeDelegationTokenOwner :: Int -> DescribeDelegationTokenOwner -> Int
wireMaxSizeDescribeDelegationTokenOwner _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeDelegationTokenOwnerPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (describeDelegationTokenOwnerPrincipalName msg))
  + 1

-- | Direct-poke encoder for DescribeDelegationTokenOwner.
wirePokeDescribeDelegationTokenOwner :: Int -> Ptr Word8 -> DescribeDelegationTokenOwner -> IO (Ptr Word8)
wirePokeDescribeDelegationTokenOwner version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeDelegationTokenOwnerPrincipalType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeDelegationTokenOwnerPrincipalName msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribeDelegationTokenOwner.
wirePeekDescribeDelegationTokenOwner :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeDelegationTokenOwner, Ptr Word8)
wirePeekDescribeDelegationTokenOwner version _fp _basePtr p0 endPtr = do
  (f0_principaltype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_principalname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribeDelegationTokenOwner { describeDelegationTokenOwnerPrincipalType = f0_principaltype, describeDelegationTokenOwnerPrincipalName = f1_principalname }, pTagsEnd)

-- | Worst-case wire size of a DescribeDelegationTokenRequest.
wireMaxSizeDescribeDelegationTokenRequest :: Int -> DescribeDelegationTokenRequest -> Int
wireMaxSizeDescribeDelegationTokenRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeDelegationTokenRequestOwners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeDelegationTokenOwner _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeDelegationTokenRequest.
wirePokeDescribeDelegationTokenRequest :: Int -> Ptr Word8 -> DescribeDelegationTokenRequest -> IO (Ptr Word8)
wirePokeDescribeDelegationTokenRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeDescribeDelegationTokenOwner version p x) p0 (describeDelegationTokenRequestOwners msg)
    pure p1
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeDescribeDelegationTokenOwner version p x) p0 (describeDelegationTokenRequestOwners msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeDelegationTokenRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeDelegationTokenRequest.
wirePeekDescribeDelegationTokenRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeDelegationTokenRequest, Ptr Word8)
wirePeekDescribeDelegationTokenRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_owners, p1) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekDescribeDelegationTokenOwner version _fp _basePtr p e) p0 endPtr
    pure (DescribeDelegationTokenRequest { describeDelegationTokenRequestOwners = f0_owners }, p1)
  | version >= 2 && version <= 3 = do
    (f0_owners, p1) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekDescribeDelegationTokenOwner version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeDelegationTokenRequest { describeDelegationTokenRequestOwners = f0_owners }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeDelegationTokenRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeDelegationTokenRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeDelegationTokenRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeDelegationTokenRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeDelegationTokenRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}