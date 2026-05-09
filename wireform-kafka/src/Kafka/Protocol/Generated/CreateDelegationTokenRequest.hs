{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateDelegationTokenRequest
Description : Kafka CreateDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 38.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateDelegationTokenRequest
  (
    CreateDelegationTokenRequest(..),
    CreatableRenewers(..),
    encodeCreateDelegationTokenRequest,
    decodeCreateDelegationTokenRequest,
    maxCreateDelegationTokenRequestVersion
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


-- | A list of those who are allowed to renew this token before it expires.
data CreatableRenewers = CreatableRenewers
  {

  -- | The type of the Kafka principal.

  -- Versions: 0+
  creatableRenewersPrincipalType :: !(KafkaString)
,

  -- | The name of the Kafka principal.

  -- Versions: 0+
  creatableRenewersPrincipalName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableRenewers with version-aware field handling.
encodeCreatableRenewers :: MonadPut m => E.ApiVersion -> CreatableRenewers -> m ()
encodeCreatableRenewers version cmsg =
  do
    if version >= 2 then serialize (toCompactString (creatableRenewersPrincipalType cmsg)) else serialize (creatableRenewersPrincipalType cmsg)
    if version >= 2 then serialize (toCompactString (creatableRenewersPrincipalName cmsg)) else serialize (creatableRenewersPrincipalName cmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatableRenewers with version-aware field handling.
decodeCreatableRenewers :: MonadGet m => E.ApiVersion -> m CreatableRenewers
decodeCreatableRenewers version =
  do
    fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatableRenewers
      {
      creatableRenewersPrincipalType = fieldprincipaltype
      ,
      creatableRenewersPrincipalName = fieldprincipalname
      }



data CreateDelegationTokenRequest = CreateDelegationTokenRequest
  {

  -- | The principal type of the owner of the token. If it's null it defaults to the token request principa

  -- Versions: 3+
  createDelegationTokenRequestOwnerPrincipalType :: !(KafkaString)
,

  -- | The principal name of the owner of the token. If it's null it defaults to the token request principa

  -- Versions: 3+
  createDelegationTokenRequestOwnerPrincipalName :: !(KafkaString)
,

  -- | A list of those who are allowed to renew this token before it expires.

  -- Versions: 0+
  createDelegationTokenRequestRenewers :: !(KafkaArray (CreatableRenewers))
,

  -- | The maximum lifetime of the token in milliseconds, or -1 to use the server side default.

  -- Versions: 0+
  createDelegationTokenRequestMaxLifetimeMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateDelegationTokenRequest.
maxCreateDelegationTokenRequestVersion :: Int16
maxCreateDelegationTokenRequestVersion = 3

-- | KafkaMessage instance for CreateDelegationTokenRequest.
instance KafkaMessage CreateDelegationTokenRequest where
  messageApiKey = 38
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Encode CreateDelegationTokenRequest with the given API version.
encodeCreateDelegationTokenRequest :: MonadPut m => E.ApiVersion -> CreateDelegationTokenRequest -> m ()
encodeCreateDelegationTokenRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 2 encodeCreatableRenewers (case P.unKafkaArray (createDelegationTokenRequestRenewers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createDelegationTokenRequestMaxLifetimeMs msg)


  | version == 2 =
    do
      E.encodeVersionedArray version 2 encodeCreatableRenewers (case P.unKafkaArray (createDelegationTokenRequestRenewers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createDelegationTokenRequestMaxLifetimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 3 =
    do
      serialize (toCompactString (createDelegationTokenRequestOwnerPrincipalType msg))
      serialize (toCompactString (createDelegationTokenRequestOwnerPrincipalName msg))
      E.encodeVersionedArray version 2 encodeCreatableRenewers (case P.unKafkaArray (createDelegationTokenRequestRenewers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createDelegationTokenRequestMaxLifetimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateDelegationTokenRequest with the given API version.
decodeCreateDelegationTokenRequest :: MonadGet m => E.ApiVersion -> m CreateDelegationTokenRequest
decodeCreateDelegationTokenRequest version
  | version == 1 =
    do
      fieldrenewers <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatableRenewers
      fieldmaxlifetimems <- deserialize
      pure CreateDelegationTokenRequest
        {
        createDelegationTokenRequestOwnerPrincipalType = P.KafkaString Null
        ,
        createDelegationTokenRequestOwnerPrincipalName = P.KafkaString Null
        ,
        createDelegationTokenRequestRenewers = fieldrenewers
        ,
        createDelegationTokenRequestMaxLifetimeMs = fieldmaxlifetimems
        }

  | version == 2 =
    do
      fieldrenewers <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatableRenewers
      fieldmaxlifetimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateDelegationTokenRequest
        {
        createDelegationTokenRequestOwnerPrincipalType = P.KafkaString Null
        ,
        createDelegationTokenRequestOwnerPrincipalName = P.KafkaString Null
        ,
        createDelegationTokenRequestRenewers = fieldrenewers
        ,
        createDelegationTokenRequestMaxLifetimeMs = fieldmaxlifetimems
        }

  | version == 3 =
    do
      fieldownerprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldownerprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldrenewers <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatableRenewers
      fieldmaxlifetimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateDelegationTokenRequest
        {
        createDelegationTokenRequestOwnerPrincipalType = fieldownerprincipaltype
        ,
        createDelegationTokenRequestOwnerPrincipalName = fieldownerprincipalname
        ,
        createDelegationTokenRequestRenewers = fieldrenewers
        ,
        createDelegationTokenRequestMaxLifetimeMs = fieldmaxlifetimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a CreatableRenewers.
wireMaxSizeCreatableRenewers :: Int -> CreatableRenewers -> Int
wireMaxSizeCreatableRenewers _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (creatableRenewersPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (creatableRenewersPrincipalName msg))
  + 1

-- | Direct-poke encoder for CreatableRenewers.
wirePokeCreatableRenewers :: Int -> Ptr Word8 -> CreatableRenewers -> IO (Ptr Word8)
wirePokeCreatableRenewers version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (creatableRenewersPrincipalType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (creatableRenewersPrincipalName msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for CreatableRenewers.
wirePeekCreatableRenewers :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableRenewers, Ptr Word8)
wirePeekCreatableRenewers version _fp _basePtr p0 endPtr = do
  (f0_principaltype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_principalname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (CreatableRenewers { creatableRenewersPrincipalType = f0_principaltype, creatableRenewersPrincipalName = f1_principalname }, pTagsEnd)

-- | Worst-case wire size of a CreateDelegationTokenRequest.
wireMaxSizeCreateDelegationTokenRequest :: Int -> CreateDelegationTokenRequest -> Int
wireMaxSizeCreateDelegationTokenRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenRequestOwnerPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenRequestOwnerPrincipalName msg))
  + (5 + (case P.unKafkaArray (createDelegationTokenRequestRenewers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableRenewers _version x ) v); P.Null -> 0 }))
  + 8
  + 1

-- | Direct-poke encoder for CreateDelegationTokenRequest.
wirePokeCreateDelegationTokenRequest :: Int -> Ptr Word8 -> CreateDelegationTokenRequest -> IO (Ptr Word8)
wirePokeCreateDelegationTokenRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatableRenewers version p x) p0 (createDelegationTokenRequestRenewers msg)
    p2 <- W.pokeInt64BE p1 (createDelegationTokenRequestMaxLifetimeMs msg)
    pure p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatableRenewers version p x) p0 (createDelegationTokenRequestRenewers msg)
    p2 <- W.pokeInt64BE p1 (createDelegationTokenRequestMaxLifetimeMs msg)
    WP.pokeEmptyTaggedFields p2
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (createDelegationTokenRequestOwnerPrincipalType msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (createDelegationTokenRequestOwnerPrincipalName msg))
    p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatableRenewers version p x) p2 (createDelegationTokenRequestRenewers msg)
    p4 <- W.pokeInt64BE p3 (createDelegationTokenRequestMaxLifetimeMs msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke CreateDelegationTokenRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateDelegationTokenRequest.
wirePeekCreateDelegationTokenRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateDelegationTokenRequest, Ptr Word8)
wirePeekCreateDelegationTokenRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_renewers, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatableRenewers version _fp _basePtr p e) p0 endPtr
    (f1_maxlifetimems, p2) <- W.peekInt64BE p1 endPtr
    pure (CreateDelegationTokenRequest { createDelegationTokenRequestOwnerPrincipalType = P.KafkaString Null, createDelegationTokenRequestOwnerPrincipalName = P.KafkaString Null, createDelegationTokenRequestRenewers = f0_renewers, createDelegationTokenRequestMaxLifetimeMs = f1_maxlifetimems }, p2)
  | version == 2 = do
    (f0_renewers, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatableRenewers version _fp _basePtr p e) p0 endPtr
    (f1_maxlifetimems, p2) <- W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (CreateDelegationTokenRequest { createDelegationTokenRequestOwnerPrincipalType = P.KafkaString Null, createDelegationTokenRequestOwnerPrincipalName = P.KafkaString Null, createDelegationTokenRequestRenewers = f0_renewers, createDelegationTokenRequestMaxLifetimeMs = f1_maxlifetimems }, pTagsEnd)
  | version == 3 = do
    (f0_ownerprincipaltype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_ownerprincipalname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_renewers, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatableRenewers version _fp _basePtr p e) p2 endPtr
    (f3_maxlifetimems, p4) <- W.peekInt64BE p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (CreateDelegationTokenRequest { createDelegationTokenRequestOwnerPrincipalType = f0_ownerprincipaltype, createDelegationTokenRequestOwnerPrincipalName = f1_ownerprincipalname, createDelegationTokenRequestRenewers = f2_renewers, createDelegationTokenRequestMaxLifetimeMs = f3_maxlifetimems }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateDelegationTokenRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec CreateDelegationTokenRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateDelegationTokenRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateDelegationTokenRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateDelegationTokenRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}