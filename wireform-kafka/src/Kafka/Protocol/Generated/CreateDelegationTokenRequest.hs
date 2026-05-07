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
import Data.Bytes.Get (MonadGet)
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