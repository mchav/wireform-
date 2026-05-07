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