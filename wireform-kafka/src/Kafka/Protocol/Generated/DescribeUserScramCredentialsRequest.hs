{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeUserScramCredentialsRequest
Description : Kafka DescribeUserScramCredentialsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 50.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeUserScramCredentialsRequest
  (
    DescribeUserScramCredentialsRequest(..),
    UserName(..),
    encodeDescribeUserScramCredentialsRequest,
    decodeDescribeUserScramCredentialsRequest,
    maxDescribeUserScramCredentialsRequestVersion
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
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The users to describe, or null/empty to describe all users.
data UserName = UserName
  {

  -- | The user name.

  -- Versions: 0+
  userNameName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode UserName with version-aware field handling.
encodeUserName :: MonadPut m => E.ApiVersion -> UserName -> m ()
encodeUserName version umsg =
  do
    if version >= 0 then serialize (toCompactString (userNameName umsg)) else serialize (userNameName umsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode UserName with version-aware field handling.
decodeUserName :: MonadGet m => E.ApiVersion -> m UserName
decodeUserName version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure UserName
      {
      userNameName = fieldname
      }



data DescribeUserScramCredentialsRequest = DescribeUserScramCredentialsRequest
  {

  -- | The users to describe, or null/empty to describe all users.

  -- Versions: 0+
  describeUserScramCredentialsRequestUsers :: !(KafkaArray (UserName))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeUserScramCredentialsRequest.
maxDescribeUserScramCredentialsRequestVersion :: Int16
maxDescribeUserScramCredentialsRequestVersion = 0

-- | Encode DescribeUserScramCredentialsRequest with the given API version.
encodeDescribeUserScramCredentialsRequest :: MonadPut m => E.ApiVersion -> DescribeUserScramCredentialsRequest -> m ()
encodeDescribeUserScramCredentialsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedNullableArray version 0 encodeUserName (describeUserScramCredentialsRequestUsers msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeUserScramCredentialsRequest with the given API version.
decodeDescribeUserScramCredentialsRequest :: MonadGet m => E.ApiVersion -> m DescribeUserScramCredentialsRequest
decodeDescribeUserScramCredentialsRequest version
  | version == 0 =
    do
      fieldusers <- E.decodeVersionedNullableArray version 0 decodeUserName
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeUserScramCredentialsRequest
        {
        describeUserScramCredentialsRequestUsers = fieldusers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeUserScramCredentialsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
