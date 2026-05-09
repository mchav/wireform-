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

-- | KafkaMessage instance for DescribeUserScramCredentialsRequest.
instance KafkaMessage DescribeUserScramCredentialsRequest where
  messageApiKey = 50
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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

-- | Worst-case wire size of a UserName.
wireMaxSizeUserName :: Int -> UserName -> Int
wireMaxSizeUserName _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (userNameName msg))
  + 1

-- | Direct-poke encoder for UserName.
wirePokeUserName :: Int -> Ptr Word8 -> UserName -> IO (Ptr Word8)
wirePokeUserName version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (userNameName msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for UserName.
wirePeekUserName :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UserName, Ptr Word8)
wirePeekUserName version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (UserName { userNameName = f0_name }, pTagsEnd)

-- | Worst-case wire size of a DescribeUserScramCredentialsRequest.
wireMaxSizeDescribeUserScramCredentialsRequest :: Int -> DescribeUserScramCredentialsRequest -> Int
wireMaxSizeDescribeUserScramCredentialsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeUserScramCredentialsRequestUsers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeUserName _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeUserScramCredentialsRequest.
wirePokeDescribeUserScramCredentialsRequest :: Int -> Ptr Word8 -> DescribeUserScramCredentialsRequest -> IO (Ptr Word8)
wirePokeDescribeUserScramCredentialsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeUserName version p x) p0 (describeUserScramCredentialsRequestUsers msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeUserScramCredentialsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeUserScramCredentialsRequest.
wirePeekDescribeUserScramCredentialsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeUserScramCredentialsRequest, Ptr Word8)
wirePeekDescribeUserScramCredentialsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_users, p1) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekUserName version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeUserScramCredentialsRequest { describeUserScramCredentialsRequestUsers = f0_users }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeUserScramCredentialsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeUserScramCredentialsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeUserScramCredentialsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeUserScramCredentialsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeUserScramCredentialsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}