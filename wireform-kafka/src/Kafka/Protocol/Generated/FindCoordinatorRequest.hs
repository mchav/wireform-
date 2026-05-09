{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FindCoordinatorRequest
Description : Kafka FindCoordinatorRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 10.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FindCoordinatorRequest
  (
    FindCoordinatorRequest(..),
    encodeFindCoordinatorRequest,
    decodeFindCoordinatorRequest,
    maxFindCoordinatorRequestVersion
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




data FindCoordinatorRequest = FindCoordinatorRequest
  {

  -- | The coordinator key.

  -- Versions: 0-3
  findCoordinatorRequestKey :: !(KafkaString)
,

  -- | The coordinator key type. (group, transaction, share).

  -- Versions: 1+
  findCoordinatorRequestKeyType :: !(Int8)
,

  -- | The coordinator keys.

  -- Versions: 4+
  findCoordinatorRequestCoordinatorKeys :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FindCoordinatorRequest.
maxFindCoordinatorRequestVersion :: Int16
maxFindCoordinatorRequestVersion = 6

-- | KafkaMessage instance for FindCoordinatorRequest.
instance KafkaMessage FindCoordinatorRequest where
  messageApiKey = 10
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 3

-- | Encode FindCoordinatorRequest with the given API version.
encodeFindCoordinatorRequest :: MonadPut m => E.ApiVersion -> FindCoordinatorRequest -> m ()
encodeFindCoordinatorRequest version msg
  | version == 0 =
    do
      serialize (findCoordinatorRequestKey msg)


  | version == 3 =
    do
      serialize (toCompactString (findCoordinatorRequestKey msg))
      serialize (findCoordinatorRequestKeyType msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (findCoordinatorRequestKey msg)
      serialize (findCoordinatorRequestKeyType msg)


  | version >= 4 && version <= 6 =
    do
      serialize (findCoordinatorRequestKeyType msg)
      E.encodeVersionedArray version 3 (\v s -> if v >= 3 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (findCoordinatorRequestCoordinatorKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FindCoordinatorRequest with the given API version.
decodeFindCoordinatorRequest :: MonadGet m => E.ApiVersion -> m FindCoordinatorRequest
decodeFindCoordinatorRequest version
  | version == 0 =
    do
      fieldkey <- deserialize
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = 0
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldkey <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldkeytype <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version >= 1 && version <= 2 =
    do
      fieldkey <- deserialize
      fieldkeytype <- deserialize
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 6 =
    do
      fieldkeytype <- deserialize
      fieldcoordinatorkeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\v -> if v >= 3 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = P.KafkaString Null
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = fieldcoordinatorkeys
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a FindCoordinatorRequest.
wireMaxSizeFindCoordinatorRequest :: Int -> FindCoordinatorRequest -> Int
wireMaxSizeFindCoordinatorRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (findCoordinatorRequestKey msg))
  + 1
  + (5 + (case P.unKafkaArray (findCoordinatorRequestCoordinatorKeys msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FindCoordinatorRequest.
wirePokeFindCoordinatorRequest :: Int -> Ptr Word8 -> FindCoordinatorRequest -> IO (Ptr Word8)
wirePokeFindCoordinatorRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (findCoordinatorRequestKey msg))
    pure p1
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (findCoordinatorRequestKey msg))
    p2 <- W.pokeWord8 p1 (fromIntegral (findCoordinatorRequestKeyType msg))
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (findCoordinatorRequestKey msg))
    p2 <- W.pokeWord8 p1 (fromIntegral (findCoordinatorRequestKeyType msg))
    pure p2
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (fromIntegral (findCoordinatorRequestKeyType msg))
    p2 <- WP.pokeVersionedArray version 3 (\p s -> if version >= 3 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p1 (findCoordinatorRequestCoordinatorKeys msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke FindCoordinatorRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for FindCoordinatorRequest.
wirePeekFindCoordinatorRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FindCoordinatorRequest, Ptr Word8)
wirePeekFindCoordinatorRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    pure (FindCoordinatorRequest { findCoordinatorRequestKey = f0_key, findCoordinatorRequestKeyType = 0, findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty }, p1)
  | version == 3 = do
    (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_keytype, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (FindCoordinatorRequest { findCoordinatorRequestKey = f0_key, findCoordinatorRequestKeyType = f1_keytype, findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_keytype, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
    pure (FindCoordinatorRequest { findCoordinatorRequestKey = f0_key, findCoordinatorRequestKeyType = f1_keytype, findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty }, p2)
  | version >= 4 && version <= 6 = do
    (f0_keytype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
    (f1_coordinatorkeys, p2) <- WP.peekVersionedArray version 3 (\p e -> if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (FindCoordinatorRequest { findCoordinatorRequestKey = P.KafkaString Null, findCoordinatorRequestKeyType = f0_keytype, findCoordinatorRequestCoordinatorKeys = f1_coordinatorkeys }, pTagsEnd)
  | otherwise = error $ "wirePeek FindCoordinatorRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec FindCoordinatorRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFindCoordinatorRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFindCoordinatorRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFindCoordinatorRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}