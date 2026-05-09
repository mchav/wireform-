{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListTransactionsRequest
Description : Kafka ListTransactionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 66.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListTransactionsRequest
  (
    ListTransactionsRequest(..),
    encodeListTransactionsRequest,
    decodeListTransactionsRequest,
    maxListTransactionsRequestVersion
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




data ListTransactionsRequest = ListTransactionsRequest
  {

  -- | The transaction states to filter by: if empty, all transactions are returned; if non-empty, then onl

  -- Versions: 0+
  listTransactionsRequestStateFilters :: !(KafkaArray (KafkaString))
,

  -- | The producerIds to filter by: if empty, all transactions will be returned; if non-empty, only transa

  -- Versions: 0+
  listTransactionsRequestProducerIdFilters :: !(KafkaArray (Int64))
,

  -- | Duration (in millis) to filter by: if < 0, all transactions will be returned; otherwise, only transa

  -- Versions: 1+
  listTransactionsRequestDurationFilter :: !(Int64)
,

  -- | The transactional ID regular expression pattern to filter by: if it is empty or null, all transactio

  -- Versions: 2+
  listTransactionsRequestTransactionalIdPattern :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListTransactionsRequest.
maxListTransactionsRequestVersion :: Int16
maxListTransactionsRequestVersion = 2

-- | KafkaMessage instance for ListTransactionsRequest.
instance KafkaMessage ListTransactionsRequest where
  messageApiKey = 66
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Encode ListTransactionsRequest with the given API version.
encodeListTransactionsRequest :: MonadPut m => E.ApiVersion -> ListTransactionsRequest -> m ()
encodeListTransactionsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (listTransactionsRequestDurationFilter msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (listTransactionsRequestDurationFilter msg)
      serialize (toCompactString (listTransactionsRequestTransactionalIdPattern msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListTransactionsRequest with the given API version.
decodeListTransactionsRequest :: MonadGet m => E.ApiVersion -> m ListTransactionsRequest
decodeListTransactionsRequest version
  | version == 0 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = (-1)
        ,
        listTransactionsRequestTransactionalIdPattern = P.KafkaString Null
        }

  | version == 1 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fielddurationfilter <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = fielddurationfilter
        ,
        listTransactionsRequestTransactionalIdPattern = P.KafkaString Null
        }

  | version == 2 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fielddurationfilter <- deserialize
      fieldtransactionalidpattern <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = fielddurationfilter
        ,
        listTransactionsRequestTransactionalIdPattern = fieldtransactionalidpattern
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a ListTransactionsRequest.
wireMaxSizeListTransactionsRequest :: Int -> ListTransactionsRequest -> Int
wireMaxSizeListTransactionsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> sum (fmap (\x -> 8 ) v); P.Null -> 0 }))
  + 8
  + WP.compactStringMaxSize (P.toCompactString (listTransactionsRequestTransactionalIdPattern msg))
  + 1

-- | Direct-poke encoder for ListTransactionsRequest.
wirePokeListTransactionsRequest :: Int -> Ptr Word8 -> ListTransactionsRequest -> IO (Ptr Word8)
wirePokeListTransactionsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (listTransactionsRequestStateFilters msg)
    p2 <- WP.pokeVersionedArray version 0 W.pokeInt64BE p1 (listTransactionsRequestProducerIdFilters msg)
    WP.pokeEmptyTaggedFields p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (listTransactionsRequestStateFilters msg)
    p2 <- WP.pokeVersionedArray version 0 W.pokeInt64BE p1 (listTransactionsRequestProducerIdFilters msg)
    p3 <- W.pokeInt64BE p2 (listTransactionsRequestDurationFilter msg)
    WP.pokeEmptyTaggedFields p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (listTransactionsRequestStateFilters msg)
    p2 <- WP.pokeVersionedArray version 0 W.pokeInt64BE p1 (listTransactionsRequestProducerIdFilters msg)
    p3 <- W.pokeInt64BE p2 (listTransactionsRequestDurationFilter msg)
    p4 <- WP.pokeCompactString p3 (P.toCompactString (listTransactionsRequestTransactionalIdPattern msg))
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke ListTransactionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListTransactionsRequest.
wirePeekListTransactionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListTransactionsRequest, Ptr Word8)
wirePeekListTransactionsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_statefilters, p1) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_produceridfilters, p2) <- WP.peekVersionedArray version 0 W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ListTransactionsRequest { listTransactionsRequestStateFilters = f0_statefilters, listTransactionsRequestProducerIdFilters = f1_produceridfilters, listTransactionsRequestDurationFilter = 0, listTransactionsRequestTransactionalIdPattern = P.KafkaString Null }, pTagsEnd)
  | version == 1 = do
    (f0_statefilters, p1) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_produceridfilters, p2) <- WP.peekVersionedArray version 0 W.peekInt64BE p1 endPtr
    (f2_durationfilter, p3) <- W.peekInt64BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ListTransactionsRequest { listTransactionsRequestStateFilters = f0_statefilters, listTransactionsRequestProducerIdFilters = f1_produceridfilters, listTransactionsRequestDurationFilter = f2_durationfilter, listTransactionsRequestTransactionalIdPattern = P.KafkaString Null }, pTagsEnd)
  | version == 2 = do
    (f0_statefilters, p1) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_produceridfilters, p2) <- WP.peekVersionedArray version 0 W.peekInt64BE p1 endPtr
    (f2_durationfilter, p3) <- W.peekInt64BE p2 endPtr
    (f3_transactionalidpattern, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ListTransactionsRequest { listTransactionsRequestStateFilters = f0_statefilters, listTransactionsRequestProducerIdFilters = f1_produceridfilters, listTransactionsRequestDurationFilter = f2_durationfilter, listTransactionsRequestTransactionalIdPattern = f3_transactionalidpattern }, pTagsEnd)
  | otherwise = error $ "wirePeek ListTransactionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ListTransactionsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListTransactionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListTransactionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListTransactionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}