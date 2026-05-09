{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListTransactionsResponse
Description : Kafka ListTransactionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 66.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListTransactionsResponse
  (
    ListTransactionsResponse(..),
    TransactionState(..),
    encodeListTransactionsResponse,
    decodeListTransactionsResponse,
    maxListTransactionsResponseVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The current state of the transaction for the transactional id.
data TransactionState = TransactionState
  {

  -- | The transactional id.

  -- Versions: 0+
  transactionStateTransactionalId :: !(KafkaString)
,

  -- | The producer id.

  -- Versions: 0+
  transactionStateProducerId :: !(Int64)
,

  -- | The current transaction state of the producer.

  -- Versions: 0+
  transactionStateTransactionState :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode TransactionState with version-aware field handling.
encodeTransactionState :: MonadPut m => E.ApiVersion -> TransactionState -> m ()
encodeTransactionState version tmsg =
  do
    if version >= 0 then serialize (toCompactString (transactionStateTransactionalId tmsg)) else serialize (transactionStateTransactionalId tmsg)
    serialize (transactionStateProducerId tmsg)
    if version >= 0 then serialize (toCompactString (transactionStateTransactionState tmsg)) else serialize (transactionStateTransactionState tmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TransactionState with version-aware field handling.
decodeTransactionState :: MonadGet m => E.ApiVersion -> m TransactionState
decodeTransactionState version =
  do
    fieldtransactionalid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldproducerid <- deserialize
    fieldtransactionstate <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TransactionState
      {
      transactionStateTransactionalId = fieldtransactionalid
      ,
      transactionStateProducerId = fieldproducerid
      ,
      transactionStateTransactionState = fieldtransactionstate
      }



data ListTransactionsResponse = ListTransactionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  listTransactionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  listTransactionsResponseErrorCode :: !(Int16)
,

  -- | Set of state filters provided in the request which were unknown to the transaction coordinator.

  -- Versions: 0+
  listTransactionsResponseUnknownStateFilters :: !(KafkaArray (KafkaString))
,

  -- | The current state of the transaction for the transactional id.

  -- Versions: 0+
  listTransactionsResponseTransactionStates :: !(KafkaArray (TransactionState))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListTransactionsResponse.
maxListTransactionsResponseVersion :: Int16
maxListTransactionsResponseVersion = 2

-- | KafkaMessage instance for ListTransactionsResponse.
instance KafkaMessage ListTransactionsResponse where
  messageApiKey = 66
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Encode ListTransactionsResponse with the given API version.
encodeListTransactionsResponse :: MonadPut m => E.ApiVersion -> ListTransactionsResponse -> m ()
encodeListTransactionsResponse version msg
  | version >= 0 && version <= 2 =
    do
      serialize (listTransactionsResponseThrottleTimeMs msg)
      serialize (listTransactionsResponseErrorCode msg)
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsResponseUnknownStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeTransactionState (case P.unKafkaArray (listTransactionsResponseTransactionStates msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListTransactionsResponse with the given API version.
decodeListTransactionsResponse :: MonadGet m => E.ApiVersion -> m ListTransactionsResponse
decodeListTransactionsResponse version
  | version >= 0 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldunknownstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldtransactionstates <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTransactionState
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsResponse
        {
        listTransactionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listTransactionsResponseErrorCode = fielderrorcode
        ,
        listTransactionsResponseUnknownStateFilters = fieldunknownstatefilters
        ,
        listTransactionsResponseTransactionStates = fieldtransactionstates
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a TransactionState.
wireMaxSizeTransactionState :: Int -> TransactionState -> Int
wireMaxSizeTransactionState _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (transactionStateTransactionalId msg))
  + 8
  + WP.compactStringMaxSize (P.toCompactString (transactionStateTransactionState msg))
  + 1

-- | Direct-poke encoder for TransactionState.
wirePokeTransactionState :: Int -> Ptr Word8 -> TransactionState -> IO (Ptr Word8)
wirePokeTransactionState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (transactionStateTransactionalId msg))
  p2 <- W.pokeInt64BE p1 (transactionStateProducerId msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (transactionStateTransactionState msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TransactionState.
wirePeekTransactionState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TransactionState, Ptr Word8)
wirePeekTransactionState version _fp _basePtr p0 endPtr = do
  (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
  (f2_transactionstate, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TransactionState { transactionStateTransactionalId = f0_transactionalid, transactionStateProducerId = f1_producerid, transactionStateTransactionState = f2_transactionstate }, pTagsEnd)

-- | Worst-case wire size of a ListTransactionsResponse.
wireMaxSizeListTransactionsResponse :: Int -> ListTransactionsResponse -> Int
wireMaxSizeListTransactionsResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (listTransactionsResponseUnknownStateFilters msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (listTransactionsResponseTransactionStates msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTransactionState _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListTransactionsResponse.
wirePokeListTransactionsResponse :: Int -> Ptr Word8 -> ListTransactionsResponse -> IO (Ptr Word8)
wirePokeListTransactionsResponse version basePtr msg
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listTransactionsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (listTransactionsResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p2 (listTransactionsResponseUnknownStateFilters msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTransactionState version p x) p3 (listTransactionsResponseTransactionStates msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke ListTransactionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ListTransactionsResponse.
wirePeekListTransactionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListTransactionsResponse, Ptr Word8)
wirePeekListTransactionsResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_unknownstatefilters, p3) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p2 endPtr
    (f3_transactionstates, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTransactionState version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ListTransactionsResponse { listTransactionsResponseThrottleTimeMs = f0_throttletimems, listTransactionsResponseErrorCode = f1_errorcode, listTransactionsResponseUnknownStateFilters = f2_unknownstatefilters, listTransactionsResponseTransactionStates = f3_transactionstates }, pTagsEnd)
  | otherwise = error $ "wirePeek ListTransactionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListTransactionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListTransactionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListTransactionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListTransactionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}