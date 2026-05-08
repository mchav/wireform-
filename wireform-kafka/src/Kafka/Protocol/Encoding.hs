{-# LANGUAGE StrictData #-}

{-|
Module      : Kafka.Protocol.Encoding
Description : Encoding and decoding utilities for Kafka protocol messages
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides utilities for encoding and decoding Kafka protocol messages
with version-aware serialization. Different API versions may have different field
layouts, and this module helps manage that complexity.

= Version-Aware Encoding

Kafka protocol messages support multiple versions, and fields may only be present
in certain version ranges. This module provides helpers for:

* Conditional field encoding/decoding based on version
* Handling default values for missing fields
* Flexible vs non-flexible message format detection

= Message Headers

Every Kafka request and response has a header that includes API metadata:

* API key (which API is being called)
* API version (which version of that API)
* Correlation ID (to match requests with responses)
* Client ID (identifies the client)

-}
module Kafka.Protocol.Encoding
  ( -- * API Version Support
    ApiVersion
  , minVersion
  , maxVersion
  , isFlexibleVersion
  , supportsTaggedFields
    -- * Conditional Encoding
  , whenVersion
  , whenVersionRange
  , encodeFieldIf
  , decodeFieldIf
  , decodeFieldDefault
    -- * Version-Aware Array Handling
  , encodeVersionedArray
  , encodeVersionedNullableArray
  , decodeVersionedArray
  , decodeVersionedNullableArray
    -- * Message Encoding
  , encodeMessage
  , decodeMessage
  , calculateMessageSize
    -- * Request/Response Correlation
  , CorrelationId
  , mkCorrelationId
  , unCorrelationId
  ) where

import Data.Bytes.Get (MonadGet, runGetS)
import Data.Bytes.Put (MonadPut, runPutS)
import Data.Bytes.Serial
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Vector as V
import qualified Kafka.Protocol.Primitives as P

-- | API version number. Each Kafka API supports a range of versions.
type ApiVersion = Int16

-- | Minimum supported version for a field or message.
type MinVersion = ApiVersion

-- | Maximum supported version for a field or message.
type MaxVersion = ApiVersion

-- | Check if a version is at least the minimum version.
minVersion :: MinVersion -> ApiVersion -> Bool
minVersion minV v = v >= minV
{-# INLINE minVersion #-}

-- | Check if a version is at most the maximum version.
maxVersion :: MaxVersion -> ApiVersion -> Bool
maxVersion maxV v = v <= maxV
{-# INLINE maxVersion #-}

-- | Check if a specific version is supported.
whenVersion :: ApiVersion -> ApiVersion -> Bool
whenVersion required actual = required == actual
{-# INLINE whenVersion #-}

-- | Check if the actual version is within a range (inclusive).
whenVersionRange :: MinVersion -> MaxVersion -> ApiVersion -> Bool
whenVersionRange minV maxV actual = actual >= minV && actual <= maxV
{-# INLINE whenVersionRange #-}

-- | Determine if a version uses flexible (compact) encoding.
-- Flexible versions support tagged fields and use compact arrays/strings.
-- Most Kafka APIs become flexible at version 9, but this varies by API.
isFlexibleVersion :: ApiVersion -> ApiVersion -> Bool
isFlexibleVersion flexibleStartVersion version = version >= flexibleStartVersion
{-# INLINE isFlexibleVersion #-}

-- | Check if a version supports tagged fields.
-- Tagged fields are only supported in flexible versions.
supportsTaggedFields :: ApiVersion -> ApiVersion -> Bool
supportsTaggedFields = isFlexibleVersion
{-# INLINE supportsTaggedFields #-}

-- | Conditionally encode a field if it's supported in the given version.
encodeFieldIf :: (Monad m) => Bool -> m () -> m ()
encodeFieldIf True action = action
encodeFieldIf False _ = return ()
{-# INLINE encodeFieldIf #-}

-- | Conditionally decode a field if it's supported in the given version.
-- Returns 'Nothing' if the version doesn't support the field.
decodeFieldIf :: (Monad m) => Bool -> m a -> m (Maybe a)
decodeFieldIf True action = Just <$> action
decodeFieldIf False _ = return Nothing
{-# INLINE decodeFieldIf #-}

-- | Decode a field with a default value if the version doesn't support it.
decodeFieldDefault :: (Monad m) => a -> Bool -> m a -> m a
decodeFieldDefault _defaultValue True action = action
decodeFieldDefault defaultValue False _ = return defaultValue
{-# INLINE decodeFieldDefault #-}

-- | Encode an array of elements using a version-aware encoding function.
-- This is used for arrays of nested structures that have version-dependent fields.
-- For flexible versions (version >= flexibleVersion), uses compact arrays (UVarInt length).
-- For non-flexible versions, uses standard arrays (Int32 length).
--
-- Example:
--
-- > encodeVersionedArray version flexibleVersion encodePartition partitions
encodeVersionedArray :: MonadPut m
                     => ApiVersion
                     -> ApiVersion  -- Flexible version threshold
                     -> (ApiVersion -> a -> m ())
                     -> V.Vector a
                     -> m ()
encodeVersionedArray version flexibleVersion encodeFn arr =
  -- Hoist the version comparison out of the inner loop. Inlined into
  -- the per-message encoders so the encodeFn dictionary disappears.
  if version >= flexibleVersion
    then do
      serialize (P.UVarInt $ fromIntegral (V.length arr) + 1)
      V.mapM_ (encodeFn version) arr
    else do
      serialize (fromIntegral (V.length arr) :: Int32)
      V.mapM_ (encodeFn version) arr
{-# INLINE encodeVersionedArray #-}

-- | Encode a nullable array of elements using a version-aware encoding function.
-- Similar to encodeVersionedArray, but handles null arrays properly.
-- For flexible versions, null is encoded as length 0; for non-flexible versions, as length -1.
encodeVersionedNullableArray :: MonadPut m
                              => ApiVersion
                              -> ApiVersion  -- Flexible version threshold
                              -> (ApiVersion -> a -> m ())
                              -> P.KafkaArray a
                              -> m ()
encodeVersionedNullableArray version flexibleVersion encodeFn karr =
  case P.unKafkaArray karr of
    P.Null ->
      if version >= flexibleVersion
        then serialize (P.UVarInt 0)
        else serialize ((-1) :: Int32)
    P.NotNull arr ->
      if version >= flexibleVersion
        then do
          serialize (P.UVarInt $ fromIntegral (V.length arr) + 1)
          V.mapM_ (encodeFn version) arr
        else do
          serialize (fromIntegral (V.length arr) :: Int32)
          V.mapM_ (encodeFn version) arr
{-# INLINE encodeVersionedNullableArray #-}

-- | Decode an array of elements using a version-aware decoding function.
-- This is used for arrays of nested structures that have version-dependent fields.
-- For flexible versions (version >= flexibleVersion), uses compact arrays (UVarInt length).
-- For non-flexible versions, uses standard arrays (Int32 length).
--
-- Example:
--
-- > partitions <- decodeVersionedArray version flexibleVersion decodePartition
decodeVersionedArray :: (MonadGet m, MonadFail m)
                     => ApiVersion
                     -> ApiVersion  -- Flexible version threshold
                     -> (ApiVersion -> m a)
                     -> m (V.Vector a)
decodeVersionedArray version flexibleVersion decodeFn =
  if version >= flexibleVersion
    then do
      P.UVarInt len <- deserialize
      if len == 0
        then return V.empty
        -- V.replicateM grows a mutable buffer of the known final size,
        -- which avoids the [1..n] / V.fromList double allocation that
        -- the older `forM [1..n] ...` form had.
        else V.replicateM (fromIntegral len - 1) (decodeFn version)
    else do
      len <- deserialize :: MonadGet m => m Int32
      V.replicateM (fromIntegral (max 0 len)) (decodeFn version)
{-# INLINE decodeVersionedArray #-}

-- | Decode a nullable array of elements using a version-aware decoding function.
-- Similar to decodeVersionedArray, but returns KafkaArray to properly handle null arrays.
-- For flexible versions, null is encoded as length 0; for non-flexible versions, as length -1.
decodeVersionedNullableArray :: (MonadGet m, MonadFail m)
                              => ApiVersion
                              -> ApiVersion  -- Flexible version threshold
                              -> (ApiVersion -> m a)
                              -> m (P.KafkaArray a)
decodeVersionedNullableArray version flexibleVersion decodeFn =
  if version >= flexibleVersion
    then do
      -- Length 0 = null, Length 1 = empty, Length n+1 = n elements.
      P.UVarInt len <- deserialize
      case len of
        0 -> return (P.KafkaArray P.Null)
        1 -> return (P.mkKafkaArray V.empty)
        _ -> do
          vec <- V.replicateM (fromIntegral len - 1) (decodeFn version)
          return (P.mkKafkaArray vec)
    else do
      len <- deserialize :: MonadGet m => m Int32
      if len == -1
        then return (P.KafkaArray P.Null)
        else do
          vec <- V.replicateM (fromIntegral len) (decodeFn version)
          return (P.mkKafkaArray vec)
{-# INLINE decodeVersionedNullableArray #-}

-- | Encode a complete message to bytes.
-- This includes the message size prefix (4 bytes) followed by the serialized message.
encodeMessage :: Serial a => a -> ByteString
encodeMessage msg = 
  let msgBytes = runPutS (serialize msg)
      msgSize = BS.length msgBytes
      sizeBytes = runPutS (serialize (fromIntegral msgSize :: Int32))
  in sizeBytes <> msgBytes

-- | Decode a complete message from bytes.
-- Expects a 4-byte size prefix followed by the message data.
decodeMessage :: Serial a => ByteString -> Either String a
decodeMessage bs = runGetS deserialize bs

-- | Calculate the serialized size of a message without encoding it.
-- Useful for buffer allocation and batch sizing.
calculateMessageSize :: Serial a => a -> Int
calculateMessageSize msg = BS.length (runPutS (serialize msg))

-- -----------------------------------------------------------------------------
-- Request/Response Correlation
-- -----------------------------------------------------------------------------

-- | Correlation ID used to match requests with responses.
-- The client assigns a unique correlation ID to each request,
-- and the broker echoes it back in the response.
newtype CorrelationId = CorrelationId { unCorrelationId :: Int32 }
  deriving (Eq, Show, Ord)

-- | Create a correlation ID from an Int32.
mkCorrelationId :: Int32 -> CorrelationId
mkCorrelationId = CorrelationId

instance Serial CorrelationId where
  serialize (CorrelationId cid) = serialize cid
  deserialize = CorrelationId <$> deserialize

