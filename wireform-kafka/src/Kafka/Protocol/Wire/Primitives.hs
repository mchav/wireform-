{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Protocol.Wire.Primitives
Description : 'Wire' instances for Kafka-protocol primitive types

Layered on "Kafka.Protocol.Wire" — the primitive byte / int helpers
live there. This module wraps the Kafka-specific length-prefixed
encodings (strings, bytes, arrays, UUIDs, tagged fields, varints in
their newtype wrappers) into 'Wire' instances /and/ exposes a small
set of @poke*@ / @peek*@ helpers the code generator drops directly
into the per-message encoders (skipping a typeclass dispatch).

The @C@ in @CompactString@ / @CompactBytes@ / @CompactArray@ marks
the flexible-version encoding: lengths are @UVarInt@ rather than
fixed-width and a length of 0 indicates null (so non-null lengths
are encoded as @len + 1@). The 'Wire' instances dispatch on the
newtype constructor; the codegen picks compact vs. non-compact
based on the API version.
-}
module Kafka.Protocol.Wire.Primitives
  ( -- * String / bytes / array helpers
    pokeKafkaString
  , peekKafkaString
  , pokeCompactString
  , peekCompactString
  , pokeNullableKafkaString
  , peekNullableKafkaString
  , pokeNullableCompactString
  , peekNullableCompactString
    -- * Bytes
  , pokeKafkaBytes
  , peekKafkaBytes
  , pokeCompactBytes
  , peekCompactBytes
  , pokeNullableKafkaBytes
  , peekNullableKafkaBytes
  , pokeNullableCompactBytes
  , peekNullableCompactBytes
    -- * UUID
  , pokeKafkaUuid
  , peekKafkaUuid
    -- * Arrays (length prefix only — element pokes supplied by caller)
  , pokeKafkaArrayLen
  , peekKafkaArrayLen
  , pokeCompactArrayLen
  , peekCompactArrayLen
  , pokeNullableArrayLen
  , peekNullableArrayLen
  , pokeNullableCompactArrayLen
  , peekNullableCompactArrayLen
    -- * Tagged fields
  , pokeEmptyTaggedFields
  , peekTaggedFieldsCount
  , skipTaggedFieldsBody
  , peekAndSkipTaggedFields
    -- * Estimators (called by 'wireMaxSize' implementations)
  , kafkaStringMaxSize
  , kafkaBytesMaxSize
  , compactStringMaxSize
  , compactBytesMaxSize
  , compactArrayHeaderMaxSize
  ) where

import Control.Exception (throwIO)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, plusPtr)

import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Wire

----------------------------------------------------------------------
-- Strings (non-compact)
----------------------------------------------------------------------

-- | Upper bound on the bytes a 'P.KafkaString' will occupy: 2 byte
-- length + UTF-8 length of the contained text.
{-# INLINE kafkaStringMaxSize #-}
kafkaStringMaxSize :: P.KafkaString -> Int
kafkaStringMaxSize (P.KafkaString P.Null) = 2
kafkaStringMaxSize (P.KafkaString (P.NotNull t)) =
  -- UTF-8 worst-case is 4 bytes per code point; over-estimating is
  -- the right call for a buffer-sizing hint.
  2 + 4 * 1 * fromIntegral (sizeOfText t)

sizeOfText :: Text -> Int
sizeOfText t = BS.length (TE.encodeUtf8 t)

{-# INLINE pokeKafkaString #-}
pokeKafkaString :: Ptr Word8 -> P.KafkaString -> IO (Ptr Word8)
pokeKafkaString p (P.KafkaString P.Null) = pokeInt16BE p (-1)
pokeKafkaString p (P.KafkaString (P.NotNull t)) = do
  let !bs = TE.encodeUtf8 t
      !n  = BS.length bs
  p1 <- pokeInt16BE p (fromIntegral n)
  pokeByteString p1 bs

{-# INLINE peekKafkaString #-}
peekKafkaString :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaString, Ptr Word8)
peekKafkaString p endPtr = do
  (len, p1) <- peekInt16BE p endPtr
  if len < 0
    then pure (P.KafkaString P.Null, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len)
      let !t = TE.decodeUtf8 bs
      pure (P.mkKafkaString t, p2)

-- | Always-non-null version (length is encoded as a non-negative
-- Int16). Used by fields that are explicitly required.
pokeNullableKafkaString :: Ptr Word8 -> P.KafkaString -> IO (Ptr Word8)
pokeNullableKafkaString = pokeKafkaString

peekNullableKafkaString :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaString, Ptr Word8)
peekNullableKafkaString = peekKafkaString

----------------------------------------------------------------------
-- Compact strings
----------------------------------------------------------------------

{-# INLINE compactStringMaxSize #-}
compactStringMaxSize :: P.CompactString -> Int
compactStringMaxSize cs = case P.unCompactString cs of
  P.Null      -> 1
  P.NotNull t -> 5 + sizeOfText t  -- 5-byte UVarInt worst case + UTF-8 length

{-# INLINE pokeCompactString #-}
pokeCompactString :: Ptr Word8 -> P.CompactString -> IO (Ptr Word8)
pokeCompactString p cs = case P.unCompactString cs of
  P.Null      -> pokeUVarInt p 0
  P.NotNull t -> do
    let !bs = TE.encodeUtf8 t
        !n  = BS.length bs
    p1 <- pokeUVarInt p (fromIntegral n + 1)
    pokeByteString p1 bs

{-# INLINE peekCompactString #-}
peekCompactString :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactString, Ptr Word8)
peekCompactString p endPtr = do
  (len, p1) <- peekUVarInt p endPtr
  if len == 0
    then pure (compactStringNull, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len - 1)
      let !t = TE.decodeUtf8 bs
      pure (P.mkCompactString t, p2)

-- | A compact string carrying 'P.Null'. Defined here because the
-- 'P.CompactString' constructor is intentionally hidden by
-- "Kafka.Protocol.Primitives"; we use the public @mk@ helpers in
-- the @NotNull@ branch and round-trip through the
-- 'P.fromCompactString' / 'P.toCompactString' coercion for the
-- null case so we don't reach into the constructor.
compactStringNull :: P.CompactString
compactStringNull = P.toCompactString (P.KafkaString P.Null)

pokeNullableCompactString
  :: Ptr Word8 -> P.CompactString -> IO (Ptr Word8)
pokeNullableCompactString = pokeCompactString

peekNullableCompactString
  :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactString, Ptr Word8)
peekNullableCompactString = peekCompactString

----------------------------------------------------------------------
-- Bytes (non-compact)
----------------------------------------------------------------------

{-# INLINE kafkaBytesMaxSize #-}
kafkaBytesMaxSize :: P.KafkaBytes -> Int
kafkaBytesMaxSize (P.KafkaBytes P.Null) = 4
kafkaBytesMaxSize (P.KafkaBytes (P.NotNull bs)) = 4 + BS.length bs

{-# INLINE pokeKafkaBytes #-}
pokeKafkaBytes :: Ptr Word8 -> P.KafkaBytes -> IO (Ptr Word8)
pokeKafkaBytes p (P.KafkaBytes P.Null) = pokeInt32BE p (-1)
pokeKafkaBytes p (P.KafkaBytes (P.NotNull bs)) = do
  let !n = BS.length bs
  p1 <- pokeInt32BE p (fromIntegral n)
  pokeByteString p1 bs

{-# INLINE peekKafkaBytes #-}
peekKafkaBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaBytes, Ptr Word8)
peekKafkaBytes p endPtr = do
  (len, p1) <- peekInt32BE p endPtr
  if len < 0
    then pure (P.KafkaBytes P.Null, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len)
      pure (P.mkKafkaBytes bs, p2)

pokeNullableKafkaBytes :: Ptr Word8 -> P.KafkaBytes -> IO (Ptr Word8)
pokeNullableKafkaBytes = pokeKafkaBytes

peekNullableKafkaBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaBytes, Ptr Word8)
peekNullableKafkaBytes = peekKafkaBytes

----------------------------------------------------------------------
-- Compact bytes
----------------------------------------------------------------------

{-# INLINE compactBytesMaxSize #-}
compactBytesMaxSize :: P.CompactBytes -> Int
compactBytesMaxSize cb = case P.unCompactBytes cb of
  P.Null       -> 1
  P.NotNull bs -> 5 + BS.length bs

{-# INLINE pokeCompactBytes #-}
pokeCompactBytes :: Ptr Word8 -> P.CompactBytes -> IO (Ptr Word8)
pokeCompactBytes p cb = case P.unCompactBytes cb of
  P.Null       -> pokeUVarInt p 0
  P.NotNull bs -> do
    let !n = BS.length bs
    p1 <- pokeUVarInt p (fromIntegral n + 1)
    pokeByteString p1 bs

{-# INLINE peekCompactBytes #-}
peekCompactBytes :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactBytes, Ptr Word8)
peekCompactBytes p endPtr = do
  (len, p1) <- peekUVarInt p endPtr
  if len == 0
    then pure (compactBytesNull, p1)
    else do
      (bs, p2) <- peekByteString p1 endPtr (fromIntegral len - 1)
      pure (P.mkCompactBytes bs, p2)

compactBytesNull :: P.CompactBytes
compactBytesNull = P.toCompactBytes (P.KafkaBytes P.Null)

pokeNullableCompactBytes
  :: Ptr Word8 -> P.CompactBytes -> IO (Ptr Word8)
pokeNullableCompactBytes = pokeCompactBytes

peekNullableCompactBytes
  :: Ptr Word8 -> Ptr Word8 -> IO (P.CompactBytes, Ptr Word8)
peekNullableCompactBytes = peekCompactBytes

----------------------------------------------------------------------
-- UUID
----------------------------------------------------------------------

{-# INLINE pokeKafkaUuid #-}
pokeKafkaUuid :: Ptr Word8 -> P.KafkaUuid -> IO (Ptr Word8)
pokeKafkaUuid p uuid = do
  let !bs = LBS.toStrict (UUID.toByteString (P.unKafkaUuid uuid))
  pokeByteString p bs

{-# INLINE peekKafkaUuid #-}
peekKafkaUuid :: Ptr Word8 -> Ptr Word8 -> IO (P.KafkaUuid, Ptr Word8)
peekKafkaUuid p endPtr = do
  (bs, p1) <- peekByteString p endPtr 16
  case UUID.fromByteString (LBS.fromStrict bs) of
    Just u  -> pure (P.mkKafkaUuid u, p1)
    Nothing -> throwIO (WireInvalid "invalid 16-byte UUID payload")

----------------------------------------------------------------------
-- Array length prefixes
----------------------------------------------------------------------

-- | Write a non-compact array's Int32 length prefix. The caller is
-- responsible for poking the elements that follow.
{-# INLINE pokeKafkaArrayLen #-}
pokeKafkaArrayLen :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
pokeKafkaArrayLen = pokeInt32BE

{-# INLINE peekKafkaArrayLen #-}
peekKafkaArrayLen :: Ptr Word8 -> Ptr Word8 -> IO (Int32, Ptr Word8)
peekKafkaArrayLen = peekInt32BE

-- | Compact array length prefix: @len + 1@ (encoded as 'UVarInt'),
-- with @0@ meaning null.
{-# INLINE pokeCompactArrayLen #-}
pokeCompactArrayLen :: Ptr Word8 -> Int -> IO (Ptr Word8)
pokeCompactArrayLen p n = pokeUVarInt p (fromIntegral n + 1)

{-# INLINE peekCompactArrayLen #-}
peekCompactArrayLen :: Ptr Word8 -> Ptr Word8 -> IO (Int, Ptr Word8)
peekCompactArrayLen p endPtr = do
  (w, p') <- peekUVarInt p endPtr
  pure (fromIntegral w - 1, p')

-- | Non-compact nullable array: @-1@ for null, @n@ for length.
{-# INLINE pokeNullableArrayLen #-}
pokeNullableArrayLen :: Ptr Word8 -> Maybe Int -> IO (Ptr Word8)
pokeNullableArrayLen p Nothing  = pokeInt32BE p (-1)
pokeNullableArrayLen p (Just n) = pokeInt32BE p (fromIntegral n)

{-# INLINE peekNullableArrayLen #-}
peekNullableArrayLen
  :: Ptr Word8 -> Ptr Word8 -> IO (Maybe Int, Ptr Word8)
peekNullableArrayLen p endPtr = do
  (n, p') <- peekInt32BE p endPtr
  pure (if n < 0 then Nothing else Just (fromIntegral n), p')

{-# INLINE pokeNullableCompactArrayLen #-}
pokeNullableCompactArrayLen :: Ptr Word8 -> Maybe Int -> IO (Ptr Word8)
pokeNullableCompactArrayLen p Nothing  = pokeUVarInt p 0
pokeNullableCompactArrayLen p (Just n) = pokeUVarInt p (fromIntegral n + 1)

{-# INLINE peekNullableCompactArrayLen #-}
peekNullableCompactArrayLen
  :: Ptr Word8 -> Ptr Word8 -> IO (Maybe Int, Ptr Word8)
peekNullableCompactArrayLen p endPtr = do
  (w, p') <- peekUVarInt p endPtr
  pure (if w == 0 then Nothing else Just (fromIntegral w - 1), p')

-- | Worst-case bytes a compact array's length prefix consumes. Used
-- by 'wireMaxSize' implementations to add 5 bytes per array header.
compactArrayHeaderMaxSize :: Int
compactArrayHeaderMaxSize = 5

----------------------------------------------------------------------
-- Tagged fields
----------------------------------------------------------------------

-- | Write the empty-tagged-fields marker (a single 0 byte). The
-- code generator emits this at every flexible message boundary.
{-# INLINE pokeEmptyTaggedFields #-}
pokeEmptyTaggedFields :: Ptr Word8 -> IO (Ptr Word8)
pokeEmptyTaggedFields p = pokeUVarInt p 0

-- | Read a tagged-fields header and return the per-tag count. The
-- code generator's per-message decoder either skips that many
-- (tag, size, bytes) triples or routes them into the message's
-- 'TaggedFields' map; this helper just gives the count.
{-# INLINE peekTaggedFieldsCount #-}
peekTaggedFieldsCount :: Ptr Word8 -> Ptr Word8 -> IO (Int, Ptr Word8)
peekTaggedFieldsCount p endPtr = do
  (n, p') <- peekUVarInt p endPtr
  pure (fromIntegral n, p')

-- | Skip a single @(tag, size, bytes)@ tagged-field triple. Reads
-- the tag UVarInt, the size UVarInt, then advances the cursor past
-- @size@ bytes. Used by 'skipTaggedFieldsBody'.
{-# INLINE skipOneTaggedField #-}
skipOneTaggedField :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
skipOneTaggedField p endPtr = do
  (_tag,  p1) <- peekUVarInt p  endPtr
  (sz,    p2) <- peekUVarInt p1 endPtr
  let !n = fromIntegral sz
  ensureBytes p2 endPtr n "tagged-field payload"
  pure (p2 `plusPtr` n)

-- | Skip @n@ tagged-field entries. The caller is responsible for
-- having already consumed the leading UVarInt count; this just
-- walks the per-entry triples.
skipTaggedFieldsBody :: Int -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
skipTaggedFieldsBody = go
  where
    go !n !p !endPtr
      | n <= 0    = pure p
      | otherwise = do
          p' <- skipOneTaggedField p endPtr
          go (n - 1) p' endPtr

-- | Combined: read the count + skip every entry, returning just the
-- cursor past the last byte consumed. The codegen-emitted Wire peek
-- uses this for messages that don't surface tagged fields to the
-- caller (i.e. discards the body, like the legacy
-- @_ <- (deserialize :: m TaggedFields)@ pattern).
{-# INLINE peekAndSkipTaggedFields #-}
peekAndSkipTaggedFields :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
peekAndSkipTaggedFields p endPtr = do
  (n, p') <- peekTaggedFieldsCount p endPtr
  skipTaggedFieldsBody n p' endPtr

----------------------------------------------------------------------
-- Wire instances
----------------------------------------------------------------------

instance Wire P.KafkaString where
  wireMaxSize = kafkaStringMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaString
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaString
  {-# INLINE wirePeek #-}

instance Wire P.CompactString where
  wireMaxSize = compactStringMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeCompactString
  {-# INLINE wirePoke #-}
  wirePeek = peekCompactString
  {-# INLINE wirePeek #-}

instance Wire P.KafkaBytes where
  wireMaxSize = kafkaBytesMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaBytes
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaBytes
  {-# INLINE wirePeek #-}

instance Wire P.CompactBytes where
  wireMaxSize = compactBytesMaxSize
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeCompactBytes
  {-# INLINE wirePoke #-}
  wirePeek = peekCompactBytes
  {-# INLINE wirePeek #-}

instance Wire P.KafkaUuid where
  wireMaxSize _ = 16
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeKafkaUuid
  {-# INLINE wirePoke #-}
  wirePeek = peekKafkaUuid
  {-# INLINE wirePeek #-}

instance Wire P.VarInt where
  wireMaxSize _ = 5
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.VarInt i) = pokeVarInt p i
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (i, p') <- peekVarInt p endPtr
    pure (P.VarInt i, p')
  {-# INLINE wirePeek #-}

instance Wire P.VarLong where
  wireMaxSize _ = 10
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.VarLong i) = pokeVarLong p i
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (i, p') <- peekVarLong p endPtr
    pure (P.VarLong i, p')
  {-# INLINE wirePeek #-}

instance Wire P.UVarInt where
  wireMaxSize _ = 5
  {-# INLINE wireMaxSize #-}
  wirePoke p (P.UVarInt w) = pokeUVarInt p w
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (w, p') <- peekUVarInt p endPtr
    pure (P.UVarInt w, p')
  {-# INLINE wirePeek #-}
