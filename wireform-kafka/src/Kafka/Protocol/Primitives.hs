{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : Kafka.Protocol.Primitives
Description : Core primitive types for the Kafka wire protocol
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides the fundamental primitive types used in the Kafka wire protocol,
including fixed-width integers, variable-length integers, strings, and other basic types.

All types implement the 'Serial' type class from the @bytes@ package, which provides
a unified interface for both @binary@ and @cereal@ serialization libraries.

The Kafka protocol uses big-endian (network byte order) for all integer types.

= Variable-Length Integers

Kafka uses variable-length encoding for integers in "compact" message formats
(introduced in flexible versions, KIP-482):

* 'VarInt' - Variable-length signed 32-bit integer using ZigZag encoding
* 'VarLong' - Variable-length signed 64-bit integer using ZigZag encoding

Variable-length integers use a continuation bit encoding where the most significant
bit indicates whether more bytes follow.

= Strings and Arrays

Kafka strings and arrays have length prefixes:

* Non-compact: 16-bit or 32-bit signed length prefix
* Compact: Variable-length unsigned integer prefix
* Nullable: Length of -1 indicates null

= Tagged Fields

Flexible message versions (9+ for most messages) support tagged fields, which allow
backward-compatible protocol evolution. Tagged fields are encoded as:

1. Tag (VarInt)
2. Size (VarInt)
3. Data (variable length)

-}
module Kafka.Protocol.Primitives
  ( -- * Fixed-Width Integer Types
    Int8
  , Int16
  , Int32
  , Int64
  , Word32
    -- * Variable-Length Integer Types
  , VarInt(..)
  , VarLong(..)
  , UVarInt(..)
    -- * String Types
  , KafkaString(..)
  , mkKafkaString
  , unKafkaString
  , CompactString
  , mkCompactString
  , unCompactString
    -- * UUID Type
  , KafkaUuid
  , mkKafkaUuid
  , unKafkaUuid
  , nullUuid
    -- * Array Types  
  , KafkaArray(..)
  , mkKafkaArray
  , unKafkaArray
  , CompactArray(..)
  , mkCompactArray
  , unCompactArray
    -- * Bytes Types
  , KafkaBytes(..)
  , mkKafkaBytes
  , unKafkaBytes
  , CompactBytes
  , mkCompactBytes
  , unCompactBytes
    -- * Tagged Fields
  , TaggedFields
  , TaggedField(..)
  , emptyTaggedFields
  , lookupTaggedField
  , insertTaggedField
    -- * Nullable Types
  , Nullable(..)
  , toNullable
  , fromNullable
    -- * Encoding Helpers
  , putVarInt
  , getVarInt
  , putVarLong
  , getVarLong
  , putUVarInt
  , getUVarInt
    -- * Type Conversions
  , toCompactString
  , fromCompactString
  , toCompactBytes
  , fromCompactBytes
  , toCompactArray
  , fromCompactArray
  ) where

import Control.Monad (replicateM)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (MonadGet, getByteString, getWord8, runGetS)
import Data.Bytes.Put (MonadPut, putByteString, putWord8, runPutS)
import Data.Bytes.Serial
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)

-- | Represents a value that may be null in the Kafka protocol.
-- Many Kafka fields can be explicitly null (encoded as -1 length).
data Nullable a = Null | NotNull a
  deriving (Eq, Show, Ord, Generic)

-- | Convert a 'Maybe' to a 'Nullable'.
toNullable :: Maybe a -> Nullable a
toNullable Nothing = Null
toNullable (Just x) = NotNull x

-- | Convert a 'Nullable' to a 'Maybe'.
fromNullable :: Nullable a -> Maybe a
fromNullable Null = Nothing
fromNullable (NotNull x) = Just x

-- | Serial instance for Nullable types.
-- Encodes Null as a byte value 0, and NotNull as byte value 1 followed by the serialized value.
-- This is used for nullable struct types.
instance Serial a => Serial (Nullable a) where
  serialize Null = putWord8 0
  serialize (NotNull x) = do
    putWord8 1
    serialize x
  
  deserialize = do
    flag <- getWord8
    case flag of
      0 -> return Null
      1 -> NotNull <$> deserialize
      _ -> fail $ "Invalid nullable flag: " ++ show flag

-- -----------------------------------------------------------------------------
-- Variable-Length Integers
-- -----------------------------------------------------------------------------

-- | Variable-length signed 32-bit integer using ZigZag encoding.
-- Used in compact message formats. Values are encoded using a continuation
-- bit scheme where the MSB indicates more bytes follow.
--
-- ZigZag encoding maps signed integers to unsigned integers:
-- 
-- > 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4, ...
newtype VarInt = VarInt { unVarInt :: Int32 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Variable-length signed 64-bit integer using ZigZag encoding.
newtype VarLong = VarLong { unVarLong :: Int64 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Variable-length unsigned 32-bit integer.
-- Used for compact array and string lengths.
newtype UVarInt = UVarInt { unUVarInt :: Word32 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Encode a signed 32-bit integer as ZigZag encoded VarInt.
zigZagEncode32 :: Int32 -> Word32
zigZagEncode32 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 31)

-- | Decode a ZigZag encoded value to a signed 32-bit integer.
zigZagDecode32 :: Word32 -> Int32
zigZagDecode32 n = fromIntegral $ (n `shiftR` 1) `xor` (-(n .&. 1))

-- | Encode a signed 64-bit integer as ZigZag encoded VarLong.
zigZagEncode64 :: Int64 -> Word64
zigZagEncode64 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 63)

-- | Decode a ZigZag encoded value to a signed 64-bit integer.
zigZagDecode64 :: Word64 -> Int64
zigZagDecode64 n = fromIntegral $ (n `shiftR` 1) `xor` (-(n .&. 1))

-- | Encode a VarInt to bytes.
putVarInt :: MonadPut m => VarInt -> m ()
putVarInt (VarInt n) = putUVarInt (UVarInt $ zigZagEncode32 n)

-- | Decode a VarInt from bytes.
getVarInt :: MonadGet m => m VarInt
getVarInt = VarInt . zigZagDecode32 . unUVarInt <$> getUVarInt

-- | Encode a VarLong to bytes.
putVarLong :: MonadPut m => VarLong -> m ()
putVarLong (VarLong n) = putUVarLong64 (zigZagEncode64 n)

-- | Decode a VarLong from bytes.
getVarLong :: MonadGet m => m VarLong
getVarLong = VarLong . zigZagDecode64 <$> getUVarLong64

-- | Encode an unsigned variable-length integer.
putUVarInt :: MonadPut m => UVarInt -> m ()
putUVarInt (UVarInt n) = go n
  where
    go v
      | v < 128 = putWord8 (fromIntegral v)
      | otherwise = do
          putWord8 (fromIntegral (v .&. 0x7F) .|. 0x80)
          go (v `shiftR` 7)
{-# INLINE putUVarInt #-}

-- | Decode an unsigned variable-length integer.
getUVarInt :: MonadGet m => m UVarInt
getUVarInt = UVarInt <$> go 0 0
  where
    go :: MonadGet m => Int -> Word32 -> m Word32
    go shift acc = do
      b <- getWord8
      let value = acc .|. ((fromIntegral (b .&. 0x7F)) `shiftL` shift)
      if b .&. 0x80 == 0
        then return value
        else go (shift + 7) value
{-# INLINE getUVarInt #-}

-- | Encode an unsigned 64-bit variable-length integer (for VarLong).
putUVarLong64 :: MonadPut m => Word64 -> m ()
putUVarLong64 n = go n
  where
    go v
      | v < 128 = putWord8 (fromIntegral v)
      | otherwise = do
          putWord8 (fromIntegral (v .&. 0x7F) .|. 0x80)
          go (v `shiftR` 7)
{-# INLINE putUVarLong64 #-}

-- | Decode an unsigned 64-bit variable-length integer (for VarLong).
getUVarLong64 :: MonadGet m => m Word64
getUVarLong64 = go 0 0
  where
    go :: MonadGet m => Int -> Word64 -> m Word64
    go shift acc = do
      b <- getWord8
      let value = acc .|. ((fromIntegral (b .&. 0x7F)) `shiftL` shift)
      if b .&. 0x80 == 0
        then return value
        else go (shift + 7) value
{-# INLINE getUVarLong64 #-}

instance Serial VarInt where
  serialize = putVarInt
  {-# INLINE serialize #-}
  deserialize = getVarInt
  {-# INLINE deserialize #-}

instance Serial VarLong where
  serialize = putVarLong
  {-# INLINE serialize #-}
  deserialize = getVarLong
  {-# INLINE deserialize #-}

instance Serial UVarInt where
  serialize = putUVarInt
  {-# INLINE serialize #-}
  deserialize = getUVarInt
  {-# INLINE deserialize #-}

-- -----------------------------------------------------------------------------
-- String Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka string with 16-bit length prefix (non-compact format).
-- A length of -1 indicates a null string.
newtype KafkaString = KafkaString { unKafkaString :: Nullable Text }
  deriving (Eq, Show, Generic)

-- | Create a non-null Kafka string.
mkKafkaString :: Text -> KafkaString
mkKafkaString = KafkaString . NotNull

instance Serial KafkaString where
  serialize (KafkaString Null) = serialize (-1 :: Int16)
  serialize (KafkaString (NotNull t)) = do
    let bs = T.encodeUtf8 t
    serialize (fromIntegral (BS.length bs) :: Int16)
    putByteString bs
  {-# INLINE serialize #-}

  deserialize = do
    len <- deserialize
    if (len :: Int16) < 0
      then return (KafkaString Null)
      else do
        bs <- getByteString (fromIntegral len)
        return $ KafkaString (NotNull $ T.decodeUtf8 bs)
  {-# INLINE deserialize #-}

-- | Compact Kafka string with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactString = CompactString { unCompactString :: Nullable Text }
  deriving (Eq, Show, Generic)

-- | Create a non-null compact Kafka string.
mkCompactString :: Text -> CompactString
mkCompactString = CompactString . NotNull

instance Serial CompactString where
  serialize (CompactString Null) = serialize (UVarInt 0)
  serialize (CompactString (NotNull t)) = do
    let bs = T.encodeUtf8 t
    serialize (UVarInt $ fromIntegral (BS.length bs) + 1)
    putByteString bs
  {-# INLINE serialize #-}

  deserialize = do
    UVarInt len <- deserialize
    if len == 0
      then return (CompactString Null)
      else do
        bs <- getByteString (fromIntegral len - 1)
        return $ CompactString (NotNull $ T.decodeUtf8 bs)
  {-# INLINE deserialize #-}

-- -----------------------------------------------------------------------------
-- UUID Type
-- -----------------------------------------------------------------------------

-- | Kafka UUID (128-bit identifier).
-- The null UUID is all zeros.
newtype KafkaUuid = KafkaUuid { unKafkaUuid :: UUID }
  deriving (Eq, Show, Ord, Generic)

-- | Create a Kafka UUID from a UUID.
mkKafkaUuid :: UUID -> KafkaUuid
mkKafkaUuid = KafkaUuid

-- | The null UUID (all zeros).
nullUuid :: KafkaUuid
nullUuid = KafkaUuid UUID.nil

instance Serial KafkaUuid where
  serialize (KafkaUuid uuid) = do
    let bs = BL.toStrict $ UUID.toByteString uuid
    putByteString bs
  
  deserialize = do
    bs <- getByteString 16
    case UUID.fromByteString (BL.fromStrict bs) of
      Just uuid -> return (KafkaUuid uuid)
      Nothing -> fail "Invalid UUID bytes"

-- -----------------------------------------------------------------------------
-- Array Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka array with 32-bit length prefix (non-compact format).
-- A length of -1 indicates a null array.
newtype KafkaArray a = KafkaArray { unKafkaArray :: Nullable (Vector a) }
  deriving (Eq, Show, Generic)

-- | Create a non-null Kafka array.
mkKafkaArray :: Vector a -> KafkaArray a
mkKafkaArray = KafkaArray . NotNull

instance Serial a => Serial (KafkaArray a) where
  serialize (KafkaArray Null) = serialize (-1 :: Int32)
  serialize (KafkaArray (NotNull vec)) = do
    serialize (fromIntegral (V.length vec) :: Int32)
    V.mapM_ serialize vec
  {-# INLINE serialize #-}

  deserialize = do
    len <- deserialize
    if (len :: Int32) < 0
      then return (KafkaArray Null)
      -- V.replicateM grows a mutable buffer in one allocation instead
      -- of building a list of length n + V.fromList.
      else KafkaArray . NotNull <$> V.replicateM (fromIntegral (len :: Int32)) deserialize
  {-# INLINE deserialize #-}

-- | Compact Kafka array with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactArray a = CompactArray { unCompactArray :: Nullable (Vector a) }
  deriving (Eq, Show, Generic)

-- | Create a non-null compact Kafka array.
mkCompactArray :: Vector a -> CompactArray a
mkCompactArray = CompactArray . NotNull

instance Serial a => Serial (CompactArray a) where
  serialize (CompactArray Null) = serialize (UVarInt 0)
  serialize (CompactArray (NotNull vec)) = do
    serialize (UVarInt $ fromIntegral (V.length vec) + 1)
    V.mapM_ serialize vec
  {-# INLINE serialize #-}

  deserialize = do
    UVarInt len <- deserialize
    if len == 0
      then return (CompactArray Null)
      else CompactArray . NotNull
             <$> V.replicateM (fromIntegral (len :: Word32) - 1) deserialize
  {-# INLINE deserialize #-}

-- -----------------------------------------------------------------------------
-- Bytes Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka bytes with 32-bit length prefix (non-compact format).
-- A length of -1 indicates null bytes.
newtype KafkaBytes = KafkaBytes { unKafkaBytes :: Nullable ByteString }
  deriving (Eq, Show, Generic)

-- | Create non-null Kafka bytes.
mkKafkaBytes :: ByteString -> KafkaBytes
mkKafkaBytes = KafkaBytes . NotNull

instance Serial KafkaBytes where
  serialize (KafkaBytes Null) = serialize (-1 :: Int32)
  serialize (KafkaBytes (NotNull bs)) = do
    serialize (fromIntegral (BS.length bs) :: Int32)
    putByteString bs
  {-# INLINE serialize #-}

  deserialize = do
    len <- deserialize
    if (len :: Int32) < 0
      then return (KafkaBytes Null)
      else KafkaBytes . NotNull <$> getByteString (fromIntegral len)
  {-# INLINE deserialize #-}

-- | Compact Kafka bytes with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactBytes = CompactBytes { unCompactBytes :: Nullable ByteString }
  deriving (Eq, Show, Generic)

-- | Create non-null compact Kafka bytes.
mkCompactBytes :: ByteString -> CompactBytes
mkCompactBytes = CompactBytes . NotNull

instance Serial CompactBytes where
  serialize (CompactBytes Null) = serialize (UVarInt 0)
  serialize (CompactBytes (NotNull bs)) = do
    serialize (UVarInt $ fromIntegral (BS.length bs) + 1)
    putByteString bs
  {-# INLINE serialize #-}

  deserialize = do
    UVarInt len <- deserialize
    if len == 0
      then return (CompactBytes Null)
      else CompactBytes . NotNull <$> getByteString (fromIntegral len - 1)
  {-# INLINE deserialize #-}

-- -----------------------------------------------------------------------------
-- Tagged Fields
-- -----------------------------------------------------------------------------

-- | A single tagged field, consisting of a tag number and raw bytes.
-- Tagged fields enable backward-compatible protocol evolution.
data TaggedField = TaggedField
  { tagNumber :: !Word32  -- ^ Tag identifier
  , tagData   :: !ByteString  -- ^ Raw field data
  } deriving (Eq, Show, Generic)

-- | Collection of tagged fields, indexed by tag number.
-- Tagged fields appear at the end of flexible message versions.
newtype TaggedFields = TaggedFields { unTaggedFields :: Map Word32 ByteString }
  deriving (Eq, Show, Generic)

-- | Empty tagged fields (no tags present).
emptyTaggedFields :: TaggedFields
emptyTaggedFields = TaggedFields Map.empty

-- | Look up a tagged field by tag number.
lookupTaggedField :: Word32 -> TaggedFields -> Maybe ByteString
lookupTaggedField tag (TaggedFields m) = Map.lookup tag m

-- | Insert or update a tagged field.
insertTaggedField :: Word32 -> ByteString -> TaggedFields -> TaggedFields
insertTaggedField tag bs (TaggedFields m) = TaggedFields (Map.insert tag bs m)

instance Serial TaggedFields where
  serialize (TaggedFields fields) = do
    -- Number of tags
    serialize (UVarInt $ fromIntegral $ Map.size fields)
    -- Each tag and its data
    mapM_ encodeField (Map.toAscList fields)
    where
      encodeField (tag, bs) = do
        serialize (UVarInt tag)  -- Tag number
        serialize (UVarInt $ fromIntegral $ BS.length bs)  -- Size
        putByteString bs  -- Data
  
  deserialize = do
    UVarInt numTags <- deserialize
    fields <- replicateM (fromIntegral numTags) decodeField
    return $ TaggedFields (Map.fromList fields)
    where
      decodeField = do
        UVarInt tag <- deserialize
        UVarInt size <- deserialize
        bs <- getByteString (fromIntegral size)
        return (tag, bs)

-- -----------------------------------------------------------------------------
-- Type Conversions
-- -----------------------------------------------------------------------------

-- | Convert a standard KafkaString to CompactString for flexible encoding.
toCompactString :: KafkaString -> CompactString
toCompactString (KafkaString Null) = CompactString Null
toCompactString (KafkaString (NotNull t)) = CompactString (NotNull t)

-- | Convert a CompactString back to standard KafkaString.
fromCompactString :: CompactString -> KafkaString
fromCompactString (CompactString Null) = KafkaString Null
fromCompactString (CompactString (NotNull t)) = KafkaString (NotNull t)

-- | Convert standard KafkaBytes to CompactBytes for flexible encoding.
toCompactBytes :: KafkaBytes -> CompactBytes
toCompactBytes (KafkaBytes Null) = CompactBytes Null
toCompactBytes (KafkaBytes (NotNull bs)) = CompactBytes (NotNull bs)

-- | Convert CompactBytes back to standard KafkaBytes.
fromCompactBytes :: CompactBytes -> KafkaBytes
fromCompactBytes (CompactBytes Null) = KafkaBytes Null
fromCompactBytes (CompactBytes (NotNull bs)) = KafkaBytes (NotNull bs)

-- | Convert standard KafkaArray to CompactArray for flexible encoding.
toCompactArray :: KafkaArray a -> CompactArray a
toCompactArray (KafkaArray Null) = CompactArray Null
toCompactArray (KafkaArray (NotNull vec)) = CompactArray (NotNull vec)

-- | Convert CompactArray back to standard KafkaArray.
fromCompactArray :: CompactArray a -> KafkaArray a
fromCompactArray (CompactArray Null) = KafkaArray Null
fromCompactArray (CompactArray (NotNull vec)) = KafkaArray (NotNull vec)

