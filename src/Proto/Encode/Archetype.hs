{-# LANGUAGE BangPatterns #-}
-- | Archetype-specialized encode functions for maximum performance.
--
-- Inspired by hyperpb's ~200 archetype thunks: each function is
-- specialized for a specific (field_number, wire_type, field_type)
-- combination, with the tag byte baked in as a compile-time constant.
--
-- These avoid the overhead of:
-- * Runtime tag computation (fieldTag fn wt)
-- * putVarint branch chain for 1-byte tags
-- * Wrapper function call chains (encodeFieldVarint -> putTag -> putVarint)
--
-- Generated code should use these directly for field numbers 1-15.
module Proto.Encode.Archetype
  ( -- * Singular field archetypes (tag byte baked in)
    archVarint
  , archSVarint32
  , archSVarint64
  , archFixed32
  , archFixed64
  , archFloat
  , archDouble
  , archBool
  , archString
  , archBytes
  , archSubmessage

    -- * Repeated field archetypes
  , archRepeatedString
  , archRepeatedSubmessage

    -- * Size archetypes
  , archVarintSize
  , archStringSize
  , archBytesSize
  , archBoolSize
  , archFixed32Size
  , archFixed64Size
  , archSubmessageSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)

import Proto.Wire.Encode (putVarint, putSVarint32, putSVarint64,
  varintSize, zigZag32, zigZag64)

-- | Archetype: varint field with baked tag byte.
-- @archVarint tagByte value@ emits the tag + varint in ~2 instructions
-- for the tag (single B.word8) + the varint.
archVarint :: Word8 -> Word64 -> B.Builder
archVarint !tag !val = B.word8 tag <> putVarint val
{-# INLINE archVarint #-}

archSVarint32 :: Word8 -> Int32 -> B.Builder
archSVarint32 !tag !val = B.word8 tag <> putSVarint32 val
{-# INLINE archSVarint32 #-}

archSVarint64 :: Word8 -> Int64 -> B.Builder
archSVarint64 !tag !val = B.word8 tag <> putSVarint64 val
{-# INLINE archSVarint64 #-}

archFixed32 :: Word8 -> Word32 -> B.Builder
archFixed32 !tag !val = B.word8 tag <> B.word32LE val
{-# INLINE archFixed32 #-}

archFixed64 :: Word8 -> Word64 -> B.Builder
archFixed64 !tag !val = B.word8 tag <> B.word64LE val
{-# INLINE archFixed64 #-}

archFloat :: Word8 -> Float -> B.Builder
archFloat !tag !val = B.word8 tag <> B.floatLE val
{-# INLINE archFloat #-}

archDouble :: Word8 -> Double -> B.Builder
archDouble !tag !val = B.word8 tag <> B.doubleLE val
{-# INLINE archDouble #-}

archBool :: Word8 -> Bool -> B.Builder
archBool !tag True  = B.word8 tag <> B.word8 1
archBool !tag False = B.word8 tag <> B.word8 0
{-# INLINE archBool #-}

-- | String archetype: tag + length varint + UTF-8 bytes.
-- On text >= 2.0, encodeUtf8 is O(1).
archString :: Word8 -> Text -> B.Builder
archString !tag !val =
  let !bs = TE.encodeUtf8 val
  in B.word8 tag <> putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE archString #-}

archBytes :: Word8 -> ByteString -> B.Builder
archBytes !tag !val =
  B.word8 tag <> putVarint (fromIntegral (BS.length val)) <> B.byteString val
{-# INLINE archBytes #-}

-- | Submessage archetype: tag + length varint + payload builder.
-- Takes a pre-computed size and the builder for the submessage body.
archSubmessage :: Word8 -> Int -> B.Builder -> B.Builder
archSubmessage !tag !sz !body =
  B.word8 tag <> putVarint (fromIntegral sz) <> body
{-# INLINE archSubmessage #-}

-- | Repeated string archetype: emits tag + string for each element.
archRepeatedString :: Word8 -> Text -> B.Builder
archRepeatedString = archString
{-# INLINE archRepeatedString #-}

-- | Repeated submessage archetype.
archRepeatedSubmessage :: Word8 -> Int -> B.Builder -> B.Builder
archRepeatedSubmessage = archSubmessage
{-# INLINE archRepeatedSubmessage #-}

-- Size archetypes: compute encoded size with tag included.

archVarintSize :: Word64 -> Int
archVarintSize !val = 1 + varintSize val
{-# INLINE archVarintSize #-}

archStringSize :: Text -> Int
archStringSize !val =
  let !len = BS.length (TE.encodeUtf8 val)
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE archStringSize #-}

archBytesSize :: ByteString -> Int
archBytesSize !val =
  let !len = BS.length val
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE archBytesSize #-}

archBoolSize :: Int
archBoolSize = 2
{-# INLINE archBoolSize #-}

archFixed32Size :: Int
archFixed32Size = 5
{-# INLINE archFixed32Size #-}

archFixed64Size :: Int
archFixed64Size = 9
{-# INLINE archFixed64Size #-}

-- | Submessage size: 1 (tag) + varint(payloadSize) + payloadSize
archSubmessageSize :: Int -> Int
archSubmessageSize !payloadSz = 1 + varintSize (fromIntegral payloadSz) + payloadSz
{-# INLINE archSubmessageSize #-}
