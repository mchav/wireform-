-- | High-level decoding interface for protobuf messages.
--
-- This module provides the 'MessageDecode' typeclass and utilities for
-- decoding messages from 'ByteString'. Key performance characteristics:
--
-- * Zero-copy: bytes and string fields reference slices of the input
-- * Lazy submessage decoding: submessage bytes are captured but not parsed
--   until the field is accessed
-- * Packed repeated field support
-- * Efficient unknown field skipping
module Proto.Decode
  ( -- * Decoding typeclass
    MessageDecode (..)

    -- * Running decoders
  , decodeMessage

    -- * Field decoding helpers
  , decodeFieldVarint
  , decodeFieldSVarint32
  , decodeFieldSVarint64
  , decodeFieldFixed32
  , decodeFieldFixed64
  , decodeFieldFloat
  , decodeFieldDouble
  , decodeFieldBool
  , decodeFieldString
  , decodeFieldBytes
  , decodeFieldMessage
  , decodeFieldEnum

    -- * Packed repeated field decoding
  , decodePackedVarint
  , decodePackedFixed32
  , decodePackedFixed64
  , decodePackedFloat
  , decodePackedDouble
  , decodePackedSVarint32
  , decodePackedSVarint64

    -- * Submessage decoding
  , decodeSubmessage

    -- * Re-exports for generated code
  , Decoder
  , DecodeError (..)
  , runDecoder
  , getVarint
  , getTagOr
  , skipField
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Proto.Wire.Decode

-- | Typeclass for types that can be decoded from protobuf wire format.
class MessageDecode a where
  -- | Decode a message from a ByteString, starting from default field values.
  -- The decoder should consume all bytes of the submessage.
  messageDecoder :: Decoder a

-- | Decode a message from a strict 'ByteString'.
decodeMessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeMessage = runDecoder messageDecoder
{-# INLINE decodeMessage #-}

-- | Decode a varint value.
decodeFieldVarint :: Decoder Word64
decodeFieldVarint = getVarint
{-# INLINE decodeFieldVarint #-}

-- | Decode a sint32 value.
decodeFieldSVarint32 :: Decoder Int32
decodeFieldSVarint32 = getSVarint32
{-# INLINE decodeFieldSVarint32 #-}

-- | Decode a sint64 value.
decodeFieldSVarint64 :: Decoder Int64
decodeFieldSVarint64 = getSVarint64
{-# INLINE decodeFieldSVarint64 #-}

-- | Decode a fixed32 value.
decodeFieldFixed32 :: Decoder Word32
decodeFieldFixed32 = getFixed32
{-# INLINE decodeFieldFixed32 #-}

-- | Decode a fixed64 value.
decodeFieldFixed64 :: Decoder Word64
decodeFieldFixed64 = getFixed64
{-# INLINE decodeFieldFixed64 #-}

-- | Decode a float value.
decodeFieldFloat :: Decoder Float
decodeFieldFloat = getFloat
{-# INLINE decodeFieldFloat #-}

-- | Decode a double value.
decodeFieldDouble :: Decoder Double
decodeFieldDouble = getDouble
{-# INLINE decodeFieldDouble #-}

-- | Decode a bool value.
decodeFieldBool :: Decoder Bool
decodeFieldBool = do
  v <- getVarint
  pure (v /= 0)
{-# INLINE decodeFieldBool #-}

-- | Decode a string value (validated UTF-8, zero-copy).
decodeFieldString :: Decoder Text
decodeFieldString = getText
{-# INLINE decodeFieldString #-}

-- | Decode a bytes value (zero-copy).
decodeFieldBytes :: Decoder ByteString
decodeFieldBytes = getByteString
{-# INLINE decodeFieldBytes #-}

-- | Decode a submessage field. The length-delimited bytes are read and then
-- the submessage decoder is run on them.
decodeFieldMessage :: MessageDecode a => Decoder a
decodeFieldMessage = do
  bs <- getLengthDelimited
  case runDecoder messageDecoder bs of
    Left e  -> Decoder $ \_ _ -> DecodeFail (SubMessageError e)
    Right a -> pure a
{-# INLINE decodeFieldMessage #-}

-- | Decode an enum field (as varint, then fromEnum).
decodeFieldEnum :: Enum a => Decoder a
decodeFieldEnum = toEnum . fromIntegral <$> getVarint
{-# INLINE decodeFieldEnum #-}

-- | Decode a submessage from raw bytes.
decodeSubmessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeSubmessage = runDecoder messageDecoder
{-# INLINE decodeSubmessage #-}

-- Packed decoders: decode a length-delimited chunk containing multiple values.

-- | Decode packed varint values.
decodePackedVarint :: Decoder (VU.Vector Word64)
decodePackedVarint = do
  bs <- getLengthDelimited
  case decodeAllVarints bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedVarint #-}

decodeAllVarints :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllVarints bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed fixed32 values.
decodePackedFixed32 :: Decoder (VU.Vector Word32)
decodePackedFixed32 = do
  bs <- getLengthDelimited
  case decodeAllFixed32 bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed32 #-}

decodeAllFixed32 :: ByteString -> Either DecodeError (VU.Vector Word32)
decodeAllFixed32 bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getFixed32 bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed fixed64 values.
decodePackedFixed64 :: Decoder (VU.Vector Word64)
decodePackedFixed64 = do
  bs <- getLengthDelimited
  case decodeAllFixed64 bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed64 #-}

decodeAllFixed64 :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllFixed64 bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getFixed64 bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed float values.
decodePackedFloat :: Decoder (VU.Vector Float)
decodePackedFloat = do
  bs <- getLengthDelimited
  case decodeAllFloat bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFloat #-}

decodeAllFloat :: ByteString -> Either DecodeError (VU.Vector Float)
decodeAllFloat bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getFloat bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed double values.
decodePackedDouble :: Decoder (VU.Vector Double)
decodePackedDouble = do
  bs <- getLengthDelimited
  case decodeAllDouble bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedDouble #-}

decodeAllDouble :: ByteString -> Either DecodeError (VU.Vector Double)
decodeAllDouble bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getDouble bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed sint32 values.
decodePackedSVarint32 :: Decoder (VU.Vector Int32)
decodePackedSVarint32 = do
  bs <- getLengthDelimited
  case decodeAllSVarint32 bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint32 #-}

decodeAllSVarint32 :: ByteString -> Either DecodeError (VU.Vector Int32)
decodeAllSVarint32 bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getSVarint32 bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e

-- | Decode packed sint64 values.
decodePackedSVarint64 :: Decoder (VU.Vector Int64)
decodePackedSVarint64 = do
  bs <- getLengthDelimited
  case decodeAllSVarint64 bs of
    Left e   -> Decoder $ \_ _ -> DecodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint64 #-}

decodeAllSVarint64 :: ByteString -> Either DecodeError (VU.Vector Int64)
decodeAllSVarint64 bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = Right (VU.fromList (reverse acc))
      | otherwise = case runDecoder' getSVarint64 bs off of
          DecodeOK v off' -> go (v : acc) off'
          DecodeFail e    -> Left e
