{-# LANGUAGE BangPatterns #-}
-- | Meta strings for Fory.
--
-- Meta strings carry namespace, type-name, and field-name metadata.
-- The full xlang spec defines five compressed character-set
-- encodings and intra-session deduplication.
--
-- This module implements a UTF-8-only encoding subset and exposes
-- the two primitive shapes the deduplication layer in
-- 'Fury.Encode' / 'Fury.Decode' composes from:
--
-- * \"fresh\" string: header @(byte_length \<\< 1) | 0@ as
--   varuint64, then a one-byte UTF-8 encoding tag (always 0), then
--   the raw bytes.
--
-- * \"reference\" string: header @((id + 1) \<\< 1) | 1@ as
--   varuint64. Bit 0 is set so the decoder can distinguish a
--   back-reference from a fresh string by inspecting that bit on
--   the first read.
--
-- The 64-bit content hash that the spec inserts after the header
-- for strings longer than 16 bytes is intentionally /not/ emitted
-- here. We use exact byte-string equality on the encoder and
-- index-based lookup on the decoder; see 'Fury.Encode' for the
-- pool implementation.
module Fury.MetaString
  ( freshMetaString
  , refMetaString
  , readMetaStringHeader
  , readFreshMetaStringPayload
  , MetaStringHeader (..)
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

import Fury.Encoding (Builder, byte, bytes, readBytes, readVaruint64, varuint64)

-- | Result of reading the leading varuint64 header of a meta
-- string: either a fresh string with a known byte length, or a
-- back-reference to a previously written string by id.
data MetaStringHeader
  = MetaStringFresh !Int   -- ^ Byte length of the encoded payload.
  | MetaStringRef   !Int   -- ^ Reference id (already adjusted: 0-based).
  deriving (Eq, Show)

-- | Builder for a fresh meta string (UTF-8 encoded).
freshMetaString :: Text -> Builder
freshMetaString !t =
  let !raw = TE.encodeUtf8 t
      !len = BS.length raw
      !hdr = (fromIntegral len `shiftL` 1) :: Word64
  in varuint64 hdr <> byte 0 <> bytes raw

-- | Builder for a reference meta string. @id@ must be the
-- 0-based index of the previously written fresh string.
refMetaString :: Int -> Builder
refMetaString rid =
  let !hdr = ((fromIntegral rid + 1) `shiftL` 1) .|. 1 :: Word64
  in varuint64 hdr

-- | Read just the header (not the payload). Use
-- 'readFreshMetaStringPayload' to consume the encoding tag + raw
-- bytes when the header is 'MetaStringFresh'.
readMetaStringHeader :: ByteString -> Int -> Either String (MetaStringHeader, Int)
readMetaStringHeader bs off = do
  (hdr, off1) <- readVaruint64 bs off
  if hdr .&. 1 /= 0
    then Right (MetaStringRef (fromIntegral (hdr `shiftR` 1) - 1), off1)
    else Right (MetaStringFresh (fromIntegral (hdr `shiftR` 1)), off1)

-- | Read the encoding tag + raw bytes of a fresh meta string of
-- the given byte length.
readFreshMetaStringPayload
  :: Int  -- ^ Byte length, as returned by 'readMetaStringHeader'.
  -> ByteString
  -> Int
  -> Either String (Text, Int)
readFreshMetaStringPayload len bs off
  | off >= BS.length bs =
      Left "Fury.MetaString.readFreshMetaStringPayload: missing encoding tag"
  | otherwise = do
      let !enc = BS.index bs off
      if enc /= 0
        then Left $
               "Fury.MetaString.readFreshMetaStringPayload: only UTF-8 (0) supported, got "
               ++ show enc
        else do
          (raw, off2) <- readBytes len bs (off + 1)
          case TE.decodeUtf8' raw of
            Left e  -> Left ("Fury.MetaString.readFreshMetaStringPayload: " ++ show e)
            Right t -> Right (t, off2)
