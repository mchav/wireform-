{-# LANGUAGE BangPatterns #-}
-- | Apache Fory meta-string wire format, byte-for-byte
-- compatible with @pyfory.context.MetaStringWriter@ /
-- @MetaStringReader@.
--
-- A fresh meta-string is laid out as
--
-- @
-- | varuint32 (length \<\< 1)              -- header (bit 0 = 0)
-- | int8 encoding                          -- only if length \<= 16 and length /= 0
-- | int64 hashcode                         -- only if length \>  16
-- | encoded data bytes                     -- length bytes
-- @
--
-- and a back-reference is laid out as
--
-- @
-- | varuint32 ((id + 1) \<\< 1) | 1 |
-- @
--
-- Encoded data is produced by 'Fory.MetaString.Encoder.encodeMetaString',
-- which selects between LATIN1 / UTF-16 / one of the four
-- bit-packed compressions and the UTF-8 fallback.
module Fory.MetaString
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
import Data.Word (Word64, Word8)

import Fory.Encoding (Builder, byte, bytes, int64LE, readBytes,
                      readInt64LE, readVaruint64, varuint64)
import Fory.MetaString.Encoder
  ( SpecialChars
  , encodingId
  , encodingFromId
  , encodeMetaString
  , decodeMetaString
  )
import qualified Fory.MetaString.Hash as Hash

-- | Result of reading the leading varuint64 header of a meta
-- string: either a fresh string (with byte length) or a
-- back-reference to a previously written one (already adjusted
-- to a 0-based index).
data MetaStringHeader
  = MetaStringFresh !Int
  | MetaStringRef   !Int
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Writer
-- ---------------------------------------------------------------------------

-- | Builder for a fresh meta-string. Encodes the input via
-- 'encodeMetaString' (selecting LOWER_SPECIAL / FIRST_TO_LOWER_SPECIAL
-- / etc.), then frames it with the appropriate header for the
-- encoded length.
freshMetaString :: SpecialChars -> Text -> Builder
freshMetaString sc !t =
  let (enc, encoded) = encodeMetaString sc t
      !len           = BS.length encoded
      !hdr           = (fromIntegral len `shiftL` 1) :: Word64
  in if len == 0
       then varuint64 hdr  -- length 0: no encoding tag, no payload
       else if len <= smallStringThreshold
         then varuint64 hdr <> byte (encodingId enc) <> bytes encoded
         else
           let !hashcode = Hash.metaStringHashcode encoded
                 (fromIntegral (encodingId enc) :: Word64)
           in varuint64 hdr <> int64LE (fromIntegral hashcode) <> bytes encoded

-- | Builder for a back-reference. @id@ is the 0-based index of
-- the previously written fresh string in the dedup pool.
refMetaString :: Int -> Builder
refMetaString rid =
  let !hdr = ((fromIntegral rid + 1) `shiftL` 1) .|. 1 :: Word64
  in varuint64 hdr

-- ---------------------------------------------------------------------------
-- Reader
-- ---------------------------------------------------------------------------

-- | Read the leading varuint64 header. The caller follows up with
-- 'readFreshMetaStringPayload' when the header is
-- 'MetaStringFresh'.
readMetaStringHeader :: ByteString -> Int -> Either String (MetaStringHeader, Int)
readMetaStringHeader bs off = do
  (hdr, off1) <- readVaruint64 bs off
  if hdr .&. 1 /= 0
    then Right (MetaStringRef (fromIntegral (hdr `shiftR` 1) - 1), off1)
    else Right (MetaStringFresh (fromIntegral (hdr `shiftR` 1)), off1)

-- | Read the encoding tag (or 64-bit hashcode for >16-byte
-- payloads) plus the encoded bytes, decoding them back to
-- 'Text' under the supplied 'SpecialChars' context.
readFreshMetaStringPayload
  :: SpecialChars
  -> Int           -- ^ Byte length, as returned by 'readMetaStringHeader'.
  -> ByteString
  -> Int
  -> Either String (Text, Int)
readFreshMetaStringPayload sc len bs off
  | len == 0 = Right (mempty, off)
  | len <= smallStringThreshold = do
      (encByte, off1) <- readByteEither bs off
      enc <- case encodingFromId encByte of
        Just e  -> Right e
        Nothing -> Left $ "Fory.MetaString: unknown encoding id "
                          ++ show encByte
      (raw, off2) <- readBytes len bs off1
      Right (decodeMetaString sc enc raw, off2)
  | otherwise = do
      (hashcode, off1) <- readInt64LE bs off
      let !encByte = fromIntegral (hashcode .&. 0xFF) :: Word8
      enc <- case encodingFromId encByte of
        Just e  -> Right e
        Nothing -> Left $ "Fory.MetaString: unknown encoding id from hashcode "
                          ++ show encByte
      (raw, off2) <- readBytes len bs off1
      Right (decodeMetaString sc enc raw, off2)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Same threshold pyfory uses
-- (@pyfory.context.SMALL_STRING_THRESHOLD = 16@).
smallStringThreshold :: Int
smallStringThreshold = 16

readByteEither :: ByteString -> Int -> Either String (Word8, Int)
readByteEither bs off
  | off >= BS.length bs =
      Left "Fory.MetaString.readFreshMetaStringPayload: missing encoding tag"
  | otherwise = Right (BS.index bs off, off + 1)
