{-# LANGUAGE BangPatterns #-}
-- | Meta strings for Fory.
--
-- Meta strings carry namespace, type-name, and field-name metadata.
-- The full xlang spec defines five compressed character-set
-- encodings (LOWER_SPECIAL, LOWER_UPPER_DIGIT_SPECIAL, etc.) plus
-- intra-session deduplication.
--
-- This module implements the simplest variant only: a fresh UTF-8
-- meta string with no deduplication. The header is the single-shot
-- @VarUint36Small@ form @(byte_length \<\< 1) | flag@ where
-- @flag = 0@ marks \"new string\" and the body is the raw UTF-8
-- bytes. We pin the encoding tag implicitly to UTF-8 by always
-- writing the same shape and refusing to decode anything that
-- doesn't match. That is enough for the in-house Haskell-to-Haskell
-- round trip but is not byte-for-byte compatible with Fory streams
-- written by other languages that pick a more compact encoding.
module Fury.MetaString
  ( metaString
  , readMetaString
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

import Fury.Encoding (Builder, byte, bytes, readBytes, readVaruint64, varuint64)

-- | Encode a meta string. Header:
--
-- @(length \<\< 1) | 0@   – fresh string follows
--
-- We always emit fresh, never a reference, so 'metaString' /
-- 'readMetaString' compose without a session table.
--
-- For UTF-8 bytes we then prepend the 1-byte encoding tag (0,
-- UTF-8) before the raw bytes. The 1-byte tag is intentionally
-- carried inline so the decoder can sanity-check it and reject
-- foreign encodings.
metaString :: Text -> Builder
metaString !t =
  let !raw = TE.encodeUtf8 t
      !len = BS.length raw
      !hdr = (fromIntegral len `shiftL` 1) :: Word64
  in varuint64 hdr <> byte 0 <> bytes raw

readMetaString :: ByteString -> Int -> Either String (Text, Int)
readMetaString bs off = do
  (hdr, off1) <- readVaruint64 bs off
  if hdr .&. 1 /= 0
    then Left "Fury.MetaString.readMetaString: meta-string references not supported"
    else do
      let !len = fromIntegral (hdr `shiftR` 1) :: Int
      if off1 >= BS.length bs
        then Left "Fury.MetaString.readMetaString: missing encoding tag"
        else do
          let !enc = BS.index bs off1
          if enc /= 0
            then Left $
                   "Fury.MetaString.readMetaString: only UTF-8 (0) supported, got "
                   ++ show enc
            else do
              (raw, off2) <- readBytes len bs (off1 + 1)
              case TE.decodeUtf8' raw of
                Left e  -> Left ("Fury.MetaString.readMetaString: " ++ show e)
                Right t -> Right (t, off2)
