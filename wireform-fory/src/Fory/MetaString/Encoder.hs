{-# LANGUAGE BangPatterns #-}
-- | Pure-Haskell port of pyfory's @MetaStringEncoder@ /
-- @MetaStringDecoder@ (see
-- @pyfory/meta/metastring.py@). Implements the 5-bit and 6-bit
-- packing, the strip-last-char header bit, and the encoding
-- selection algorithm pyfory uses for namespace, type-name, and
-- field-name meta-strings.
module Fory.MetaString.Encoder
  ( Encoding (..)
  , encodingId
  , encodingFromId

  , SpecialChars (..)
  , namespaceSpecialChars
  , typenameSpecialChars

  , chooseEncoding
  , encodeMetaString
  , decodeMetaString
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, ord, isUpper, isLower, isDigit, toUpper, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Encoding tag
-- ---------------------------------------------------------------------------

data Encoding
  = UTF8
  | LowerSpecial
  | LowerUpperDigitSpecial
  | FirstToLowerSpecial
  | AllToLowerSpecial
  deriving (Eq, Show)

encodingId :: Encoding -> Word8
encodingId UTF8                   = 0
encodingId LowerSpecial           = 1
encodingId LowerUpperDigitSpecial = 2
encodingId FirstToLowerSpecial    = 3
encodingId AllToLowerSpecial      = 4

encodingFromId :: Word8 -> Maybe Encoding
encodingFromId 0 = Just UTF8
encodingFromId 1 = Just LowerSpecial
encodingFromId 2 = Just LowerUpperDigitSpecial
encodingFromId 3 = Just FirstToLowerSpecial
encodingFromId 4 = Just AllToLowerSpecial
encodingFromId _ = Nothing

-- | Two context-dependent characters that the
-- @LOWER_UPPER_DIGIT_SPECIAL@ encoding maps to bit values 62 and
-- 63. pyfory uses @('.', '_')@ for namespaces and @('$', '_')@
-- for type names; field names use @('.', '_')@ too in our usage.
data SpecialChars = SpecialChars { sc1 :: !Char, sc2 :: !Char }
  deriving (Eq, Show)

namespaceSpecialChars :: SpecialChars
namespaceSpecialChars = SpecialChars '.' '_'

typenameSpecialChars :: SpecialChars
typenameSpecialChars = SpecialChars '$' '_'

-- ---------------------------------------------------------------------------
-- Encoding selection
-- ---------------------------------------------------------------------------

data Stats = Stats
  { canLowerSpecial          :: !Bool
  , canLowerUpperDigitSpecial :: !Bool
  , digitCount               :: !Int
  , upperCount               :: !Int
  } deriving (Show)

computeStats :: SpecialChars -> Text -> Stats
computeStats (SpecialChars c1 c2) t =
  T.foldl' step (Stats True True 0 0) t
  where
    step !s c =
      let !lds  = canLowerUpperDigitSpecial s
                  && (isLower c || isUpper c || isDigit c
                      || c == c1 || c == c2)
          !ls   = canLowerSpecial s
                  && (isLower c
                      || c `elem` ['.', '_', '$', '|'])
          !dc   = digitCount s + (if isDigit c then 1 else 0)
          !uc   = upperCount s + (if isUpper c then 1 else 0)
      in Stats ls lds dc uc

-- | Match pyfory's @MetaStringEncoder.compute_encoding@: try
-- 'LowerSpecial' first, then the upper/digit/special set with
-- preference for the @FirstToLowerSpecial@ / @AllToLowerSpecial@
-- short-cuts when they shrink the encoded length, falling back
-- to UTF-8 otherwise.
chooseEncoding :: SpecialChars -> Text -> Encoding
chooseEncoding sc t
  | T.null t  = LowerSpecial
  | canLowerSpecial st = LowerSpecial
  | canLowerUpperDigitSpecial st =
      if digitCount st /= 0
        then LowerUpperDigitSpecial
        else
          let !uc = upperCount st
              !len = T.length t
          in if uc == 1 && isUpper (T.head t)
                then FirstToLowerSpecial
                else if (len + uc) * 5 < len * 6
                       then AllToLowerSpecial
                       else LowerUpperDigitSpecial
  | otherwise = UTF8
  where
    !st = computeStats sc t

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Encode a meta string. For the empty string, returns
-- @(UTF8, "")@. Otherwise selects an encoding via
-- 'chooseEncoding' and packs the bytes accordingly.
encodeMetaString :: SpecialChars -> Text -> (Encoding, ByteString)
encodeMetaString sc t
  | T.null t  = (UTF8, BS.empty)
  | otherwise = case enc of
      LowerSpecial           ->
        (enc, encodeGenericWith charValue5 5 (T.unpack t))
      LowerUpperDigitSpecial ->
        (enc, encodeGenericWith (charValue6 sc) 6 (T.unpack t))
      FirstToLowerSpecial    ->
        let chars = T.unpack t
            chars' = case chars of
                       (h:rest) -> toLower h : rest
                       []       -> []
        in (enc, encodeGenericWith charValue5 5 chars')
      AllToLowerSpecial      ->
        (enc, encodeGenericWith charValue5 5
                (concatMap escapeUpper (T.unpack t)))
      UTF8                   -> (enc, TE.encodeUtf8 t)
  where
    !enc = chooseEncoding sc t
    escapeUpper c
      | isUpper c = ['|', toLower c]
      | otherwise = [c]

-- The 5-bit code is fixed (special chars . _ $ |); the 6-bit code
-- is parameterised by the context's special characters. We
-- therefore have two slightly different "value" lookups; package
-- both into a single generic packer.

charValue5 :: Char -> Word8
charValue5 c
  | 'a' <= c && c <= 'z' = fromIntegral (ord c - ord 'a')
  | c == '.'  = 26
  | c == '_'  = 27
  | c == '$'  = 28
  | c == '|'  = 29
  | otherwise = error $ "Fory.MetaString.Encoder.charValue5: unsupported char "
                        ++ show c

charValue6 :: SpecialChars -> Char -> Word8
charValue6 (SpecialChars c1 c2) c
  | 'a' <= c && c <= 'z' = fromIntegral (ord c - ord 'a')
  | 'A' <= c && c <= 'Z' = fromIntegral (26 + (ord c - ord 'A'))
  | '0' <= c && c <= '9' = fromIntegral (52 + (ord c - ord '0'))
  | c == c1   = 62
  | c == c2   = 63
  | otherwise = error $ "Fory.MetaString.Encoder.charValue6: unsupported char "
                        ++ show c

-- | Pack @[Char]@ into bytes at @bitsPerChar@ bits per char,
-- using the supplied char→value lookup. Mirrors pyfory's
-- @MetaStringEncoder._encode_generic@ exactly, including the
-- strip-last-char header bit set on @bytes_array[0]@ when the
-- final character would otherwise sit alone in a partially-used
-- byte.
encodeGenericWith :: (Char -> Word8) -> Int -> [Char] -> ByteString
encodeGenericWith toValue bitsPerChar chars =
  let !n          = length chars
      !totalBits  = n * bitsPerChar + 1
      !byteLen    = (totalBits + 7) `quot` 8
      buf0        = BS.replicate byteLen 0
      bufFilled   = goLoop toValue bitsPerChar chars 1 buf0
      !stripLast  = byteLen * 8 >= totalBits + bitsPerChar
  in if stripLast
       then setHighBit bufFilled
       else bufFilled
  where
    goLoop _f _bpc []     _cur b = b
    goLoop  f !bpc (c:cs) !cur b =
      let !v = f c
          b' = setBits cur bpc v b
      in goLoop f bpc cs (cur + bpc) b'

setBits :: Int -> Int -> Word8 -> ByteString -> ByteString
setBits cur bpc v bs0 =
  let goB :: Int -> ByteString -> ByteString
      goB i b
        | i >= bpc = b
        | otherwise =
            let bytePos    = (cur + i) `quot` 8
                bitPos     = (cur + i) `rem`  8
                bitValue   = (v `shiftR` (bpc - 1 - i)) .&. 1
            in if bitValue == 0
                 then goB (i + 1) b
                 else
                   let !curByte = BSU.unsafeIndex b bytePos
                       !newByte = curByte .|. (1 `shiftL` (7 - bitPos))
                   in goB (i + 1) (replaceByte bytePos newByte b)
  in goB 0 bs0

setHighBit :: ByteString -> ByteString
setHighBit bs
  | BS.null bs = bs
  | otherwise  = replaceByte 0 (BSU.unsafeIndex bs 0 .|. 0x80) bs

replaceByte :: Int -> Word8 -> ByteString -> ByteString
replaceByte i v bs =
  BS.take i bs `BS.append` BS.singleton v `BS.append` BS.drop (i + 1) bs

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | Decode bytes encoded with the given encoding back to text.
decodeMetaString :: SpecialChars -> Encoding -> ByteString -> Text
decodeMetaString sc enc bs
  | BS.null bs = T.empty
  | otherwise = case enc of
      UTF8                   ->
        case TE.decodeUtf8' bs of
          Right t -> t
          Left e  -> error ("Fory.MetaString.Encoder.decodeMetaString UTF8: "
                             ++ show e)
      LowerSpecial           -> T.pack (decodeGeneric 5 sc bs decodeChar5)
      LowerUpperDigitSpecial -> T.pack (decodeGeneric 6 sc bs decodeChar6)
      FirstToLowerSpecial    ->
        let s = decodeGeneric 5 sc bs decodeChar5
        in T.pack (case s of
              []     -> []
              (c:cs) -> toUpper c : cs)
      AllToLowerSpecial      ->
        let s = decodeGeneric 5 sc bs decodeChar5
        in T.pack (decodeAllToLower s)
  where
    decodeAllToLower :: String -> String
    decodeAllToLower []           = []
    decodeAllToLower ['|']        = ['|']    -- malformed, surface as-is
    decodeAllToLower ('|':c:rest) = toUpper c : decodeAllToLower rest
    decodeAllToLower (c:rest)     = c        : decodeAllToLower rest

decodeChar5 :: SpecialChars -> Word8 -> Char
decodeChar5 _ v
  | v <= 25  = chr (ord 'a' + fromIntegral v)
  | v == 26  = '.'
  | v == 27  = '_'
  | v == 28  = '$'
  | v == 29  = '|'
  | otherwise = error $
      "Fory.MetaString.Encoder.decodeChar5: invalid char value " ++ show v

decodeChar6 :: SpecialChars -> Word8 -> Char
decodeChar6 (SpecialChars c1 c2) v
  | v <= 25  = chr (ord 'a' + fromIntegral v)
  | v <= 51  = chr (ord 'A' + fromIntegral (v - 26))
  | v <= 61  = chr (ord '0' + fromIntegral (v - 52))
  | v == 62  = c1
  | v == 63  = c2
  | otherwise = error $
      "Fory.MetaString.Encoder.decodeChar6: invalid char value " ++ show v

decodeGeneric
  :: Int                                  -- ^ bits per char
  -> SpecialChars
  -> ByteString
  -> (SpecialChars -> Word8 -> Char)
  -> [Char]
decodeGeneric !bpc sc bs charOf
  | BS.null bs = []
  | otherwise =
      let !numBits     = BS.length bs * 8
          !stripLast   = (BSU.unsafeIndex bs 0 .&. 0x80) /= 0
          !bitMask     = (1 `shiftL` bpc) - 1 :: Word8
          go bitIndex
            | bitIndex + bpc > numBits = []
            | stripLast && bitIndex + 2 * bpc > numBits = []
            | otherwise =
                let byteIdx        = bitIndex `quot` 8
                    intraByte      = bitIndex `rem`  8
                    !b0            = fromIntegral (BSU.unsafeIndex bs byteIdx) :: Int
                    !b1            = if byteIdx + 1 < BS.length bs
                                       then fromIntegral (BSU.unsafeIndex bs (byteIdx + 1)) :: Int
                                       else 0
                    !shifted       = ((b0 `shiftL` 8) .|. b1) `shiftR` (16 - bpc - intraByte)
                    !v             = fromIntegral shifted .&. bitMask
                in charOf sc v : go (bitIndex + bpc)
      in go 1
