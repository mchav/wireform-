{- | Percent-encoding helpers (RFC 3986 \u00a72.1, \u00a72.4).

Three escape policies covering the per-component sets defined by
RFC 3986. Use 'percentEncode' / 'percentDecode' for raw bytes; the
component-specific helpers ('encodePathSegment' etc.) target the
right \"safe\" set for each component.

@application\/x-www-form-urlencoded@ escaping is similar but distinct:
spaces become @+@ and the safe set is smaller. See
'encodeFormComponent' for that.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.PercentEncoding
  ( -- * Generic
    percentEncode
  , percentEncodeWith
  , percentDecode
    -- * Predicates
  , isUnreserved
  , isPathSafe
  , isQuerySafe
    -- * Component-specific
  , encodePathSegment
  , encodePath
  , encodeQueryComponent
  , encodeFormComponent
  , decodeQueryString
    -- * Query strings
  , renderQueryString
  , renderFormBody
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Bits ((.&.), shiftR)
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

-- | RFC 3986 \"unreserved\": @A-Z a-z 0-9 - _ . ~@.
isUnreserved :: Word8 -> Bool
isUnreserved w =
     (w >= 0x41 && w <= 0x5A)  -- A-Z
  || (w >= 0x61 && w <= 0x7A)  -- a-z
  || (w >= 0x30 && w <= 0x39)  -- 0-9
  || w == 0x2D                 -- -
  || w == 0x5F                 -- _
  || w == 0x2E                 -- .
  || w == 0x7E                 -- ~

-- | Bytes that may appear unescaped in a path segment: unreserved plus
-- the sub-delims @!$&'()*+,;=@ plus @:@ and @\@@.
isPathSafe :: Word8 -> Bool
isPathSafe w =
     isUnreserved w
  || w == 0x21 || w == 0x24 || w == 0x26 || w == 0x27
  || w == 0x28 || w == 0x29 || w == 0x2A || w == 0x2B
  || w == 0x2C || w == 0x3B || w == 0x3D
  || w == 0x3A || w == 0x40

-- | Bytes that may appear unescaped in a query component value:
-- unreserved plus sub-delims minus @&@ and @=@ (those are reserved
-- for query separators), plus @:@, @\@@, @\/@ and @?@ which the
-- query production explicitly allows.
isQuerySafe :: Word8 -> Bool
isQuerySafe w =
     isUnreserved w
  || w == 0x21 || w == 0x24 || w == 0x27
  || w == 0x28 || w == 0x29 || w == 0x2A || w == 0x2B
  || w == 0x2C || w == 0x3B
  || w == 0x3A || w == 0x40
  || w == 0x2F || w == 0x3F

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | Percent-encode every byte that fails the predicate.
percentEncodeWith :: (Word8 -> Bool) -> ByteString -> ByteString
percentEncodeWith safe = BSL.toStrict . BB.toLazyByteString . BS.foldl' step mempty
  where
    step acc w
      | safe w    = acc <> BB.word8 w
      | otherwise = acc <> BB.charUtf8 '%' <> hexNibble (w `shiftR` 4)
                        <> hexNibble (w .&. 0x0F)
    hexNibble n
      | n < 10    = BB.word8 (n + 0x30)
      | otherwise = BB.word8 (n + 0x37)  -- A=0x41, so 10+0x37 = 0x41

-- | Percent-encode the unreserved set.
percentEncode :: ByteString -> ByteString
percentEncode = percentEncodeWith isUnreserved

-- | Decode percent-escapes. Bytes that aren't @%xx@ pass through
-- verbatim. Returns 'Nothing' on a truncated or non-hex escape.
percentDecode :: ByteString -> Maybe ByteString
percentDecode bs0 = go bs0 mempty
  where
    go bs acc = case BS.uncons bs of
      Nothing -> Just (BSL.toStrict (BB.toLazyByteString acc))
      Just (0x25, rest) -> case BS.uncons rest of
        Just (h1, rest1) -> case BS.uncons rest1 of
          Just (h2, rest2) -> case (hexVal h1, hexVal h2) of
            (Just a, Just b) ->
              go rest2 (acc <> BB.word8 (fromIntegral (a * 16 + b)))
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
      Just (w, rest) -> go rest (acc <> BB.word8 w)
    hexVal w
      | w >= 0x30 && w <= 0x39 = Just (fromIntegral w - 0x30)
      | w >= 0x41 && w <= 0x46 = Just (fromIntegral w - 0x37)
      | w >= 0x61 && w <= 0x66 = Just (fromIntegral w - 0x57)
      | otherwise              = Nothing

-- ---------------------------------------------------------------------------
-- Component-specific
-- ---------------------------------------------------------------------------

-- | Encode a single path segment (must not contain @\/@).
encodePathSegment :: ByteString -> ByteString
encodePathSegment = percentEncodeWith (\w -> isPathSafe w && w /= 0x2F)

-- | Encode a path while preserving @\/@ separators.
encodePath :: ByteString -> ByteString
encodePath = percentEncodeWith isPathSafe

-- | Encode a single query value or key. Reserves @&@, @=@, @+@,
-- @;@, and the path separators are kept by 'isQuerySafe'.
encodeQueryComponent :: ByteString -> ByteString
encodeQueryComponent =
  percentEncodeWith (\w -> isQuerySafe w && w /= 0x26 && w /= 0x3D && w /= 0x2B)

-- | Encode for @application\/x-www-form-urlencoded@: like
-- 'encodeQueryComponent' but spaces become @+@ instead of @%20@.
encodeFormComponent :: ByteString -> ByteString
encodeFormComponent =
    BSL.toStrict . BB.toLazyByteString . BS.foldl' step mempty
  where
    step acc w
      | w == 0x20 = acc <> BB.word8 0x2B
      | isQuerySafe w && w /= 0x26 && w /= 0x3D && w /= 0x2B =
          acc <> BB.word8 w
      | otherwise = acc <> BB.charUtf8 '%' <> hex (w `shiftR` 4) <> hex (w .&. 0x0F)
    hex n
      | n < 10    = BB.word8 (n + 0x30)
      | otherwise = BB.word8 (n + 0x37)

-- ---------------------------------------------------------------------------
-- Query strings
-- ---------------------------------------------------------------------------

-- | Render a list of @(key, value)@ pairs as a query string body
-- (without the leading @?@). Each component is percent-encoded
-- following 'encodeQueryComponent'. Use 'renderFormBody' for the
-- form-urlencoded variant (spaces become @+@).
renderQueryString :: [(ByteString, ByteString)] -> ByteString
renderQueryString = renderQS encodeQueryComponent

renderFormBody :: [(ByteString, ByteString)] -> ByteString
renderFormBody = renderQS encodeFormComponent

renderQS :: (ByteString -> ByteString) -> [(ByteString, ByteString)] -> ByteString
renderQS enc = BS.intercalate "&" . map one
  where
    one (k, v) = enc k <> "=" <> enc v

-- | Decode a query string into @(key, value)@ pairs. Decodes both
-- @+@ and @%xx@ on the value side (so it works for both query
-- strings and form bodies). Pairs with no @=@ are surfaced with an
-- empty value.
decodeQueryString :: ByteString -> [(ByteString, ByteString)]
decodeQueryString bs =
  [ (decodeOrRaw k, decodeOrRaw v)
  | pair <- filter (not . BS.null) (BS.split 0x26 bs)
  , let (k, eqV) = BS.break (== 0x3D) pair
        v = case BS.uncons eqV of
              Just (0x3D, r) -> r
              _              -> BS.empty
  ]
  where
    decodeOrRaw bs0 =
      let plusToSpace = BS.map (\w -> if w == 0x2B then 0x20 else w) bs0
      in case percentDecode plusToSpace of
           Just out -> out
           Nothing  -> bs0

