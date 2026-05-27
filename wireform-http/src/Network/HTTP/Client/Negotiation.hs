{- | Content-negotiation header helpers (RFC 9110 \u00a712.5).

The 'Accept', 'Accept-Language', 'Accept-Charset', and 'Accept-Encoding'
headers all share the same shape: a comma-separated list of values each
optionally followed by @;q=0.x@ weights. This module provides a single
'renderQList' renderer that's reused across all four, plus typed
helpers for the most common builders.

The decoder side already comes from the media-type / content-encoding
parsers (hermes); these helpers handle the build/inspect side.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Negotiation
  ( -- * Quality-list rendering (shared by Accept*)
    renderQList
  , renderQuality
    -- * Accept-Language
  , Language
  , language
  , acceptLanguageValue
    -- * Accept-Charset
  , Charset
  , charset
  , acceptCharsetValue
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Network.HTTP.Client.Media (Quality (..))

-- | Render a list of @(token, quality)@ pairs as a quality-weighted
-- comma list. Quality 1.0 is omitted; other qualities are rendered
-- per RFC 9110 \u00a712.4.2 (max three decimals, no exponent, trailing
-- zeros dropped).
renderQList :: [(ByteString, Quality)] -> ByteString
renderQList = BS.intercalate ", " . map one
  where
    one (tok, Quality q)
      | q >= 1.0  = tok
      | otherwise = tok <> "; q=" <> renderQuality q

-- | Render a quality value (clamped to @[0,1]@) per RFC 9110 \u00a712.4.2.
renderQuality :: Double -> ByteString
renderQuality d
  | d >= 1.0  = "1"
  | d <= 0.0  = "0"
  | otherwise =
      let milli   = round (d * 1000) :: Int
          padded  = pad3 (BS8.pack (show milli))
          trimmed = BS.dropWhileEnd (== 0x30) padded
      in if BS.null trimmed
           then "0"
           else "0." <> trimmed
  where
    pad3 bs =
      let n = BS.length bs
      in if n >= 3 then bs else BS.replicate (3 - n) 0x30 <> bs

-- ---------------------------------------------------------------------------
-- Accept-Language (RFC 9110 \u00a712.5.4 + RFC 5646 language tags)
-- ---------------------------------------------------------------------------

-- | A language tag in BCP 47 form (e.g. @\"en-US\"@). The 'IsString'
-- instance lets you write language literals directly.
newtype Language = Language { unLanguage :: ByteString }
  deriving stock (Eq, Show)

-- | Smart constructor. Bytes are stored verbatim; the spec allows
-- @ALPHA *(\"-\" subtag)@ and @\"*\"@.
language :: ByteString -> Language
language = Language

acceptLanguageValue :: [(Language, Quality)] -> ByteString
acceptLanguageValue = renderQList . map (\(Language l, q) -> (l, q))

-- ---------------------------------------------------------------------------
-- Accept-Charset (RFC 9110 \u00a712.5.2 \u2014 obsoleted but still seen)
-- ---------------------------------------------------------------------------

-- | A charset name (e.g. @\"utf-8\"@). Charset matching is
-- case-insensitive but stored verbatim because that's what the wire
-- expects.
newtype Charset = Charset { unCharset :: ByteString }
  deriving stock (Eq, Show)

charset :: ByteString -> Charset
charset = Charset

acceptCharsetValue :: [(Charset, Quality)] -> ByteString
acceptCharsetValue = renderQList . map (\(Charset c, q) -> (c, q))

-- ---------------------------------------------------------------------------
-- Quality + parsing helpers
-- ---------------------------------------------------------------------------

-- | Clamping smart constructor for 'Quality': anything outside
-- @[0,1]@ gets pinned to the boundary. Use this rather than the
-- 'Quality' constructor when the value comes from caller code or
-- the network — the quality grammar (RFC 9110 §12.4.2) only
-- admits values in that range.
mkQuality :: Double -> Quality
mkQuality d
  | d <= 0    = Quality 0
  | d >= 1    = Quality 1
  | otherwise = Quality d

-- | Parse a comma-separated quality-weighted list of opaque tokens.
-- Each entry is @token *(OWS \";\" OWS attrs) [;q=Q]@; we only
-- look at the leading token and the final @q=@ if present, which
-- is what every Accept* header in RFC 9110 §12.4 wants.
--
-- Bytes inside parameters are passed through; malformed entries
-- are dropped.
parseQList :: ByteString -> [(ByteString, Quality)]
parseQList raw = mapMaybe one (BS.split 0x2C raw)
  where
    isWS w = w == 0x20 || w == 0x09
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    one entryRaw =
      let entry = trim entryRaw
      in if BS.null entry
           then Nothing
           else case BS.split 0x3B entry of
             (tok : params) ->
               let q = case mapMaybe parseQ params of
                         (qv : _) -> qv
                         []       -> Quality 1
               in Just (trim tok, q)
             [] -> Nothing
    parseQ rawParam =
      let p = trim rawParam
      in if BS.length p >= 2 && BS8.take 2 p `elem` ["q=", "Q="]
           then case BS8.readSigned BS8.readDouble (BS.drop 2 p) of
             Just (d, leftover) | BS.null (trim leftover) -> Just (mkQuality d)
             _ -> Nothing
           else Nothing

-- | Parse an @Accept-Language@ value (RFC 9110 §12.5.4) into a
-- list of @(language tag, quality)@ pairs.
parseAcceptLanguage :: ByteString -> [(Language, Quality)]
parseAcceptLanguage = map (\(t, q) -> (Language t, q)) . parseQList

-- | Parse an @Accept-Charset@ value (RFC 9110 §12.5.2 — obsolete
-- but still seen on legacy services).
parseAcceptCharset :: ByteString -> [(Charset, Quality)]
parseAcceptCharset = map (\(t, q) -> (Charset t, q)) . parseQList
