{- | Content-negotiation header helpers (RFC 9110 §12.5).

This module is a thin wireform-flavoured shim over the hermes
parsers — those are the canonical home for the wire grammar of
@Accept@, @Accept-Language@, @Accept-Charset@, and @Accept-Encoding@
(see "Network.HTTP.Headers.Accept",
"Network.HTTP.Headers.AcceptLanguage",
"Network.HTTP.Headers.AcceptCharset", and
"Network.HTTP.Headers.AcceptEncoding"). When the wire grammar
needs work, fix it /there/; this module only adds:

* The 'Quality' newtype with a clamping smart constructor
  ('mkQuality') that hermes intentionally doesn't ship — hermes\u2019s
  parsers expose plain 'Double' weights.
* Convenience builders so client code can hand the middleware a
  list of @(token, Quality)@ pairs without first wrapping in a
  hermes record type.
* Re-exports of the hermes-side types so callers don't need a
  second import.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Negotiation
  ( -- * Quality
    Quality (..)
  , mkQuality
    -- * Outbound builders
  , acceptLanguageValue
  , acceptCharsetValue
  , acceptEncodingValue
    -- * Inbound parsers (re-exported from hermes for convenience)
  , parseAccept
  , parseAcceptLanguage
  , parseAcceptCharset
  , parseAcceptEncoding
    -- * Hermes types (re-exported for callers)
  , H.Accept (..)
  , HCN.WeightedMediaRange (..)
  , HCN.MediaRange (..)
  , HCN.MediaType (..)
  , HL.AcceptLanguage (..)
  , HL.WeightedLanguage (..)
  , HC.AcceptCharset (..)
  , HC.WeightedCharset (..)
  , HE.AcceptEncoding (..)
  , HE.WeightedEncoding (..)
  , HE.EncodingTag (..)
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Short as ST

import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.ContentCoding         as CC
import qualified Network.HTTP.ContentNegotiation    as HCN
import qualified Network.HTTP.Headers.Accept         as H
import qualified Network.HTTP.Headers.AcceptCharset  as HC
import qualified Network.HTTP.Headers.AcceptEncoding as HE
import qualified Network.HTTP.Headers.AcceptLanguage as HL

-- ---------------------------------------------------------------------------
-- Quality
-- ---------------------------------------------------------------------------

-- | A clamped quality value in @[0,1]@.  Hermes parses raw
-- 'Double' weights (its parsers already constrain the on-wire
-- grammar to @0@..@1@); this newtype prevents construction of
-- out-of-range values from caller code.
newtype Quality = Quality { unQuality :: Double }
  deriving stock (Eq, Show)

-- | Clamping smart constructor.  Anything outside @[0,1]@ gets
-- pinned to the boundary.
mkQuality :: Double -> Quality
mkQuality d
  | d <= 0    = Quality 0
  | d >= 1    = Quality 1
  | otherwise = Quality d

-- ---------------------------------------------------------------------------
-- Outbound builders
-- ---------------------------------------------------------------------------

-- | Build an @Accept-Language@ value from a list of @(BCP47 tag,
-- quality)@ pairs.
acceptLanguageValue :: [(ByteString, Quality)] -> ByteString
acceptLanguageValue =
  M.toStrictByteString
    . HL.renderAcceptLanguage
    . HL.AcceptLanguage
    . map (\(tag, Quality q) -> HL.WeightedLanguage (textShortFromBytes tag) q)

-- | Build an @Accept-Charset@ value.  Note: RFC 9110 §12.5.2
-- /deprecates/ this header — modern services should default to
-- UTF-8 and not negotiate.  We still ship a builder for clients
-- talking to legacy backends.
acceptCharsetValue :: [(ByteString, Quality)] -> ByteString
acceptCharsetValue =
  M.toStrictByteString
    . HC.renderAcceptCharset
    . HC.AcceptCharset
    . map (\(c, Quality q) -> HC.WeightedCharset (textShortFromBytes c) q)

-- | Build an @Accept-Encoding@ value.  Pass 'Nothing' for the
-- @*@ wildcard; otherwise the 'CC.ContentCoding' is rendered via
-- hermes ('CC.renderContentCoding').
acceptEncodingValue :: [(Maybe CC.ContentCoding, Quality)] -> ByteString
acceptEncodingValue =
  M.toStrictByteString
    . HE.renderAcceptEncoding
    . HE.AcceptEncoding
    . map (\(mc, Quality q) -> HE.WeightedEncoding (tagOf mc) q)
  where
    tagOf Nothing  = HE.AnyEncoding
    tagOf (Just c) = HE.NamedEncoding c

-- ---------------------------------------------------------------------------
-- Inbound parsers
-- ---------------------------------------------------------------------------

-- | Parse an @Accept@ header into a list of weighted media
-- ranges.  Returns 'Nothing' on a malformed value.
parseAccept :: ByteString -> Maybe H.Accept
parseAccept bs = case runParser H.acceptParser bs of
  OK x leftover | BS.null leftover -> Just x
  _ -> Nothing

-- | Parse an @Accept-Language@ header.
parseAcceptLanguage :: ByteString -> Maybe HL.AcceptLanguage
parseAcceptLanguage bs = case runParser HL.acceptLanguageParser bs of
  OK x leftover | BS.null leftover -> Just x
  _ -> Nothing

-- | Parse an @Accept-Charset@ header.
parseAcceptCharset :: ByteString -> Maybe HC.AcceptCharset
parseAcceptCharset bs = case runParser HC.acceptCharsetParser bs of
  OK x leftover | BS.null leftover -> Just x
  _ -> Nothing

-- | Parse an @Accept-Encoding@ header.  See
-- "Network.HTTP.Headers.AcceptEncoding" for the list-with-quality
-- shape, which we reuse in preference to the legacy single-token
-- representation.
parseAcceptEncoding :: ByteString -> Maybe HE.AcceptEncoding
parseAcceptEncoding bs = case runParser HE.acceptEncodingParser bs of
  OK x leftover | BS.null leftover -> Just x
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

textShortFromBytes :: ByteString -> ST.ShortText
textShortFromBytes bs = case ST.fromByteString bs of
  Just t  -> t
  -- Accept-* tokens are ASCII by grammar; this branch is only
  -- reached for malformed input. Use lenient UTF-8 so we never
  -- throw at the build boundary.
  Nothing -> ST.fromText (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bs)
