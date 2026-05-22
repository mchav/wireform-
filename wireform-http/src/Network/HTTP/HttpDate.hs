{- | RFC 9110 \u00a75.6.7 HTTP-date helpers.

The preferred format is RFC 5322 IMF-fixdate as restricted by RFC 9110:

@
    Sun, 06 Nov 1994 08:49:37 GMT
    --- ^^ ^^^^ ^^^^^^^^^ -^- TZ
    DOW   day-month-year      always GMT
@

For backwards compatibility, RFC 9110 also says recipients SHOULD parse
the obsolete RFC 850 (@Sunday, 06-Nov-94 08:49:37 GMT@) and asctime
(@Sun Nov  6 08:49:37 1994@) formats; we accept both on the read path
but only emit IMF-fixdate.

Used by @Date@, @Last-Modified@, @Expires@, @If-Modified-Since@,
@If-Unmodified-Since@, @If-Range@.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.HttpDate
  ( -- * Building blocks
    formatHttpDate
  , parseHttpDate
  , parseHttpDateMaybe
    -- * Header helpers
  , httpDateHeader
  , readHttpDateHeader
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Time as Time
import Data.Time (UTCTime)

import qualified Network.HTTP.Types.Header as H

-- | Render an 'UTCTime' as an IMF-fixdate string (RFC 9110 \u00a75.6.7).
formatHttpDate :: UTCTime -> ByteString
formatHttpDate =
  BS8.pack
    . Time.formatTime Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

-- | Parse an HTTP-date in any of the three RFC 9110 formats.
parseHttpDateMaybe :: ByteString -> Maybe UTCTime
parseHttpDateMaybe bs0 =
  let s = BS8.unpack bs0
      try fmt = Time.parseTimeM True Time.defaultTimeLocale fmt s
  in    try "%a, %d %b %Y %H:%M:%S GMT"   -- IMF-fixdate (preferred)
    <|> try "%A, %d-%b-%y %H:%M:%S GMT"   -- RFC 850 (obsolete)
    <|> try "%a %b %_d %H:%M:%S %Y"       -- asctime (obsolete)
  where
    Nothing <|> y = y
    x       <|> _ = x

-- | Like 'parseHttpDateMaybe' but with a description on failure.
parseHttpDate :: ByteString -> Either String UTCTime
parseHttpDate bs = case parseHttpDateMaybe bs of
  Just t  -> Right t
  Nothing -> Left ("malformed HTTP-date: " <> BS8.unpack bs)

-- ---------------------------------------------------------------------------
-- Header convenience
-- ---------------------------------------------------------------------------

-- | Build a @(name, value)@ pair for a date-typed header.
httpDateHeader :: H.HeaderName -> UTCTime -> H.Header
httpDateHeader name t = (name, formatHttpDate t)

-- | Look up a date-typed header by name and parse the value.
readHttpDateHeader :: H.HeaderName -> H.Headers -> Maybe UTCTime
readHttpDateHeader name hdrs = H.lookupHeader name hdrs >>= parseHttpDateMaybe
