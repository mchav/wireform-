{-# LANGUAGE OverloadedStrings #-}

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

The actual parsing and rendering is delegated to hermes's
'Network.HTTP.Headers.Date' so the per-format flatparse switches and
the padded 'M.Builder' formatter are shared with the rest of the
hermes-driven header layer.

Used by @Date@, @Last-Modified@, @Expires@, @If-Modified-Since@,
@If-Unmodified-Since@, @If-Range@.
-}
module Network.HTTP.HttpDate (
  -- * Building blocks
  formatHttpDate,
  parseHttpDate,
  parseHttpDateMaybe,

  -- * Header helpers
  httpDateHeader,
  readHttpDateHeader,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (UTCTime)
import Network.HTTP.Headers.Date qualified as Hermes
import Network.HTTP.Headers.Mason qualified as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Types.Header qualified as H


-- | Render an 'UTCTime' as an IMF-fixdate string (RFC 9110 \u00a75.6.7).
formatHttpDate :: UTCTime -> ByteString
formatHttpDate = M.toStrictByteString . Hermes.renderDate


{- | Parse an HTTP-date in any of the three RFC 9110 formats
(IMF-fixdate, RFC 850, asctime). Delegates to hermes's
'Hermes.dateParser', which uses TH-driven flatparse switches for
the day / month tokens.
-}
parseHttpDateMaybe :: ByteString -> Maybe UTCTime
parseHttpDateMaybe bs = case runParser Hermes.dateParser bs of
  OK t leftover | BS8.null leftover -> Just t
  _ -> Nothing


-- | Like 'parseHttpDateMaybe' but with a description on failure.
parseHttpDate :: ByteString -> Either String UTCTime
parseHttpDate bs = case parseHttpDateMaybe bs of
  Just t -> Right t
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
