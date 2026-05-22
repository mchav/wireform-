{- | Conditional-request helpers (RFC 9110 \u00a713).

Builders for the @If-Match@, @If-None-Match@, @If-Modified-Since@,
@If-Unmodified-Since@, and @If-Range@ headers, plus a small ETag
parser. These are pure, value-level helpers \u2014 no middleware
needed, callers thread them onto a 'Request' via 'addHeader' /
'setHeader'.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Conditional
  ( -- * ETag values
    ETag (..)
  , strongETag
  , weakETag
  , parseETag
  , renderETag
    -- * If-Match / If-None-Match
  , ifMatchHeader
  , ifNoneMatchHeader
  , ifMatchAny
  , ifNoneMatchAny
    -- * Date-bearing precondition headers
  , ifModifiedSinceHeader
  , ifUnmodifiedSinceHeader
    -- * If-Range
  , IfRange (..)
  , ifRangeHeader
    -- * Combinators on Request
  , ifMatch
  , ifNoneMatch
  , ifModifiedSince
  , ifUnmodifiedSince
  , ifRange
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Time (UTCTime)

import Network.HTTP.HttpDate (formatHttpDate)
import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.Request (Request, setHeader)

-- ---------------------------------------------------------------------------
-- ETag
-- ---------------------------------------------------------------------------

-- | An entity tag. A strong tag uniquely identifies a representation;
-- a weak tag (prefixed @W\/@) only identifies the resource state for
-- the purposes of cache validation.
data ETag = ETag
  { etagWeak  :: !Bool
  , etagOpaque :: !ByteString
    -- ^ The opaque-tag bytes /without/ the surrounding double quotes.
  }
  deriving stock (Eq, Show)

strongETag :: ByteString -> ETag
strongETag = ETag False

weakETag :: ByteString -> ETag
weakETag = ETag True

-- | Render an 'ETag' to its canonical wire form: @W\/@ prefix if weak,
-- followed by the opaque tag in double quotes.
renderETag :: ETag -> ByteString
renderETag (ETag weak op) =
  let core = "\"" <> op <> "\""
  in if weak then "W/" <> core else core

-- | Parse a single ETag (strong or weak). Returns 'Nothing' for input
-- that doesn't look like @\"...\"@ or @W\/\"...\"@.
parseETag :: ByteString -> Maybe ETag
parseETag raw0 =
  let raw = BS.dropWhile isWS (BS.dropWhileEnd isWS raw0)
      (weak, rest) = case BS.stripPrefix "W/" raw of
        Just r  -> (True,  r)
        Nothing -> (False, raw)
  in case BS.uncons rest of
       Just (0x22, body) -> case BS.unsnoc body of
         Just (op, 0x22) -> Just (ETag weak op)
         _               -> Nothing
       _ -> Nothing
  where
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- If-Match / If-None-Match
-- ---------------------------------------------------------------------------

-- | Build an @If-Match@ value from a non-empty list of tags. RFC 9110
-- \u00a713.1.1 requires at least one entry.
ifMatchHeader :: [ETag] -> ByteString
ifMatchHeader = renderETagList

ifNoneMatchHeader :: [ETag] -> ByteString
ifNoneMatchHeader = renderETagList

-- | The wildcard @*@ value for both @If-Match@ and @If-None-Match@:
-- \"any current representation\".
ifMatchAny :: ByteString
ifMatchAny = "*"

ifNoneMatchAny :: ByteString
ifNoneMatchAny = "*"

renderETagList :: [ETag] -> ByteString
renderETagList = mconcat . intersperse ", " . map renderETag

-- ---------------------------------------------------------------------------
-- If-Modified-Since / If-Unmodified-Since
-- ---------------------------------------------------------------------------

ifModifiedSinceHeader :: UTCTime -> ByteString
ifModifiedSinceHeader = formatHttpDate

ifUnmodifiedSinceHeader :: UTCTime -> ByteString
ifUnmodifiedSinceHeader = formatHttpDate

-- ---------------------------------------------------------------------------
-- If-Range
-- ---------------------------------------------------------------------------

-- | RFC 9110 \u00a713.1.5: @If-Range@ takes either an entity tag or an
-- HTTP-date. Sending it with a range header makes the server return
-- the partial response only when the validator still matches.
data IfRange = IfRangeETag !ETag | IfRangeDate !UTCTime
  deriving stock (Eq, Show)

ifRangeHeader :: IfRange -> ByteString
ifRangeHeader (IfRangeETag t) = renderETag t
ifRangeHeader (IfRangeDate d) = formatHttpDate d

-- ---------------------------------------------------------------------------
-- Request combinators
-- ---------------------------------------------------------------------------

ifMatch :: [ETag] -> Request a -> Request a
ifMatch tags = setHeader H.hIfMatch (ifMatchHeader tags)

ifNoneMatch :: [ETag] -> Request a -> Request a
ifNoneMatch tags = setHeader H.hIfNoneMatch (ifNoneMatchHeader tags)

ifModifiedSince :: UTCTime -> Request a -> Request a
ifModifiedSince t = setHeader H.hIfModifiedSince (ifModifiedSinceHeader t)

ifUnmodifiedSince :: UTCTime -> Request a -> Request a
ifUnmodifiedSince t = setHeader H.hIfUnmodifiedSince (ifUnmodifiedSinceHeader t)

ifRange :: IfRange -> Request a -> Request a
ifRange ir = setHeader H.hIfRange (ifRangeHeader ir)
