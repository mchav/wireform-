{- | Conditional-request helpers (RFC 9110 \u00a713).

Builders for the @If-Match@, @If-None-Match@, @If-Modified-Since@,
@If-Unmodified-Since@, and @If-Range@ headers, plus a small ETag
parser \/ renderer.

The ETag and If-Match grammars are parsed and rendered by hermes
('Network.HTTP.Headers.ETag', 'Network.HTTP.Headers.IfMatch',
'Network.HTTP.Headers.IfNoneMatch'); we wrap them in builders that
produce raw header bytes plus combinators on 'Request'. This module
is the API surface application code uses; the heavy lifting (the
@token@ \/ @entity-tag@ grammar and the @M.Builder@ formatters)
lives in hermes.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Conditional
  ( -- * ETag values (re-exported from hermes)
    EntityTag (..)
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Short as ST
import Data.Time (UTCTime)

import qualified Network.HTTP.Headers.ETag  as Hermes
import qualified Network.HTTP.Headers.Mason as M

import Network.HTTP.Headers.ETag (EntityTag (..))

import Network.HTTP.HttpDate (formatHttpDate)
import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.Request (Request, setHeader)

-- ---------------------------------------------------------------------------
-- ETag
-- ---------------------------------------------------------------------------

-- | Build a strong entity tag from raw opaque bytes (without the
-- surrounding double quotes). Bytes that aren't valid @etagc@ will
-- be rejected on parse round-trip.
strongETag :: ByteString -> EntityTag
strongETag = StrongETag . shortFromBytes

-- | Build a weak entity tag (will render with the @W\/@ prefix).
weakETag :: ByteString -> EntityTag
weakETag = WeakETag . shortFromBytes

-- | Render an entity tag to its on-the-wire form.
renderETag :: EntityTag -> ByteString
renderETag = M.toStrictByteString . Hermes.renderEntityTag

-- | Parse a single entity tag (strong or weak).  Wraps hermes's
-- 'Hermes.parseETag' and projects out the entity-tag part.
parseETag :: ByteString -> Maybe EntityTag
parseETag bs = case Hermes.parseETag bs of
  Right etag -> Just (Hermes.etag etag)
  Left  _    -> Nothing

shortFromBytes :: ByteString -> ST.ShortText
shortFromBytes bs = case ST.fromByteString bs of
  Just t  -> t
  Nothing -> ST.fromText (TE.decodeUtf8With TE.lenientDecode bs)

-- ---------------------------------------------------------------------------
-- If-Match / If-None-Match
-- ---------------------------------------------------------------------------

-- | Build an @If-Match@ value from a list of tags.  An empty list
-- collapses to the wildcard form (@\"*\"@); a non-empty list is
-- rendered as a comma-separated 'EntityTag' list, formatted via
-- hermes's 'Hermes.renderEntityTag'.
ifMatchHeader :: [EntityTag] -> ByteString
ifMatchHeader tags = case NE.nonEmpty tags of
  Nothing -> ifMatchAny
  Just ne -> renderEntityTagList ne

ifNoneMatchHeader :: [EntityTag] -> ByteString
ifNoneMatchHeader tags = case NE.nonEmpty tags of
  Nothing -> ifNoneMatchAny
  Just ne -> renderEntityTagList ne

renderEntityTagList :: NE.NonEmpty EntityTag -> ByteString
renderEntityTagList ne =
  M.toStrictByteString (M.intersperse ", " (fmap Hermes.renderEntityTag ne))

-- | The wildcard @*@ value for both @If-Match@ and @If-None-Match@:
-- \"any current representation\".
ifMatchAny :: ByteString
ifMatchAny = "*"

ifNoneMatchAny :: ByteString
ifNoneMatchAny = "*"

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
data IfRange
  = IfRangeETag !EntityTag
  | IfRangeDate !UTCTime
  deriving stock (Eq, Show)

ifRangeHeader :: IfRange -> ByteString
ifRangeHeader = \case
  IfRangeETag t -> renderETag t
  IfRangeDate d -> formatHttpDate d

-- ---------------------------------------------------------------------------
-- Request combinators
-- ---------------------------------------------------------------------------

ifMatch :: [EntityTag] -> Request a -> Request a
ifMatch tags = setHeader H.hIfMatch (ifMatchHeader tags)

ifNoneMatch :: [EntityTag] -> Request a -> Request a
ifNoneMatch tags = setHeader H.hIfNoneMatch (ifNoneMatchHeader tags)

ifModifiedSince :: UTCTime -> Request a -> Request a
ifModifiedSince t = setHeader H.hIfModifiedSince (ifModifiedSinceHeader t)

ifUnmodifiedSince :: UTCTime -> Request a -> Request a
ifUnmodifiedSince t = setHeader H.hIfUnmodifiedSince (ifUnmodifiedSinceHeader t)

ifRange :: IfRange -> Request a -> Request a
ifRange ir = setHeader H.hIfRange (ifRangeHeader ir)
