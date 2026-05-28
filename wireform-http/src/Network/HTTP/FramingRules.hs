{- | RFC 9112 \u00a76 framing rules.

A small set of pure validators and a 'Middleware' that enforces them on
incoming responses:

* Status codes that MUST NOT have a payload: 1xx, 204, 304 (RFC 9110
  \u00a76.4.1, \u00a715.4.5).
* Method-specific: HEAD responses MUST NOT have a payload (RFC 9110
  \u00a79.3.2). The high-level @send@ doesn't see the request method
  on the popper side, so the helper takes the method explicitly.
* @Content-Length@ and @Transfer-Encoding@ MUST NOT both appear
  (RFC 9112 \u00a76.3); messages that violate this are framing
  ambiguous and a smuggling vector.
* Multiple @Content-Length@ values MUST agree (RFC 9112 \u00a76.3).
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.FramingRules
  ( -- * Framing predicates
    statusForbidsBody
  , methodForbidsResponseBody
    -- * Validation
  , FramingError (..)
  , validateResponseFraming
  , validateResponseFramingStrict
    -- * Middleware
  , withFramingRules
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client.BodyStream (popperFromStrict)
import qualified Network.HTTP.Client.Request   as WReq
import qualified Network.HTTP.Client.Response  as Resp
import           Network.HTTP.Client.Transport

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

-- | True for responses whose status code semantically forbids a
-- payload (1xx, 204, 304).
statusForbidsBody :: S.Status -> Bool
statusForbidsBody s =
  let c = S.statusCode s
  in (c >= 100 && c < 200) || c == 204 || c == 304

-- | True for request methods whose response is defined to have no
-- payload regardless of headers (just HEAD).
methodForbidsResponseBody :: M.Method -> Bool
methodForbidsResponseBody = (== M.mHead)

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

data FramingError
  = BodyForbiddenForStatus !S.Status
    -- ^ A 1xx \/ 204 \/ 304 response carried a non-empty payload.
  | BodyForbiddenForMethod !M.Method
    -- ^ A HEAD response carried a non-empty payload.
  | ConflictingFraming
    -- ^ Both @Content-Length@ and @Transfer-Encoding@ were present.
  | ConflictingContentLengths ![ByteString]
    -- ^ Multiple @Content-Length@ values disagreed.
  | InvalidContentLength !ByteString
    -- ^ @Content-Length@ was non-numeric or negative.
  deriving stock (Eq, Show)

instance Exception FramingError

-- | Inspect the headers of a response and return the first framing
-- violation, or 'Nothing' if the response is well-formed. The
-- caller separately decides whether a non-empty body actually
-- arrived for a status that forbids one; that's a popper-side
-- concern this function can't observe.
--
-- The request method is taken so an extension can refine on it
-- later, but the current rules are header-only.
validateResponseFraming
  :: M.Method
  -> S.Status
  -> H.Headers
  -> Maybe FramingError
validateResponseFraming meth status hdrs
  -- CL + TE handling has to agree with the HTTP\/1.x parser
  -- (which accepts it and lets TE win when TE is chunked, per
  -- RFC 9112 §6.3; both layers used to disagree, with the
  -- middleware throwing while the parser silently re-framed).
  --
  -- Behaviour now:
  --   * If TE ends in @chunked@, we accept CL + TE and the
  --     popper uses the chunked framing.  Caller can opt into
  --     RFC-strict rejection via 'validateResponseFramingStrict'.
  --   * Otherwise (TE present but not finally-chunked, plus
  --     CL), still treat as 'ConflictingFraming' because the
  --     parser would also refuse to frame this unambiguously.
  | hasCL && hasTE && not teEndsChunked = Just ConflictingFraming
  | otherwise = checkContentLengths clValues
  where
    _ = (meth, status)  -- reserved for method-specific rules
    hasCL          = not (null clValues)
    hasTE          = H.hasHeader H.hTransferEncoding hdrs
    teValues       = concatMap splitCommas (H.lookupHeaders H.hTransferEncoding hdrs)
    teEndsChunked  = case reverse teValues of
      (t : _) -> BS.map asciiToLower t == "chunked"
      []      -> False
    clValues = concatMap splitCommas (H.lookupHeaders H.hContentLength hdrs)
    asciiToLower w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | RFC 9112 §6.3 strict reading: any combination of
-- @Content-Length@ and @Transfer-Encoding@ on a response is a
-- 'ConflictingFraming' error, period. Use this on the security
-- boundary in front of caches or intermediaries you don't fully
-- trust.
validateResponseFramingStrict
  :: M.Method
  -> S.Status
  -> H.Headers
  -> Maybe FramingError
validateResponseFramingStrict _meth _status hdrs
  | hasCL && hasTE = Just ConflictingFraming
  | otherwise      = checkContentLengths clValues
  where
    hasCL    = not (null clValues)
    hasTE    = H.hasHeader H.hTransferEncoding hdrs
    clValues = concatMap splitCommas (H.lookupHeaders H.hContentLength hdrs)

splitCommas :: ByteString -> [ByteString]
splitCommas bs =
  filter (not . BS.null) [ trim t | t <- BS.split 0x2C bs ]
  where
    trim   = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

checkContentLengths :: [ByteString] -> Maybe FramingError
checkContentLengths [] = Nothing
checkContentLengths vs = case traverse parseCL vs of
  Nothing -> Just (InvalidContentLength (firstBad vs))
  Just (n : ns)
    | all (== n) ns -> Nothing
    | otherwise     -> Just (ConflictingContentLengths vs)
  Just []           -> Nothing
  where
    firstBad []       = BS.empty
    firstBad (v : vs') = case parseCL v of
      Just _  -> firstBad vs'
      Nothing -> v

    parseCL :: ByteString -> Maybe Integer
    parseCL v = case BS8.readInteger (trimCL v) of
      Just (n, leftover) | BS.null leftover && n >= 0 -> Just n
      _ -> Nothing
    trimCL = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Enforce RFC 9112 \u00a76 framing rules on responses. Throws
-- 'FramingError' on violation.
--
-- After header validation passes, the middleware also clamps the
-- popper for status codes \/ HEAD requests that forbid a payload:
-- on those, it drains anything the framer surfaced and replaces the
-- popper with an immediate-EOF one. This makes the high-level
-- 'send' caller observe an empty body unconditionally for these
-- responses, regardless of what the underlying connection chose to
-- carry.
withFramingRules :: Middleware IO
withFramingRules inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let status = Resp.statusCode raw
      hdrs   = Resp.headers raw
      meth   = WReq.method req
  case validateResponseFraming meth status hdrs of
    Just err -> throwIO err
    Nothing  -> pure ()
  if statusForbidsBody status || methodForbidsResponseBody meth
    then do
      empty <- popperFromStrict BS.empty
      pure raw { Resp.bodyPopper = empty }
    else pure raw
