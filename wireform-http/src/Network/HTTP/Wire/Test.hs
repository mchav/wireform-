{- | Mock transports, stub helpers, request logging, and assertions
for testing wireform HTTP clients.

The big idea is that a 'Transport' is a newtype around a function,
so a mock is a lambda. No frameworks, no mocking libraries:

> stub :: Transport IO
> stub = mockTransport (\\_ -> pure (ok200 "ok"))

The helpers in this module add convenience: route tables, JSON
stubs, request log, and a small assertion library that pairs
matchers (also reused as routing predicates) with expected
properties of a recorded request log.

This module is part of @wireform-http@ rather than a separate
@-test@ package because the request \/ response types it manipulates
are the same ones used in production — VCR sanitisation and
assertions need to talk in the same vocabulary as the live client.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Wire.Test
  ( -- * Mock construction
    mockTransport
  , stub
  , stubBytes
  , stubStatus
  , stubJSON
  , stubSequence
  , stubRoutes
    -- * Raw-response builders
  , rawResponse
  , ok200
  , json200
  , notFound404
  , serverError500
    -- * Request log
  , RequestLog
  , RecordedRequest (..)
  , RecordedResponse (..)
  , recordRequest
  , withRequestLog
  , requestsOf
  , clearLog
    -- * Matchers
  , RequestMatcher (..)
  , ResponseMatcher (..)
  , hasMethod
  , hasURI
  , hasURIPrefix
  , hasHeaderEq
  , hasHeaderPresent
  , hasBody
  , hasBodyContaining
  , hasStatus
  , hasResponseHeader
  , matchesRequest
  , matchesResponse
    -- * Assertions
  , Assertion (..)
  , AssertionFailure (..)
  , assertLog
  , requestCount
  , requestCountWhere
  , anyRequest
  , noRequest
  , nthRequest
  , responseFor
    -- * Errors
  , UnexpectedRequest (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Wire.BodyStream
import Network.HTTP.Wire.Protocol
import Network.HTTP.Wire.Request
import Network.HTTP.Wire.Response
import Network.HTTP.Wire.Transport
import Network.HTTP.Wire.URI

-- ---------------------------------------------------------------------------
-- Raw-response builders
-- ---------------------------------------------------------------------------

-- | Build a 'RawResponse' from a strict body and a header list. The
-- popper yields the body once and then EOF.
rawResponse :: S.Status -> [H.Header] -> ByteString -> IO RawResponse
rawResponse status hdrs body_ = do
  p <- popperFromStrict body_
  pure RawResponse
    { statusCode   = status
    , headers      = hdrs
    , bodyPopper   = p
    , protocolInfo = HTTP1_1
    }

ok200 :: ByteString -> IO RawResponse
ok200 = rawResponse S.status200 []

json200 :: ToJSON a => a -> IO RawResponse
json200 a = rawResponse S.status200
  [(H.hContentType, "application/json; charset=utf-8")]
  (BSL.toStrict (Aeson.encode a))

notFound404 :: IO RawResponse
notFound404 = rawResponse S.status404 [] ""

serverError500 :: IO RawResponse
serverError500 = rawResponse S.status500 [] ""

-- ---------------------------------------------------------------------------
-- Mock construction
-- ---------------------------------------------------------------------------

-- | The fundamental mock: a function from request to response. This
-- is literally the 'Transport' constructor; the name exists for
-- discoverability.
mockTransport :: (Request BodyStream -> IO RawResponse) -> Transport IO
mockTransport = Transport

-- | A transport that always returns the same status and body.
stub :: S.Status -> ByteString -> Transport IO
stub status body_ = Transport $ \_ -> rawResponse status [] body_

-- | Same as 'stub' but with caller-provided headers.
stubBytes :: S.Status -> [H.Header] -> ByteString -> Transport IO
stubBytes status hdrs body_ = Transport $ \_ -> rawResponse status hdrs body_

-- | A transport that always returns the given status code and an
-- empty body.
stubStatus :: S.Status -> Transport IO
stubStatus s = Transport $ \_ -> rawResponse s [] ""

-- | A transport that encodes a value as JSON and returns it with the
-- given status (defaults to 200 inside the body of the lambda).
stubJSON :: ToJSON a => S.Status -> a -> Transport IO
stubJSON status a = Transport $ \_ -> rawResponse status
  [(H.hContentType, "application/json; charset=utf-8")]
  (BSL.toStrict (Aeson.encode a))

-- | A transport that returns responses in the given sequence. Each
-- call advances the cursor; running off the end throws.
stubSequence :: [IO RawResponse] -> IO (Transport IO)
stubSequence responses = do
  ref <- newIORef responses
  pure $ Transport $ \req -> atomicModifyIORef' ref (\case
    []     -> ([], Nothing)
    (r:rs) -> (rs, Just r)) >>= \case
      Just r  -> r
      Nothing -> throwIO (UnexpectedRequest (mkRecordedRequest req ""))

-- | A simple routing mock: try each predicate in order, first match
-- wins. The 'Nothing' result of all predicates throws
-- 'UnexpectedRequest'.
stubRoutes
  :: [(RequestMatcher, Request BodyStream -> IO RawResponse)]
  -> Transport IO
stubRoutes routes = Transport $ \req -> do
  reqBody <- drainBodyStream (body req)
  bsRebuilt <- streamFromStrict reqBody
  let reReq = req { body = bsRebuilt }
      rec' = mkRecordedRequest req reqBody
  case lookupRoute rec' routes of
    Just handler -> handler reReq
    Nothing      -> throwIO (UnexpectedRequest rec')

lookupRoute
  :: RecordedRequest
  -> [(RequestMatcher, a)]
  -> Maybe a
lookupRoute _ [] = Nothing
lookupRoute rec' ((m, h) : rest)
  | matches m rec' = Just h
  | otherwise      = lookupRoute rec' rest

-- ---------------------------------------------------------------------------
-- Request log
-- ---------------------------------------------------------------------------

-- | A recording of a request as it left the transport stack. The body
-- is fully materialised for inspection.
data RecordedRequest = RecordedRequest
  { rrMethod  :: !M.Method
  , rrURI     :: !T.Text
  , rrHeaders :: ![H.Header]
  , rrBody    :: !ByteString
  }
  deriving stock (Show, Eq)

-- | A recording of a response as it came back from the transport. The
-- body is fully drained.
data RecordedResponse = RecordedResponse
  { rsStatus  :: !S.Status
  , rsHeaders :: ![H.Header]
  , rsBody    :: !ByteString
  }
  deriving stock (Show, Eq)

newtype RequestLog = RequestLog
  { logRef :: IORef [(RecordedRequest, RecordedResponse)] }

-- | Build a 'RequestLog' and a transport that appends to it. The
-- recorded request includes the drained body; the response body is
-- drained, stored, and replaced with a popper served from the
-- buffered bytes so downstream callers see the same data.
withRequestLog :: Transport IO -> IO (Transport IO, RequestLog)
withRequestLog inner = do
  ref <- newIORef []
  let wrapped = Transport $ \req -> do
        reqBody <- drainBodyStream (body req)
        bs <- streamFromStrict reqBody
        let req' = req { body = bs }
        raw <- sendRaw inner req'
        respBody <- drainPopper (bodyPopper raw)
        let rec' = mkRecordedRequest req' reqBody
            res' = RecordedResponse (statusCode raw) (Network.HTTP.Wire.Response.headers raw) respBody
        atomicModifyIORef' ref $ \xs -> (xs <> [(rec', res')], ())
        newPopper <- popperFromStrict respBody
        pure raw { bodyPopper = newPopper }
  pure (wrapped, RequestLog ref)

-- | Snapshot the log. Pure-ish: returns whatever was recorded up to
-- this point. The list is in chronological order.
requestsOf :: RequestLog -> IO [(RecordedRequest, RecordedResponse)]
requestsOf = readIORef . logRef

-- | Drop all recorded interactions. Useful between subtests.
clearLog :: RequestLog -> IO ()
clearLog l = writeIORef (logRef l) []

-- | Manually append to a log. Mostly for VCR replay paths that want
-- the same log surface.
recordRequest :: RequestLog -> RecordedRequest -> RecordedResponse -> IO ()
recordRequest l q r = atomicModifyIORef' (logRef l) $ \xs -> (xs <> [(q, r)], ())

mkRecordedRequest :: Request BodyStream -> ByteString -> RecordedRequest
mkRecordedRequest req drained = RecordedRequest
  { rrMethod  = method req
  , rrURI     = requestURIToText (requestURI req)
  , rrHeaders = Network.HTTP.Wire.Request.headers req
  , rrBody    = drained
  }

-- ---------------------------------------------------------------------------
-- Matchers
-- ---------------------------------------------------------------------------

-- | A predicate on a recorded request with a description string for
-- error messages. 'Semigroup' composes by AND.
data RequestMatcher = RequestMatcher
  { matcherDescription :: !T.Text
  , matcherPredicate   :: !(RecordedRequest -> Bool)
  }

instance Semigroup RequestMatcher where
  a <> b = RequestMatcher
    { matcherDescription = matcherDescription a <> " AND " <> matcherDescription b
    , matcherPredicate   = \r -> matcherPredicate a r && matcherPredicate b r
    }

instance Monoid RequestMatcher where
  mempty = RequestMatcher "any" (\_ -> True)

data ResponseMatcher = ResponseMatcher
  { responseMatcherDescription :: !T.Text
  , responseMatcherPredicate   :: !(RecordedResponse -> Bool)
  }

instance Semigroup ResponseMatcher where
  a <> b = ResponseMatcher
    { responseMatcherDescription = responseMatcherDescription a <> " AND " <> responseMatcherDescription b
    , responseMatcherPredicate   = \r -> responseMatcherPredicate a r && responseMatcherPredicate b r
    }

instance Monoid ResponseMatcher where
  mempty = ResponseMatcher "any" (\_ -> True)

matches :: RequestMatcher -> RecordedRequest -> Bool
matches = matcherPredicate

matchesRequest :: RequestMatcher -> RecordedRequest -> Bool
matchesRequest = matcherPredicate

matchesResponse :: ResponseMatcher -> RecordedResponse -> Bool
matchesResponse = responseMatcherPredicate

hasMethod :: M.Method -> RequestMatcher
hasMethod m = RequestMatcher
  { matcherDescription = "method == " <> T.pack (show m)
  , matcherPredicate   = (== m) . rrMethod
  }

hasURI :: T.Text -> RequestMatcher
hasURI u = RequestMatcher
  { matcherDescription = "uri == " <> u
  , matcherPredicate   = (== u) . rrURI
  }

hasURIPrefix :: T.Text -> RequestMatcher
hasURIPrefix u = RequestMatcher
  { matcherDescription = "uri starts with " <> u
  , matcherPredicate   = T.isPrefixOf u . rrURI
  }

hasHeaderEq :: H.HeaderName -> H.HeaderValue -> RequestMatcher
hasHeaderEq n v = RequestMatcher
  { matcherDescription = "header " <> T.pack (show n) <> " == " <> TE.decodeUtf8 v
  , matcherPredicate   = \r -> H.lookupHeader n (rrHeaders r) == Just v
  }

hasHeaderPresent :: H.HeaderName -> RequestMatcher
hasHeaderPresent n = RequestMatcher
  { matcherDescription = "has header " <> T.pack (show n)
  , matcherPredicate   = \r -> H.hasHeader n (rrHeaders r)
  }

hasBody :: ByteString -> RequestMatcher
hasBody bs = RequestMatcher
  { matcherDescription = "body == " <> T.pack (show bs)
  , matcherPredicate   = (== bs) . rrBody
  }

hasBodyContaining :: ByteString -> RequestMatcher
hasBodyContaining needle = RequestMatcher
  { matcherDescription = "body contains " <> T.pack (show needle)
  , matcherPredicate   = \r -> needle `BS.isInfixOf` rrBody r
  }

hasStatus :: S.Status -> ResponseMatcher
hasStatus s = ResponseMatcher
  { responseMatcherDescription = "status == " <> T.pack (show s)
  , responseMatcherPredicate   = (== s) . rsStatus
  }

hasResponseHeader :: H.HeaderName -> H.HeaderValue -> ResponseMatcher
hasResponseHeader n v = ResponseMatcher
  { responseMatcherDescription = "response header " <> T.pack (show n) <> " == " <> TE.decodeUtf8 v
  , responseMatcherPredicate   = \r -> H.lookupHeader n (rsHeaders r) == Just v
  }

-- ---------------------------------------------------------------------------
-- Assertions
-- ---------------------------------------------------------------------------

data Assertion = Assertion
  { assertionDescription :: !T.Text
  , assertionCheck       :: [(RecordedRequest, RecordedResponse)] -> Either AssertionFailure ()
  }

data AssertionFailure = AssertionFailure
  { failureDescription :: !T.Text
  , failureActual      :: !T.Text
  }
  deriving stock (Show)

instance Exception AssertionFailure

-- | Run an assertion against a log. Throws 'AssertionFailure' on
-- failure, so it composes with hspec\/tasty\/HUnit out of the box.
assertLog :: RequestLog -> Assertion -> IO ()
assertLog log_ a = do
  xs <- requestsOf log_
  case assertionCheck a xs of
    Right () -> pure ()
    Left err -> throwIO err

requestCount :: Int -> Assertion
requestCount n = Assertion
  { assertionDescription = "exactly " <> T.pack (show n) <> " requests"
  , assertionCheck = \xs ->
      if length xs == n
        then Right ()
        else Left AssertionFailure
          { failureDescription = "expected " <> T.pack (show n) <> " requests"
          , failureActual      = "got " <> T.pack (show (length xs))
          }
  }

requestCountWhere :: RequestMatcher -> Int -> Assertion
requestCountWhere m n = Assertion
  { assertionDescription = matcherDescription m <> " : exactly " <> T.pack (show n)
  , assertionCheck = \xs ->
      let actual = length (filter (matches m . fst) xs)
      in if actual == n
           then Right ()
           else Left AssertionFailure
             { failureDescription = "expected " <> T.pack (show n) <> " matching '" <> matcherDescription m <> "'"
             , failureActual      = "got " <> T.pack (show actual)
             }
  }

anyRequest :: RequestMatcher -> Assertion
anyRequest m = Assertion
  { assertionDescription = "at least one request matches " <> matcherDescription m
  , assertionCheck = \xs ->
      if any (matches m . fst) xs
        then Right ()
        else Left AssertionFailure
          { failureDescription = "expected at least one request matching '" <> matcherDescription m <> "'"
          , failureActual      = "none"
          }
  }

noRequest :: RequestMatcher -> Assertion
noRequest m = Assertion
  { assertionDescription = "no request matches " <> matcherDescription m
  , assertionCheck = \xs ->
      if not (any (matches m . fst) xs)
        then Right ()
        else Left AssertionFailure
          { failureDescription = "expected no request matching '" <> matcherDescription m <> "'"
          , failureActual      = "got " <> T.pack (show (length (filter (matches m . fst) xs)))
          }
  }

nthRequest :: Int -> RequestMatcher -> Assertion
nthRequest i m = Assertion
  { assertionDescription = "request " <> T.pack (show i) <> " matches " <> matcherDescription m
  , assertionCheck = \xs ->
      if i < length xs && matches m (fst (xs !! i))
        then Right ()
        else Left AssertionFailure
          { failureDescription = "request " <> T.pack (show i) <> " to match '" <> matcherDescription m <> "'"
          , failureActual      = "index out of bounds or non-match"
          }
  }

responseFor :: RequestMatcher -> ResponseMatcher -> Assertion
responseFor rm responseM = Assertion
  { assertionDescription = "response for " <> matcherDescription rm <> " matches " <> responseMatcherDescription responseM
  , assertionCheck = \xs ->
      let matchingIdx = [ snd x | x <- xs, matches rm (fst x) ]
      in case matchingIdx of
           []      -> Left AssertionFailure
             { failureDescription = "no request matched '" <> matcherDescription rm <> "'"
             , failureActual      = "0 matches"
             }
           (r:_)
             | matchesResponse responseM r -> Right ()
             | otherwise -> Left AssertionFailure
                 { failureDescription = "response should match '" <> responseMatcherDescription responseM <> "'"
                 , failureActual      = T.pack (show r)
                 }
  }

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

newtype UnexpectedRequest = UnexpectedRequest RecordedRequest
  deriving stock (Show)

instance Exception UnexpectedRequest

-- Pin imports we don\'t use elsewhere so -Wunused doesn\'t bite.
_unused :: BS8.ByteString
_unused = BS8.pack ""
