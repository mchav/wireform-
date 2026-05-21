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
module Network.HTTP.Client.Test
  ( -- * Mock construction
    mockTransport
  , stub
  , stubBytes
  , stubStatus
  , stubJSON
  , stubSequence
  , stubRoutes
    -- * MockAPI declarative routing
  , MockAPI (..)
  , Route
  , on
  , on_
  , mockAPI
  , throwUnexpected
    -- * Resource mocks (CRUD)
  , ResourceConfig (..)
  , resource
    -- * State machines
  , StateMachine (..)
  , stateMachine
    -- * Expectations
  , withExpectations
  , expect
  , expect_
  , MockExpectation
  , ExpectedCount (..)
  , ExpectationNotMet (..)
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
  , hasURIPath
  , hasURIPathPrefix
  , hasQueryParam
  , hasQueryParamPresent
  , hasHeaderEq
  , hasHeaderPresent
  , hasBody
  , hasBodyContaining
  , hasJSONBody
  , bodyMatches
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

import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI

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
stubRoutes rs = Transport $ \req -> do
  reqBody <- bodyStreamBytes (body req)
  bsRebuilt <- streamFromStrict reqBody
  let reReq = req { body = bsRebuilt }
      rec' = mkRecordedRequest req reqBody
  case lookupRoute rec' rs of
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
-- Declarative MockAPI
-- ---------------------------------------------------------------------------

-- | A pair of a request matcher and a handler. Routes are tried in
-- order; the first match wins.
data Route = Route !RequestMatcher !(Request BodyStream -> ByteString -> IO RawResponse)

-- | A small DSL for assembling a mock HTTP API from matched routes.
data MockAPI = MockAPI
  { routes    :: ![Route]
  , fallback  :: !(Request BodyStream -> ByteString -> IO RawResponse)
    -- ^ Called when no route matches. The provided body bytes are
    --   the already-drained request body.
  }

-- | Pair a matcher with a handler. The handler receives the prepared
-- request (with a fresh popper that yields the buffered body once)
-- and the drained body as a strict 'ByteString' for convenience.
on
  :: RequestMatcher
  -> (Request BodyStream -> ByteString -> IO RawResponse)
  -> Route
on = Route

-- | Shorthand for a route that always returns a constant response.
on_ :: RequestMatcher -> IO RawResponse -> Route
on_ m r = Route m (\_ _ -> r)

-- | Assemble a 'MockAPI' into a 'Transport'. Request bodies are
-- drained upfront and rebuilt as a fresh popper so handlers can
-- inspect them and downstream code still sees a streamable body.
mockAPI :: MockAPI -> Transport IO
mockAPI api = Transport $ \req -> do
  reqBody <- bodyStreamBytes (body req)
  rebuilt <- streamFromStrict reqBody
  let reReq = req { body = rebuilt }
      rec'  = mkRecordedRequest req reqBody
      step []                  = fallback api reReq reqBody
      step (Route m h : rest)
        | matches m rec' = h reReq reqBody
        | otherwise      = step rest
  step (routes api)

-- | Default fallback: throw 'UnexpectedRequest'. Use this for tests
-- that should fail loudly on unexpected calls.
throwUnexpected :: Request BodyStream -> ByteString -> IO RawResponse
throwUnexpected req drained =
  throwIO (UnexpectedRequest (mkRecordedRequest req drained))

-- ---------------------------------------------------------------------------
-- Resource mocks: a small CRUD API generated from a config
-- ---------------------------------------------------------------------------

-- | Configuration for a 'resource' mock collection.
data ResourceConfig a = ResourceConfig
  { basePath   :: !Text
    -- ^ e.g. @"\/users"@.
  , idField    :: !(a -> Text)
    -- ^ How to extract the canonical id from a value.
  , generateId :: !(IO Text)
    -- ^ How to generate an id for newly-POSTed items that don't
    --   carry one.
  }

-- | Build the route table for a CRUD collection backed by an in-memory
-- map. Generates:
--
--   * @GET    \<base\>@         — list all (JSON array)
--   * @GET    \<base\>\/{id}@   — get one, 404 if missing
--   * @POST   \<base\>@         — create (returns 201)
--   * @PUT    \<base\>\/{id}@   — replace, 404 if missing
--   * @DELETE \<base\>\/{id}@   — remove, 404 if missing
resource
  :: forall a. (FromJSON a, ToJSON a)
  => ResourceConfig a -> IO [Route]
resource cfg = do
  store <- newTVarIO (Map.empty :: Map Text a)
  let ResourceConfig
        { basePath   = base
        , idField    = idOf
        , generateId = genId
        } = cfg
      jsonHdr = [(H.hContentType, "application/json")]
      jsonResp s v = rawResponse s jsonHdr (BSL.toStrict (Aeson.encode v))
  pure
    [ Route (hasMethod M.mGet <> hasURIPath base) $ \_ _ -> do
        xs <- Map.elems <$> readTVarIO store
        jsonResp S.status200 xs

    , Route (hasMethod M.mGet <> hasURIPathPrefix (base <> "/")) $ \req _ -> do
        let i = idFromPath base (rrURI (mkRecordedRequest req ""))
        m <- readTVarIO store
        case Map.lookup i m of
          Just v  -> jsonResp S.status200 v
          Nothing -> rawResponse S.status404 [] ""

    , Route (hasMethod M.mPost <> hasURIPath base) $ \_ rawBody ->
        case Aeson.eitherDecodeStrict rawBody of
          Left err -> rawResponse S.status400 [] (BS8.pack err)
          Right v  -> do
            -- If the value already carries an id (via idField),
            -- prefer that; otherwise mint a fresh one.
            i <- let candidate = idOf v
                 in if T.null candidate
                      then genId
                      else pure candidate
            atomically (modifyTVar' store (Map.insert i v))
            jsonResp S.status201 v

    , Route (hasMethod M.mPut <> hasURIPathPrefix (base <> "/")) $ \req rawBody -> do
        let i = idFromPath base (rrURI (mkRecordedRequest req ""))
        existing <- Map.member i <$> readTVarIO store
        if not existing
          then rawResponse S.status404 [] ""
          else case Aeson.eitherDecodeStrict rawBody of
            Left err -> rawResponse S.status400 [] (BS8.pack err)
            Right v  -> do
              atomically (modifyTVar' store (Map.insert i v))
              jsonResp S.status200 v

    , Route (hasMethod M.mDelete <> hasURIPathPrefix (base <> "/")) $ \req _ -> do
        let i = idFromPath base (rrURI (mkRecordedRequest req ""))
        wasPresent <- atomically $ do
          m <- readTVar store
          if Map.member i m
            then writeTVar store (Map.delete i m) >> pure True
            else pure False
        if wasPresent
          then rawResponse S.status204 [] ""
          else rawResponse S.status404 [] ""
    ]
  where
    idFromPath base p =
      let prefix = base <> "/"
      in case T.stripPrefix prefix (T.takeWhile (/= '?') p) of
           Just rest -> T.takeWhile (/= '/') rest
           Nothing   -> ""

-- ---------------------------------------------------------------------------
-- State machine mocks
-- ---------------------------------------------------------------------------

-- | A mock whose response depends on accumulated state. The state
-- variable lives in a 'TVar' so the transition is atomic.
data StateMachine s = StateMachine
  { initialState :: !s
  , transition   :: !(s -> Request BodyStream -> ByteString -> IO (s, RawResponse))
  }

-- | Allocate a state-machine-backed transport.
stateMachine :: StateMachine s -> IO (Transport IO)
stateMachine sm = do
  var <- newTVarIO (initialState sm)
  pure $ Transport $ \req -> do
    bs <- bodyStreamBytes (body req)
    rebuilt <- streamFromStrict bs
    let req' = req { body = rebuilt }
    s <- readTVarIO var
    (s', resp) <- transition sm s req' bs
    atomically (writeTVar var s')
    pure resp

-- ---------------------------------------------------------------------------
-- Expectations
-- ---------------------------------------------------------------------------

-- | A single expectation: a 'RequestMatcher', a count bound, and a
-- handler. Used with 'withExpectations'. Constructed via 'expect'
-- or 'expect_'.
data MockExpectation = MockExpectation
  !RequestMatcher
  !ExpectedCount
  !(Request BodyStream -> ByteString -> IO RawResponse)

data ExpectedCount
  = Exactly !Int
  | AtLeast !Int
  | AtMost  !Int
  | Between !Int !Int
  | AnyTimes
  deriving stock (Eq, Show)

satisfiesCount :: ExpectedCount -> Int -> Bool
satisfiesCount c n = case c of
  Exactly k     -> n == k
  AtLeast k     -> n >= k
  AtMost  k     -> n <= k
  Between lo hi -> n >= lo && n <= hi
  AnyTimes      -> True

data ExpectationNotMet = ExpectationNotMet
  { metMatcher  :: !Text
  , metExpected :: !ExpectedCount
  , metActual   :: !Int
  }
  deriving stock (Show)

instance Exception ExpectationNotMet

expect
  :: RequestMatcher
  -> ExpectedCount
  -> (Request BodyStream -> ByteString -> IO RawResponse)
  -> MockExpectation
expect = MockExpectation

expect_ :: RequestMatcher -> ExpectedCount -> IO RawResponse -> MockExpectation
expect_ m c r = MockExpectation m c (\_ _ -> r)

-- | Run an action against a transport whose behaviour is governed by
-- a list of 'MockExpectation's. Throws 'UnexpectedRequest' if a
-- non-matching request arrives and 'ExpectationNotMet' at teardown
-- if any expected count is violated.
withExpectations :: [MockExpectation] -> (Transport IO -> IO a) -> IO a
withExpectations es action = do
  counters <- mapM (\_ -> newTVarIO (0 :: Int)) es
  let transport = Transport $ \req -> do
        bs <- bodyStreamBytes (body req)
        rebuilt <- streamFromStrict bs
        let req' = req { body = rebuilt }
            rec' = mkRecordedRequest req bs
            go [] _ = throwIO (UnexpectedRequest rec')
            go (MockExpectation m _ h : restE) (cnt : restC)
              | matches m rec' = do
                  atomically (modifyTVar' cnt (+ 1))
                  h req' bs
              | otherwise = go restE restC
            go _ _ = throwIO (UnexpectedRequest rec')
        go es counters
  result <- action transport
  forM_ (zip es counters) $ \(MockExpectation m c _, cntVar) -> do
    actual <- readTVarIO cntVar
    unless (satisfiesCount c actual) $
      throwIO ExpectationNotMet
        { metMatcher  = matcherDescription m
        , metExpected = c
        , metActual   = actual
        }
  pure result

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
        reqBody <- bodyStreamBytes (body req)
        bs <- streamFromStrict reqBody
        let req' = req { body = bs }
        raw <- sendRaw inner req'
        respBody <- popperBytes (bodyPopper raw)
        let rec' = mkRecordedRequest req' reqBody
            res' = RecordedResponse (statusCode raw) (Network.HTTP.Client.Response.headers raw) respBody
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
  , rrHeaders = Network.HTTP.Client.Request.headers req
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

-- | Match on just the path (ignores query string and fragment).
hasURIPath :: T.Text -> RequestMatcher
hasURIPath p = RequestMatcher
  { matcherDescription = "path == " <> p
  , matcherPredicate   = (== p) . pathPart . rrURI
  }

-- | Match on a path prefix (ignores query string and fragment).
hasURIPathPrefix :: T.Text -> RequestMatcher
hasURIPathPrefix p = RequestMatcher
  { matcherDescription = "path starts with " <> p
  , matcherPredicate   = T.isPrefixOf p . pathPart . rrURI
  }

pathPart :: T.Text -> T.Text
pathPart t =
  let withoutQuery = T.takeWhile (/= '?') t
  in T.takeWhile (/= '#') withoutQuery

-- | Match a specific @?name=value@ query parameter.
hasQueryParam :: T.Text -> T.Text -> RequestMatcher
hasQueryParam name val = RequestMatcher
  { matcherDescription = "?" <> name <> "=" <> val
  , matcherPredicate = \r -> (name, Just val) `elem` queryPairs (rrURI r)
  }

-- | Match the presence of a query parameter regardless of value.
hasQueryParamPresent :: T.Text -> RequestMatcher
hasQueryParamPresent name = RequestMatcher
  { matcherDescription = "?" <> name
  , matcherPredicate = \r -> any (\(k, _) -> k == name) (queryPairs (rrURI r))
  }

queryPairs :: T.Text -> [(T.Text, Maybe T.Text)]
queryPairs t =
  let after = T.dropWhile (/= '?') t
  in case T.uncons after of
       Just ('?', rest) ->
         let stripped = T.takeWhile (/= '#') rest
             pieces = T.splitOn "&" stripped
         in map parseQueryPiece (filter (not . T.null) pieces)
       _ -> []
  where
    parseQueryPiece p =
      case T.breakOn "=" p of
        (k, eqv) -> case T.uncons eqv of
          Just ('=', v) -> (k, Just v)
          _             -> (k, Nothing)

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

-- | Match when the request body parses as JSON equal to the given
-- value. Useful for testing that a serialised payload matches an
-- expected shape regardless of field ordering.
hasJSONBody :: (FromJSON a, Eq a) => a -> RequestMatcher
hasJSONBody expected = RequestMatcher
  { matcherDescription = "JSON body matches expected value"
  , matcherPredicate   = \r -> case Aeson.eitherDecodeStrict (rrBody r) of
      Right v -> v == expected
      Left  _ -> False
  }

bodyMatches :: (ByteString -> Bool) -> RequestMatcher
bodyMatches p = RequestMatcher
  { matcherDescription = "body matches predicate"
  , matcherPredicate   = p . rrBody
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
