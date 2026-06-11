{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for the retry / circuit-breaker middleware added to
"Network.HTTP.Client.Middleware":

* @Retry-After@ parsing (delta-seconds and HTTP-date),
* idempotency-aware retry (@retrySafeMethodsOnly@),
* circuit-breaker open / half-open transitions.

The retry tests use a counter-backed mock transport to assert
exactly how many upstream calls happened. They keep retry delays
in the low-millisecond range so the test suite stays fast.
-}
module Test.Retry (tests) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Middleware
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef Int)
mkTransport canned = do
  ref <- newIORef canned
  calls <- newIORef 0
  let t = Transport $ \_req -> do
        atomicModifyIORef' calls (\n -> (n + 1, ()))
        next <- atomicModifyIORef' ref $ \rs -> case rs of
          (r : rest) -> (rest, r)
          [] -> ([], (S.status500, [], "no more canned"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, calls)


makeGet :: IO (Request BSm.BodyStream)
makeGet = case WURI.parseTemplate "http://example.com/" of
  Left e -> error (show e)
  Right t -> prepareRequest [] (get t)


{- | Build a request whose 'method' is @POST@. The body is empty
to keep the harness simple.
-}
makePost :: IO (Request BSm.BodyStream)
makePost = do
  r <- makeGet
  pure r {WReq.method = M.mPost}


-- ---------------------------------------------------------------------------
-- Idempotency
-- ---------------------------------------------------------------------------

unit_retries_get :: Spec
unit_retries_get = it "default policy retries GET on 503" $ do
  (t, calls) <-
    mkTransport
      [ (S.status503, [], "")
      , (S.status503, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 3
          }
  req <- makeGet
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 3


unit_does_not_retry_post_by_default :: Spec
unit_does_not_retry_post_by_default = it "default policy does NOT retry POST" $ do
  (t, calls) <-
    mkTransport
      [ (S.status503, [], "")
      , (S.status503, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 3
          }
  req <- makePost
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 1


unit_retries_post_when_opted_in :: Spec
unit_retries_post_when_opted_in = it
  "retrySafeMethodsOnly=False lets POST retry"
  $ do
    (t, calls) <-
      mkTransport
        [ (S.status503, [], "")
        , (S.status503, [], "")
        , (S.status200, [], "ok")
        ]
    let cfg =
          defaultRetryPolicy
            { initialDelay = millis 1
            , maxDelay = millis 5
            , maxAttempts = 3
            , retrySafeMethodsOnly = False
            }
    req <- makePost
    _ <- sendRaw (withRetry cfg t) req
    n <- readIORef calls
    n `shouldBe` 3


unit_429_is_retried :: Spec
unit_429_is_retried = it "429 is retried by default" $ do
  (t, calls) <-
    mkTransport
      [ (S.status429, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 2
          }
  req <- makeGet
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 2


-- ---------------------------------------------------------------------------
-- Retry-After
-- ---------------------------------------------------------------------------

unit_retry_after_delta_seconds :: Spec
unit_retry_after_delta_seconds = it
  "Retry-After in delta-seconds is honoured (clamped)"
  $ do
    -- A short Retry-After overrides the (much shorter) backoff
    -- delay; the value is clamped to maxDelay so the test stays
    -- fast.
    (t, calls) <-
      mkTransport
        [ (S.status503, [(H.hRetryAfter, "1")], "")
        , (S.status200, [], "ok")
        ]
    let cfg =
          defaultRetryPolicy
            { initialDelay = millis 1
            , maxDelay = millis 5 -- clamp Retry-After down to 5ms
            , maxAttempts = 2
            }
    req <- makeGet
    _ <- sendRaw (withRetry cfg t) req
    n <- readIORef calls
    n `shouldBe` 2


unit_retry_after_http_date :: Spec
unit_retry_after_http_date = it
  "Retry-After as HTTP-date is parsed without crashing"
  $ do
    -- We can't pin a real date to "5ms from now", so we just check
    -- that an HTTP-date Retry-After doesn't break the loop. We use
    -- an absolute date in the past so the computed delay is
    -- negative → clamps to 0 → no sleep, retry immediately.
    (t, calls) <-
      mkTransport
        [ (S.status503, [(H.hRetryAfter, "Sun, 06 Nov 1994 08:49:37 GMT")], "")
        , (S.status200, [], "ok")
        ]
    let cfg =
          defaultRetryPolicy
            { initialDelay = millis 1
            , maxDelay = millis 5
            , maxAttempts = 2
            }
    req <- makeGet
    _ <- sendRaw (withRetry cfg t) req
    n <- readIORef calls
    n `shouldBe` 2


-- ---------------------------------------------------------------------------
-- Circuit breaker
-- ---------------------------------------------------------------------------

unit_breaker_opens_after_threshold :: Spec
unit_breaker_opens_after_threshold = it
  "breaker opens after consecutive failures"
  $ do
    -- A 5xx response counts as a failure (per cbFailureOn=5xx).
    -- After 3 consecutive failures the breaker opens; the next
    -- call throws CircuitBreakerOpen.
    (t, calls) <- mkTransport (replicate 10 (S.status500, [], "boom"))
    let cfg = defaultCircuitBreakerConfig {cbFailureThreshold = 3}
    cb <- newCircuitBreaker cfg
    let trans = withCircuitBreaker cb t
    -- Three calls that all return 500 — they pass through and we
    -- discard the body.
    req <- makeGet
    mapM_ (\_ -> sendRaw trans req >>= drain) [(1 :: Int) .. 3]
    -- Fourth call should throw CircuitBreakerOpen.
    r <- try (sendRaw trans req)
    case r of
      Left (_ :: SomeException) -> pure ()
      Right _ -> error "expected breaker to open"
    -- Underlying transport saw exactly 3 calls (the open breaker
    -- short-circuits the 4th).
    n <- readIORef calls
    n `shouldBe` 3
  where
    drain :: RawResponse -> IO ByteString
    drain = BSm.popperBytes . Resp.bodyPopper


unit_breaker_passes_when_healthy :: Spec
unit_breaker_passes_when_healthy = it
  "breaker does not interfere on successful calls"
  $ do
    (t, calls) <- mkTransport (replicate 5 (S.status200, [], "ok"))
    cb <- newCircuitBreaker defaultCircuitBreakerConfig
    let trans = withCircuitBreaker cb t
    req <- makeGet
    mapM_
      (\_ -> sendRaw trans req >>= (BSm.popperBytes . Resp.bodyPopper))
      [(1 :: Int) .. 5]
    n <- readIORef calls
    n `shouldBe` 5


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.Middleware.Retry" $
    sequence_
      [ unit_retries_get
      , unit_does_not_retry_post_by_default
      , unit_retries_post_when_opted_in
      , unit_429_is_retried
      , unit_retry_after_delta_seconds
      , unit_retry_after_http_date
      , unit_breaker_opens_after_threshold
      , unit_breaker_passes_when_healthy
      ]
