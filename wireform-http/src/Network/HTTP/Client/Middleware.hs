{- | Built-in middleware combinators.

Every middleware here is @'Transport' m -> 'Transport' m@. Composition
is just function composition, so a stack reads top-down:

@
stack =
    withLogging logger
  . withRetry retryPolicy
  . withTimeout (seconds 30)
  . withAuth (Bearer \"abc\")
  $ baseTransport
@

The middleware in this module is deliberately mechanical — no
state-management, no clever scheduling. Stateful behaviour (rate
limiting, cookie jar) lives in dedicated modules so the state
allocation is explicit at the call site.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Middleware
  ( -- * Re-exports
    Middleware
    -- * Logging
  , Logger (..)
  , stdoutLogger
  , noopLogger
  , withLogging
    -- * Auth
  , AuthScheme (..)
  , withAuth
    -- * Timing
  , Duration
  , millis
  , seconds
  , toMicros
  , withTimeout
    -- * Retry
  , RetryPolicy (..)
  , defaultRetryPolicy
  , exponentialBackoff
  , withRetry
    -- * URI rewriting
  , withBaseURL
    -- * Rate limiting
  , RateLimit
  , newRateLimit
  , withRateLimit
    -- * Fault injection (test fixtures)
  , withLatency
  , withFailureRate
  , failFirstN
  , withSlowBody
  , newFailFirstN
  , newFailureRate
  , withJitter
  , withConnectionReset
  , withTruncation
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified System.Timeout
import qualified UnliftIO.Exception as U

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

newtype Logger = Logger { logLine :: Text -> IO () }

stdoutLogger :: Logger
stdoutLogger = Logger (\t -> putStrLn ("[wireform-http] " <> T.unpack t))

noopLogger :: Logger
noopLogger = Logger (\_ -> pure ())

-- | Log the request method and URI before sending, and the status
-- after. Wraps any 'IO'-based transport.
withLogging :: Logger -> Middleware IO
withLogging logger inner = Transport $ \req -> do
  logLine logger $ "-> " <> T.pack (show (method req)) <> " " <> requestURIToText (requestURI req)
  raw <- sendRaw inner req
  logLine logger $ "<- " <> T.pack (show (statusCode raw))
  pure raw

-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

data AuthScheme
  = Bearer !ByteString
  | Basic  !ByteString !ByteString
  | RawAuth !ByteString  -- exact header value
  deriving stock (Show, Eq)

withAuth :: AuthScheme -> Middleware m
withAuth scheme inner = Transport $ \req ->
  let val = case scheme of
        Bearer t     -> "Bearer " <> t
        Basic u p    -> "Basic "  <> B64.encode (u <> ":" <> p)
        RawAuth raw  -> raw
  in sendRaw inner req
       { Network.HTTP.Client.Request.headers =
           H.insertHeader H.hAuthorization val (Network.HTTP.Client.Request.headers req)
       }

-- ---------------------------------------------------------------------------
-- Duration / Timeout
-- ---------------------------------------------------------------------------

-- | A nanosecond-resolution duration. We resolve to microseconds when
-- handing off to 'System.Timeout.timeout' and 'threadDelay'.
newtype Duration = Duration { unDuration :: Int }
  -- ^ microseconds
  deriving stock (Eq, Ord, Show)

millis :: Int -> Duration
millis n = Duration (n * 1000)

seconds :: Int -> Duration
seconds n = Duration (n * 1_000_000)

toMicros :: Duration -> Int
toMicros = unDuration

data TimeoutException = RequestTimeout
  deriving stock (Show)

instance Exception TimeoutException

withTimeout :: MonadUnliftIO m => Duration -> Middleware m
withTimeout (Duration us) inner = Transport $ \req -> withRunInIO $ \run -> do
  result <- System.Timeout.timeout us (run (sendRaw inner req))
  case result of
    Just r  -> pure r
    Nothing -> throwIO RequestTimeout

-- ---------------------------------------------------------------------------
-- Retry
-- ---------------------------------------------------------------------------

data RetryPolicy = RetryPolicy
  { maxAttempts   :: !Int
  , initialDelay  :: !Duration
  , maxDelay      :: !Duration
  , backoffFactor :: !Double
  , retryOn       :: !(S.Status -> Bool)
  }

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { maxAttempts   = 3
  , initialDelay  = millis 100
  , maxDelay      = seconds 5
  , backoffFactor = 2.0
  , retryOn       = \s -> let c = S.statusCode s in c >= 500 && c < 600
  }

exponentialBackoff :: Int -> RetryPolicy
exponentialBackoff n = defaultRetryPolicy { maxAttempts = n }

-- | Retry middleware. Buffers the request body upfront so the body
-- can be replayed across attempts — this is a deliberate choice;
-- callers who want to retry without buffering should compose retry
-- /below/ a layer that handles their streaming semantics.
withRetry :: RetryPolicy -> Middleware IO
withRetry policy inner = Transport $ \req -> do
  buffered <- bodyStreamBytes (body req)
  let attempt n delay = do
        bs <- streamFromStrict buffered
        let attemptReq = req { Network.HTTP.Client.Request.body = bs }
        raw <- sendRaw inner attemptReq
        if retryOn policy (statusCode raw) && n < maxAttempts policy
          then do
            threadDelay (toMicros delay)
            let next = scaleDuration delay (backoffFactor policy)
                                            (maxDelay policy)
            -- Flush the body of this failed attempt so the
            -- connection / mock can advance, but don't allocate a
            -- ByteString for bytes we won't look at.
            drainPopper (bodyPopper raw)
            attempt (n + 1) next
          else pure raw
  attempt 1 (initialDelay policy)

scaleDuration :: Duration -> Double -> Duration -> Duration
scaleDuration (Duration us) factor (Duration cap) =
  let scaled = round (fromIntegral us * factor :: Double)
  in Duration (min scaled cap)

-- ---------------------------------------------------------------------------
-- URI rewriting / base URL
-- ---------------------------------------------------------------------------

-- | Re-root requests at a 'BaseURL'. Absolute request URIs are passed
-- through untouched; relative paths are joined to the base.
withBaseURL :: BaseURL -> Middleware m
withBaseURL base inner = Transport $ \req ->
  let rewritten = req { requestURI = resolveTemplate base (requestURI req) }
  in sendRaw inner rewritten

resolveTemplate :: BaseURL -> RequestURI -> RequestURI
resolveTemplate base ru =
  let reqText = requestURIToText ru
  in if isAbsolute reqText
       then ru
       else
         let baseText = TE.decodeUtf8 (renderBaseURL base)
             joined =
               case T.uncons reqText of
                 Nothing            -> baseText
                 Just ('/', _)      -> dropTrailingSlash baseText <> reqText
                 _                  -> dropTrailingSlash baseText <> "/" <> reqText
         in staticURI joined
  where
    isAbsolute t =
         "http://"  `T.isPrefixOf` t
      || "https://" `T.isPrefixOf` t
    dropTrailingSlash t
      | "/" `T.isSuffixOf` t = T.dropEnd 1 t
      | otherwise            = t

-- ---------------------------------------------------------------------------
-- Rate limiting
-- ---------------------------------------------------------------------------

-- | A token-bucket-ish rate limiter. The simplest possible
-- implementation: an 'MVar' counter that admits at most @rate@
-- requests per @window@ duration. New permits arrive lazily, one at
-- a time, as the window slides.
data RateLimit = RateLimit
  { rlState :: !(MVar (UTCTime, Int))
  , rlRate  :: !Int
  , rlWindow :: !NominalDiffTime
  }

newRateLimit
  :: Int               -- ^ requests per window
  -> NominalDiffTime   -- ^ window duration
  -> IO RateLimit
newRateLimit r w = do
  now <- getCurrentTime
  m <- newMVar (now, r)
  pure RateLimit { rlState = m, rlRate = r, rlWindow = w }

withRateLimit :: RateLimit -> Middleware IO
withRateLimit rl inner = Transport $ \req -> do
  awaitPermit rl
  sendRaw inner req

awaitPermit :: RateLimit -> IO ()
awaitPermit rl = do
  modifyMVar_ (rlState rl) $ \(windowStart, tokens) -> do
    now <- getCurrentTime
    let elapsed = diffUTCTime now windowStart
        (winStart', tokens')
          | elapsed >= rlWindow rl = (addUTCTime (rlWindow rl) windowStart, rlRate rl)
          | otherwise              = (windowStart, tokens)
    if tokens' > 0
      then pure (winStart', tokens' - 1)
      else do
        -- Sleep until the next window boundary and refill.
        let sleepFor = rlWindow rl - elapsed
            us       = max 0 (round (1_000_000 * realToFrac sleepFor :: Double))
        threadDelay us
        let nextStart = addUTCTime (rlWindow rl) windowStart
        pure (nextStart, rlRate rl - 1)

-- ---------------------------------------------------------------------------
-- Fault injection (intended for tests)
-- ---------------------------------------------------------------------------

withLatency :: Duration -> Middleware IO
withLatency (Duration us) inner = Transport $ \req -> do
  threadDelay us
  sendRaw inner req

-- | Allocate a state-bearing middleware that fails the first @n@
-- requests with @canned@ and then passes through. The spec
-- presented this as a pure @Int -> RawResponse -> Middleware IO@,
-- but the per-call counter has to live somewhere — exposing the
-- 'IO' wrap-up explicitly is more honest than hiding an
-- 'unsafePerformIO'.
newFailFirstN :: Int -> RawResponse -> IO (Middleware IO)
newFailFirstN n canned = do
  ref <- newIORef n
  pure $ \inner -> Transport $ \req -> do
    keepFailing <- atomicModifyIORef' ref $ \k ->
      if k > 0 then (k - 1, True) else (0, False)
    if keepFailing then pure canned else sendRaw inner req

-- | Backwards-compatible alias: builds the middleware inside 'IO' and
-- immediately applies it. Useful when wiring stacks together
-- imperatively.
failFirstN :: Int -> RawResponse -> Transport IO -> IO (Transport IO)
failFirstN n canned inner = do
  m <- newFailFirstN n canned
  pure (m inner)

-- | Allocate a randomised failure-injector. Returns @canned@ with
-- probability @p@; otherwise forwards.
newFailureRate :: Double -> RawResponse -> IO (Middleware IO)
newFailureRate p canned = do
  seedRef <- newIORef =<< initialSeed
  pure $ \inner -> Transport $ \req -> do
    roll <- atomicModifyIORef' seedRef $ \s ->
      let s' = stepLcg s
          d  = fromIntegral (s' `div` 2 :: Word64) / fromIntegral (maxBound :: Word64) :: Double
      in (s', d)
    if roll < p then pure canned else sendRaw inner req

-- | Convenience wrapper: same as 'newFailureRate' but applies
-- immediately. Mostly for symmetry with 'failFirstN'.
withFailureRate :: Double -> RawResponse -> Transport IO -> IO (Transport IO)
withFailureRate p canned inner = do
  m <- newFailureRate p canned
  pure (m inner)

-- | Add latency between response body chunks. The transport returns
-- normally; the popper sleeps before yielding each chunk.
withSlowBody :: Duration -> Middleware IO
withSlowBody (Duration us) inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let slow = do
        threadDelay us
        bodyPopper raw
  pure raw { bodyPopper = slow }

-- | Sleep for a uniform-random duration in the closed range before
-- each request. Useful for shaking out race conditions in code that
-- expects responses to arrive faster than they actually do.
--
-- The RNG is an internally-owned LCG seeded from the current time;
-- it's good enough for jitter but not for anything that needs real
-- randomness.
withJitter :: (Duration, Duration) -> IO (Middleware IO)
withJitter (Duration lo, Duration hi)
  | hi < lo   = pure (\inner -> inner)
  | hi == lo  = pure (withLatency (Duration lo))
  | otherwise = do
      seedRef <- newIORef =<< initialSeed
      pure $ \inner -> Transport $ \req -> do
        d <- atomicModifyIORef' seedRef $ \s ->
          let s' = stepLcg s
              fraction = fromIntegral (s' `mod` 1000000) / 1000000 :: Double
              jittered = lo + round (fraction * fromIntegral (hi - lo) :: Double)
          in (s', jittered)
        threadDelay d
        sendRaw inner req

-- | Throw an 'IOException' simulating a closed peer connection with
-- probability @p@; otherwise forward the request. The probability is
-- evaluated independently per request.
withConnectionReset :: Double -> IO (Middleware IO)
withConnectionReset p = do
  seedRef <- newIORef =<< initialSeed
  pure $ \inner -> Transport $ \req -> do
    roll <- atomicModifyIORef' seedRef $ \s ->
      let s' = stepLcg s
          d  = fromIntegral (s' `mod` 1000000) / 1000000 :: Double
      in (s', d)
    if roll < p
      then throwIO (userError "withConnectionReset: peer closed connection")
      else sendRaw inner req

-- | Truncate response bodies after @n@ bytes (simulates a mid-body
-- network drop). Stops yielding chunks once @n@ bytes have been
-- delivered.
withTruncation :: Int -> Middleware IO
withTruncation n inner = Transport $ \req -> do
  raw <- sendRaw inner req
  remainingRef <- newIORef n
  let truncated = do
        remaining <- readIORef remainingRef
        if remaining <= 0
          then pure BS.empty
          else do
            chunk <- bodyPopper raw
            if BS.null chunk
              then pure BS.empty
              else do
                let take_ = min remaining (BS.length chunk)
                writeIORef remainingRef (remaining - take_)
                pure (BS.take take_ chunk)
  pure raw { bodyPopper = truncated }

-- ---------------------------------------------------------------------------
-- Shared LCG (cheap, time-seeded; suitable for tests, not crypto)
-- ---------------------------------------------------------------------------

initialSeed :: IO Word64
initialSeed = do
  now <- getCurrentTime
  pure $! fromIntegral (round (realToFrac
                  (diffUTCTime now (read "1970-01-01 00:00:00 UTC"))
                  * 1000000 :: Double) :: Integer)

stepLcg :: Word64 -> Word64
stepLcg s = s * 6364136223846793005 + 1442695040888963407
