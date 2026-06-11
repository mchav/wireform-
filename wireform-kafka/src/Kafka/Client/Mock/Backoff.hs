{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Client.Mock.Backoff
Description : Exponential backoff helper for mock-cluster tests

Mirrors what the JVM client (and librdkafka) compute internally
after a sequence of retriable broker errors: the next backoff
doubles up to a configured ceiling, with optional jitter that
keeps multiple clients from synchronising their retry storms.

librdkafka mock-test ports:
  * 0127_fetch_queue_backoff
  * 0143_exponential_backoff_mock
-}
module Kafka.Client.Mock.Backoff (
  BackoffPolicy (..),
  defaultBackoffPolicy,
  nextBackoffMs,
  backoffSeries,
) where

import Data.Int (Int64)
import GHC.Generics (Generic)


-- | Knobs for an exponential backoff curve.
data BackoffPolicy = BackoffPolicy
  { bpInitialMs :: !Int64
  , bpMaxMs :: !Int64
  , bpMultiplier :: !Double
  , bpJitter :: !Double
  {- ^ 0.0 = no jitter; 0.2 = ±20% (deterministic via the
  'attempt' counter so tests stay reproducible).
  -}
  }
  deriving stock (Eq, Show, Generic)


{- | Defaults that match librdkafka's
@retry.backoff.ms = 100, retry.backoff.max.ms = 1000, multiplier 2x@.
-}
defaultBackoffPolicy :: BackoffPolicy
defaultBackoffPolicy =
  BackoffPolicy
    { bpInitialMs = 100
    , bpMaxMs = 1000
    , bpMultiplier = 2.0
    , bpJitter = 0.2
    }


{- | Compute the next backoff for the given attempt number
(0-indexed: attempt 0 returns 'bpInitialMs').

@
backoff_n = min(bpMaxMs, bpInitialMs * bpMultiplier^n)
@

with deterministic jitter applied as a function of @n@ so
tests can reproduce the curve exactly.
-}
nextBackoffMs :: BackoffPolicy -> Int -> Int64
nextBackoffMs BackoffPolicy {..} attempt =
  let !raw = fromIntegral bpInitialMs * (bpMultiplier ^ attempt)
      !capped = min (fromIntegral bpMaxMs) raw :: Double
      -- Deterministic +/- jitter. We avoid 'random' so two
      -- runs of the test produce the same numbers.
      !jit = sin (fromIntegral attempt) * bpJitter
  in max 0 (round (capped * (1 + jit)))


{- | The first @n@ backoffs as a list. Convenient for assertions
that verify the curve shape (monotonic, hits the ceiling, etc.).
-}
backoffSeries :: BackoffPolicy -> Int -> [Int64]
backoffSeries bp n = map (nextBackoffMs bp) [0 .. n - 1]
