{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.AsyncIO.Config
-- Description : Configuration types for the asynchronous I\/O operator family
--
-- This module is part of the /Riffle/ extension tier
-- (see @wireform-kafka/streams/RIFFLE_SPEC.md@): the additive
-- post-parity additions that sit on top of the Apache Kafka Streams
-- DSL port. It carries no runtime state; it just spells out the knobs
-- the 'Kafka.Streams.AsyncIO' processor consults.
--
-- The async-I\/O operator decouples per-record latency from
-- throughput by running the user-supplied @IO@ on a bounded worker
-- pool — instead of the synchronous-on-stream-thread contract that
-- 'Kafka.Streams.KStream.mapValuesM' and its siblings hold to.
-- Backpressure is preserved (bounded in-flight queue blocks the
-- stream thread when saturated); exactly-once is preserved (the
-- runtime drains every in-flight request through the commit cycle
-- before offsets are committed); per-key ordering is preserved when
-- 'OrderedOutput' is selected.
--
-- The four objections that justify the existing refusal to ship
-- @foreachAsync@ in 'Kafka.Streams.Topology.Free' — no backpressure,
-- silent errors, EOS incompatibility, lost ordering — are addressed
-- explicitly by the four config fields below:
--
-- * 'aioBufferCapacity' caps in-flight work to bound memory and
--   provides the natural-backpressure signal up the stream-thread
--   chain.
-- * 'aioOnFailure' is the explicit failure policy: a record's
--   @IO@ throwing is never silently dropped.
-- * The transactional drain hook (see @runCommitCycle@) guarantees
--   the in-flight queue is empty before offsets are committed.
-- * 'aioOutputMode' picks per-key ordering ('OrderedOutput') or
--   pure throughput ('UnorderedOutput').
module Kafka.Streams.AsyncIO.Config
  ( -- * Top-level config
    AsyncIOConfig (..)
  , defaultAsyncIOConfig
    -- * Output ordering
  , AsyncOutputMode (..)
    -- * Failure policy
  , AsyncFailurePolicy (..)
    -- * Retry strategy
  , AsyncRetryStrategy (..)
  , noRetry
  , retryFixed
    -- * Drain triggers
  , AsyncDrainTrigger (..)
  ) where

import Control.Exception (SomeException)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Time (Duration, millis)

-- | Configuration for one async-I\/O processor instance.
--
-- The defaults ('defaultAsyncIOConfig') trade conservative throughput
-- for tight correctness:
--
-- * 32 in-flight requests
-- * 4 worker threads
-- * ordered output
-- * 30-second per-request timeout
-- * no retries
-- * fail the task on any user @IO@ exception
-- * drain via the per-record entry hook + a 25 ms wall-clock
--   punctuator
--
-- Almost every production deployment will tune at least
-- 'aioBufferCapacity' and 'aioWorkers'; everything else is a
-- reasonable starting point.
data AsyncIOConfig = AsyncIOConfig
  { aioBufferCapacity :: !Int
    -- ^ Maximum number of in-flight requests. When the queue is
    -- full, the stream thread blocks on enqueue — that's the
    -- backpressure signal.
  , aioWorkers        :: !Int
    -- ^ Number of worker threads draining the in-flight queue.
    -- Each worker runs one user @IO@ action at a time. Must be
    -- @>= 1@.
  , aioOutputMode     :: !AsyncOutputMode
    -- ^ Whether downstream sees results in input order
    -- ('OrderedOutput') or completion order ('UnorderedOutput').
  , aioTimeout        :: !Duration
    -- ^ Per-request timeout. After this elapses without a result,
    -- the runtime treats the request as failed; the configured
    -- 'aioRetry' strategy applies, then 'aioOnFailure'.
  , aioRetry          :: !AsyncRetryStrategy
    -- ^ How to recover from a failed (thrown / timed-out)
    -- request before falling back to 'aioOnFailure'.
  , aioOnFailure      :: !AsyncFailurePolicy
    -- ^ Terminal action once retries are exhausted.
  , aioDrainTrigger   :: !AsyncDrainTrigger
    -- ^ When the stream thread should sweep results from the
    -- reorder buffer onto downstream.
  , aioName           :: !Text
    -- ^ Logical operator name used in processor / metric labels.
    -- Defaults to @"KSTREAM-ASYNC"@ via 'defaultAsyncIOConfig';
    -- override for finer-grained observability.
  , aioOnDeposit      :: !(IO ())
    -- ^ Worker-thread hook fired after each deposit into the
    -- reorder buffer (whether the slot is a success batch or a
    -- skip-on-failure). Defaults to @pure ()@. Production use:
    -- wire to a metrics counter so a gauge tracks
    -- @completed-requests@. Test use: bump a 'TVar' so the
    -- harness can observe deposits without racing the user IO.
  }

-- | Conservative defaults; tune 'aioBufferCapacity' and
-- 'aioWorkers' for your I\/O profile.
defaultAsyncIOConfig :: AsyncIOConfig
defaultAsyncIOConfig = AsyncIOConfig
  { aioBufferCapacity = 32
  , aioWorkers        = 4
  , aioOutputMode     = OrderedOutput
  , aioTimeout        = millis 30000
  , aioRetry          = NoRetry
  , aioOnFailure      = FailTask
  , aioDrainTrigger   = DrainOnEntryAndPunctuator (millis 25)
  , aioName           = "KSTREAM-ASYNC"
  , aioOnDeposit      = pure ()
  }

-- | Downstream emission order.
data AsyncOutputMode
  = OrderedOutput
    -- ^ Drain in input-record order. Slow requests block faster
    -- ones in the reorder buffer; the buffer never reorders past
    -- input. Same per-key ordering guarantee as the synchronous
    -- 'mapValuesM' contract.
  | UnorderedOutput
    -- ^ Drain in completion order. Higher throughput when
    -- requests have heterogeneous latencies; downstream may see
    -- records out-of-order relative to input. Use only when
    -- downstream operators do not rely on per-key ordering
    -- (counters, set-shaped state, idempotent writes).
  deriving stock (Eq, Show, Generic)

-- | What to do when a request fails (thrown synchronous exception
-- or 'aioTimeout' exceeded after retries are exhausted).
data AsyncFailurePolicy
  = FailTask
    -- ^ Re-throw on the stream thread at the next drain. The
    -- engine's uncaught-exception handler catches it; the task
    -- shuts down. Default: matches the JVM \"strict\" mode.
  | DropAndContinue
    -- ^ Silently drop the record. The reorder buffer records a
    -- skip slot ('OrderedOutput') or simply doesn't emit
    -- ('UnorderedOutput'). Use only when the downstream
    -- semantics tolerate lossy enrichment.
  | LogAndContinue
    -- ^ Same as 'DropAndContinue' but logs the exception via
    -- the engine's @logAndContinue@-style handler before
    -- skipping. Recommended for best-effort enrichment.
  | CustomFailure !(SomeException -> IO ())
    -- ^ User callback. Runs on a worker thread (not the stream
    -- thread); must be thread-safe. The processor treats the
    -- record as dropped after the callback returns. Use this to
    -- wire a custom dead-letter sink, metric, or out-of-band
    -- alert.

-- | Retry strategy for failed requests. The runtime retries up to
-- the configured attempt count, then applies 'aioOnFailure'.
data AsyncRetryStrategy
  = NoRetry
    -- ^ First failure goes straight to 'aioOnFailure'.
  | RetryFixed !Int !Duration
    -- ^ Up to @n@ retry attempts, each separated by a fixed
    -- delay.
  | RetryBackoff !Int !Duration !Int
    -- ^ @RetryBackoff attempts initial multiplier@. Up to
    -- @attempts@ retries; the delay before retry @k@ (zero-
    -- indexed) is @initial * multiplier ^ k@.
    --
    -- The multiplier is an 'Int' on purpose — integer math
    -- only, no @Double@ in the hot path. Typical values are
    -- @2@ (exponential backoff) or @10@ (decade backoff). For
    -- a non-integer ratio, model it as multiple short fixed
    -- retries instead.
    --
    -- Overflow on extreme inputs is clamped at @maxBound ::
    -- Int64@ ms (~292 million years); a single retry will
    -- never wait longer than that.
  deriving stock (Eq, Show, Generic)

-- | Convenience: don't retry.
noRetry :: AsyncRetryStrategy
noRetry = NoRetry

-- | Convenience: retry @n@ times with the given fixed delay.
retryFixed :: Int -> Duration -> AsyncRetryStrategy
retryFixed = RetryFixed

-- | When the stream thread sweeps completed requests out of the
-- reorder buffer and forwards them downstream.
--
-- The sweep MUST run on the stream thread because
-- 'Kafka.Streams.Processor.ctxForward' is not thread-safe (it walks
-- 'IORef'-backed forwarders that downstream processors read without
-- synchronisation). Worker threads only deposit results into the
-- buffer; the stream thread is the one that empties it.
data AsyncDrainTrigger
  = DrainOnEntry
    -- ^ Sweep at the head of every 'procProcess' call. Stable
    -- behaviour under steady input; a long pause in upstream
    -- traffic stalls completed records in the buffer.
  | DrainOnEntryAndPunctuator !Duration
    -- ^ 'DrainOnEntry' plus a wall-clock punctuator at the given
    -- interval. Default for 'defaultAsyncIOConfig' so a stalled
    -- input topic doesn't strand completed enrichment results.
  deriving stock (Eq, Show, Generic)
