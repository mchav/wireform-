{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Telemetry.OpenTelemetry
Description : OpenTelemetry instrumentation for Kafka client
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides OpenTelemetry instrumentation for the Kafka client,
following the OpenTelemetry semantic conventions for messaging systems.

= Instrumentation

The client automatically creates spans for:

* Producer operations (send, batch send)
* Consumer operations (poll, commit)
* Transactional operations (begin, commit, abort)

= Semantic Conventions

Following the OpenTelemetry semantic conventions for Kafka:

__Span Attributes__:

* @messaging.system@ = \"kafka\"
* @messaging.destination.name@ = topic name
* @messaging.operation@ = \"publish\" | \"receive\" | \"process\"
* @messaging.kafka.partition@ = partition number
* @messaging.kafka.offset@ = message offset
* @messaging.kafka.consumer.group@ = consumer group ID
* @messaging.message.id@ = correlation ID

__Metrics__:

* @messaging.kafka.producer.messages@ - Counter of produced messages
* @messaging.kafka.consumer.messages@ - Counter of consumed messages
* @messaging.kafka.request.duration@ - Histogram of request durations
* @messaging.kafka.batch.size@ - Histogram of batch sizes

= Context Propagation

The client supports automatic context propagation through Kafka headers,
allowing distributed traces across producers and consumers.

Reference: <https://opentelemetry.io/docs/specs/semconv/messaging/kafka/>

-}
module Kafka.Telemetry.OpenTelemetry
  ( -- * Span Creation
    createProducerSpan
  , createConsumerSpan
  , createTransactionSpan
    -- * Metrics
  , recordMessageSent
  , recordMessageReceived
  , recordRequestDuration
  , recordBatchSize
    -- * Context Propagation
  , injectContextHeaders
  , extractContextHeaders
  , injectSpanContextHeaders
  , extractSpanContextHeaders
    -- * Producer + consumer header bridges
  , injectIntoProducerHeaders
  , extractFromConsumerHeaders
  , tracingProducerInterceptor
    -- * Telemetry Configuration
  , TelemetryConfig(..)
  , defaultTelemetryConfig
  ) where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Int
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Kafka.Telemetry.TraceContext as TC

-- Note: hs-opentelemetry-api integration would go here for the
-- /span/ + /metric/ side; the W3C-Trace-Context propagation
-- below is fully implemented locally (see
-- "Kafka.Telemetry.TraceContext") and works without a tracing
-- SDK.

-- | Telemetry configuration.
data TelemetryConfig = TelemetryConfig
  { telemetryEnabled :: !Bool
    -- ^ Whether telemetry is enabled (default: True)
  , telemetryServiceName :: !Text
    -- ^ Service name for traces (default: "kafka-native")
  , telemetryIncludePayload :: !Bool
    -- ^ Whether to include message payload in spans (default: False for privacy)
  }

-- | Default telemetry configuration.
defaultTelemetryConfig :: TelemetryConfig
defaultTelemetryConfig = TelemetryConfig
  { telemetryEnabled = True
  , telemetryServiceName = "kafka-native"
  , telemetryIncludePayload = False
  }

-- | Create a span for a producer operation.
--
-- Attributes:
-- * messaging.system = "kafka"
-- * messaging.destination.name = topic
-- * messaging.operation = "publish"
-- * messaging.kafka.partition = partition
--
-- TODO: Implement producer span creation
-- Requires:
--   - Integration with hs-opentelemetry-api
--   - Span creation with proper attributes
--   - Parent context handling
createProducerSpan
  :: Text      -- ^ Topic name
  -> Int32     -- ^ Partition number
  -> IO ()     -- ^ Returns span handle (TODO: proper type)
createProducerSpan topic partition = do
  -- TODO: Create OpenTelemetry span
  -- Implementation steps:
  --   1. Get tracer from global tracer provider
  --   2. Start span with name "kafka.publish"
  --   3. Set attributes:
  --      - messaging.system = "kafka"
  --      - messaging.destination.name = topic
  --      - messaging.operation = "publish"
  --      - messaging.kafka.partition = partition
  --   4. Return span for ending later
  return ()

-- | Create a span for a consumer operation.
--
-- Attributes:
-- * messaging.system = "kafka"
-- * messaging.destination.name = topic
-- * messaging.operation = "receive" or "process"
-- * messaging.kafka.partition = partition
-- * messaging.kafka.offset = offset
-- * messaging.kafka.consumer.group = consumer group
--
-- TODO: Implement consumer span creation
createConsumerSpan
  :: Text      -- ^ Topic name
  -> Int32     -- ^ Partition number
  -> Int64     -- ^ Offset
  -> Text      -- ^ Consumer group
  -> IO ()     -- ^ Returns span handle (TODO: proper type)
createConsumerSpan topic partition offset consumerGroup = do
  -- TODO: Create OpenTelemetry span
  -- Similar to producer span but with consumer-specific attributes
  return ()

-- | Create a span for a transactional operation.
--
-- Attributes:
-- * messaging.system = "kafka"
-- * messaging.operation = "begin" | "commit" | "abort"
-- * messaging.kafka.transaction.id = transaction ID
--
-- TODO: Implement transaction span creation
createTransactionSpan
  :: Text      -- ^ Operation ("begin", "commit", "abort")
  -> Text      -- ^ Transaction ID
  -> IO ()     -- ^ Returns span handle (TODO: proper type)
createTransactionSpan operation txnId = do
  -- TODO: Create OpenTelemetry span
  return ()

-- | Record a metric for a message sent.
--
-- TODO: Implement metrics recording
recordMessageSent
  :: Text      -- ^ Topic name
  -> Int       -- ^ Message size in bytes
  -> IO ()
recordMessageSent topic size = do
  -- TODO: Increment counter metric
  -- messaging.kafka.producer.messages
  return ()

-- | Record a metric for a message received.
--
-- TODO: Implement metrics recording
recordMessageReceived
  :: Text      -- ^ Topic name
  -> Int       -- ^ Message size in bytes
  -> IO ()
recordMessageReceived topic size = do
  -- TODO: Increment counter metric
  -- messaging.kafka.consumer.messages
  return ()

-- | Record request duration metric.
--
-- TODO: Implement histogram recording
recordRequestDuration
  :: Text      -- ^ Request type (e.g., "produce", "fetch")
  -> Double    -- ^ Duration in seconds
  -> IO ()
recordRequestDuration requestType duration = do
  -- TODO: Record histogram value
  -- messaging.kafka.request.duration
  return ()

-- | Record batch size metric.
--
-- TODO: Implement histogram recording
recordBatchSize
  :: Int       -- ^ Batch size (number of messages)
  -> IO ()
recordBatchSize size = do
  -- TODO: Record histogram value
  -- messaging.kafka.batch.size
  return ()

-- | Inject the W3C Trace Context @traceparent@ + @tracestate@
-- headers (per
-- <https://www.w3.org/TR/trace-context/>) into a Kafka message
-- header map. The result includes (or replaces) the
-- @traceparent@ and @tracestate@ entries so consumers can
-- continue the trace.
--
-- This is the SDK-free path: pass in a 'TC.SpanContext' you've
-- assembled yourself (or that was extracted from an upstream
-- message). For the in-process / current-span case, plug your
-- tracing SDK in at the call site and translate its span context
-- to 'TC.SpanContext' before calling.
injectSpanContextHeaders
  :: TC.SpanContext
  -> Map Text Text
  -> Map Text Text
injectSpanContextHeaders = TC.injectIntoHeaders

-- | Extract a W3C Trace Context 'TC.SpanContext' from a Kafka
-- message header map, returning:
--
--   * 'Nothing' when the @traceparent@ header is absent (untraced
--     message);
--   * @'Just' ('Left' err)@ when the header is present but
--     malformed (caller decides whether to drop or warn);
--   * @'Just' ('Right' ctx)@ when the message carries a valid
--     trace context to continue.
--
-- The companion @tracestate@ header is parsed automatically.
extractSpanContextHeaders
  :: Map Text Text
  -> Maybe (Either TC.TraceContextError TC.SpanContext)
extractSpanContextHeaders = TC.extractFromHeaders

-- | Backwards-compatible @IO@-flavoured shim over
-- 'injectSpanContextHeaders'. Returns the input headers
-- unchanged when no current-span integration is wired up.
--
-- TODO: when an OTel SDK is integrated, this should pull the
-- /current/ span context off the local thread-state and inject
-- it. For now it's a no-op so existing call sites compile.
injectContextHeaders
  :: Map Text Text
  -> IO (Map Text Text)
injectContextHeaders = pure

-- | Backwards-compatible @IO@-flavoured shim over
-- 'extractSpanContextHeaders'. Currently a no-op for symmetry
-- with 'injectContextHeaders'; once an OTel SDK is wired up this
-- should /set/ the current-span context to the extracted value.
extractContextHeaders
  :: Map Text Text
  -> IO ()
extractContextHeaders _ = pure ()

----------------------------------------------------------------------
-- Producer + consumer header bridges
--
-- Producer 'recordHeaders' / consumer 'crHeaders' are
-- @[(Text, ByteString)]@ pairs (UTF-8 strings on the value side
-- per the JVM client's idiom, even when the spec is bytes-only).
-- The helpers below sit between that shape and the
-- @Map<Text, Text>@ shape that 'TC.injectIntoHeaders' /
-- 'TC.extractFromHeaders' work with, and are the recommended way
-- to thread W3C Trace Context across producer → consumer hops.
----------------------------------------------------------------------

-- | Inject a 'TC.SpanContext' into a producer's @recordHeaders@
-- list (UTF-8-encoded). Existing @traceparent@ / @tracestate@
-- headers are replaced; unrelated headers are passed through
-- unchanged. The output preserves the original header ordering
-- (with non-trace headers first), then appends the freshly-
-- minted trace headers.
injectIntoProducerHeaders
  :: TC.SpanContext
  -> [(Text, ByteString)]
  -> [(Text, ByteString)]
injectIntoProducerHeaders sc headers =
  let keepKey k     = k /= TC.traceparentHeader && k /= TC.tracestateHeader
      preserved     = filter (keepKey . fst) headers
      withInjected  = TC.injectIntoHeaders sc Map.empty
      injectedPairs = [ (k, TE.encodeUtf8 v)
                      | (k, v) <- Map.toList withInjected
                      ]
  in preserved <> injectedPairs

-- | Extract a 'TC.SpanContext' from a consumer record's
-- @[(Text, ByteString)]@ headers, or 'Nothing' if no
-- @traceparent@ is present.  Errors during parse are surfaced as
-- @Just (Left err)@ so the caller can decide whether to drop the
-- record, log, or treat it as an un-traced message.
extractFromConsumerHeaders
  :: [(Text, ByteString)]
  -> Maybe (Either TC.TraceContextError TC.SpanContext)
extractFromConsumerHeaders headers =
  let asMap = Map.fromList
        [ (k, t)
        | (k, v) <- headers
        , Right t <- [TE.decodeUtf8' v]
        , k == TC.traceparentHeader || k == TC.tracestateHeader
        ]
  in TC.extractFromHeaders asMap

-- | Build a pre-send header interceptor that injects whatever
-- 'TC.SpanContext' the supplied callback returns. Callers wire
-- it into 'ProducerConfig.producerInterceptor' by threading the
-- result through their record:
--
-- @
-- let injectHdrs = tracingProducerInterceptor MyTracer.currentSpanContext
--     onSend rec = do
--       hs' <- injectHdrs (recordHeaders rec)
--       pure rec { recordHeaders = hs' }
--     cfg = defaultProducerConfig { producerInterceptor = onSend }
-- @
--
-- The callback runs once per record from the producer thread;
-- return 'Nothing' from it to skip injection (e.g. when no
-- current span exists).
--
-- This module deliberately does /not/ import
-- @Kafka.Client.Producer@ — that's a much heavier dependency
-- and would create a coupling between telemetry and the
-- producer surface. The trade-off is the small wrapper above
-- at the call site.
tracingProducerInterceptor
  :: IO (Maybe TC.SpanContext)
  -> [(Text, ByteString)]
  -> IO [(Text, ByteString)]
tracingProducerInterceptor pull headers = do
  ctxM <- pull
  case ctxM of
    Nothing -> pure headers
    Just sc -> pure (injectIntoProducerHeaders sc headers)

