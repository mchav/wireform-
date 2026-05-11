{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Telemetry.OpenTelemetry
Description : W3C trace context propagation across Kafka producer / consumer hops.
Copyright   : (c) 2025
License     : BSD-3-Clause

W3C Trace Context (<https://www.w3.org/TR/trace-context/>) injection
and extraction over Kafka record headers. This is the
__SDK-independent half__ of OpenTelemetry: pass it a 'TC.SpanContext'
you assembled yourself (or pulled out of your tracing SDK) and it
will round-trip the trace through Kafka headers so consumers can
continue the trace on the other side.

This module deliberately does /not/ depend on any OpenTelemetry SDK.
Span / metric creation through @hs-opentelemetry-api@ (or equivalent)
is the application's responsibility — pull a 'TC.SpanContext' off
your tracer, hand it to 'injectIntoProducerHeaders' /
'tracingProducerInterceptor', and you are done.

= Producer side

@
let injectHdrs = 'tracingProducerInterceptor' Tracer.currentSpanContext
    onSend rec = do
      hs' <- injectHdrs (recordHeaders rec)
      pure rec { recordHeaders = hs' }
    cfg = 'defaultProducerConfig' { 'producerInterceptor' = onSend }
@

= Consumer side

@
case 'extractFromConsumerHeaders' (crHeaders rec) of
  Nothing             -> -- untraced message, start a new root span
  Just (Left  err)    -> -- malformed traceparent header, log + skip
  Just (Right parent) -> -- continue the trace under @parent@
@

= Semantic conventions

The following attributes are the right ones to set on whatever span
you create around a Kafka operation, per the
[messaging semantic conventions](https://opentelemetry.io/docs/specs/semconv/messaging/kafka/):

  * @messaging.system@ = \"kafka\"
  * @messaging.destination.name@ = topic name
  * @messaging.operation@ = \"publish\" | \"receive\" | \"process\"
  * @messaging.kafka.partition@ = partition number
  * @messaging.kafka.offset@ = message offset
  * @messaging.kafka.consumer.group@ = consumer group id
  * @messaging.message.id@ = correlation id

Suggested metrics: @messaging.kafka.producer.messages@,
@messaging.kafka.consumer.messages@, @messaging.kafka.request.duration@,
@messaging.kafka.batch.size@.

We deliberately do not provide span / metric helpers here — every
SDK shapes those differently and a stub would be misleading.
-}
module Kafka.Telemetry.OpenTelemetry
  ( -- * Context Propagation
    injectContextHeaders
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
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Kafka.Telemetry.TraceContext as TC

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

