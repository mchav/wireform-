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
    -- * Telemetry Configuration
  , TelemetryConfig(..)
  , defaultTelemetryConfig
  ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Note: hs-opentelemetry-api integration would go here
-- For now, we define the interface and TODOs for implementation

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

-- | Inject OpenTelemetry context into Kafka message headers.
-- This enables distributed tracing across producers and consumers.
--
-- The context is injected as headers following the W3C Trace Context format:
-- * traceparent header
-- * tracestate header (optional)
--
-- TODO: Implement context injection
injectContextHeaders
  :: Map Text Text  -- ^ Existing headers
  -> IO (Map Text Text)  -- ^ Headers with injected context
injectContextHeaders headers = do
  -- TODO: Get current span context
  -- TODO: Serialize to W3C Trace Context format
  -- TODO: Add traceparent and tracestate headers
  return headers

-- | Extract OpenTelemetry context from Kafka message headers.
-- This enables distributed tracing across producers and consumers.
--
-- TODO: Implement context extraction
extractContextHeaders
  :: Map Text Text  -- ^ Message headers
  -> IO ()  -- ^ Extracted context (TODO: proper type)
extractContextHeaders headers = do
  -- TODO: Extract traceparent header
  -- TODO: Extract tracestate header
  -- TODO: Parse W3C Trace Context format
  -- TODO: Create span context
  return ()

