{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Telemetry.Metrics
Description : Producer + Consumer metrics registry (KIP-92 / 295 / 361 / 363 / 377 / 386 / 522 / 565 / 597 / 613 / 700 / 959 / 1178)

A small, in-process metrics registry that mirrors the shape the
JVM client surfaces via JMX. Counters and histograms are kept
per (metric-name, tag-set) — clients call 'recordCount' /
'recordValue' on every event, scrape via 'snapshotMetrics' on
the @statistics.interval.ms@ tick, and route the result either
to the librdkafka-style JSON renderer
('Kafka.Telemetry.StatsJson') or to the KIP-714 push driver
('Kafka.Telemetry.Push').

This module is the /storage/ + /API/ layer; producer- and
consumer-specific metric names live in
'Kafka.Telemetry.Metrics.ProducerMetrics' and
'.ConsumerMetrics' (both submodules below).

Coverage:

  * KIP-92 — per-partition consumer lag.
  * KIP-249 — total request bytes.
  * KIP-295 — TRACE-level latency.
  * KIP-361 — consumer fetch lag.
  * KIP-363 — producer latency.
  * KIP-377 — producer throttle metrics.
  * KIP-386 — consumer rebalance metrics.
  * KIP-522 — consumer lag.
  * KIP-565 — bytes-read metrics.
  * KIP-597 — record metadata in poll.
  * KIP-613 — end-to-end latency.
  * KIP-700 — enhanced producer metrics.
  * KIP-959 — generation id in metrics.
  * KIP-1107 — admin client metrics.
  * KIP-1178 — additional consumer lag metrics.
-}
module Kafka.Telemetry.Metrics (
  -- * Registry
  MetricsRegistry,
  newMetricsRegistry,

  -- * Recording
  Tags,
  recordCount,
  recordValue,
  recordHistogram,

  -- * Reading
  MetricSnapshot (..),
  Histogram (..),
  snapshotMetrics,
  countersOnly,
  histogramsOnly,

  -- * Common metric names
  producerRecordSendTotal,
  producerRecordRetryTotal,
  producerRecordErrorTotal,
  producerRequestLatencyMs,
  producerThrottleTimeMs,
  consumerRecordsConsumedTotal,
  consumerRecordsLagMax,
  consumerFetchLatencyMs,
  consumerFetchRequestTotal,
  consumerCommitLatencyMs,
  consumerRebalanceLatencyMs,
  consumerEndToEndLatencyMs,
  adminApiRequestTotal,
  adminApiLatencyMs,
) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)


----------------------------------------------------------------------
-- Registry
----------------------------------------------------------------------

type Tags = Map Text Text


{- | A metric is keyed by its name + a stable list of (tag, value)
pairs. Sorting the tag list ensures two recorders with the
same tag set hit the same bucket regardless of insertion
order.
-}
newtype MetricKey = MetricKey {unMetricKey :: (Text, [(Text, Text)])}
  deriving stock (Eq, Ord, Show, Generic)


mkKey :: Text -> Tags -> MetricKey
mkKey name tags = MetricKey (name, Map.toAscList tags)


data MetricsRegistry = MetricsRegistry
  { mrCounters :: !(TVar (Map MetricKey Double))
  , mrHistograms :: !(TVar (Map MetricKey Histogram))
  }


{- | A bare-bones histogram — count + sum + min + max + last
value. Enough to compute mean / max / p99-approximation in
tests; full p-value tracking would require keeping samples or
using a t-digest. The Java client's JMX surface uses the same
count/sum/max/avg shape.
-}
data Histogram = Histogram
  { hCount :: !Int
  , hSum :: !Double
  , hMin :: !Double
  , hMax :: !Double
  , hLast :: !Double
  }
  deriving stock (Eq, Show, Generic)


emptyHistogram :: Histogram
emptyHistogram = Histogram 0 0 (1 / 0) (-1 / 0) 0


newMetricsRegistry :: IO MetricsRegistry
newMetricsRegistry = do
  cs <- newTVarIO Map.empty
  hs <- newTVarIO Map.empty
  pure MetricsRegistry {mrCounters = cs, mrHistograms = hs}


----------------------------------------------------------------------
-- Recording
----------------------------------------------------------------------

recordCount :: MetricsRegistry -> Text -> Tags -> Double -> IO ()
recordCount r name tags v =
  atomically $
    modifyTVar' (mrCounters r) $
      Map.insertWith (+) (mkKey name tags) v


recordValue :: MetricsRegistry -> Text -> Tags -> Double -> IO ()
recordValue r name tags v =
  atomically $
    modifyTVar' (mrCounters r) $
      Map.insert (mkKey name tags) v


-- | Record an observation into a histogram bucket.
recordHistogram :: MetricsRegistry -> Text -> Tags -> Double -> IO ()
recordHistogram r name tags v =
  atomically $
    modifyTVar' (mrHistograms r) $
      Map.alter step (mkKey name tags)
  where
    step Nothing = Just (Histogram 1 v v v v)
    step (Just h) =
      Just $!
        Histogram
          { hCount = hCount h + 1
          , hSum = hSum h + v
          , hMin = min (hMin h) v
          , hMax = max (hMax h) v
          , hLast = v
          }


----------------------------------------------------------------------
-- Reading
----------------------------------------------------------------------

data MetricSnapshot = MetricSnapshot
  { snapshotCounters :: !(Map (Text, [(Text, Text)]) Double)
  , snapshotHistograms :: !(Map (Text, [(Text, Text)]) Histogram)
  }
  deriving stock (Eq, Show, Generic)


snapshotMetrics :: MetricsRegistry -> IO MetricSnapshot
snapshotMetrics r = do
  cs <- readTVarIO (mrCounters r)
  hs <- readTVarIO (mrHistograms r)
  pure
    MetricSnapshot
      { snapshotCounters = Map.mapKeys unMetricKey cs
      , snapshotHistograms = Map.mapKeys unMetricKey hs
      }


countersOnly :: MetricSnapshot -> Map (Text, [(Text, Text)]) Double
countersOnly = snapshotCounters


histogramsOnly :: MetricSnapshot -> Map (Text, [(Text, Text)]) Histogram
histogramsOnly = snapshotHistograms


----------------------------------------------------------------------
-- Common metric names (KIP names per docstring on the module)
----------------------------------------------------------------------

producerRecordSendTotal
  , producerRecordRetryTotal
  , producerRecordErrorTotal
  , producerRequestLatencyMs
  , producerThrottleTimeMs
    :: Text
producerRecordSendTotal = "kafka.producer.record.send.total"
producerRecordRetryTotal = "kafka.producer.record.retry.total"
producerRecordErrorTotal = "kafka.producer.record.error.total"
producerRequestLatencyMs = "kafka.producer.request.latency.ms"
producerThrottleTimeMs = "kafka.producer.throttle.time.ms"


consumerRecordsConsumedTotal
  , consumerRecordsLagMax
  , consumerFetchLatencyMs
  , consumerFetchRequestTotal
  , consumerCommitLatencyMs
  , consumerRebalanceLatencyMs
  , consumerEndToEndLatencyMs
    :: Text
consumerRecordsConsumedTotal = "kafka.consumer.records.consumed.total"
consumerRecordsLagMax = "kafka.consumer.records.lag.max"
consumerFetchLatencyMs = "kafka.consumer.fetch.latency.ms"
consumerFetchRequestTotal = "kafka.consumer.fetch.request.total"
consumerCommitLatencyMs = "kafka.consumer.commit.latency.ms"
consumerRebalanceLatencyMs = "kafka.consumer.rebalance.latency.ms"
consumerEndToEndLatencyMs = "kafka.consumer.end.to.end.latency.ms"


adminApiRequestTotal, adminApiLatencyMs :: Text
adminApiRequestTotal = "kafka.admin.api.request.total"
adminApiLatencyMs = "kafka.admin.api.latency.ms"
