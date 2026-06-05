{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Telemetry.Push
Description : KIP-714 client telemetry push to broker

KIP-714 lets a Kafka client push its own metrics to the broker
on a configurable cadence. The broker then forwards them to its
configured metric receiver (Prometheus, OTLP, etc.). This is the
preferred path on Kafka 3.7+ because the broker decides what to
collect via @GetTelemetrySubscriptions@: clients that don't
support every metric the broker asks for simply omit them and
the broker retries on the next poll.

Wire-level coverage:

  * @GetTelemetrySubscriptionsRequest@ / @Response@ — broker
    tells the client what metrics it wants, what serialisation
    format (currently OTLP-protobuf), and the push cadence.
  * @PushTelemetryRequest@ / @Response@ — client uploads the
    encoded metrics blob.

Both wire surfaces are already generated under
"Kafka.Protocol.Generated.GetTelemetrySubscriptionsRequest" /
@PushTelemetryRequest@. This module is the high-level driver:

  * 'TelemetrySubscription' — what the broker last told us.
  * 'TelemetryStateMachine' — pure state representing
    "uninitialised" / "subscribed" / "pushing" with cadence
    bookkeeping.
  * 'planTelemetryStep' — pure decision: should the client
    refresh the subscription, push now, or sleep?

The IO driver that actually issues the requests sits in a
separate runtime module (a 'Kafka.Telemetry.PushRuntime' that
the producer / consumer threads can fork). Tests against this
module exercise the cadence math without a broker.
-}
module Kafka.Telemetry.Push
  ( -- * Subscription
    TelemetrySubscription (..)
  , noSubscription
    -- * State machine
  , TelemetryStateMachine (..)
  , initialState
  , planTelemetryStep
  , applyTelemetryRefresh
  , applyTelemetryPush
  , markTelemetryTerminating
  , TelemetryAction (..)
    -- * Encoding shape
  , TelemetryFormat (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The wire format the broker asked us to encode metrics in.
-- KIP-714 currently mandates OTLP / protobuf; the field is
-- carried in case the spec expands.
data TelemetryFormat
  = OTLPProtobuf
  | OTLPJSON
  deriving stock (Eq, Show, Generic)

-- | What the broker last told us via
-- @GetTelemetrySubscriptionsResponse@.
data TelemetrySubscription = TelemetrySubscription
  { tsClientInstanceId   :: !Text
    -- ^ Broker-assigned UUID identifying this client instance.
    --   Reuse across requests so the broker can dedup.
  , tsSubscriptionId     :: !Int32
  , tsRequestedMetrics   :: !(Set Text)
    -- ^ Metric-name allowlist (matched by prefix).
  , tsAcceptedFormats    :: ![TelemetryFormat]
  , tsPushIntervalMs     :: !Int32
    -- ^ How often the broker wants 'PushTelemetry' to fire.
  , tsTelemetryMaxBytes  :: !Int32
    -- ^ Cap on a single PushTelemetry payload.
  , tsDeltaTemporality   :: !Bool
    -- ^ When 'True', the client should send delta values;
    --   when 'False', cumulative.
  }
  deriving stock (Eq, Show, Generic)

noSubscription :: Text -> TelemetrySubscription
noSubscription instanceId = TelemetrySubscription
  { tsClientInstanceId  = instanceId
  , tsSubscriptionId    = 0
  , tsRequestedMetrics  = Set.empty
  , tsAcceptedFormats   = [OTLPProtobuf]
  , tsPushIntervalMs    = 0
  , tsTelemetryMaxBytes = 0
  , tsDeltaTemporality  = True
  }

-- | The driver's view of where it is in the lifecycle.
data TelemetryStateMachine = TelemetryStateMachine
  { tsmSubscription   :: !(Maybe TelemetrySubscription)
  , tsmLastPushAtMs   :: !Int64
  , tsmLastSubAtMs    :: !Int64
  , tsmTerminating    :: !Bool
  }
  deriving stock (Eq, Show, Generic)

initialState :: TelemetryStateMachine
initialState = TelemetryStateMachine
  { tsmSubscription = Nothing
  , tsmLastPushAtMs = 0
  , tsmLastSubAtMs  = 0
  , tsmTerminating  = False
  }

-- | The next step the driver should take.
data TelemetryAction
  = TARefreshSubscription
  | TAPushNow !ByteString
    -- ^ The opaque blob would be encoded by the metrics layer.
    --   The driver asks the metrics encoder for it; if the
    --   encoder returns empty bytes, the driver skips the push
    --   and waits for the next cycle.
  | TASleepUntilMs !Int64
  | TADone
    -- ^ Terminating + final push delivered.
  deriving stock (Eq, Show, Generic)

-- | Decide what to do next given the wall-clock + the state.
-- Pure; the 'ByteString' the encoder would produce is omitted —
-- callers are expected to compose this with their metric layer.
--
--   * No subscription yet -> refresh.
--   * Subscription expired (cadence elapsed since last
--     subscribe) -> refresh.
--   * Subscription valid and push interval elapsed -> push now.
--   * Otherwise -> sleep until the next event.
planTelemetryStep
  :: Int64                -- ^ now (ms)
  -> TelemetryStateMachine
  -> TelemetryAction
planTelemetryStep now st
  | tsmTerminating st = TADone
  | otherwise = case tsmSubscription st of
      Nothing -> TARefreshSubscription
      Just sub
        | shouldRefresh sub
            -> TARefreshSubscription
        | shouldPush sub
            -> TAPushNow mempty
        | otherwise
            -> TASleepUntilMs (nextWakeup sub)
  where
    !lastSub  = tsmLastSubAtMs st
    !lastPush = tsmLastPushAtMs st
    -- Refresh the subscription on the same cadence as the push:
    -- the broker can change parameters at any time and a long-
    -- lived client must re-pull at least once per push window.
    refreshIntervalFor sub =
      let !w = max 1 (tsPushIntervalMs sub)
      in 5 * fromIntegral w  -- match Java client's heuristic.
    shouldRefresh sub =
      now - lastSub >= refreshIntervalFor sub
    shouldPush sub =
      tsPushIntervalMs sub > 0
        && now - lastPush >= fromIntegral (tsPushIntervalMs sub)
    nextWakeup sub =
      let !nextRefresh = lastSub + refreshIntervalFor sub
          !nextPush    = lastPush + fromIntegral (tsPushIntervalMs sub)
      in min nextRefresh nextPush

applyTelemetryRefresh
  :: Int64
  -> TelemetrySubscription
  -> TelemetryStateMachine
  -> TelemetryStateMachine
applyTelemetryRefresh now sub st = st
  { tsmSubscription = Just sub
  , tsmLastSubAtMs = now
  }

applyTelemetryPush :: Int64 -> TelemetryStateMachine -> TelemetryStateMachine
applyTelemetryPush now st = st { tsmLastPushAtMs = now }

markTelemetryTerminating :: TelemetryStateMachine -> TelemetryStateMachine
markTelemetryTerminating st = st { tsmTerminating = True }
