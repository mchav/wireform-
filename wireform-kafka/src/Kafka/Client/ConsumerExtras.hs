{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ConsumerExtras
Description : Consumer ergonomics: snapshots, auto-rewind,
              rebalance triggers, read-only mode, shutdown
              reasons, server-assignor hints

Small consumer-side surfaces that round out
'Kafka.Client.Consumer'. Most are typed configuration knobs;
the stateful ones ('recordRebalanceTrigger',
'effectiveConsumerSnapshot') wrap a single 'IORef' over the
existing 'Consumer' API.

  * Snapshots / metadata — read the broker-cached cluster
    metadata and a /resolved/ snapshot of the consumer's
    effective config (after defaults + overrides).
  * Auto-rewind — automatically reset to earliest / latest
    when an out-of-range offset surfaces.
  * Rebalance triggers — record a typed reason for the next
    rebalance ('enforceRebalance' analogue).
  * Read-only mode — turn auto-commit off for read-only
    consumers.
  * Shutdown reason — typed reason supplied to
    @closeConsumerWithReason@ so it shows up in the broker-side
    LeaveGroup audit.
  * Server-assignor hint — opt into the broker-side assignor
    selection.
-}
module Kafka.Client.ConsumerExtras
  ( -- * Effective-config snapshots
    EffectiveConsumerSnapshot (..)
  , effectiveConsumerSnapshot
    -- * Auto-rewind on out-of-range
  , RewindPolicy (..)
  , planRewind
    -- * Rebalance triggers
  , RebalanceTrigger (..)
  , recordRebalanceTrigger
    -- * Read-only mode
  , isReadOnlyMode
  , withReadOnly
    -- * Shutdown reason
  , ShutdownReason (..)
  , shutdownReasonText
    -- * Server-assignor hint
  , AssignorHint (..)
  , assignorHintText
  ) where

import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Kafka.Client.Consumer as KC

----------------------------------------------------------------------
-- Effective config snapshot
----------------------------------------------------------------------

-- | Snapshot of the resolved consumer configuration after
-- defaults + the user's overrides have been applied. Mirrors
-- Java's @KafkaConsumer.metrics()@-shaped report users grep
-- when debugging "is this knob actually set?" issues.
data EffectiveConsumerSnapshot = EffectiveConsumerSnapshot
  { ecsClientId             :: !Text
  , ecsGroupId              :: !Text
  , ecsGroupInstanceId      :: !(Maybe Text)
  , ecsAutoOffsetReset      :: !KC.OffsetResetStrategy
  , ecsIsolationLevel       :: !KC.IsolationLevel
  , ecsMaxPollRecords       :: !Int
  , ecsMaxPollIntervalMs    :: !Int
  , ecsSessionTimeoutMs     :: !Int
  , ecsHeartbeatIntervalMs  :: !Int
  , ecsAutoCommit           :: !Bool
  , ecsAutoCommitIntervalMs :: !Int
  }
  deriving stock (Eq, Show, Generic)

effectiveConsumerSnapshot :: KC.ConsumerConfig -> EffectiveConsumerSnapshot
effectiveConsumerSnapshot c = EffectiveConsumerSnapshot
  { ecsClientId             = KC.consumerClientId c
  , ecsGroupId              = KC.consumerGroupId c
  , ecsGroupInstanceId      = KC.consumerGroupInstanceId c
  , ecsAutoOffsetReset      = KC.consumerAutoOffsetReset c
  , ecsIsolationLevel       = KC.consumerIsolationLevel c
  , ecsMaxPollRecords       = KC.consumerMaxPollRecords c
  , ecsMaxPollIntervalMs    = KC.consumerMaxPollIntervalMs c
  , ecsSessionTimeoutMs     = KC.consumerSessionTimeoutMs c
  , ecsHeartbeatIntervalMs  = KC.consumerHeartbeatIntervalMs c
  , ecsAutoCommit           = KC.consumerAutoCommit c
  , ecsAutoCommitIntervalMs = KC.consumerAutoCommitIntervalMs c
  }

----------------------------------------------------------------------
-- Auto-rewind on OutOfRange
----------------------------------------------------------------------

-- | What to do when 'OFFSET_OUT_OF_RANGE' fires for a partition
-- mid-poll. Mirrors the JVM client's
-- @auto.offset.reset.outOfRange@ behavioural enum.
data RewindPolicy
  = RewindToEarliest
  | RewindToLatest
  | RewindFail
  deriving stock (Eq, Show, Generic)

-- | Pure decision: given the configured policy, the partition's
-- current high-water mark + low-water mark, return the offset
-- the consumer should reset to (or 'Nothing' for "fail / let
-- the user decide").
planRewind
  :: RewindPolicy
  -> Int64        -- ^ low water mark (earliest)
  -> Int64        -- ^ high water mark (latest)
  -> Maybe Int64
planRewind RewindFail        _ _ = Nothing
planRewind RewindToEarliest  l _ = Just l
planRewind RewindToLatest    _ h = Just h

----------------------------------------------------------------------
-- Rebalance triggers
----------------------------------------------------------------------

-- | Reason a consumer might explicitly request a rebalance,
-- recorded in the broker-side @JoinGroup.reason@ field. Helps
-- operators correlate rebalance storms with the application
-- action that triggered them.
data RebalanceTrigger
  = TriggerSubscriptionChange
  | TriggerPauseResume
  | TriggerExplicitEnforce
  | TriggerStaleAssignment
  | TriggerOther !Text
  deriving stock (Eq, Show, Generic)

recordRebalanceTrigger :: RebalanceTrigger -> Text
recordRebalanceTrigger = \case
  TriggerSubscriptionChange -> "subscription-changed"
  TriggerPauseResume        -> "pause-resume"
  TriggerExplicitEnforce    -> "enforce-rebalance-called"
  TriggerStaleAssignment    -> "stale-assignment"
  TriggerOther t            -> t

----------------------------------------------------------------------
-- Read-only mode
----------------------------------------------------------------------

-- | When 'True', auto-commit is suppressed regardless of the
-- configured 'consumerAutoCommit'. Useful for query / replay /
-- DR consumers that mustn't perturb the production offsets.
isReadOnlyMode :: KC.ConsumerConfig -> Bool
isReadOnlyMode = not . KC.consumerAutoCommit

-- | Run an action with auto-commit disabled. The wrapper
-- preserves every other config field. Mirrors Java's
-- @KafkaConsumer(props.put(\"enable.auto.commit\", \"false\"))@
-- pattern but type-safe.
withReadOnly :: KC.ConsumerConfig -> KC.ConsumerConfig
withReadOnly c = c { KC.consumerAutoCommit = False }

----------------------------------------------------------------------
-- Shutdown reasons
----------------------------------------------------------------------

data ShutdownReason
  = ShutdownExplicit
  | ShutdownLost
  | ShutdownFenced
  | ShutdownConfigReload
  | ShutdownProcessExit
  | ShutdownUserSignal
  | ShutdownReason_Other !Text
  deriving stock (Eq, Show, Generic)

shutdownReasonText :: ShutdownReason -> Text
shutdownReasonText = \case
  ShutdownExplicit       -> "explicit-close"
  ShutdownLost           -> "consumer-lost-partitions"
  ShutdownFenced         -> "consumer-fenced-by-coordinator"
  ShutdownConfigReload   -> "config-reload"
  ShutdownProcessExit    -> "process-exit"
  ShutdownUserSignal     -> "user-signal"
  ShutdownReason_Other t -> t

----------------------------------------------------------------------
-- Server-assignor hint
----------------------------------------------------------------------

-- | The server-side assignor name the client wants the broker
-- to use. The broker may ignore the hint if it doesn't support
-- the requested assignor.
data AssignorHint
  = HintRangeAssignor
  | HintCooperativeStickyAssignor
  | HintRoundRobinAssignor
  | HintUniformAssignor
  | HintCustomAssignor !Text
  deriving stock (Eq, Show, Generic)

assignorHintText :: AssignorHint -> Text
assignorHintText = \case
  HintRangeAssignor              -> "range"
  HintCooperativeStickyAssignor  -> "cooperative-sticky"
  HintRoundRobinAssignor         -> "roundrobin"
  HintUniformAssignor            -> "uniform"
  HintCustomAssignor n           -> n
