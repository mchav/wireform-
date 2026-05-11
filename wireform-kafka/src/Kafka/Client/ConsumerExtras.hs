{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ConsumerExtras
Description : KIP-238 / 302 / 389 / 391 / 396 / 421 / 424 / 470 / 477 /
              485 / 568 / 587 / 597 / 843 / 941 / 974 / 1114 — consumer
              ergonomics

A grab-bag of small consumer-side surfaces that the JVM client
exposes but the original wireform-kafka 'Consumer' didn't.

  * KIP-238: 'consumerMetadata' — read the broker-cached
    cluster metadata via the consumer.
  * KIP-302: 'recordMetadataFromBatch' — surface the per-batch
    leader epoch + base offset alongside each record.
  * KIP-389: 'leaveOnUnknownMember' — explicit
    @leave-group-on-unknown-member@ knob.
  * KIP-391: 'commitWithSync' — wait for the commit to be
    visible to a re-fetch.
  * KIP-396: 'commitInBackground' — fire-and-forget commit on
    the background thread.
  * KIP-421: 'rewindOnOutOfRange' — automatically reset to
    earliest / latest when an OutOfRange surfaces.
  * KIP-424: 'effectiveConsumerConfig' — snapshot of the
    /resolved/ consumer config (after defaults + overrides).
  * KIP-470: 'TopicDescriptionForOffset' — surface
    TopicDescription as part of @committed@.
  * KIP-485: 'committedOffsetsInRebalance' — extra hook on
    'RebalanceListener' for committed-offset visibility.
  * KIP-568: 'enforceRebalance' — explicitly trigger a rebalance.
  * KIP-587: 'suppressAutoCommitInReadOnly' — turn auto-commit
    off for read-only consumers.
  * KIP-597 / KIP-843: surfaced via
    "Kafka.Client.RecordMetadata" (this module re-exports for
    convenience).
  * KIP-941: 'allowMembersWithExistingAssignments' — server-side
    assignor hint.
  * KIP-974: 'idleExpiryFix' — per-connection idle expiry
    helper (wraps the existing connections.max.idle.ms knob).
  * KIP-1114: 'shutdownReason' — typed reason supplied to
    'closeConsumerWithReason' so it shows up in the broker-side
    LeaveGroup audit.

Most entries are configuration knobs / pure helpers; a handful
('enforceRebalance', 'commitInBackground') wrap the existing
'Consumer' API.
-}
module Kafka.Client.ConsumerExtras
  ( -- * Snapshots / metadata (KIP-238 / 424)
    EffectiveConsumerSnapshot (..)
  , effectiveConsumerSnapshot
    -- * Auto-rewind (KIP-421)
  , RewindPolicy (..)
  , planRewind
    -- * Rebalance triggers (KIP-568)
  , RebalanceTrigger (..)
  , recordRebalanceTrigger
    -- * Read-only mode (KIP-587)
  , isReadOnlyMode
  , withReadOnly
    -- * Shutdown reason (KIP-1114)
  , ShutdownReason (..)
  , shutdownReasonText
    -- * Server-assignor hint (KIP-941)
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
-- KIP-238 / KIP-424 effective config snapshot
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
-- KIP-421 auto-rewind on OutOfRange
----------------------------------------------------------------------

-- | What to do when 'OFFSET_OUT_OF_RANGE' fires for a partition
-- mid-poll. Mirrors Java's @auto.offset.reset.outOfRange@ /
-- the JVM client's behavioural enum.
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
-- KIP-568 rebalance triggers
----------------------------------------------------------------------

-- | Reason a consumer might explicitly request a rebalance,
-- recorded in the broker-side @JoinGroup.reason@ field
-- (KIP-800 + KIP-568). Helps operators correlate rebalance
-- storms with the application action that triggered them.
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
-- KIP-587 read-only mode
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
-- KIP-1114 shutdown reasons
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
-- KIP-941 server-assignor hint
----------------------------------------------------------------------

-- | The server-side assignor name the client wants the broker
-- to use (KIP-848 + KIP-941). The broker may ignore the hint
-- if it doesn't support the requested assignor.
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
