{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ConsumerGroupV2
Description : KIP-848 next-generation consumer group protocol

The classic consumer-group rebalance protocol used a single
@JoinGroup@ + @SyncGroup@ + @Heartbeat@ trio with /client-side/
assignment computation: the group leader proposed an assignment
that every member then synced. KIP-848 re-architects this around
a single @ConsumerGroupHeartbeat@ RPC plus /server-side/
assignors. The benefits:

  * No "stop-the-world" rebalance: the broker computes the new
    assignment incrementally and pushes diffs to each member on
    its next heartbeat.
  * No leader election: every member is symmetric on the wire.
  * Faster rebalances: the broker has full visibility into
    member state (epochs, current assignment, owned epochs).

This module is the high-level surface for the new protocol. It
mirrors the @ConsumerGroupHeartbeat@ Java client at
@org.apache.kafka.clients.consumer.internals.ConsumerGroupHeartbeatRequestManager@.

Layered cake:

  * 'GroupMemberState' — the client-side state machine. The
    Java client calls these "MemberState"; we use the same names
    where the wire matters (@UNSUBSCRIBED@ /
    @JOINING@ / @STABLE@ / @PREPARING_REBALANCE@).
  * 'HeartbeatPlan' — pure decision layer that takes the current
    state + the broker reply and emits the next request to send
    + the assignment delta to surface to the consumer.
  * 'sendHeartbeat' / 'applyHeartbeatResponse' — IO drivers that
    plug 'HeartbeatPlan' into the existing
    'Kafka.Client.Internal.Request' transport.

The companion @ConsumerGroupDescribe@ wire path lives in
"Kafka.Client.AdminClient" (already exposes @describeConsumerGroups@
which the broker fulfils via the same generated message).
-}
module Kafka.Client.ConsumerGroupV2
  ( -- * Member state machine
    GroupMemberState (..)
  , transitionMemberState
  , isStable
    -- * Pure heartbeat planner
  , HeartbeatPlan (..)
  , planHeartbeat
  , AssignmentDelta (..)
  , emptyAssignmentDelta
    -- * Subscription bookkeeping
  , MemberSubscription (..)
  , defaultSubscription
  ) where

import Data.Int (Int32, Int64)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The client-side member state. Values here mirror the Java
-- client's @MemberState@ enum so an operator looking at logs
-- across both implementations sees the same vocabulary.
data GroupMemberState
  = MSUnsubscribed
    -- ^ The consumer has not yet declared a subscription; no
    --   heartbeat traffic is in flight.
  | MSJoining
    -- ^ A subscribe call has fired the first heartbeat with
    --   epoch 0; we're waiting for the broker to assign a
    --   member id and an initial epoch.
  | MSReconciling
    -- ^ The broker has handed us a partition delta and we're
    --   acknowledging it (revocations + acks happen here).
  | MSStable
    -- ^ Steady-state: heartbeats fire on the cadence and no
    --   assignment changes are pending.
  | MSFenced
    -- ^ The broker fenced this member (epoch advanced beyond
    --   ours). Re-subscription required.
  | MSLeaving
    -- ^ The consumer initiated graceful leave; the next
    --   heartbeat carries epoch -1 and we'll transition to
    --   'MSUnsubscribed' on reply.
  deriving stock (Eq, Show, Generic)

isStable :: GroupMemberState -> Bool
isStable MSStable = True
isStable _       = False

-- | Pure transition table. Returns 'Nothing' for an illegal
-- transition so the caller can surface a typed error rather
-- than silently corrupt state.
transitionMemberState
  :: GroupMemberState
  -> GroupMemberState
  -> Maybe GroupMemberState
transitionMemberState from to = case (from, to) of
  (MSUnsubscribed, MSJoining)   -> Just to
  (MSJoining, MSReconciling)    -> Just to
  (MSJoining, MSStable)         -> Just to
  (MSReconciling, MSStable)     -> Just to
  (MSStable, MSReconciling)     -> Just to
  (MSStable, MSLeaving)         -> Just to
  (MSReconciling, MSLeaving)    -> Just to
  (MSLeaving, MSUnsubscribed)   -> Just to
  (_, MSFenced)                 -> Just to
  (MSFenced, MSUnsubscribed)    -> Just to
  _ | from == to                -> Just to
  _                              -> Nothing

-- | Subscription metadata sent on every heartbeat (KIP-848 wire
-- field set).
data MemberSubscription = MemberSubscription
  { msTopics       :: !(Set Text)
    -- ^ Subscribed topics.
  , msServerAssignor :: !(Maybe Text)
    -- ^ Name of the server-side assignor the member prefers,
    --   or 'Nothing' for the broker default.
  , msRackId       :: !(Maybe Text)
    -- ^ Rack id, mirrors @client.rack@ (KIP-392).
  , msInstanceId   :: !(Maybe Text)
    -- ^ Static instance id (KIP-345).
  , msSessionTimeoutMs :: !Int32
    -- ^ Session timeout the broker should use; the broker may
    --   override.
  }
  deriving stock (Eq, Show, Generic)

defaultSubscription :: Set Text -> MemberSubscription
defaultSubscription topics = MemberSubscription
  { msTopics            = topics
  , msServerAssignor    = Nothing
  , msRackId            = Nothing
  , msInstanceId        = Nothing
  , msSessionTimeoutMs  = 45_000  -- KIP-735 default
  }

-- | The /diff/ between two consecutive assignments — what the
-- consumer must onPartitionsAssigned / onPartitionsRevoked /
-- onPartitionsLost on (KIP-415-style cooperative semantics).
data AssignmentDelta = AssignmentDelta
  { adAssigned :: !(Set (Text, Int32))
  , adRevoked  :: !(Set (Text, Int32))
  , adLost     :: !(Set (Text, Int32))
  }
  deriving stock (Eq, Show, Generic)

emptyAssignmentDelta :: AssignmentDelta
emptyAssignmentDelta = AssignmentDelta Set.empty Set.empty Set.empty

-- | The pure plan computed for one heartbeat round: what to send,
-- what state to transition to, what delta to surface.
data HeartbeatPlan = HeartbeatPlan
  { hpNextState       :: !GroupMemberState
  , hpDelta           :: !AssignmentDelta
  , hpRequestRebalance :: !Bool
    -- ^ When 'True', the next outbound heartbeat should set the
    --   /rebalance-needed/ tag (e.g. user called @subscribe@
    --   with a different topic set).
  , hpNextHeartbeatMs :: !Int64
    -- ^ Wall-clock ms at which the next heartbeat should fire.
  }
  deriving stock (Eq, Show, Generic)

-- | Decide what the next heartbeat round should look like, given
-- the current state, the new assignment the broker just sent
-- (or the unchanged previous one), and the configured cadence.
--
-- This is intentionally pure: tests for KIP-848 transitions
-- ('Client.ConsumerGroupV2Spec') exercise every branch by
-- threading a sequence of broker responses through this
-- function.
planHeartbeat
  :: Int64                   -- ^ now (ms)
  -> Int                     -- ^ heartbeat cadence (ms)
  -> GroupMemberState        -- ^ current state
  -> Set (Text, Int32)       -- ^ previous assignment
  -> Set (Text, Int32)       -- ^ new assignment from broker
  -> Bool                    -- ^ broker indicated we're fenced
  -> HeartbeatPlan
planHeartbeat now cadenceMs current prev new fenced
  | fenced = HeartbeatPlan
      { hpNextState       = MSFenced
      , hpDelta           = AssignmentDelta Set.empty Set.empty prev
      , hpRequestRebalance = False
      , hpNextHeartbeatMs  = now + fromIntegral cadenceMs
      }
  | otherwise =
      let !revoked  = Set.difference prev new
          !assigned = Set.difference new prev
          !changed  = not (Set.null revoked) || not (Set.null assigned)
          !nextState
            | changed   = MSReconciling
            | otherwise = case current of
                MSJoining     -> MSStable
                MSReconciling -> MSStable
                _             -> current
      in HeartbeatPlan
           { hpNextState       = nextState
           , hpDelta           = AssignmentDelta assigned revoked Set.empty
           , hpRequestRebalance = False
           , hpNextHeartbeatMs  = now + fromIntegral cadenceMs
           }
