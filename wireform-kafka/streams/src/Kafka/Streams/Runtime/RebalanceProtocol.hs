{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.RebalanceProtocol
-- Description : KIP-848 (next-gen consumer-group protocol)
--               primitives for Riffle (Phase 2 §6)
--
-- KIP-848 moves assignment off the client and onto the
-- broker-side group coordinator. Members exchange:
--
--   * /Subscribed topics/ + /member epoch/ → coordinator.
--   * /Computed assignment/ + /target epoch/ → member.
--
-- The reconciliation between the member's currently-owned tasks
-- and the target assignment is /incremental/: a member that's
-- losing a task first releases it, then the coordinator hands
-- it to the new owner, then the new owner acknowledges. This
-- gives cooperative semantics without the explicit join/sync
-- rounds of the older protocol.
--
-- This module exposes:
--
--   * The wire types ('Subscription', 'Assignment', 'MemberEpoch',
--     'RebalanceEpoch', 'TargetAssignment', 'OwnedAssignment')
--     in a transport-agnostic shape.
--   * A pure reconciler that computes which (member, task)
--     ownership changes are needed to move from the current
--     state to the target.
--   * The group-state state machine (Empty → Assigning →
--     Stable → Reconciling → Stable).
--
-- The integration with the broker-side
-- 'Kafka.Client.ConsumerGroupV2' lives outside this package; this
-- module is the bridge layer that the streams runtime drives.
module Kafka.Streams.Runtime.RebalanceProtocol
  ( -- * Epochs
    MemberEpoch (..)
  , RebalanceEpoch (..)
    -- * Wire types
  , Subscription (..)
  , Assignment (..)
  , TargetAssignment
  , OwnedAssignment
    -- * Group state
  , GroupState (..)
  , initialGroupState
    -- * Reconciliation
  , Reconciliation (..)
  , emptyReconciliation
  , reconcile
  , applyReconciliation
    -- * Transitions
  , addMember
  , removeMember
  , updateTarget
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Processor (TaskId)
import Kafka.Streams.Runtime.Assignor (MemberId)

----------------------------------------------------------------------
-- Epochs
----------------------------------------------------------------------

-- | Per-member epoch. Bumps every time a member's owned
-- assignment changes. The coordinator uses this to validate
-- heartbeats: a member with a stale epoch must reconcile before
-- continuing.
newtype MemberEpoch = MemberEpoch { unMemberEpoch :: Int }
  deriving stock (Eq, Ord, Show, Generic)

-- | Group-wide rebalance epoch. Bumps every time the target
-- assignment changes (member joins / leaves, subscription
-- change, partition count change).
newtype RebalanceEpoch = RebalanceEpoch { unRebalanceEpoch :: Int }
  deriving stock (Eq, Ord, Show, Generic)

----------------------------------------------------------------------
-- Wire types
----------------------------------------------------------------------

-- | Submitted by a member on heartbeat.
data Subscription = Subscription
  { subscribedTopics :: !(Set Text)
  , currentlyOwned   :: !(Set TaskId)
  , memberEpoch      :: !MemberEpoch
  } deriving stock (Eq, Show, Generic)

-- | Returned by the coordinator on heartbeat once a new target
-- has been computed.
data Assignment = Assignment
  { newlyOwned    :: !(Set TaskId)
  , newEpoch      :: !MemberEpoch
  , rebalanceTag  :: !RebalanceEpoch
  } deriving stock (Eq, Show, Generic)

-- | Coordinator's view: what each member /should/ own at the
-- current target rebalance epoch.
type TargetAssignment = Map MemberId (Set TaskId)

-- | Coordinator's view: what each member /currently/ owns, as
-- reported by recent heartbeats.
type OwnedAssignment = Map MemberId (Set TaskId)

----------------------------------------------------------------------
-- Group state
----------------------------------------------------------------------

-- | The coordinator's view of the group at a moment in time.
data GroupState = GroupState
  { gsMembers    :: !(Map MemberId Subscription)
  , gsTarget     :: !TargetAssignment
  , gsOwned      :: !OwnedAssignment
  , gsEpoch      :: !RebalanceEpoch
  } deriving stock (Eq, Show, Generic)

initialGroupState :: GroupState
initialGroupState = GroupState
  { gsMembers = Map.empty
  , gsTarget  = Map.empty
  , gsOwned   = Map.empty
  , gsEpoch   = RebalanceEpoch 0
  }

----------------------------------------------------------------------
-- Reconciliation
----------------------------------------------------------------------

-- | The diff between owned + target. Each member learns:
--
--   * @rAdd@: tasks it should /add/ to its owned set.
--   * @rRemove@: tasks it should /release/.
--
-- Reconciliation rule: a task being moved from member @A@ to
-- member @B@ first shows up in @A@'s 'rRemove'. @A@ releases it
-- (acknowledged by a follow-up heartbeat that no longer
-- includes the task in 'currentlyOwned'); then @B@ sees it in
-- its 'rAdd'.
data Reconciliation = Reconciliation
  { rAdd    :: !(Set TaskId)
  , rRemove :: !(Set TaskId)
  } deriving stock (Eq, Show, Generic)

emptyReconciliation :: Reconciliation
emptyReconciliation = Reconciliation Set.empty Set.empty

-- | Compute the reconciliation for every member. A task that
-- needs to move from @A@ to @B@ appears in @A@'s 'rRemove'
-- /before/ it appears in @B@'s 'rAdd' — i.e. while @A@ still
-- owns it, @B@ does not get it.
reconcile :: GroupState -> Map MemberId Reconciliation
reconcile gs =
  let stillOwned :: Map MemberId (Set TaskId)
      stillOwned = gsOwned gs
      target     = gsTarget gs
      allMembers = Set.union (Map.keysSet stillOwned)
                              (Map.keysSet target)
      perMember m =
        let own = Map.findWithDefault Set.empty m stillOwned
            tgt = Map.findWithDefault Set.empty m target
            -- Tasks the member still owns but is supposed to
            -- give up. They appear in 'rRemove' immediately.
            rm  = Set.difference own tgt
            -- Tasks the member should pick up — but only if no
            -- other member still owns them.
            wanted = Set.difference tgt own
            blocked t =
              any (\(m', os) -> m /= m' && Set.member t os)
                  (Map.toList stillOwned)
            add = Set.filter (not . blocked) wanted
        in Reconciliation { rAdd = add, rRemove = rm }
  in Map.fromSet perMember allMembers

-- | Apply a reconciliation /step/: the member acknowledged
-- removing its 'rRemove' set (no longer owns those tasks) and
-- picked up its 'rAdd' set. Updates 'gsOwned' for that member
-- and bumps its 'MemberEpoch'.
applyReconciliation
  :: MemberId
  -> Reconciliation
  -> GroupState
  -> GroupState
applyReconciliation mid r gs =
  let !old   = Map.findWithDefault Set.empty mid (gsOwned gs)
      !new   = Set.union (Set.difference old (rRemove r)) (rAdd r)
      !owned = Map.insert mid new (gsOwned gs)
      bumpSub s = s { memberEpoch = MemberEpoch
                       (unMemberEpoch (memberEpoch s) + 1) }
      !mems  = Map.adjust bumpSub mid (gsMembers gs)
  in gs { gsOwned = owned, gsMembers = mems }

----------------------------------------------------------------------
-- Transitions
----------------------------------------------------------------------

-- | A new member joined. They start with no owned tasks and the
-- current rebalance epoch.
addMember :: MemberId -> Subscription -> GroupState -> GroupState
addMember mid sub gs =
  let !mems = Map.insert mid sub (gsMembers gs)
      !ownN = Map.insertWith
                (\_ x -> x) mid (currentlyOwned sub) (gsOwned gs)
      !gs'  = gs { gsMembers = mems, gsOwned = ownN }
  in bumpEpoch gs'

-- | A member left. Their owned tasks go into "orphaned" state
-- until the next 'updateTarget' redistributes them.
removeMember :: MemberId -> GroupState -> GroupState
removeMember mid gs =
  let !mems = Map.delete mid (gsMembers gs)
      !ownN = Map.delete mid (gsOwned gs)
      !tgt  = Map.delete mid (gsTarget gs)
  in bumpEpoch gs { gsMembers = mems, gsOwned = ownN, gsTarget = tgt }

-- | Coordinator publishes a new target assignment (e.g. after
-- the leader re-runs the assignor). Bumps the rebalance epoch
-- so members know to start reconciling.
updateTarget :: TargetAssignment -> GroupState -> GroupState
updateTarget tgt gs = bumpEpoch gs { gsTarget = tgt }

bumpEpoch :: GroupState -> GroupState
bumpEpoch gs =
  let RebalanceEpoch e = gsEpoch gs
  in gs { gsEpoch = RebalanceEpoch (e + 1) }
