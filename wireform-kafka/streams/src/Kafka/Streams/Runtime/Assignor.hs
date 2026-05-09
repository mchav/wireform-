{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.Assignor
-- Description : Pure cooperative-sticky partition assignor for streams
--
-- Maps every (active) task and every standby replica to a member of
-- the consumer group, using a sticky strategy: the previous
-- assignment is preserved as much as possible, and only over-/
-- under-loaded members exchange tasks.
--
-- Mirrors @org.apache.kafka.streams.processor.internals.StreamsPartitionAssignor@
-- + cooperative-sticky semantics (KIP-429), but trimmed:
--
--   * Cooperative-sticky two-phase rebalance is /not/ implemented at
--     this layer — that's a property of the consumer-group protocol
--     plumbing. The 'assign' function here computes what the leader
--     /would/ propose; making it cooperative is an additive
--     property of the consumer-group rebalance state machine.
--   * Rack-aware assignment is not yet implemented.
--
-- == Properties (verified by 'Streams.AssignorSpec')
--
--   * /Total coverage/: every task is assigned exactly once across
--     active members. Every task is assigned to at most
--     @numStandbyReplicas@ standbys.
--   * /Disjoint active/standby per task/: a single member never
--     hosts both the active and a standby of the same task.
--   * /Stickiness/: tasks the previous assignment placed on member
--     M stay on M, unless rebalance forces movement.
--   * /Balance/: no member has more than @ceil(N/M)@ active tasks.
module Kafka.Streams.Runtime.Assignor
  ( -- * Inputs
    MemberId (..)
  , TaskAssignment (..)
  , PreviousAssignment
    -- * Outputs
  , NewAssignment
    -- * Pure assignor
  , assign
  , balanceLoad
    -- * Property-style invariants
  , validateAssignment
  , AssignmentInvariant (..)
  ) where

import Data.Hashable (Hashable)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Kafka.Streams.Processor (TaskId (..))

newtype MemberId = MemberId { unMemberId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

-- | The previous assignment for a single member (carried in the
-- Subscription user-data blob in real Kafka).
data TaskAssignment = TaskAssignment
  { taActive   :: !(Set TaskId)
  , taStandby  :: !(Set TaskId)
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup TaskAssignment where
  TaskAssignment a1 s1 <> TaskAssignment a2 s2 =
    TaskAssignment (a1 <> a2) (s1 <> s2)

instance Monoid TaskAssignment where
  mempty = TaskAssignment Set.empty Set.empty

type PreviousAssignment = Map MemberId TaskAssignment
type NewAssignment      = Map MemberId TaskAssignment

----------------------------------------------------------------------
-- Pure assignor
----------------------------------------------------------------------

-- | The core assignor. Inputs:
--
--   * @members@: the alive members in the group.
--   * @tasks@: the full set of active tasks the group must cover
--     (one per (subtopology, partition) pair).
--   * @numStandby@: how many standby replicas to assign per task.
--   * @prev@: the previous assignment, used for stickiness. Pass
--     'Map.empty' to compute from scratch.
--
-- Output: a fresh 'NewAssignment' covering every task exactly once
-- on the active side and at most @numStandby@ times on the standby
-- side.
assign
  :: Set MemberId
  -> Set TaskId
  -> Int                   -- ^ numStandbyReplicas
  -> PreviousAssignment
  -> NewAssignment
assign members tasks numStandby prev =
  let memberList     = Set.toAscList members
      taskList       = Set.toAscList tasks
      taskCount_     = length taskList
      memberCount    = length memberList

      -- Step 1: keep prior active assignments where the member is
      -- still alive. Strip standbys (we'll re-derive them).
      keptActive :: Map MemberId (Set TaskId)
      keptActive = Map.fromList
        [ ( m
          , maybe Set.empty
              (Set.intersection tasks . taActive)
              (Map.lookup m prev)
          )
        | m <- memberList
        ]

      -- Tasks that have a sticky home.
      stickyAssigned :: Set TaskId
      stickyAssigned = foldr Set.union Set.empty (Map.elems keptActive)

      -- Tasks that need fresh placement.
      orphanTasks :: [TaskId]
      orphanTasks = filter (not . (`Set.member` stickyAssigned)) taskList

      -- Step 2: place orphans round-robin among under-loaded
      -- members. We compute the load target as ceil(taskCount/memberCount).
      target = if memberCount == 0
                  then 0
                  else (taskCount_ + memberCount - 1) `div` memberCount

      activeAfterPlacement =
        if memberCount == 0
          then keptActive
          else placeOrphans target memberList orphanTasks keptActive

      -- Step 3: rebalance — if any member ended up with more than
      -- @target@ tasks, redistribute the extras.
      activeBalanced = balanceLoad target memberList activeAfterPlacement

      -- Step 4: standbys — for each task, pick the first @numStandby@
      -- members that DON'T host the active task.
      standbyAssigned = computeStandbys numStandby memberList activeBalanced
   in Map.fromList
        [ (m, TaskAssignment
                (Map.findWithDefault Set.empty m activeBalanced)
                (Map.findWithDefault Set.empty m standbyAssigned))
        | m <- memberList
        ]

-- | Place each orphan task on the first member (in order) that's
-- below 'target'. If all members are at target, place on whoever
-- has the fewest tasks (which is now equal to target everywhere
-- except the very last member).
placeOrphans
  :: Int
  -> [MemberId]
  -> [TaskId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
placeOrphans _target members orphans assigned0 =
  foldl place assigned0 orphans
  where
    place acc t =
      let -- Always pick the lightest-loaded member, breaking ties
          -- by member order. This keeps the load delta <= 1
          -- regardless of how high 'target' is.
          loads = [ (Set.size (Map.findWithDefault Set.empty m acc), idx, m)
                  | (idx, m) <- zip [0 :: Int ..] members
                  ]
          (_, _, chosen) = minimum loads
       in Map.adjust (Set.insert t) chosen
            (Map.insertWith (\_ old -> old) chosen Set.empty acc)

-- | Move tasks off over-loaded members onto under-loaded ones.
-- Stops when every member has at most 'target' tasks.
balanceLoad
  :: Int
  -> [MemberId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
balanceLoad target members assigned0 = go assigned0
  where
    go acc =
      let loads = [ (m, Set.size (Map.findWithDefault Set.empty m acc))
                  | m <- members
                  ]
          over  = filter (\(_, n) -> n > target) loads
          under = filter (\(_, n) -> n < target) loads
       in case (over, under) of
            ((mFrom, _) : _, (mTo, _) : _) ->
              case Set.lookupMin (Map.findWithDefault Set.empty mFrom acc) of
                Nothing -> acc
                Just t  ->
                  let !acc1 = Map.adjust (Set.delete t) mFrom acc
                      !acc2 = Map.adjust (Set.insert t) mTo   acc1
                   in go acc2
            _ -> acc

-- | For each task, pick @numStandby@ members that don't already
-- host the active task to be the standby replicas.
computeStandbys
  :: Int
  -> [MemberId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
computeStandbys 0          _       _ = Map.empty
computeStandbys numStandby members active =
  let allTasks = foldr Set.union Set.empty (Map.elems active)
      -- For each task, determine the active member.
      activeOf t = head
        [ m | (m, ts) <- Map.toList active, Set.member t ts ]
      -- For each task, take the next @numStandby@ members
      -- (modulo active member exclusion) in a round-robin fashion.
      step (idx, accStandbys) t =
        let aMember = activeOf t
            (newIdx, picks) = pickStandbys idx members numStandby aMember
            !accStandbys' = foldl
              (\acc m ->
                 Map.insertWith Set.union m (Set.singleton t) acc)
              accStandbys
              picks
         in (newIdx, accStandbys')
      (_, result) = foldl step (0, Map.empty) (Set.toAscList allTasks)
   in result

-- | Pick @n@ standby members starting at the rotating @idx@,
-- skipping the active member.
pickStandbys
  :: Int                   -- starting rotation index
  -> [MemberId]            -- all members
  -> Int                   -- numStandby
  -> MemberId              -- active member to exclude
  -> (Int, [MemberId])     -- (new index, picked standbys)
pickStandbys startIdx members n exclude =
  -- Project the input into a 'Vector' so the rotating @i `mod` total@
  -- lookup is O(1) instead of the previous list (!!) walk; the cost
  -- of the conversion is paid once per outer 'computeStandbys' fold
  -- rather than per element.
  let !memV = V.fromList members
      !total = V.length memV
      go i picked needed
        | needed == 0 = (i, reverse picked)
        | i >= startIdx + total = (startIdx + total, reverse picked)
        | otherwise =
            let !m = memV `V.unsafeIndex` (i `mod` total)
             in if m == exclude || m `elem` picked
                  then go (i + 1) picked needed
                  else go (i + 1) (m : picked) (needed - 1)
   in go startIdx [] n

----------------------------------------------------------------------
-- Validation
----------------------------------------------------------------------

data AssignmentInvariant
  = MissingActiveTask !TaskId
  | DuplicateActiveTask !TaskId !MemberId !MemberId
  | TooManyStandbys !TaskId !Int
  | OverlapActiveStandby !TaskId !MemberId
  deriving stock (Eq, Show, Generic)

-- | Check that an assignment satisfies the documented invariants.
validateAssignment
  :: Set TaskId
  -> Int                   -- ^ numStandbyReplicas
  -> NewAssignment
  -> [AssignmentInvariant]
validateAssignment tasks numStandby asg =
  let
    activeMap :: Map TaskId [MemberId]
    activeMap = Map.fromListWith (++)
      [ (t, [m])
      | (m, ta) <- Map.toList asg
      , t <- Set.toList (taActive ta)
      ]

    standbyMap :: Map TaskId [MemberId]
    standbyMap = Map.fromListWith (++)
      [ (t, [m])
      | (m, ta) <- Map.toList asg
      , t <- Set.toList (taStandby ta)
      ]

    missing = [ MissingActiveTask t
              | t <- Set.toList tasks
              , not (Map.member t activeMap)
              ]
    dups = [ DuplicateActiveTask t (head ms) (ms !! 1)
           | (t, ms) <- Map.toList activeMap, length ms >= 2
           ]
    tooMany = [ TooManyStandbys t (length ms)
              | (t, ms) <- Map.toList standbyMap, length ms > numStandby
              ]
    overlap = [ OverlapActiveStandby t m
              | (m, ta) <- Map.toList asg
              , t <- Set.toList (taActive ta `Set.intersection` taStandby ta)
              ]
   in missing <> dups <> tooMany <> overlap