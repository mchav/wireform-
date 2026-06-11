{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Runtime.Assignor
Description : Pure cooperative-sticky partition assignor for streams

Maps every (active) task and every standby replica to a member of
the consumer group, using a sticky strategy: the previous
assignment is preserved as much as possible, and only over-/
under-loaded members exchange tasks.

Mirrors @org.apache.kafka.streams.processor.internals.StreamsPartitionAssignor@
+ cooperative-sticky semantics (KIP-429), but trimmed:

  * Cooperative-sticky two-phase rebalance is /not/ implemented at
    this layer — that's a property of the consumer-group protocol
    plumbing. The 'assign' function here computes what the leader
    /would/ propose; making it cooperative is an additive
    property of the consumer-group rebalance state machine.
  * Rack-aware assignment is not yet implemented.

== Properties (verified by 'Streams.AssignorSpec')

  * /Total coverage/: every task is assigned exactly once across
    active members. Every task is assigned to at most
    @numStandbyReplicas@ standbys.
  * /Disjoint active/standby per task/: a single member never
    hosts both the active and a standby of the same task.
  * /Stickiness/: tasks the previous assignment placed on member
    M stay on M, unless rebalance forces movement.
  * /Balance/: no member has more than @ceil(N/M)@ active tasks.
-}
module Kafka.Streams.Runtime.Assignor (
  -- * Inputs
  MemberId (..),
  TaskAssignment (..),
  PreviousAssignment,

  -- * Outputs
  NewAssignment,

  -- * Pure assignor
  assign,
  balanceLoad,

  -- * Rack-aware
  RackInfo (..),
  RackAwareCost (..),
  defaultRackAwareCost,
  assignRackAware,

  -- * Property-style invariants
  validateAssignment,
  AssignmentInvariant (..),

  -- * Key-group-aware assignment (Riffle Phase 2 §6)
  KeyGroupAssignment,
  assignKeyGroups,
  validateKeyGroupAssignment,
  KeyGroupInvariant (..),
) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Kafka.Streams.Processor (TaskId (..))


newtype MemberId = MemberId {unMemberId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


{- | The previous assignment for a single member (carried in the
Subscription user-data blob in real Kafka).
-}
data TaskAssignment = TaskAssignment
  { active :: !(Set TaskId)
  , standby :: !(Set TaskId)
  }
  deriving stock (Eq, Show, Generic)


instance Semigroup TaskAssignment where
  TaskAssignment a1 s1 <> TaskAssignment a2 s2 =
    TaskAssignment (a1 <> a2) (s1 <> s2)


instance Monoid TaskAssignment where
  mempty = TaskAssignment Set.empty Set.empty


type PreviousAssignment = Map MemberId TaskAssignment


type NewAssignment = Map MemberId TaskAssignment


----------------------------------------------------------------------
-- Pure assignor
----------------------------------------------------------------------

{- | The core assignor. Inputs:

  * @members@: the alive members in the group.
  * @tasks@: the full set of active tasks the group must cover
    (one per (subtopology, partition) pair).
  * @numStandby@: how many standby replicas to assign per task.
  * @prev@: the previous assignment, used for stickiness. Pass
    'Map.empty' to compute from scratch.

Output: a fresh 'NewAssignment' covering every task exactly once
on the active side and at most @numStandby@ times on the standby
side.
-}
assign
  :: Set MemberId
  -> Set TaskId
  -> Int
  -- ^ numStandbyReplicas
  -> PreviousAssignment
  -> NewAssignment
assign members tasks numStandby prev =
  let memberList = Set.toAscList members
      taskList = Set.toAscList tasks
      taskCount_ = length taskList
      memberCount = length memberList

      -- Step 1: keep prior active assignments where the member is
      -- still alive. Strip standbys (we'll re-derive them).
      keptActive :: Map MemberId (Set TaskId)
      keptActive =
        Map.fromList
          [ ( m
            , maybe
                Set.empty
                (Set.intersection tasks . (\ta -> ta.active))
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
      target =
        if memberCount == 0
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
       [ ( m
         , TaskAssignment
             (Map.findWithDefault Set.empty m activeBalanced)
             (Map.findWithDefault Set.empty m standbyAssigned)
         )
       | m <- memberList
       ]


----------------------------------------------------------------------
-- KIP-925: rack-aware assignment
----------------------------------------------------------------------

{- | Per-member + per-task rack information consulted by
'assignRackAware'. Members without a rack entry are treated
as rack-unknown; tasks without rack entries are treated as
partition-rack-unknown.
-}
data RackInfo = RackInfo
  { memberRack :: !(Map MemberId Text)
  , taskRacks :: !(Map TaskId (Set Text))
  -- ^ Racks of the partitions a task processes.
  }
  deriving stock (Eq, Show, Generic)


{- | Cost knobs mirroring the JVM
@rack.aware.assignment.traffic.cost@ +
@.non.overlap.cost@ config keys.
-}
data RackAwareCost = RackAwareCost
  { trafficCost :: !Int
  {- ^ Per-task cost charged for placing on a member whose
  rack does NOT overlap with any of the task's
  partition racks. The JVM default is 1.
  -}
  , nonOverlapCost :: !Int
  {- ^ Cost charged for placing two replicas of the same
  task in the same rack (i.e. losing rack diversity for
  standbys). JVM default is 10.
  -}
  }
  deriving stock (Eq, Show, Generic)


defaultRackAwareCost :: RackAwareCost
defaultRackAwareCost =
  RackAwareCost
    { trafficCost = 1
    , nonOverlapCost = 10
    }


{- | Rack-aware variant of 'assign'. The algorithm is the same
sticky-then-place-then-balance one with one extra step:
when 'placeOrphans' has multiple lightest-loaded members
to pick from, the one whose rack overlaps with the task's
partition racks wins (so cross-rack traffic stays small).

When @cost.trafficCost@ / @cost.nonOverlapCost@ are both
zero the function is equivalent to 'assign'. When
@rackInfo.memberRack@ is empty the function is equivalent
to 'assign'.
-}
assignRackAware
  :: Set MemberId
  -> Set TaskId
  -> Int
  -- ^ numStandbyReplicas
  -> PreviousAssignment
  -> RackInfo
  -> RackAwareCost
  -> NewAssignment
assignRackAware members tasks numStandby prev rackInfo cost =
  let memberList = Set.toAscList members
      taskList = Set.toAscList tasks
      taskCount_ = length taskList
      memberCount = length memberList

      keptActive :: Map MemberId (Set TaskId)
      keptActive =
        Map.fromList
          [ ( m
            , maybe
                Set.empty
                (Set.intersection tasks . (\ta -> ta.active))
                (Map.lookup m prev)
            )
          | m <- memberList
          ]

      stickyAssigned :: Set TaskId
      stickyAssigned = foldr Set.union Set.empty (Map.elems keptActive)

      orphanTasks :: [TaskId]
      orphanTasks = filter (not . (`Set.member` stickyAssigned)) taskList

      target =
        if memberCount == 0
          then 0
          else (taskCount_ + memberCount - 1) `div` memberCount

      activeAfterPlacement =
        if memberCount == 0
          then keptActive
          else
            placeOrphansRackAware
              memberList
              rackInfo
              cost
              orphanTasks
              keptActive

      activeBalanced =
        balanceLoad target memberList activeAfterPlacement

      standbyAssigned =
        computeStandbysRackAware
          numStandby
          memberList
          rackInfo
          cost
          activeBalanced
  in Map.fromList
       [ ( m
         , TaskAssignment
             (Map.findWithDefault Set.empty m activeBalanced)
             (Map.findWithDefault Set.empty m standbyAssigned)
         )
       | m <- memberList
       ]


{- | Like 'placeOrphans' but the tie-breaker among
lightest-loaded members prefers the member whose rack
overlaps with the task's partition racks. Task is charged
@cost.trafficCost@ for each non-overlapping placement.
-}
placeOrphansRackAware
  :: [MemberId]
  -> RackInfo
  -> RackAwareCost
  -> [TaskId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
placeOrphansRackAware members rackInfo cost orphans assigned0 =
  foldl place assigned0 orphans
  where
    place acc t =
      let !taskRacksOf =
            Map.findWithDefault
              Set.empty
              t
              rackInfo.taskRacks
          -- (load, traffic-cost, member-order, MemberId) — the
          -- tuple ordering is the priority. We pick the
          -- minimum on load first, then traffic cost,
          -- then declaration order.
          scored =
            [ ( Set.size (Map.findWithDefault Set.empty m acc)
              , trafficForMember m taskRacksOf
              , idx
              , m
              )
            | (idx, m) <- zip [0 :: Int ..] members
            ]
          (_, _, _, chosen) = minimum scored
      in Map.adjust
           (Set.insert t)
           chosen
           (Map.insertWith (\_ old -> old) chosen Set.empty acc)
    trafficForMember m racks
      | cost.trafficCost == 0 = 0
      | otherwise = case Map.lookup m rackInfo.memberRack of
          Nothing -> cost.trafficCost
          Just r ->
            if Set.member r racks
              then 0
              else cost.trafficCost


{- | Standby placement preferring members in /different/ racks
from the active to keep failure-domain diversity. Charges
@cost.nonOverlapCost@ when the only remaining candidates share
the active's rack.
-}
computeStandbysRackAware
  :: Int
  -> [MemberId]
  -> RackInfo
  -> RackAwareCost
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
computeStandbysRackAware numStandby members rackInfo cost activeMap
  | numStandby <= 0 = Map.fromList [(m, Set.empty) | m <- members]
  | otherwise =
      foldr
        addStandbysForTask
        (Map.fromList [(m, Set.empty) | m <- members])
        allTasks
  where
    allTasks =
      Set.toAscList
        ( foldr
            Set.union
            Set.empty
            (Map.elems activeMap)
        )
    activeOf t =
      [ m
      | (m, ts) <- Map.toAscList activeMap
      , Set.member t ts
      ]
    rackOf m = Map.lookup m rackInfo.memberRack
    addStandbysForTask t acc =
      let active = activeOf t
          activeRacks =
            Set.fromList
              [r | m <- active, Just r <- [rackOf m]]
          candidates =
            [ ( Set.size (Map.findWithDefault Set.empty m acc)
              , standbyCost m activeRacks
              , idx
              , m
              )
            | (idx, m) <- zip [0 :: Int ..] members
            , m `notElem` active
            ]
          chosen =
            take
              numStandby
              ( map
                  (\(_, _, _, m) -> m)
                  (List.sort candidates)
              )
      in foldl'
           (\m_ pick -> Map.adjust (Set.insert t) pick m_)
           acc
           chosen
    standbyCost m activeRacks
      | cost.nonOverlapCost == 0 = 0
      | otherwise = case rackOf m of
          Nothing -> 0
          Just r ->
            if Set.member r activeRacks
              then cost.nonOverlapCost
              else 0


{- | Place each orphan task on the first member (in order) that's
below 'target'. If all members are at target, place on whoever
has the fewest tasks (which is now equal to target everywhere
except the very last member).
-}
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
      let
        -- Always pick the lightest-loaded member, breaking ties
        -- by member order. This keeps the load delta <= 1
        -- regardless of how high 'target' is.
        loads =
          [ (Set.size (Map.findWithDefault Set.empty m acc), idx, m)
          | (idx, m) <- zip [0 :: Int ..] members
          ]
        (_, _, chosen) = minimum loads
      in
        Map.adjust
          (Set.insert t)
          chosen
          (Map.insertWith (\_ old -> old) chosen Set.empty acc)


{- | Move tasks off over-loaded members onto under-loaded ones.
Stops when every member has at most 'target' tasks.
-}
balanceLoad
  :: Int
  -> [MemberId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
balanceLoad target members assigned0 = go assigned0
  where
    go acc =
      let loads =
            [ (m, Set.size (Map.findWithDefault Set.empty m acc))
            | m <- members
            ]
          over = filter (\(_, n) -> n > target) loads
          under = filter (\(_, n) -> n < target) loads
      in case (over, under) of
           ((mFrom, _) : _, (mTo, _) : _) ->
             case Set.lookupMin (Map.findWithDefault Set.empty mFrom acc) of
               Nothing -> acc
               Just t ->
                 let !acc1 = Map.adjust (Set.delete t) mFrom acc
                     !acc2 = Map.adjust (Set.insert t) mTo acc1
                 in go acc2
           _ -> acc


{- | For each task, pick @numStandby@ members that don't already
host the active task to be the standby replicas.
-}
computeStandbys
  :: Int
  -> [MemberId]
  -> Map MemberId (Set TaskId)
  -> Map MemberId (Set TaskId)
computeStandbys 0 _ _ = Map.empty
computeStandbys numStandby members active =
  let allTasks = foldr Set.union Set.empty (Map.elems active)
      -- For each task, determine the active member.
      activeOf t =
        head
          [m | (m, ts) <- Map.toList active, Set.member t ts]
      -- For each task, take the next @numStandby@ members
      -- (modulo active member exclusion) in a round-robin fashion.
      step (idx, accStandbys) t =
        let aMember = activeOf t
            (newIdx, picks) = pickStandbys idx members numStandby aMember
            !accStandbys' =
              foldl
                ( \acc m ->
                    Map.insertWith Set.union m (Set.singleton t) acc
                )
                accStandbys
                picks
        in (newIdx, accStandbys')
      (_, result) = foldl step (0, Map.empty) (Set.toAscList allTasks)
  in result


{- | Pick @n@ standby members starting at the rotating @idx@,
skipping the active member.
-}
pickStandbys
  :: Int -- starting rotation index
  -> [MemberId] -- all members
  -> Int -- numStandby
  -> MemberId -- active member to exclude
  -> (Int, [MemberId]) -- (new index, picked standbys)
pickStandbys startIdx members n exclude =
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
  -> Int
  -- ^ numStandbyReplicas
  -> NewAssignment
  -> [AssignmentInvariant]
validateAssignment tasks numStandby asg =
  let
    activeMap :: Map TaskId [MemberId]
    activeMap =
      Map.fromListWith
        (++)
        [ (t, [m])
        | (m, ta) <- Map.toList asg
        , t <- Set.toList ta.active
        ]

    standbyMap :: Map TaskId [MemberId]
    standbyMap =
      Map.fromListWith
        (++)
        [ (t, [m])
        | (m, ta) <- Map.toList asg
        , t <- Set.toList ta.standby
        ]

    missing =
      [ MissingActiveTask t
      | t <- Set.toList tasks
      , not (Map.member t activeMap)
      ]
    dups =
      [ DuplicateActiveTask t (head ms) (ms !! 1)
      | (t, ms) <- Map.toList activeMap
      , length ms >= 2
      ]
    tooMany =
      [ TooManyStandbys t (length ms)
      | (t, ms) <- Map.toList standbyMap
      , length ms > numStandby
      ]
    overlap =
      [ OverlapActiveStandby t m
      | (m, ta) <- Map.toList asg
      , t <- Set.toList (ta.active `Set.intersection` ta.standby)
      ]
  in
    missing <> dups <> tooMany <> overlap


----------------------------------------------------------------------
-- Key-group-aware assignment (Riffle Phase 2 §6)
----------------------------------------------------------------------

{- | One member's assignment in a key-group-aware deployment:
the set of key-group ids it owns. This is the building block
on which the runtime constructs per-worker
'KeyGroupRange's.
-}
type KeyGroupAssignment = Map MemberId (Set Int)


{- | Stick-y assignment of a flat key-group id space across the
given members. Mirrors Flink's @KeyGroupRangeAssignment@: the
key-group id space is partitioned into roughly-equal
contiguous ranges so that hash-routed records on the hot path
pay only a range check instead of a map lookup.

  * Stickiness: if @previous@ already assigned key-group @i@
    to a still-live member, the new assignment keeps it
    there.
  * Balance: each member gets either @floor(N/M)@ or
    @ceil(N/M)@ key-groups.
  * Total coverage: every key-group in @[0, count - 1]@ is
    assigned to exactly one member.
-}
assignKeyGroups
  :: Set MemberId
  -> Int
  -- ^ total number of key-groups
  -> KeyGroupAssignment
  -- ^ previous assignment (sticky)
  -> KeyGroupAssignment
assignKeyGroups members keyGroupCount previous
  | Set.null members = Map.empty
  | otherwise =
      let memList = Set.toAscList members
          everyKg = [0 .. keyGroupCount - 1]
          -- Stickiness: keep key-groups whose previous owner is
          -- still in the member set.
          kept :: KeyGroupAssignment
          kept =
            Map.fromList
              [ ( m
                , Set.filter
                    (\k -> k >= 0 && k < keyGroupCount)
                    (Map.findWithDefault Set.empty m previous)
                )
              | m <- memList
              ]
          assignedSoFar :: Set Int
          assignedSoFar =
            foldr Set.union Set.empty (Map.elems kept)
          unassigned = filter (`Set.notMember` assignedSoFar) everyKg
          -- Compute target sizes.
          n = keyGroupCount
          m = length memList
          baseSize = n `div` m
          extras = n `mod` m
          targetSize idx
            | idx < extras = baseSize + 1
            | otherwise = baseSize
          go [] _ acc = acc
          go _ [] acc = acc
          go (k : rest) ((mid, idx) : queue) acc =
            let already = Map.findWithDefault Set.empty mid acc
            in if Set.size already < targetSize idx
                 then
                   go
                     rest
                     queue
                     (Map.adjust (Set.insert k) mid acc)
                 else go (k : rest) queue acc
          memQueue = zip memList [0 ..]
          startAcc =
            Map.fromList
              [ (mid, Map.findWithDefault Set.empty mid kept)
              | mid <- memList
              ]
          dropped =
            dropOverflow
              memList
              kept
              (zip memList [0 ..])
              startAcc
          afterRedistribute = go unassigned (cycleQueue memQueue) dropped
          rebalanced =
            redistribute
              memList
              afterRedistribute
              [targetSize idx | idx <- [0 .. m - 1]]
      in rebalanced


{- | Cycle through a non-empty queue forever. Used as the
round-robin order when handing out unassigned key-groups.
-}
cycleQueue :: [a] -> [a]
cycleQueue xs = xs ++ cycleQueue xs


{- | If a member's sticky inheritance is larger than its target
size, drop the overflow into the unassigned pool so a smaller
member can pick it up.
-}
dropOverflow
  :: [MemberId]
  -> KeyGroupAssignment
  -> [(MemberId, Int)]
  -> KeyGroupAssignment
  -> KeyGroupAssignment
dropOverflow memList kept queue acc =
  let n = sum (Set.size <$> Map.elems acc)
      m = length memList
      baseSize = n `div` m
      extras = n `mod` m
      targetSize idx
        | idx < extras = baseSize + 1
        | otherwise = baseSize
      shrink (mid, idx) m_ =
        let cur = Map.findWithDefault Set.empty mid m_
            t = targetSize idx
        in if Set.size cur <= t
             then m_
             else
               let !shrinkBy = Set.size cur - t
                   !toDrop =
                     Set.fromList
                       (take shrinkBy (Set.toAscList cur))
                   !cur' = Set.difference cur toDrop
               in Map.insert mid cur' m_
  in foldr shrink acc queue
{-# WARNING dropOverflow "internal helper; do not export" #-}


{- | After the greedy fill, walk the members once more and move
key-groups from over-sized members to under-sized ones to hit
the per-member target exactly. Run iteratively in case
redistributing creates new imbalance.
-}
redistribute
  :: [MemberId]
  -> KeyGroupAssignment
  -> [Int] -- per-member target sizes (idx-aligned)
  -> KeyGroupAssignment
redistribute mems acc targets = loop acc
  where
    loop a =
      let pairs = zip mems targets
          (overs, unders) =
            List.partition
              (\(mid, t) -> Set.size (Map.findWithDefault Set.empty mid a) > t)
              pairs
          undersByDeficit =
            filter
              (\(mid, t) -> Set.size (Map.findWithDefault Set.empty mid a) < t)
              pairs
      in case (overs, undersByDeficit) of
           ((mO, _) : _, (mU, _) : _) ->
             let setO = Map.findWithDefault Set.empty mO a
                 (one, _setO') = (Set.findMin setO, Set.deleteMin setO)
                 !a' = Map.adjust (Set.delete one) mO a
                 !a'' = Map.adjust (Set.insert one) mU a'
             in loop a''
           _ -> a


{- | Property-style invariants for a 'KeyGroupAssignment'. The
assignment is well-formed if every key-group in @[0, count -
1]@ is assigned to exactly one member and the per-member
sizes are within one of the average.
-}
data KeyGroupInvariant
  = MissingKeyGroup !Int
  | DuplicateKeyGroup !Int !MemberId !MemberId
  | OutOfRangeKeyGroup !Int !MemberId
  | -- | member, size, expected
    UnbalancedMember !MemberId !Int !Int
  deriving stock (Eq, Show)


validateKeyGroupAssignment
  :: Int
  -- ^ total key-groups
  -> KeyGroupAssignment
  -> [KeyGroupInvariant]
validateKeyGroupAssignment n asg =
  let everyKg = [0 .. n - 1]
      ownerMap :: Map Int [MemberId]
      ownerMap =
        Map.fromListWith
          (<>)
          [ (k, [mid])
          | (mid, ks) <- Map.toList asg
          , k <- Set.toList ks
          ]
      missing =
        [ MissingKeyGroup k
        | k <- everyKg
        , not (Map.member k ownerMap)
        ]
      dups =
        [ DuplicateKeyGroup k a b
        | (k, ms) <- Map.toList ownerMap
        , a : b : _ <- [ms]
        ]
      outRange =
        [ OutOfRangeKeyGroup k mid
        | (mid, ks) <- Map.toList asg
        , k <- Set.toList ks
        , k < 0 || k >= n
        ]
      m = Map.size asg
      baseSize = if m == 0 then 0 else n `div` m
      extras = if m == 0 then 0 else n `mod` m
      sortedSizes =
        List.sortBy
          (\(_, s1) (_, s2) -> compare s2 s1)
          [ (mid, Set.size ks)
          | (mid, ks) <- Map.toList asg
          ]
      expected idx
        | idx < extras = baseSize + 1
        | otherwise = baseSize
      unbal =
        [ UnbalancedMember mid sz (expected idx)
        | ((mid, sz), idx) <- zip sortedSizes [0 ..]
        , sz /= expected idx
        ]
  in missing <> dups <> outRange <> unbal
