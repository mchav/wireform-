{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Properties.KeyGroupAssignorSpec
-- Description : Property suite for key-group routing,
--               key-group-aware assignor, and KIP-848 protocol
--               primitives
--
-- Properties:
--
--   1. /Key-group routing/: 'keyGroupOf' lands in @[0, count - 1]@.
--   2. /Range membership/: round-tripping through 'rangeFromList'
--      and 'rangeToList' is the identity (modulo duplicate
--      removal).
--   3. /Assignor coverage/: every key-group in @[0, count - 1]@
--      is owned by exactly one member.
--   4. /Assignor balance/: each member has either @floor(n/m)@ or
--      @ceil(n/m)@ key-groups.
--   5. /Sticky/: re-running 'assignKeyGroups' with the previous
--      result as @previous@ leaves the assignment unchanged.
--   6. /Reconciliation no-double-ownership/: in
--      'Reconciliation', a task being moved appears in the
--      losing member's 'rRemove' /before/ it appears in the
--      gaining member's 'rAdd'.
--   7. /Reconciliation convergence/: applying every member's
--      reconciliation drives 'gsOwned' toward 'gsTarget'.
module Streams.Properties.KeyGroupAssignorSpec (tests) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor
import Kafka.Streams.Runtime.KeyGroup
import Kafka.Streams.Runtime.RebalanceProtocol

----------------------------------------------------------------------
-- Routing
----------------------------------------------------------------------

prop_keygroup_in_bounds :: H.Property
prop_keygroup_in_bounds = H.property $ do
  count <- H.forAll (Gen.int (Range.linear 1 128))
  ks    <- H.forAll
             (Gen.list (Range.linear 1 50) (Gen.int (Range.linear 0 10_000)))
  let kgs = map (keyGroupOf (KeyGroupCount count)) ks
  H.assert (all (\(KeyGroupId k) -> k >= 0 && k < count) kgs)

prop_range_round_trip :: H.Property
prop_range_round_trip = H.property $ do
  raw <- H.forAll
           (Gen.list (Range.linear 0 30)
             (KeyGroupId <$> Gen.int (Range.linear 0 127)))
  let r = rangeFromList raw
      back = rangeToList r
  -- 'back' is the deduped, sorted version of 'raw'.
  back H.=== List.sort (List.nub raw)
  -- Membership matches.
  mapM_ (\k -> H.assert (inKeyGroupRange r k)) back

----------------------------------------------------------------------
-- Assignor
----------------------------------------------------------------------

prop_assignor_total_coverage :: H.Property
prop_assignor_total_coverage = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 1 5))
  n <- H.forAll (Gen.int (Range.linear 1 64))
  let members = Set.fromList [ MemberId (T.pack ("m" <> show i))
                             | i <- [0 .. m - 1] ]
      asg = assignKeyGroups members n Map.empty
  validateKeyGroupAssignment n asg H.=== []

prop_assignor_balance :: H.Property
prop_assignor_balance = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 1 5))
  n <- H.forAll (Gen.int (Range.linear 1 64))
  let members = Set.fromList [ MemberId (T.pack ("m" <> show i))
                             | i <- [0 .. m - 1] ]
      asg = assignKeyGroups members n Map.empty
      sizes = Map.elems (Set.size <$> asg)
  case sizes of
    [] -> pure ()
    _  -> H.assert (maximum sizes - minimum sizes <= 1)

prop_assignor_sticky :: H.Property
prop_assignor_sticky = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 1 5))
  n <- H.forAll (Gen.int (Range.linear 1 64))
  let members = Set.fromList [ MemberId (T.pack ("m" <> show i))
                             | i <- [0 .. m - 1] ]
      first   = assignKeyGroups members n Map.empty
      second  = assignKeyGroups members n first
  second H.=== first

----------------------------------------------------------------------
-- KIP-848 reconciliation
----------------------------------------------------------------------

unit_reconciliation_no_double_owner :: TestTree
unit_reconciliation_no_double_owner =
  testCase "reconciliation does not hand task t to B while A still owns it" $ do
    let mA = MemberId "A"
        mB = MemberId "B"
        t  = TaskId 0 0
        gs = initialGroupState
          { gsMembers = Map.fromList
              [ (mA, Subscription Set.empty (Set.singleton t) (MemberEpoch 0))
              , (mB, Subscription Set.empty Set.empty (MemberEpoch 0))
              ]
          , gsOwned = Map.fromList
              [ (mA, Set.singleton t), (mB, Set.empty) ]
          , gsTarget = Map.fromList
              [ (mA, Set.empty), (mB, Set.singleton t) ]
          }
        r = reconcile gs
    -- A must release the task.
    rRemove (r Map.! mA) @?= Set.singleton t
    -- B must NOT yet have it — A still owns it.
    rAdd (r Map.! mB) @?= Set.empty

unit_reconciliation_two_step :: TestTree
unit_reconciliation_two_step =
  testCase "after A releases, B can take ownership" $ do
    let mA = MemberId "A"
        mB = MemberId "B"
        t  = TaskId 0 0
        gs = initialGroupState
          { gsMembers = Map.fromList
              [ (mA, Subscription Set.empty (Set.singleton t) (MemberEpoch 0))
              , (mB, Subscription Set.empty Set.empty (MemberEpoch 0))
              ]
          , gsOwned = Map.fromList
              [ (mA, Set.singleton t), (mB, Set.empty) ]
          , gsTarget = Map.fromList
              [ (mA, Set.empty), (mB, Set.singleton t) ]
          }
        -- Step 1: A acknowledges the release.
        r1   = reconcile gs
        gs'  = applyReconciliation mA (r1 Map.! mA) gs
        -- Step 2: B now sees the task in its 'rAdd'.
        r2   = reconcile gs'
    rAdd (r2 Map.! mB) @?= Set.singleton t
    rRemove (r2 Map.! mB) @?= Set.empty
    -- And B applies.
    let gs'' = applyReconciliation mB (r2 Map.! mB) gs'
    Map.findWithDefault Set.empty mA (gsOwned gs'') @?= Set.empty
    Map.findWithDefault Set.empty mB (gsOwned gs'') @?= Set.singleton t

----------------------------------------------------------------------
-- Property: many-step reconciliation converges to target
----------------------------------------------------------------------

prop_reconciliation_converges :: H.Property
prop_reconciliation_converges = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 2 4))
  n <- H.forAll (Gen.int (Range.linear 2 16))
  -- Random target distribution: each task goes to a random
  -- member.
  let mems = [ MemberId (T.pack ("m" <> show i)) | i <- [0 .. m - 1] ]
      tids = [ TaskId 0 (fromIntegral i) | i <- [0 .. n - 1] ]
  taskOwners <- H.forAll $ Gen.list (Range.singleton n)
                  (Gen.element mems)
  let target = Map.fromListWith Set.union
                 [ (mid, Set.singleton t)
                 | (t, mid) <- zip tids taskOwners ]
      -- Initial: every task is owned by the first member.
      initialOwned = Map.fromList
        [ (head mems, Set.fromList tids) ]
                <> Map.fromList [ (mid, Set.empty) | mid <- tail mems ]
      gs0 = initialGroupState
        { gsMembers = Map.fromList
            [ (mid, Subscription Set.empty
                      (Map.findWithDefault Set.empty mid initialOwned)
                      (MemberEpoch 0))
            | mid <- mems
            ]
        , gsOwned  = initialOwned
        , gsTarget = target
        }
      -- Reconcile in a loop, applying every non-empty step in
      -- declaration order until 'gsOwned == gsTarget'.
      -- Normalise both sides by dropping members whose set is
      -- empty: 'gsTarget' may not carry empty entries explicitly.
      normalise = Map.filter (not . Set.null)
      converged gs = normalise (gsOwned gs) == normalise (gsTarget gs)
      go iters gs
        | converged gs = Right gs
        | iters <= 0 = Left ("did not converge after limit; "
                            <> show (gsOwned gs))
        | otherwise =
            let r = reconcile gs
                gs' = foldr
                  (\mid acc ->
                      let rm = Map.findWithDefault
                                emptyReconciliation mid r
                      in applyReconciliation mid rm acc)
                  gs
                  mems
            in if gs' == gs
                 then Left ("no progress; owned = "
                           <> show (gsOwned gs))
                 else go (iters - 1) gs'
  case go (4 * n) gs0 of
    Right gs ->
      Map.filter (not . Set.null) (gsOwned gs)
        H.=== Map.filter (not . Set.null) target
    Left err -> do
      H.annotate err
      H.failure

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Key-group + KIP-848"
  [ testProperty "keyGroupOf lands in [0, count - 1]" $
      H.withTests 100 prop_keygroup_in_bounds
  , testProperty "KeyGroupRange round-trip is identity (modulo dedup)" $
      H.withTests 80 prop_range_round_trip
  , testProperty "assignKeyGroups: total coverage, no duplicates" $
      H.withTests 80 prop_assignor_total_coverage
  , testProperty "assignKeyGroups: max-load - min-load <= 1" $
      H.withTests 80 prop_assignor_balance
  , testProperty "assignKeyGroups is sticky across re-runs" $
      H.withTests 80 prop_assignor_sticky
  , unit_reconciliation_no_double_owner
  , unit_reconciliation_two_step
  , testProperty "reconciliation converges to target after enough rounds" $
      H.withTests 60 prop_reconciliation_converges
  ]
