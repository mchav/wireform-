{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.KeyGroupAssignorSpec
Description : Property suite for key-group routing,
              key-group-aware assignor, and KIP-848 protocol
              primitives

Properties:

  1. /Key-group routing/: 'keyGroupOf' lands in @[0, count - 1]@.
  2. /Range membership/: round-tripping through 'rangeFromList'
     and 'rangeToList' is the identity (modulo duplicate
     removal).
  3. /Assignor coverage/: every key-group in @[0, count - 1]@
     is owned by exactly one member.
  4. /Assignor balance/: each member has either @floor(n/m)@ or
     @ceil(n/m)@ key-groups.
  5. /Sticky/: re-running 'assignKeyGroups' with the previous
     result as @previous@ leaves the assignment unchanged.
  6. /Reconciliation no-double-ownership/: in
     'Reconciliation', a task being moved appears in the
     losing member's 'rRemove' /before/ it appears in the
     gaining member's 'rAdd'.
  7. /Reconciliation convergence/: applying every member's
     reconciliation drives 'gsOwned' toward 'gsTarget'.
-}
module Streams.Properties.KeyGroupAssignorSpec (tests) where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor
import Kafka.Streams.Runtime.KeyGroup
import Kafka.Streams.Runtime.RebalanceProtocol
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Routing
----------------------------------------------------------------------

prop_keygroup_in_bounds :: H.Property
prop_keygroup_in_bounds = H.property $ do
  count <- H.forAll (Gen.int (Range.linear 1 128))
  ks <-
    H.forAll
      (Gen.list (Range.linear 1 50) (Gen.int (Range.linear 0 10_000)))
  let kgs = map (keyGroupOf (KeyGroupCount count)) ks
  H.assert (all (\(KeyGroupId k) -> k >= 0 && k < count) kgs)


prop_range_round_trip :: H.Property
prop_range_round_trip = H.property $ do
  raw <-
    H.forAll
      ( Gen.list
          (Range.linear 0 30)
          (KeyGroupId <$> Gen.int (Range.linear 0 127))
      )
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
  let members =
        Set.fromList
          [ MemberId (T.pack ("m" <> show i))
          | i <- [0 .. m - 1]
          ]
      asg = assignKeyGroups members n Map.empty
  validateKeyGroupAssignment n asg H.=== []


prop_assignor_balance :: H.Property
prop_assignor_balance = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 1 5))
  n <- H.forAll (Gen.int (Range.linear 1 64))
  let members =
        Set.fromList
          [ MemberId (T.pack ("m" <> show i))
          | i <- [0 .. m - 1]
          ]
      asg = assignKeyGroups members n Map.empty
      sizes = Map.elems (Set.size <$> asg)
  case sizes of
    [] -> pure ()
    _ -> H.assert (maximum sizes - minimum sizes <= 1)


prop_assignor_sticky :: H.Property
prop_assignor_sticky = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 1 5))
  n <- H.forAll (Gen.int (Range.linear 1 64))
  let members =
        Set.fromList
          [ MemberId (T.pack ("m" <> show i))
          | i <- [0 .. m - 1]
          ]
      first = assignKeyGroups members n Map.empty
      second = assignKeyGroups members n first
  second H.=== first


----------------------------------------------------------------------
-- KIP-848 reconciliation
----------------------------------------------------------------------

unit_reconciliation_no_double_owner :: Spec
unit_reconciliation_no_double_owner =
  it "reconciliation does not hand task t to B while A still owns it" $ do
    let mA = MemberId "A"
        mB = MemberId "B"
        t = TaskId 0 0
        gs =
          initialGroupState
            { gsMembers =
                Map.fromList
                  [ (mA, Subscription Set.empty (Set.singleton t) (MemberEpoch 0))
                  , (mB, Subscription Set.empty Set.empty (MemberEpoch 0))
                  ]
            , gsOwned =
                Map.fromList
                  [(mA, Set.singleton t), (mB, Set.empty)]
            , gsTarget =
                Map.fromList
                  [(mA, Set.empty), (mB, Set.singleton t)]
            }
        r = reconcile gs
    -- A must release the task.
    rRemove (r Map.! mA) `shouldBe` Set.singleton t
    -- B must NOT yet have it — A still owns it.
    rAdd (r Map.! mB) `shouldBe` Set.empty


unit_reconciliation_two_step :: Spec
unit_reconciliation_two_step =
  it "after A releases, B can take ownership" $ do
    let mA = MemberId "A"
        mB = MemberId "B"
        t = TaskId 0 0
        gs =
          initialGroupState
            { gsMembers =
                Map.fromList
                  [ (mA, Subscription Set.empty (Set.singleton t) (MemberEpoch 0))
                  , (mB, Subscription Set.empty Set.empty (MemberEpoch 0))
                  ]
            , gsOwned =
                Map.fromList
                  [(mA, Set.singleton t), (mB, Set.empty)]
            , gsTarget =
                Map.fromList
                  [(mA, Set.empty), (mB, Set.singleton t)]
            }
        -- Step 1: A acknowledges the release.
        r1 = reconcile gs
        gs' = applyReconciliation mA (r1 Map.! mA) gs
        -- Step 2: B now sees the task in its 'rAdd'.
        r2 = reconcile gs'
    rAdd (r2 Map.! mB) `shouldBe` Set.singleton t
    rRemove (r2 Map.! mB) `shouldBe` Set.empty
    -- And B applies.
    let gs'' = applyReconciliation mB (r2 Map.! mB) gs'
    Map.findWithDefault Set.empty mA (gsOwned gs'') `shouldBe` Set.empty
    Map.findWithDefault Set.empty mB (gsOwned gs'') `shouldBe` Set.singleton t


----------------------------------------------------------------------
-- Property: many-step reconciliation converges to target
----------------------------------------------------------------------

prop_reconciliation_converges :: H.Property
prop_reconciliation_converges = H.property $ do
  m <- H.forAll (Gen.int (Range.linear 2 4))
  n <- H.forAll (Gen.int (Range.linear 2 16))
  -- Random target distribution: each task goes to a random
  -- member.
  let mems = [MemberId (T.pack ("m" <> show i)) | i <- [0 .. m - 1]]
      tids = [TaskId 0 (fromIntegral i) | i <- [0 .. n - 1]]
  taskOwners <-
    H.forAll $
      Gen.list
        (Range.singleton n)
        (Gen.element mems)
  let target =
        Map.fromListWith
          Set.union
          [ (mid, Set.singleton t)
          | (t, mid) <- zip tids taskOwners
          ]
      -- Initial: every task is owned by the first member.
      initialOwned =
        Map.fromList
          [(head mems, Set.fromList tids)]
          <> Map.fromList [(mid, Set.empty) | mid <- tail mems]
      gs0 =
        initialGroupState
          { gsMembers =
              Map.fromList
                [ ( mid
                  , Subscription
                      Set.empty
                      (Map.findWithDefault Set.empty mid initialOwned)
                      (MemberEpoch 0)
                  )
                | mid <- mems
                ]
          , gsOwned = initialOwned
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
        | iters <= 0 =
            Left
              ( "did not converge after limit; "
                  <> show (gsOwned gs)
              )
        | otherwise =
            let r = reconcile gs
                gs' =
                  foldr
                    ( \mid acc ->
                        let rm =
                              Map.findWithDefault
                                emptyReconciliation
                                mid
                                r
                        in applyReconciliation mid rm acc
                    )
                    gs
                    mems
            in if gs' == gs
                 then
                   Left
                     ( "no progress; owned = "
                         <> show (gsOwned gs)
                     )
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

tests :: Spec
tests =
  describe "Key-group + KIP-848" $
    sequence_
      [ it "keyGroupOf lands in [0, count - 1]" $
          H.withTests 100 prop_keygroup_in_bounds
      , it "KeyGroupRange round-trip is identity (modulo dedup)" $
          H.withTests 80 prop_range_round_trip
      , it "assignKeyGroups: total coverage, no duplicates" $
          H.withTests 80 prop_assignor_total_coverage
      , it "assignKeyGroups: max-load - min-load <= 1" $
          H.withTests 80 prop_assignor_balance
      , it "assignKeyGroups is sticky across re-runs" $
          H.withTests 80 prop_assignor_sticky
      , unit_reconciliation_no_double_owner
      , unit_reconciliation_two_step
      , it "reconciliation converges to target after enough rounds" $
          H.withTests 60 prop_reconciliation_converges
      ]
