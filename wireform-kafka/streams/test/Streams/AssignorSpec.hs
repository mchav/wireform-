{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.AssignorSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Hedgehog
import Hedgehog ((===), assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor

tests :: TestTree
tests = testGroup "Assignor"
  [ assigns_total_coverage
  , balances_evenly
  , standby_does_not_overlap_active
  , sticky_keeps_existing_assignment
  , properties
  ]

mkMembers :: [Int] -> Set MemberId
mkMembers xs = Set.fromList [memberOf n | n <- xs]

memberOf :: Int -> MemberId
memberOf n = MemberId ("m" <> T.pack (show n))

mkTasks :: [Int] -> Set TaskId
mkTasks xs = Set.fromList [TaskId 0 (fromIntegral n) | n <- xs]

assigns_total_coverage :: TestTree
assigns_total_coverage =
  testCase "every task is assigned to exactly one active member" $ do
    let ms = Set.fromList [MemberId "m0", MemberId "m1"]
        ts = mkTasks [0, 1, 2, 3, 4]
        asg = assign ms ts 0 Map.empty
    validateAssignment ts 0 asg @?= []
    -- Total active count == total tasks.
    let allActive = foldr Set.union Set.empty (taActive <$> Map.elems asg)
    Set.size allActive @?= 5
    allActive @?= ts

balances_evenly :: TestTree
balances_evenly =
  testCase "no member has more than ceil(N/M) tasks" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b", MemberId "c"]
        ts = mkTasks [0..6]   -- 7 tasks, 3 members → ceil = 3
        asg = assign ms ts 0 Map.empty
    let loads = map (Set.size . taActive) (Map.elems asg)
    maximum loads @?= 3

standby_does_not_overlap_active :: TestTree
standby_does_not_overlap_active =
  testCase "no member is both active and standby for the same task" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b", MemberId "c"]
        ts = mkTasks [0, 1, 2]
        asg = assign ms ts 1 Map.empty
    validateAssignment ts 1 asg @?= []
    -- For each task, the active and standby member must differ.
    let pairs =
          [ (t, m)
          | (m, ta) <- Map.toList asg
          , t <- Set.toList (taActive ta)
          ]
    sequence_
      [ do let active = head [m | (m, ta) <- Map.toList asg, Set.member t (taActive ta)]
               standbys = [m | (m, ta) <- Map.toList asg, Set.member t (taStandby ta)]
           assertNotElem active standbys
      | (t, _) <- pairs
      ]
  where
    assertNotElem x xs
      | x `elem` xs = error ("active member also has standby: " <> show x)
      | otherwise   = pure ()

sticky_keeps_existing_assignment :: TestTree
sticky_keeps_existing_assignment =
  testCase "previous assignment is preserved when no rebalance is needed" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b"]
        ts = mkTasks [0, 1, 2, 3]
        prev = Map.fromList
          [ (MemberId "a", TaskAssignment (Set.fromList [TaskId 0 0, TaskId 0 1]) Set.empty)
          , (MemberId "b", TaskAssignment (Set.fromList [TaskId 0 2, TaskId 0 3]) Set.empty)
          ]
        asg = assign ms ts 0 prev
    -- Same partition assignment as before (zero standbys) → no change.
    Map.lookup (MemberId "a") asg
      @?= Just (TaskAssignment (Set.fromList [TaskId 0 0, TaskId 0 1]) Set.empty)
    Map.lookup (MemberId "b") asg
      @?= Just (TaskAssignment (Set.fromList [TaskId 0 2, TaskId 0 3]) Set.empty)

properties :: TestTree
properties = testGroup "properties"
  [ testProperty "validateAssignment finds no errors for any random input" $ property $ do
      mCount <- forAll (Gen.int (Range.linear 1 5))
      tCount <- forAll (Gen.int (Range.linear 1 30))
      nStandby <- forAll (Gen.int (Range.linear 0 (max 0 (mCount - 1))))
      let ms = mkMembers [0 .. mCount - 1]
          ts = mkTasks [0 .. tCount - 1]
          asg = assign ms ts nStandby Map.empty
      validateAssignment ts nStandby asg === []

  , testProperty "balance: max load - min load <= 1" $ property $ do
      mCount <- forAll (Gen.int (Range.linear 1 5))
      tCount <- forAll (Gen.int (Range.linear 1 30))
      let ms = mkMembers [0 .. mCount - 1]
          ts = mkTasks [0 .. tCount - 1]
          asg = assign ms ts 0 Map.empty
          loads = map (Set.size . taActive) (Map.elems asg)
      assert (maximum loads - minimum loads <= 1)
  ]

-- silence unused-warnings for Hedgehog if the property body shrinks.
_silence :: Hedgehog.PropertyT IO ()
_silence = pure ()