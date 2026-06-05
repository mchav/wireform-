{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.AssignorSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Hedgehog ((===), assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor

tests :: Spec
tests = describe "Assignor" $ sequence_
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

assigns_total_coverage :: Spec
assigns_total_coverage =
  it "every task is assigned to exactly one active member" $ do
    let ms = Set.fromList [MemberId "m0", MemberId "m1"]
        ts = mkTasks [0, 1, 2, 3, 4]
        asg = assign ms ts 0 Map.empty
    validateAssignment ts 0 asg `shouldBe` []
    -- Total active count == total tasks.
    let allActive = foldr Set.union Set.empty
                      ((\ta -> ta.active) <$> Map.elems asg)
    Set.size allActive `shouldBe` 5
    allActive `shouldBe` ts

balances_evenly :: Spec
balances_evenly =
  it "no member has more than ceil(N/M) tasks" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b", MemberId "c"]
        ts = mkTasks [0..6]   -- 7 tasks, 3 members → ceil = 3
        asg = assign ms ts 0 Map.empty
    let loads = map (\ta -> Set.size ta.active) (Map.elems asg)
    maximum loads `shouldBe` 3

standby_does_not_overlap_active :: Spec
standby_does_not_overlap_active =
  it "no member is both active and standby for the same task" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b", MemberId "c"]
        ts = mkTasks [0, 1, 2]
        asg = assign ms ts 1 Map.empty
    validateAssignment ts 1 asg `shouldBe` []
    -- For each task, the active and standby member must differ.
    let pairs =
          [ (t, m)
          | (m, ta) <- Map.toList asg
          , t <- Set.toList ta.active
          ]
    sequence_
      [ do let active = head [m | (m, ta) <- Map.toList asg, Set.member t ta.active]
               standbys = [m | (m, ta) <- Map.toList asg, Set.member t ta.standby]
           assertNotElem active standbys
      | (t, _) <- pairs
      ]
  where
    assertNotElem x xs
      | x `elem` xs = error ("active member also has standby: " <> show x)
      | otherwise   = pure ()

sticky_keeps_existing_assignment :: Spec
sticky_keeps_existing_assignment =
  it "previous assignment is preserved when no rebalance is needed" $ do
    let ms = Set.fromList [MemberId "a", MemberId "b"]
        ts = mkTasks [0, 1, 2, 3]
        prev = Map.fromList
          [ (MemberId "a", TaskAssignment (Set.fromList [TaskId 0 0, TaskId 0 1]) Set.empty)
          , (MemberId "b", TaskAssignment (Set.fromList [TaskId 0 2, TaskId 0 3]) Set.empty)
          ]
        asg = assign ms ts 0 prev
    -- Same partition assignment as before (zero standbys) → no change.
    Map.lookup (MemberId "a") asg
      `shouldBe` Just (TaskAssignment (Set.fromList [TaskId 0 0, TaskId 0 1]) Set.empty)
    Map.lookup (MemberId "b") asg
      `shouldBe` Just (TaskAssignment (Set.fromList [TaskId 0 2, TaskId 0 3]) Set.empty)

properties :: Spec
properties = describe "properties" $ sequence_
  [ it "validateAssignment finds no errors for any random input" $ property $ do
      mCount <- forAll (Gen.int (Range.linear 1 5))
      tCount <- forAll (Gen.int (Range.linear 1 30))
      nStandby <- forAll (Gen.int (Range.linear 0 (max 0 (mCount - 1))))
      let ms = mkMembers [0 .. mCount - 1]
          ts = mkTasks [0 .. tCount - 1]
          asg = assign ms ts nStandby Map.empty
      validateAssignment ts nStandby asg === []

  , it "balance: max load - min load <= 1" $ property $ do
      mCount <- forAll (Gen.int (Range.linear 1 5))
      tCount <- forAll (Gen.int (Range.linear 1 30))
      let ms = mkMembers [0 .. mCount - 1]
          ts = mkTasks [0 .. tCount - 1]
          asg = assign ms ts 0 Map.empty
          loads = map (\ta -> Set.size ta.active) (Map.elems asg)
      assert (maximum loads - minimum loads <= 1)
  ]
