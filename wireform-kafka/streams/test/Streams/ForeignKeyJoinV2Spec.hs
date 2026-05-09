{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streams.ForeignKeyJoinV2Spec (tests) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.DSL.ForeignKeyJoinV2 as FK

tests :: TestTree
tests = testGroup "ForeignKeyJoinV2 (KIP-213)"
  [ testCase "left then right -> single join output"
      left_then_right
  , testCase "right then left -> single join output"
      right_then_left
  , testCase "left tombstone emits a join tombstone"
      left_tombstone
  , testCase "right tombstone emits join tombstones for each subscriber"
      right_tombstone
  , testProperty
      "permutation invariance: final state independent of event order"
      prop_permutation_invariance
  ]

mkSt :: FK.FkJoinState Int Int Int Int (Int, Int)
mkSt = FK.emptyState (\vl vr -> (vl, vr)) id

left_then_right :: IO ()
left_then_right = do
  let (st1, j1, sub1) = FK.stepLeft  mkSt (FK.LeftPut 1 100)
      (_st2, j2)      = FK.stepRight st1 (FK.RightPut 100 7)
  -- The first put has no right yet -> tombstone.
  j1 @?= [FK.JoinOutput 1 Nothing]
  -- The right arrival emits the join.
  j2 @?= [FK.JoinOutput 1 (Just (100, 7))]
  -- And we sent a subscription out.
  case sub1 of
    Just s -> FK.smPropagate s @?= 100
    Nothing -> error "expected subscription"

right_then_left :: IO ()
right_then_left = do
  let (st1, _j1)      = FK.stepRight mkSt (FK.RightPut 100 7)
      (_st2, j2, _)   = FK.stepLeft  st1 (FK.LeftPut 1 100)
  j2 @?= [FK.JoinOutput 1 (Just (100, 7))]

left_tombstone :: IO ()
left_tombstone = do
  let (st1, _, _)   = FK.stepLeft mkSt (FK.LeftPut 1 100)
      (_st2, j2, _) = FK.stepLeft st1  (FK.LeftDelete 1)
  j2 @?= [FK.JoinOutput 1 Nothing]

right_tombstone :: IO ()
right_tombstone = do
  let (st1, _, _) = FK.stepLeft  mkSt (FK.LeftPut 1 100)
      (st2, _)    = FK.stepRight st1  (FK.RightPut 100 7)
      (_st3, j3)  = FK.stepRight st2  (FK.RightDelete 100)
  j3 @?= [FK.JoinOutput 1 Nothing]

prop_permutation_invariance :: Property
prop_permutation_invariance = property $ do
  -- Generate a fixed input set with /distinct/ left keys + foreign
  -- keys (i.e. no overwrites, so the final cache is order-independent
  -- over the permutations we test).
  fks    <- forAll $ Gen.list (Range.linear 1 4) (Gen.int (Range.linear 1 3))
  let !uniqueFks = take 4 (sort (Set.toList (Set.fromList fks)))
  uniqueLeftKs <- forAll $ Gen.set (Range.linear 1 4) (Gen.int (Range.linear 1 4))
  let lefts  = zip (Set.toList uniqueLeftKs) (cycle uniqueFks)
      rights = zip uniqueFks (map (* 7) [1 ..])
      leftEvents  = map (\(k, fk) -> Left  (FK.LeftPut k fk)) lefts
      rightEvents = map (\(fk, v) -> Right (FK.RightPut fk v)) rights
      sequenced   = leftEvents ++ rightEvents
      permuted    = rightEvents ++ leftEvents
      (sFinal, _, _) = FK.runEvents mkSt sequenced
      (pFinal, _, _) = FK.runEvents mkSt permuted
  Map.toAscList (FK.fjsLefts sFinal)
    === Map.toAscList (FK.fjsLefts pFinal)
  Map.toAscList (FK.fjsRights sFinal)
    === Map.toAscList (FK.fjsRights pFinal)
