{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-848 next-generation consumer group
-- protocol state machine.
module Client.ConsumerGroupV2Spec (tests) where

import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.ConsumerGroupV2 as G

tests :: TestTree
tests = testGroup "ConsumerGroupV2 (KIP-848)"
  [ testGroup "transitionMemberState"
      [ testCase "Unsubscribed -> Joining ok"
          (G.transitionMemberState G.MSUnsubscribed G.MSJoining @?= Just G.MSJoining)
      , testCase "Joining -> Stable ok"
          (G.transitionMemberState G.MSJoining G.MSStable @?= Just G.MSStable)
      , testCase "Stable -> Reconciling ok"
          (G.transitionMemberState G.MSStable G.MSReconciling @?= Just G.MSReconciling)
      , testCase "Stable -> Unsubscribed rejected"
          (G.transitionMemberState G.MSStable G.MSUnsubscribed @?= Nothing)
      , testCase "Anything -> Fenced ok"
          (G.transitionMemberState G.MSStable G.MSFenced @?= Just G.MSFenced)
      , testCase "Fenced -> Unsubscribed ok"
          (G.transitionMemberState G.MSFenced G.MSUnsubscribed @?= Just G.MSUnsubscribed)
      ]
  , testGroup "planHeartbeat"
      [ testCase "fenced -> MSFenced + every previous partition lost"
          fenced_path
      , testCase "first assignment -> MSStable, all delta in adAssigned"
          first_assignment
      , testCase "no change -> stays in same state, empty delta"
          steady_state
      , testCase "new partitions -> MSReconciling + adAssigned set"
          gain_partitions
      , testCase "lost partitions -> MSReconciling + adRevoked set"
          lose_partitions
      , testCase "next-heartbeat-ms = now + cadence"
          next_hb_arithmetic
      ]
  , testGroup "subscription helpers"
      [ testCase "defaultSubscription uses 45_000ms session timeout"
          default_subscription
      ]
  ]

fenced_path :: IO ()
fenced_path = do
  let prev = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev Set.empty True
  G.hpNextState plan @?= G.MSFenced
  G.adLost (G.hpDelta plan) @?= prev

first_assignment :: IO ()
first_assignment = do
  let new = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSJoining Set.empty new False
  G.hpNextState plan @?= G.MSReconciling
  G.adAssigned (G.hpDelta plan) @?= new
  G.adRevoked (G.hpDelta plan) @?= Set.empty

steady_state :: IO ()
steady_state = do
  let asg = Set.fromList [("t", 0)]
      plan = G.planHeartbeat 1000 5000 G.MSStable asg asg False
  G.hpNextState plan @?= G.MSStable
  G.adAssigned (G.hpDelta plan) @?= Set.empty
  G.adRevoked (G.hpDelta plan) @?= Set.empty

gain_partitions :: IO ()
gain_partitions = do
  let prev = Set.fromList [("t", 0)]
      new  = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev new False
  G.hpNextState plan @?= G.MSReconciling
  G.adAssigned (G.hpDelta plan) @?= Set.singleton ("t", 1)

lose_partitions :: IO ()
lose_partitions = do
  let prev = Set.fromList [("t", 0), ("t", 1)]
      new  = Set.fromList [("t", 0)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev new False
  G.hpNextState plan @?= G.MSReconciling
  G.adRevoked (G.hpDelta plan) @?= Set.singleton ("t", 1)

next_hb_arithmetic :: IO ()
next_hb_arithmetic = do
  let plan = G.planHeartbeat 1000 5000 G.MSStable Set.empty Set.empty False
  G.hpNextHeartbeatMs plan @?= 6000

default_subscription :: IO ()
default_subscription =
  G.msSessionTimeoutMs (G.defaultSubscription Set.empty) @?= 45_000
