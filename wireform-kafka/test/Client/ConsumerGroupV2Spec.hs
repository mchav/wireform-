{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the KIP-848 next-generation consumer group
protocol state machine.
-}
module Client.ConsumerGroupV2Spec (tests) where

import Data.Set qualified as Set
import Kafka.Client.ConsumerGroupV2 qualified as G
import Test.Syd


tests :: Spec
tests =
  describe "ConsumerGroupV2 (KIP-848)" $
    sequence_
      [ describe "transitionMemberState" $
          sequence_
            [ it
                "Unsubscribed -> Joining ok"
                (G.transitionMemberState G.MSUnsubscribed G.MSJoining `shouldBe` Just G.MSJoining)
            , it
                "Joining -> Stable ok"
                (G.transitionMemberState G.MSJoining G.MSStable `shouldBe` Just G.MSStable)
            , it
                "Stable -> Reconciling ok"
                (G.transitionMemberState G.MSStable G.MSReconciling `shouldBe` Just G.MSReconciling)
            , it
                "Stable -> Unsubscribed rejected"
                (G.transitionMemberState G.MSStable G.MSUnsubscribed `shouldBe` Nothing)
            , it
                "Anything -> Fenced ok"
                (G.transitionMemberState G.MSStable G.MSFenced `shouldBe` Just G.MSFenced)
            , it
                "Fenced -> Unsubscribed ok"
                (G.transitionMemberState G.MSFenced G.MSUnsubscribed `shouldBe` Just G.MSUnsubscribed)
            ]
      , describe "planHeartbeat" $
          sequence_
            [ it
                "fenced -> MSFenced + every previous partition lost"
                fenced_path
            , it
                "first assignment -> MSStable, all delta in adAssigned"
                first_assignment
            , it
                "no change -> stays in same state, empty delta"
                steady_state
            , it
                "new partitions -> MSReconciling + adAssigned set"
                gain_partitions
            , it
                "lost partitions -> MSReconciling + adRevoked set"
                lose_partitions
            , it
                "next-heartbeat-ms = now + cadence"
                next_hb_arithmetic
            ]
      , describe "subscription helpers" $
          sequence_
            [ it
                "defaultSubscription uses 45_000ms session timeout"
                default_subscription
            ]
      ]


fenced_path :: IO ()
fenced_path = do
  let prev = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev Set.empty True
  G.hpNextState plan `shouldBe` G.MSFenced
  G.adLost (G.hpDelta plan) `shouldBe` prev


first_assignment :: IO ()
first_assignment = do
  let new = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSJoining Set.empty new False
  G.hpNextState plan `shouldBe` G.MSReconciling
  G.adAssigned (G.hpDelta plan) `shouldBe` new
  G.adRevoked (G.hpDelta plan) `shouldBe` Set.empty


steady_state :: IO ()
steady_state = do
  let asg = Set.fromList [("t", 0)]
      plan = G.planHeartbeat 1000 5000 G.MSStable asg asg False
  G.hpNextState plan `shouldBe` G.MSStable
  G.adAssigned (G.hpDelta plan) `shouldBe` Set.empty
  G.adRevoked (G.hpDelta plan) `shouldBe` Set.empty


gain_partitions :: IO ()
gain_partitions = do
  let prev = Set.fromList [("t", 0)]
      new = Set.fromList [("t", 0), ("t", 1)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev new False
  G.hpNextState plan `shouldBe` G.MSReconciling
  G.adAssigned (G.hpDelta plan) `shouldBe` Set.singleton ("t", 1)


lose_partitions :: IO ()
lose_partitions = do
  let prev = Set.fromList [("t", 0), ("t", 1)]
      new = Set.fromList [("t", 0)]
      plan = G.planHeartbeat 1000 5000 G.MSStable prev new False
  G.hpNextState plan `shouldBe` G.MSReconciling
  G.adRevoked (G.hpDelta plan) `shouldBe` Set.singleton ("t", 1)


next_hb_arithmetic :: IO ()
next_hb_arithmetic = do
  let plan = G.planHeartbeat 1000 5000 G.MSStable Set.empty Set.empty False
  G.hpNextHeartbeatMs plan `shouldBe` 6000


default_subscription :: IO ()
default_subscription =
  G.msSessionTimeoutMs (G.defaultSubscription Set.empty) `shouldBe` 45_000
