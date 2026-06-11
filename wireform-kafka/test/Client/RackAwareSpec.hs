{-# LANGUAGE OverloadedStrings #-}

module Client.RackAwareSpec (tests) where

import Data.Map.Strict qualified as Map
import Kafka.Client.RackAware qualified as RA
import Test.Syd


tests :: Spec
tests =
  describe "Rack-aware assignment (KIP-881)" $
    sequence_
      [ it
          "rackAffinityScore: leader rack -> 100"
          score_leader
      , it
          "rackAffinityScore: replica rack -> 50"
          score_replica
      , it
          "rackAffinityScore: no match -> 0"
          score_none
      , it
          "preferLocalRack filters by leader-rack match"
          filter_local
      , it
          "rackAwareAssignment respects target load + prefers same rack"
          rack_assign
      ]


score_leader :: IO ()
score_leader =
  RA.rackAffinityScore
    (Just (RA.RackId "us-east-1a"))
    ( RA.PartitionRackInfo
        (0 :: Int)
        (Just (RA.RackId "us-east-1a"))
        [RA.RackId "us-east-1b"]
    )
    `shouldBe` 100


score_replica :: IO ()
score_replica =
  RA.rackAffinityScore
    (Just (RA.RackId "us-east-1b"))
    ( RA.PartitionRackInfo
        (0 :: Int)
        (Just (RA.RackId "us-east-1a"))
        [RA.RackId "us-east-1b"]
    )
    `shouldBe` 50


score_none :: IO ()
score_none =
  RA.rackAffinityScore
    (Just (RA.RackId "eu-west-1a"))
    ( RA.PartitionRackInfo
        (0 :: Int)
        (Just (RA.RackId "us-east-1a"))
        [RA.RackId "us-east-1b"]
    )
    `shouldBe` 0


filter_local :: IO ()
filter_local = do
  let pris =
        [ RA.PartitionRackInfo
            (0 :: Int)
            (Just (RA.RackId "a"))
            []
        , RA.PartitionRackInfo 1 (Just (RA.RackId "b")) []
        , RA.PartitionRackInfo 2 (Just (RA.RackId "a")) []
        ]
  RA.preferLocalRack (Just (RA.RackId "a")) pris `shouldBe` [0, 2]


rack_assign :: IO ()
rack_assign = do
  let inputs =
        RA.RackAwareInputs
          { RA.raiMembers =
              Map.fromList
                [ ("m1" :: String, Just (RA.RackId "a"))
                , ("m2", Just (RA.RackId "b"))
                ]
          , RA.raiPartitions =
              [ RA.PartitionRackInfo (0 :: Int) (Just (RA.RackId "a")) []
              , RA.PartitionRackInfo 1 (Just (RA.RackId "b")) []
              , RA.PartitionRackInfo 2 (Just (RA.RackId "a")) []
              , RA.PartitionRackInfo 3 (Just (RA.RackId "b")) []
              ]
          , RA.raiTargetLoad = 2
          }
      !asg = RA.rackAwareAssignment inputs
  -- Each member should land on partitions with its own rack.
  let !m1 = Map.findWithDefault [] "m1" asg
      !m2 = Map.findWithDefault [] "m2" asg
  -- Rack-a partitions (0, 2) on m1; rack-b partitions (1, 3) on m2.
  (all (`elem` [0, 2 :: Int]) m1) `shouldBe` True
  (all (`elem` [1, 3 :: Int]) m2) `shouldBe` True
  -- Total assignment covers everything.
  length m1 + length m2 `shouldBe` 4
