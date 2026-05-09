{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the latest tier-of-parity:
-- Topology.optimize, addReadOnlyStateStore, lag listener.
module Streams.MorePartiyTwoSpec (tests) where

import Data.IORef
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams

tests :: TestTree
tests = testGroup "ParityRoundTwo"
  [ optimize_topology_is_identity_when_off
  , optimize_topology_with_default_config
  , lag_listener_receives_published_snapshot
  , lag_listener_default_does_nothing
  ]

mkSimpleTopo :: IO Topology
mkSimpleTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  buildTopology b

optimize_topology_is_identity_when_off :: TestTree
optimize_topology_is_identity_when_off =
  testCase "optimizeTopology is the identity when no toggles are enabled" $ do
    topo <- mkSimpleTopo
    let cfg = OptimizationConfig
          { optMergeRepartitionTopics = False
          , optReuseSourceKTable      = False
          }
        topo' = optimizeTopology cfg topo
    -- Same node count after optimisation (currently a no-op).
    length (topoOrder topo') @?= length (topoOrder topo)

optimize_topology_with_default_config :: TestTree
optimize_topology_with_default_config =
  testCase "optimizeTopology with the default config still yields a valid topology" $ do
    topo <- mkSimpleTopo
    let topo' = optimizeTopology defaultOptimizationConfig topo
    case validateTopology topo' of
      Right _  -> pure ()
      Left err -> error (show err)

mkRuntime :: IO KafkaStreams
mkRuntime = do
  topo <- mkSimpleTopo
  case validateTopology topo of
    Left  err -> error (show err)
    Right v   -> newKafkaStreams defaultStreamsConfig
                    { applicationId    = "lag-app"
                    , bootstrapServers = ["mock:0"]
                    } v

lag_listener_receives_published_snapshot :: TestTree
lag_listener_receives_published_snapshot =
  testCase "publishLag dispatches to the registered listener" $ do
    ks <- mkRuntime
    received <- newIORef ([] :: [LagInfo])
    setLagListener ks (writeIORef received)

    let snapshot =
          [ LagInfo (TaskId 0 0) 100 200
          , LagInfo (TaskId 0 1) 300 305
          ]
    publishLag ks snapshot
    readIORef received >>= (@?= snapshot)
    closeKafkaStreams ks

lag_listener_default_does_nothing :: TestTree
lag_listener_default_does_nothing =
  testCase "default lag listener is a no-op (no exception when called)" $ do
    ks <- mkRuntime
    publishLag ks [LagInfo (TaskId 0 0) 0 0]
    closeKafkaStreams ks
