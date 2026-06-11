{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.ClusterBringup
Description : Start a multi-instance cluster on the mock broker

A 3-instance cluster ('Kafka.Streams.Runtime.MultiInstanceMockHarness')
subscribes to a 6-partition input topic. The demo prints the
partition assignment each instance sees post-rebalance, then
drives one record per partition through the cluster and
confirms each record lands in the @out@ topic exactly once.

This is the analogue of "kafka-streams-application
--num-stream-threads=3" coming up cold against a fresh broker.
-}
module Kafka.Streams.Examples.Ops.ClusterBringup (
  runDemo,
) where

import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Mock.Cluster qualified as MC
import Kafka.Streams.Runtime.MultiInstanceMockHarness qualified as H


runDemo :: IO ()
runDemo = do
  section "ClusterBringupDemo"
  topo <- passthroughTopo
  set <- H.newMockSet topo 6 "ops-bringup" 3

  asg <- H.instanceAssignments set
  printAssignments "Post-rebalance assignment" asg

  -- Seed one record per input partition so every instance has
  -- something to chew on.
  let pairs :: [(Int, Text)]
      pairs = [(p, T.pack ("p" <> show p)) | p <- [0 .. 5]]
  mapM_
    ( \(p, v) ->
        H.send set (topicName "in") (fromIntegral p) Nothing (bytes v) ts0
    )
    pairs
  H.tickAllUntilQuiet set

  -- The sink mirrors the source (6 partitions). The union of all
  -- partitions should be exactly the record set we sent.
  outs <-
    mapM
      (\p -> map (unbytes . MC.srValue) <$> H.readSink set (topicName "out") p)
      [0 .. 5]
  let delivered = Set.fromList (concat outs)
      expected = Set.fromList (map snd pairs)
  bullet
    ( "Records delivered to sink: "
        <> show (Set.size delivered)
        <> " / "
        <> show (Set.size expected)
    )
  let missing = L.sort (Set.toList (expected `Set.difference` delivered))
  bullet ("Missing: " <> show missing)

  H.closeMockSet set
