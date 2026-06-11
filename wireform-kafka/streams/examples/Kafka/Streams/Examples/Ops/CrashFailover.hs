{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.CrashFailover
Description : One instance dies; peers inherit its partitions

Two instances share a 4-partition input. We send the first
batch, then call 'H.crashInstance' on @i0@ (which closes its
driver and 'MC.leaveGroup's its consumer) and 'H.refreshAll'.
After the rebalance @i1@ owns all four partitions; the second
batch flows entirely through @i1@.

Equivalent to a pod dying mid-flight in a Kafka Streams
StatefulSet — the surviving replica picks up the orphaned
tasks once the consumer group coordinator notices the missing
heartbeat.
-}
module Kafka.Streams.Examples.Ops.CrashFailover (
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
  section "CrashFailoverDemo"
  topo <- passthroughTopo
  set <- H.newMockSet topo 4 "ops-crash" 2

  before <- H.instanceAssignments set
  printAssignments "Before crash" before

  -- Batch 1: every instance gets work.
  let batch1 :: [(Int, Text)]
      batch1 = [(p, T.pack ("a" <> show p)) | p <- [0 .. 3]]
  mapM_
    ( \(p, v) ->
        H.send set (topicName "in") (fromIntegral p) Nothing (bytes v) ts0
    )
    batch1
  H.tickAllUntilQuiet set

  -- Crash i0. The mock cluster evicts its membership; the
  -- survivor needs 'refreshAll' so the assignor re-runs.
  bullet "Crashing i0..."
  H.crashInstance set "i0"
  H.refreshAll set

  after <- H.instanceAssignments set
  printAssignments "After crash" after

  -- Batch 2: only i1 is alive — it should own every partition
  -- and process every record.
  let batch2 :: [(Int, Text)]
      batch2 = [(p, T.pack ("b" <> show p)) | p <- [0 .. 3]]
  mapM_
    ( \(p, v) ->
        H.send set (topicName "in") (fromIntegral p) Nothing (bytes v) ts0
    )
    batch2
  H.tickAllUntilQuiet set

  outs <-
    mapM
      (\p -> map (unbytes . MC.srValue) <$> H.readSink set (topicName "out") p)
      [0 .. 3]
  let delivered = Set.fromList (concat outs)
      expected = Set.fromList (map snd batch1 ++ map snd batch2)
  bullet ("Records observed after failover: " <> show (Set.size delivered))
  bullet
    ( "Missing: "
        <> show (L.sort (Set.toList (expected `Set.difference` delivered)))
    )

  H.closeMockSet set
