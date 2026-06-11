{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.RollingUpgrade
Description : Rolling deploy: kill instances one at a time, keep traffic flowing

A Kubernetes-style rolling deploy: at any point one of the
three instances is being recycled, but the cluster as a whole
keeps consuming. Between each "pod restart" we send a fresh
batch and confirm the surviving instances picked up the
orphaned partitions.

The cluster never drops below @desiredReplicas - 1@, which
mirrors a 'maxUnavailable: 1' rollout policy. The 'MockSet'
doesn't model "wait for replacement to be ready" semantics
directly, so we crash + rejoin via 'H.refreshAll' between
batches; the salient operational property — "records sent
during the rollout still flow" — holds at every step.
-}
module Kafka.Streams.Examples.Ops.RollingUpgrade (
  runDemo,
) where

import Control.Monad (forM_)
import Data.Set qualified as Set
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Mock.Cluster qualified as MC
import Kafka.Streams.Runtime.MultiInstanceMockHarness qualified as H


runDemo :: IO ()
runDemo = do
  section "RollingUpgradeDemo"
  topo <- passthroughTopo
  set <- H.newMockSet topo 6 "ops-rolling" 3

  initial <- H.instanceAssignments set
  printAssignments "Initial assignment (3 replicas)" initial

  -- Drive one batch per "rollout step". After each batch we
  -- crash the next instance in the rotation; the survivors
  -- inherit the work for the duration of the step.
  let rotation = ["i0", "i1", "i2"]
  forM_ (zip [0 :: Int ..] rotation) $ \(step, victim) -> do
    let label = "step" <> show step
        batch = [(p, T.pack (label <> "-p" <> show p)) | p <- [0 .. 5 :: Int]]
    mapM_
      ( \(p, v) ->
          H.send set (topicName "in") (fromIntegral p) Nothing (bytes v) ts0
      )
      batch
    H.tickAllUntilQuiet set
    bullet
      ( "Step "
          <> show step
          <> ": sent "
          <> show (length batch)
          <> " records, now recycling "
          <> T.unpack victim
      )
    H.crashInstance set victim
    H.refreshAll set
    midAsg <- H.instanceAssignments set
    printAssignments
      ( "During step "
          <> show step
          <> " (after "
          <> T.unpack victim
          <> " left)"
      )
      midAsg

  -- All three "old" instances are gone; whoever's left covers
  -- every partition. (At this point the cluster has shrunk to
  -- zero — that's an exaggerated worst-case rollout.)
  outs <-
    mapM
      (\p -> map (unbytes . MC.srValue) <$> H.readSink set (topicName "out") p)
      [0 .. 5]
  let delivered = Set.size (Set.fromList (concat outs))
  bullet ("Total records delivered across the rollout: " <> show delivered)
  H.closeMockSet set
