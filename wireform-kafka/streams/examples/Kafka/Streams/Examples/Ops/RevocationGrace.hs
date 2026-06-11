{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.RevocationGrace
Description : KIP-869 soft-revocation grace window decisions

Pure-policy demo: there's no engine, no broker, just the
'Kafka.Streams.Runtime.RevocationGrace' module that the engine
driver consults whenever the consumer group coordinator tells
us to give a task back.

This is the operational lever that controls how aggressively
the runtime hands back partitions during a rebalance. With
@grace = 0@ (the classic behaviour) revoked tasks are dropped
immediately; with @grace > 0@ we keep them as read-only
standbys until the deadline expires — useful when the
coordinator flaps and the next assignment hands the same task
back to us anyway.
-}
module Kafka.Streams.Examples.Ops.RevocationGrace (
  runDemo,
) where

import Kafka.Streams.Examples.Ops.Helpers (bullet, section)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.RevocationGrace qualified as RG


runDemo :: IO ()
runDemo = do
  section "RevocationGraceDemo"

  let revoked = [TaskId 0 0, TaskId 0 1, TaskId 1 0]
      now = 1_000_000 :: Int

  bullet "Policy: grace = 0 (legacy / immediate)"
  printPlan (RG.planRevocation (fromIntegral now) 0 revoked)

  bullet "Policy: grace = 30s (soft revocation)"
  printPlan (RG.planRevocation (fromIntegral now) 30_000 revoked)

  bullet "Policy: grace = 5m (long grace; coordinator-flap tolerant)"
  printPlan (RG.planRevocation (fromIntegral now) 300_000 revoked)
  where
    printPlan =
      mapM_
        ( \(RG.RevocationPlan t outcome) ->
            bullet ("    " <> show t <> " -> " <> show outcome)
        )
