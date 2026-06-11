{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Kafka.Streams.Runtime.RevocationGrace
Description : KIP-869 high-availability task revocation policy

When the consumer-group rebalance revokes a partition, today's
runtime drops the matching task immediately. KIP-869 keeps the
task running as a /read-only standby/ for a configurable grace
window so the new owner can warm up its state from the changelog
without serving stale reads from the previous host.

This module is the pure decision layer: tests exercise the
classification ('classifyRevocation') without spinning up the
runtime, and the engine driver invokes 'planRevocation' when the
consumer rebalance listener fires.
-}
module Kafka.Streams.Runtime.RevocationGrace (
  RevocationOutcome (..),
  RevocationPlan (..),
  classifyRevocation,
  planRevocation,
) where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Kafka.Streams.Processor (TaskId)


-- | The shape of a single revocation decision.
data RevocationOutcome
  = {- | Drop the task immediately. The legacy behaviour and the
    only correct option when no grace window is configured.
    -}
    RevokeImmediate
  | {- | Keep the task running as a read-only standby until the
    given absolute deadline (epoch ms). Once the deadline
    elapses without a re-promotion, the engine drops the
    task.
    -}
    KeepAsStandby !Int64
  deriving stock (Eq, Show, Generic)


data RevocationPlan = RevocationPlan
  { rpTask :: !TaskId
  , rpOutcome :: !RevocationOutcome
  }
  deriving stock (Eq, Show, Generic)


{- | Pure classification of a single revocation. The grace window
comes from the @task.timeout.ms@-style config knob; @0@ disables
the grace and yields 'RevokeImmediate'.
-}
classifyRevocation
  :: Int64
  -- ^ now (ms)
  -> Int
  -- ^ grace window (ms; 0 = no grace)
  -> RevocationOutcome
classifyRevocation now graceMs
  | graceMs <= 0 = RevokeImmediate
  | otherwise = KeepAsStandby (now + fromIntegral graceMs)


planRevocation
  :: Int64
  -- ^ now (ms)
  -> Int
  -- ^ grace window (ms)
  -> [TaskId]
  -> [RevocationPlan]
planRevocation now graceMs =
  map (\t -> RevocationPlan t (classifyRevocation now graceMs))
