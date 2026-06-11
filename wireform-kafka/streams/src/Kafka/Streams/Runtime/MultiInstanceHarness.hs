{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.MultiInstanceHarness
Description : Pure scenario harness for multi-instance liveness simulation

Lets a test enumerate failure orderings (process crash,
partition isolation, slow GC) over a small input fixture and
verify the surviving instances still produce the same output
stream as a no-failure run.

The model is intentionally pure so Hedgehog can shrink failures.
The harness operates over a list of 'Instance's, an input stream
of records, and a sequence of 'Failure' events that punctuate
record arrivals. Each instance independently observes records
from the input, classifies them ("would have processed", "would
have skipped due to failure"), and outputs the combined join
sequence at the end.
-}
module Kafka.Streams.Runtime.MultiInstanceHarness (
  -- * Model
  InstanceId (..),
  InstanceState (..),
  Failure (..),
  Event (..),

  -- * Pure interpreter
  runHarness,
  RunResult (..),
) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)


newtype InstanceId = InstanceId {unInstanceId :: Int}
  deriving stock (Eq, Ord, Show, Generic)


data InstanceState
  = Healthy
  | Crashed
  | Isolated
  | SlowGc
  deriving stock (Eq, Show, Generic)


-- | A failure event that can be injected between input records.
data Failure
  = Crash !InstanceId
  | Recover !InstanceId
  | Isolate !InstanceId
  | UnIsolate !InstanceId
  | StallGc !InstanceId
  | ResumeGc !InstanceId
  deriving stock (Eq, Show, Generic)


{- | Either a record observed by the system (key, value) or a
failure event that fires before the next record.
-}
data Event k v
  = EvRecord !k !v
  | EvFailure !Failure
  deriving stock (Eq, Show, Generic)


-- | Outcome of running one harness scenario.
data RunResult k v = RunResult
  { rrFinalStates :: !(Map InstanceId InstanceState)
  , rrProcessed :: ![(k, v)]
  {- ^ Records the surviving instances would have processed in
  the order they arrived.
  -}
  , rrSkipped :: ![(k, v)]
  {- ^ Records that arrived during a window where every
  instance was unhealthy. With at least one healthy
  instance left, the workload is preserved.
  -}
  }
  deriving stock (Eq, Show, Generic)


{- | Interpret a scenario. Starts with every instance in 'Healthy'.
Records are processed iff at least one instance is healthy at
the time they arrive.
-}
runHarness
  :: [InstanceId]
  -> [Event k v]
  -> RunResult k v
runHarness instances events =
  let initialStates =
        Map.fromList
          [(i, Healthy) | i <- instances]
      step (states, !processed, !skipped) ev = case ev of
        EvFailure f ->
          (applyFailure f states, processed, skipped)
        EvRecord k v ->
          if any isLive (Map.elems states)
            then (states, processed ++ [(k, v)], skipped)
            else (states, processed, skipped ++ [(k, v)])
      (final, prc, skp) = foldl' step (initialStates, [], []) events
  in RunResult final prc skp


isLive :: InstanceState -> Bool
isLive = \case
  Healthy -> True
  -- A slow-GC instance is still considered live for liveness
  -- (the mark-and-sweep eventually returns); isolation / crash
  -- are not.
  SlowGc -> True
  Crashed -> False
  Isolated -> False


applyFailure
  :: Failure
  -> Map InstanceId InstanceState
  -> Map InstanceId InstanceState
applyFailure f = Map.alter (alter1 f) (target f)
  where
    target = \case
      Crash i -> i
      Recover i -> i
      Isolate i -> i
      UnIsolate i -> i
      StallGc i -> i
      ResumeGc i -> i
    alter1 g _ = case g of
      Crash {} -> Just Crashed
      Recover {} -> Just Healthy
      Isolate {} -> Just Isolated
      UnIsolate {} -> Just Healthy
      StallGc {} -> Just SlowGc
      ResumeGc {} -> Just Healthy
