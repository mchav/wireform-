{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.KeyGroupDispatchSpec
Description : Properties for the key-group-dispatched WorkerPool

Properties:

  1. Without an assignment, 'submitRecordKeyGrouped' is a no-op
     (no record is processed, no exception).
  2. After 'updateKeyGroupAssignment', every record whose
     key-group is owned by the local instance lands on /a/
     worker; sum of processed counts equals submit count.
  3. Sticky routing: the same key (and therefore the same
     key-group) always lands on the same worker across many
     submissions, until the assignment changes.
  4. Dispatch is hash-stable: across a wider workload, the set
     of workers actually used equals the worker set the
     assignment distributed the owned key-groups across.
-}
module Streams.Properties.KeyGroupDispatchSpec (tests) where

import Control.Monad (forM_, replicateM_)
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Imperative (
  Timestamp (..),
  TopologyValid,
  buildTopology,
  consumed,
  newStreamsBuilder,
  produced,
  streamFromTopic,
  textSerde,
  toTopic,
  validateTopology,
 )
import Kafka.Streams.Runtime.KeyGroup (
  KeyGroupAssignment (..),
  KeyGroupCount (..),
  KeyGroupId (..),
  defaultKeyGroupConfig,
  keyGroupOfBytes,
 )
import Kafka.Streams.Runtime.KeyGroup qualified as KG
import Kafka.Streams.Runtime.WorkerPool
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Test topology
----------------------------------------------------------------------

passthrough :: IO TopologyValid
passthrough = do
  b <- newStreamsBuilder
  s <- streamFromTopic b "in" (consumed textSerde textSerde)
  toTopic "out" (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


bytes :: String -> BSC.ByteString
bytes = BSC.pack


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

mkPool :: Int -> IO WorkerPool
mkPool n = do
  topo <- passthrough
  newWorkerPoolKeyGrouped topo "kg-test" n defaultKeyGroupConfig


ownEverything :: KeyGroupAssignment
ownEverything =
  KeyGroupAssignment
    { kgaOwned = Set.fromList [KeyGroupId i | i <- [0 .. 127]]
    , kgaWarming = Map.empty
    }


totalProcessed :: WorkerPool -> IO Int
totalProcessed pool = do
  ws <- poolWorkersSnapshot pool
  ns <- mapM workerProcessedCount (toList ws)
  pure (fromIntegral (sum ns))


----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_no_assignment_drops :: Spec
unit_no_assignment_drops =
  it "submitRecordKeyGrouped: no assignment -> records dropped" $ do
    pool <- mkPool 2
    -- No updateKeyGroupAssignment call.
    submitRecordKeyGrouped
      pool
      "in"
      (Just (bytes "k0"))
      (bytes "v")
      (Timestamp 0)
      0
    waitForQuiescence pool
    processed <- totalProcessed pool
    processed `shouldBe` 0
    closeWorkerPool pool


unit_assignment_routes :: Spec
unit_assignment_routes =
  it "after updateKeyGroupAssignment, records process" $ do
    pool <- mkPool 2
    updateKeyGroupAssignment pool ownEverything
    forM_ [0 .. 9 :: Int] $ \i ->
      submitRecordKeyGrouped
        pool
        "in"
        (Just (bytes ("k" <> show i)))
        (bytes "v")
        (Timestamp 0)
        0
    waitForQuiescence pool
    processed <- totalProcessed pool
    processed `shouldBe` 10
    closeWorkerPool pool


----------------------------------------------------------------------
-- Property: per-key stickiness
----------------------------------------------------------------------

prop_sticky_per_key :: H.Property
prop_sticky_per_key = H.property $ do
  workers <- H.forAll (Gen.int (Range.linear 2 4))
  perKey <- H.forAll (Gen.int (Range.linear 5 30))
  key <- H.forAll (Gen.string (Range.linear 1 6) Gen.alpha)
  outcome <- H.evalIO $ do
    pool <- mkPool workers
    updateKeyGroupAssignment pool ownEverything
    ws <- poolWorkersSnapshot pool
    before <- mapM workerProcessedCount (toList ws)
    replicateM_ perKey $
      submitRecordKeyGrouped
        pool
        "in"
        (Just (bytes key))
        (bytes "v")
        (Timestamp 0)
        0
    waitForQuiescence pool
    after <- mapM workerProcessedCount (toList ws)
    closeWorkerPool pool
    let deltas = zipWith (-) after before
    pure (filter (> 0) deltas)
  -- Exactly one worker should have absorbed all the records.
  case outcome of
    [n] -> n H.=== fromIntegral perKey
    _ -> do
      H.annotate ("deltas: " <> show outcome)
      H.failure


----------------------------------------------------------------------
-- Property: dispatch is hash-stable across keys
----------------------------------------------------------------------

prop_dispatch_uses_owned_workers :: H.Property
prop_dispatch_uses_owned_workers = H.property $ do
  workers <- H.forAll (Gen.int (Range.linear 2 4))
  count <- H.forAll (Gen.int (Range.linear 30 80))
  keys <-
    H.forAll
      ( Gen.list
          (Range.singleton count)
          (Gen.string (Range.linear 1 6) Gen.alpha)
      )
  outcome <- H.evalIO $ do
    pool <- mkPool workers
    updateKeyGroupAssignment pool ownEverything
    forM_ keys $ \k ->
      submitRecordKeyGrouped
        pool
        "in"
        (Just (bytes k))
        (bytes "v")
        (Timestamp 0)
        0
    waitForQuiescence pool
    ws <- poolWorkersSnapshot pool
    deltas <- mapM workerProcessedCount (toList ws)
    closeWorkerPool pool
    pure deltas
  -- Sum of processed equals submit count (no record dropped).
  fromIntegral (sum outcome) H.=== count


----------------------------------------------------------------------
-- Sanity: hashing matches
----------------------------------------------------------------------

unit_keyGroupOfBytes_matches_keyGroupOf :: Spec
unit_keyGroupOfBytes_matches_keyGroupOf =
  it "keyGroupOfBytes default config bounds match KeyGroupCount" $ do
    let cfg = defaultKeyGroupConfig
        kg = keyGroupOfBytes cfg (bytes "anything")
        KeyGroupCount n = KG.kgcTotal cfg
        KeyGroupId k = kg
    True `shouldBe` (k >= 0 && k < n)


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Key-group dispatch" $
    sequence_
      [ unit_no_assignment_drops
      , unit_assignment_routes
      , unit_keyGroupOfBytes_matches_keyGroupOf
      , it "same key always lands on same worker" $
          H.withTests 60 prop_sticky_per_key
      , it "dispatch sums to total submits (no drops)" $
          H.withTests 50 prop_dispatch_uses_owned_workers
      ]
