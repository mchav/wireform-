{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.AtLeastOnceRedeliverySpec
Description : At-least-once delivery + induced-redelivery
              property tests

At-least-once delivery is the contract Kafka Streams falls back
to when EOS is off: every record processed by a topology
/may/ be re-fed if the consumer rewinds, but no record is ever
dropped before commit. We stress this property-style by
injecting explicit consumer rewinds while records are in flight
and asserting the multiset invariants downstream.

Properties:

  1. /No fabrication/: every output value appears in the input.
  2. /No drops/: every input value appears at least once in
     the output (the multiset of outputs is a /superset/ of
     the multiset of inputs).
  3. /Idempotent over rewind/: re-running the same workload
     with no rewind produces an output that is a permutation
     of the input (no duplicates, no losses).

The test uses 'MockStreamsDriver' in 'AtLeastOnceMode' and
induces redelivery via 'seekMC', which is exactly how the
broker-backed runtime would replay records after an
uncommitted-rebalance.
-}
module Streams.Properties.AtLeastOnceRedeliverySpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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
  topicName,
  validateTopology,
 )
import Kafka.Streams.Mock.Cluster (
  dumpPartition,
  newMockCluster,
  srValue,
 )
import Kafka.Streams.Mock.Consumer (
  seekMC,
 )
import Kafka.Streams.Mock.Fault (noFaults)
import Kafka.Streams.Mock.StreamsDriver (
  closeMockDriver,
  driverConsumer,
  externalSend,
  newMockStreamsDriver,
  runUntilQuiet,
  tickDriver,
 )
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


passthroughValid :: IO TopologyValid
passthroughValid = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

genValue :: H.Gen Text
genValue =
  -- Small alphabet so the multiset has interesting collisions
  -- (any duplicate output really is a redelivery, not a coincidence).
  Gen.element ["v0", "v1", "v2", "v3", "v4"]


----------------------------------------------------------------------
-- Property 1: no fabrication + no drops under no-rewind workload
----------------------------------------------------------------------

prop_no_rewind_is_exact :: H.Property
prop_no_rewind_is_exact = H.property $ do
  inputs <- H.forAll (Gen.list (Range.linear 1 30) genValue)
  out <- H.evalIO $ do
    cluster <- newMockCluster 1
    fp <- noFaults
    topo <- passthroughValid
    d <- newMockStreamsDriver cluster fp topo "alo-noseek" 1
    mapM_
      ( \v ->
          externalSend
            d
            (topicName "in")
            0
            Nothing
            (bytes v)
            (Timestamp 0)
      )
      inputs
    runUntilQuiet d
    rs <- dumpPartition cluster (topicName "out") 0
    closeMockDriver d
    pure (map (unbytes . srValue) rs)
  -- No rewind: output equals input. Single-partition driver and
  -- a passthrough topology preserve order; we assert the stronger
  -- ordered-equality so any regression in the mock driver shows up
  -- immediately.
  out H.=== inputs


----------------------------------------------------------------------
-- Property 2: at-least-once under induced redelivery
----------------------------------------------------------------------

prop_redelivery_is_superset :: H.Property
prop_redelivery_is_superset = H.property $ do
  inputs <-
    H.forAll
      (Gen.list (Range.linear 4 30) genValue)
  -- A rewind point in the middle of the workload.
  rewindAfter <-
    H.forAll
      (Gen.int (Range.linear 1 (length inputs - 1)))
  rewindTo <-
    H.forAll
      (Gen.int64 (Range.linear 0 (fromIntegral rewindAfter - 1)))
  outputs <- H.evalIO $ do
    cluster <- newMockCluster 1
    fp <- noFaults
    topo <- passthroughValid
    d <- newMockStreamsDriver cluster fp topo "alo-seek" 1
    let (pre, post) = splitAt rewindAfter inputs
    mapM_
      ( \v ->
          externalSend
            d
            (topicName "in")
            0
            Nothing
            (bytes v)
            (Timestamp 0)
      )
      pre
    -- Tick once to consume the pre-rewind records and emit them.
    _ <- tickDriver d
    -- Seek backwards: the consumer will re-read records starting
    -- at `rewindTo`, but the producer-side has already emitted
    -- the pre-rewind outputs.
    seekMC (driverConsumer d) (topicName "in") 0 rewindTo
    -- Push the remaining inputs.
    mapM_
      ( \v ->
          externalSend
            d
            (topicName "in")
            0
            Nothing
            (bytes v)
            (Timestamp 0)
      )
      post
    runUntilQuiet d
    rs <- dumpPartition cluster (topicName "out") 0
    closeMockDriver d
    pure (map (unbytes . srValue) rs)
  let inMS = counts inputs
      outMS = counts outputs
  H.annotate ("inputs:  " <> show inputs)
  H.annotate ("outputs: " <> show outputs)
  H.annotate ("in MS:   " <> show inMS)
  H.annotate ("out MS:  " <> show outMS)
  -- No drops: every input value appears at least as often in the
  -- output (multiset superset).
  H.assert (multisetSubsetOf inMS outMS)
  -- No fabrication: every distinct value in the output also
  -- appears in the input alphabet.
  H.assert (Set.fromList outputs `Set.isSubsetOf` Set.fromList inputs)


----------------------------------------------------------------------
-- Property 3: every redelivery cycle is bounded
----------------------------------------------------------------------

prop_redelivery_count_is_bounded :: H.Property
prop_redelivery_count_is_bounded = H.property $ do
  -- Same shape as prop_redelivery_is_superset, but additionally
  -- assert each value's output-count is at most input-count + 1.
  -- A single backwards seek can only redeliver each pre-rewind
  -- record once (the post-rewind catch-up).
  inputs <-
    H.forAll
      (Gen.list (Range.linear 4 20) genValue)
  rewindAfter <-
    H.forAll
      (Gen.int (Range.linear 1 (length inputs - 1)))
  rewindTo <-
    H.forAll
      (Gen.int64 (Range.linear 0 (fromIntegral rewindAfter - 1)))
  outputs <- H.evalIO $ do
    cluster <- newMockCluster 1
    fp <- noFaults
    topo <- passthroughValid
    d <- newMockStreamsDriver cluster fp topo "alo-bounded" 1
    let (pre, post) = splitAt rewindAfter inputs
    mapM_
      ( \v ->
          externalSend
            d
            (topicName "in")
            0
            Nothing
            (bytes v)
            (Timestamp 0)
      )
      pre
    _ <- tickDriver d
    seekMC (driverConsumer d) (topicName "in") 0 rewindTo
    mapM_
      ( \v ->
          externalSend
            d
            (topicName "in")
            0
            Nothing
            (bytes v)
            (Timestamp 0)
      )
      post
    runUntilQuiet d
    rs <- dumpPartition cluster (topicName "out") 0
    closeMockDriver d
    pure (map (unbytes . srValue) rs)
  -- For every distinct value, output-count <= input-count +
  -- (rewindAfter - rewindTo). Each potentially-redelivered
  -- pre-rewind record contributes at most one extra output on
  -- top of what was originally fed.
  let inMS = counts inputs
      outMS = counts outputs
      slack = rewindAfter - fromIntegral rewindTo
  H.annotate ("inputs:  " <> show inputs)
  H.annotate ("outputs: " <> show outputs)
  H.annotate ("slack:   " <> show slack)
  let bounded =
        all
          ( \v ->
              Map.findWithDefault 0 v outMS
                <= Map.findWithDefault 0 v inMS + slack
          )
          (Map.keys outMS)
  H.assert bounded


----------------------------------------------------------------------
-- Multiset helpers
----------------------------------------------------------------------

counts :: Ord a => [a] -> Map a Int
counts = Map.fromListWith (+) . map (\x -> (x, 1))


multisetSubsetOf :: Ord a => Map a Int -> Map a Int -> Bool
multisetSubsetOf a b =
  all (\(k, n) -> Map.findWithDefault 0 k b >= n) (Map.toAscList a)


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "AtLeastOnce redelivery" $
    sequence_
      [ it "no rewind: output multiset equals input multiset" $
          H.withTests 80 prop_no_rewind_is_exact
      , it "induced redelivery: output multiset is superset of input" $
          H.withTests 80 prop_redelivery_is_superset
      , it "redelivery count is bounded by rewind distance" $
          H.withTests 60 prop_redelivery_count_is_bounded
      ]
