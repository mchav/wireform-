{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end multi-instance rebalance verification.
--
-- Spins up N 'Kafka.Streams.Mock.StreamsDriver.MockStreamsDriver'
-- against a shared 'Kafka.Streams.Mock.Cluster.MockCluster' via
-- 'Kafka.Streams.Runtime.MultiInstanceMockHarness.MockSet', then:
--
--   1. Asserts that the union of assignments covers every
--      partition of every subscribed source topic exactly once
--      (the assignor's correctness invariant).
--   2. Drives a few records through the harness and asserts the
--      sink topic carries every record exactly once
--      (the runtime's correctness invariant).
--   3. Crashes one instance, refreshes the survivors, and
--      verifies they pick up the orphaned partitions and keep
--      processing new records.
--   4. Hedgehog property: for any topology + partition-count +
--      instance-count, the union of post-rebalance assignments
--      always equals the full set of partitions.
module Streams.MultiInstanceRebalanceSpec (tests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Imperative
import qualified Kafka.Streams.Mock.Cluster as MC
import qualified Kafka.Streams.Runtime.MultiInstanceMockHarness as H

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

ts0 :: Timestamp
ts0 = Timestamp 0

tests :: Spec
tests = describe "Multi-instance rebalance (mock cluster harness)" $ sequence_
  [ assignments_cover_every_partition
  , records_route_to_partition_owner
  , surviving_instance_inherits_orphans
  , prop_assignments_cover_and_disjoint
  ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Passthrough topology. Used everywhere — the multi-instance
-- properties under test are about /assignment/ and /routing/,
-- not about processor semantics.
passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

----------------------------------------------------------------------
-- Test cases
----------------------------------------------------------------------

assignments_cover_every_partition :: Spec
assignments_cover_every_partition =
  it
    "two instances split a 4-partition input — union covers every partition exactly once"
    $ do
        topo <- passthroughTopo
        set  <- H.newMockSet topo 4 "shared" 2

        asg <- H.instanceAssignments set
        -- Both instances are subscribed.
        map fst asg `shouldBe` ["i0", "i1"]
        -- The union spans every partition; the intersection is
        -- empty.
        let union = Set.fromList (concatMap snd asg)
            expected = Set.fromList
              [(topicName "in", p) | p <- [0 .. 3]]
        union `shouldBe` expected
        let totalLength = sum (map (length . snd) asg)
        totalLength `shouldBe` Set.size expected
        H.closeMockSet set

records_route_to_partition_owner :: Spec
records_route_to_partition_owner =
  it "records sent to each input partition show up in the sink, end-to-end" $ do
    topo <- passthroughTopo
    set  <- H.newMockSet topo 4 "shared" 2

    -- Seed each input partition with a distinct value.
    let pairs = [(0, "p0"), (1, "p1"), (2, "p2"), (3, "p3")]
    mapM_
      (\(p, v) ->
         H.send set (topicName "in") p Nothing (bytes v) ts0)
      pairs
    H.tickAllUntilQuiet set

    -- Without a key, the driver hashes 'Nothing' to partition 0
    -- of the output topic, so every output value lands there.
    out0 <- map (unbytes . MC.srValue) <$> H.readSink set (topicName "out") 0
    L.sort out0 `shouldBe` L.sort [v | (_, v) <- pairs]

    H.closeMockSet set

surviving_instance_inherits_orphans :: Spec
surviving_instance_inherits_orphans =
  it "crashing one instance migrates its partitions to the survivor" $ do
    topo <- passthroughTopo
    set  <- H.newMockSet topo 4 "shared" 2

    -- Baseline: each instance has 2 of 4 partitions.
    before <- H.instanceAssignments set
    map (length . snd) before `shouldBe` [2, 2]

    -- Crash i0. The mock cluster evicts its membership; refresh
    -- every survivor so the assignor redeals.
    H.crashInstance set "i0"
    H.refreshAll set

    after <- H.instanceAssignments set
    map fst after `shouldBe` ["i1"]
    -- The lone survivor now owns every partition.
    Set.fromList (concatMap snd after)
      `shouldBe` Set.fromList [(topicName "in", p) | p <- [0 .. 3]]

    -- And it still processes records sent post-rebalance.
    mapM_
      (\(p, v) ->
         H.send set (topicName "in") p Nothing (bytes v) ts0)
      [(0, "x0"), (1, "x1"), (2, "x2"), (3, "x3")]
    H.tickAllUntilQuiet set

    out0 <- map (unbytes . MC.srValue) <$> H.readSink set (topicName "out") 0
    Set.fromList out0
      `assertSupersetOf` Set.fromList ["x0", "x1", "x2", "x3"]

    H.closeMockSet set
  where
    assertSupersetOf got expect =
      (if (expect `Set.isSubsetOf` got) then pure () else expectationFailure ("expected " <> show expect <> " ⊆ " <> show got))

prop_assignments_cover_and_disjoint :: Spec
prop_assignments_cover_and_disjoint =
  it
    "assignor partitions cover & are disjoint for every (instances, parts) shape"
    $ property $ do
        parts     <- forAll (Gen.int (Range.linear 1 8))
        instances <- forAll (Gen.int (Range.linear 1 4))
        let appId = T.pack ("prop-" <> show parts <> "-" <> show instances)
        topo <- liftIO passthroughTopo
        set  <- liftIO (H.newMockSet topo parts appId instances)
        asg  <- liftIO (H.instanceAssignments set)
        let union  = Set.fromList (concatMap snd asg)
            sizes  = map (Set.fromList . snd) asg
            disjoint = foldr Set.union Set.empty sizes
            sizesSum = sum (map length sizes)
        -- Coverage.
        union === Set.fromList [(topicName "in", fromIntegral p)
                               | p <- [0 .. parts - 1]]
        -- Disjointness: total length equals union size.
        sizesSum === Set.size disjoint
        liftIO (H.closeMockSet set)
