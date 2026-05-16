{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.TopologyFreeSpec
-- Description : Tests for the GADT-based topology builder
--
-- Exercises the deep-embedded 'Kafka.Streams.Topology.Free.Topology'
-- end-to-end against the in-process driver. Each test builds a
-- topology as a /pure value/ and only at the boundary does it
-- compile + drive the topology — proving the new front-end can
-- express the same topologies the imperative builder can, and
-- behaves identically when interpreted.
module Streams.TopologyFreeSpec (tests) where

import Control.Arrow ((***), (&&&), (>>>))
import qualified Control.Category as Cat
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology.Free as F

tests :: TestTree
tests = testGroup "Topology.Free (GADT topology builder)"
  [ test_source_sink_passthrough
  , test_chain_of_stateless_transforms
  , test_fanout_two_sinks
  , test_parallel_two_independent_pipelines
  , test_stream_table_join
  , test_groupby_count
  , test_inspect_records_constructors
  , test_category_id_left_right_identity
  ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t0 :: Timestamp
t0 = Timestamp 0

----------------------------------------------------------------------
-- 1. Source >>> Sink: the smallest closed topology
----------------------------------------------------------------------

test_source_sink_passthrough :: TestTree
test_source_sink_passthrough =
  testCase "source >>> sink passes records through unchanged" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.sink "out" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-passthrough"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v2") t0 0

    out <- readOutput driver (topicName "out")
    map (fmap unbytes . crKey) out @?= [Just "k1", Just "k2"]
    map (unbytes . crValue) out    @?= ["v1", "v2"]
    closeDriver driver

----------------------------------------------------------------------
-- 2. Long chain of stateless transforms
----------------------------------------------------------------------

test_chain_of_stateless_transforms :: TestTree
test_chain_of_stateless_transforms =
  testCase "chain of mapValues / filter / flatMapValues works" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.strip
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.flatMapValues T.words
            >>> F.mapValues T.toUpper
            >>> F.sink "out" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-stateless-chain"

    pipeInput driver (topicName "in") Nothing (bytes "  hello world  ") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "")               t0 0
    pipeInput driver (topicName "in") Nothing (bytes "kafka streams")   t0 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["HELLO", "WORLD", "KAFKA", "STREAMS"]
    closeDriver driver

----------------------------------------------------------------------
-- 3. Fanout: one source, two parallel sinks
----------------------------------------------------------------------

test_fanout_two_sinks :: TestTree
test_fanout_two_sinks =
  testCase "Fanout (&&&) routes the same stream to two sinks" $ do
    -- source >>> (upper-sink &&& lower-sink) >>> drop-pair
    let upper :: F.Topology (KStream Text Text) ()
        upper = F.mapValues T.toUpper
            >>> F.sink "upper" textSerde textSerde

        lower :: F.Topology (KStream Text Text) ()
        lower = F.mapValues T.toLower
            >>> F.sink "lower" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> (upper &&& lower)
            >>> F.liftIO_ "drop-pair" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-fanout"

    pipeInput driver (topicName "in") Nothing (bytes "Mixed") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "Case")  t0 0

    upperOut <- readOutput driver (topicName "upper")
    lowerOut <- readOutput driver (topicName "lower")
    map (unbytes . crValue) upperOut @?= ["MIXED", "CASE"]
    map (unbytes . crValue) lowerOut @?= ["mixed", "case"]
    closeDriver driver

----------------------------------------------------------------------
-- 4. Parallel: two independent void-input pipelines
----------------------------------------------------------------------

test_parallel_two_independent_pipelines :: TestTree
test_parallel_two_independent_pipelines =
  testCase "two void-input pipelines combine via Fanout (&&&)" $ do
    -- Two completely independent closed topologies fused into one.
    -- 'Fanout' delivers Void to both subgraphs; each subgraph just
    -- ignores it (Source primitives don't pattern-match on Void),
    -- so we get one composite graph with two independent lineages
    -- — exactly the "parallel" semantics from the user's GADT
    -- sketch.
    let leftHalf :: F.Topology Void ()
        leftHalf =
          F.source "left-in" textSerde textSerde
            >>> F.mapValues (T.append "L:")
            >>> F.sink "left-out" textSerde textSerde

        rightHalf :: F.Topology Void ()
        rightHalf =
          F.source "right-in" textSerde textSerde
            >>> F.mapValues (T.append "R:")
            >>> F.sink "right-out" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          (leftHalf &&& rightHalf)
            >>> F.liftIO_ "drop-pair" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-parallel"

    pipeInput driver (topicName "left-in")  Nothing (bytes "1") t0 0
    pipeInput driver (topicName "right-in") Nothing (bytes "2") t0 0
    pipeInput driver (topicName "left-in")  Nothing (bytes "3") t0 0

    lOut <- readOutput driver (topicName "left-out")
    rOut <- readOutput driver (topicName "right-out")
    map (unbytes . crValue) lOut @?= ["L:1", "L:3"]
    map (unbytes . crValue) rOut @?= ["R:2"]
    closeDriver driver

----------------------------------------------------------------------
-- 5. Stream-table join via the tuple input shape
----------------------------------------------------------------------

test_stream_table_join :: TestTree
test_stream_table_join =
  testCase "StreamTableJoin pairs records with the latest table value" $ do
    let streamSide :: F.Topology Void (KStream Text Text)
        streamSide = F.source "stream-in" textSerde textSerde

        tableSide :: F.Topology Void (KTable Text Text)
        tableSide = F.tableSource "table-in" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          (streamSide &&& tableSide)
            >>> F.streamTableJoin
                  (\v vt -> v <> "|" <> vt)
                  (joined textSerde textSerde textSerde)
            >>> F.sink "joined-out" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-streamtable-join"

    pipeInput driver (topicName "table-in") (Just (bytes "k1")) (bytes "T1") t0 0
    pipeInput driver (topicName "table-in") (Just (bytes "k2")) (bytes "T2") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k1")) (bytes "S1") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k2")) (bytes "S2") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k3")) (bytes "miss") t0 0

    out <- readOutput driver (topicName "joined-out")
    map (unbytes . crValue) out @?= ["S1|T1", "S2|T2"]
    closeDriver driver

----------------------------------------------------------------------
-- 6. groupBy + count materialised into a KTable
----------------------------------------------------------------------

test_groupby_count :: TestTree
test_groupby_count =
  testCase "groupByKey >>> count materialises counts per key" $ do
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-count-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source "in" textSerde textSerde
            >>> F.groupByKey (grouped textSerde textSerde)
            >>> F.count countMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-count"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just 3)
        kvsGet kvs "b" >>= (@?= Just 1)
      Nothing -> error "free-count: count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 7. AST introspection: 'inspect' records constructor names
----------------------------------------------------------------------

test_inspect_records_constructors :: TestTree
test_inspect_records_constructors =
  testCase "inspect produces a constructor listing for static analysis" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.sink "out" textSerde textSerde

        ops = F.inspect topology

    -- The exact internal labels are subject to change, but the major
    -- operations must all show up so users can grep their topologies.
    assertBool "expected Source label" $
      any ("Source" `T.isPrefixOf`) ops
    assertBool "expected MapValues label" $
      "MapValues" `elem` ops
    assertBool "expected Filter label" $
      "Filter" `elem` ops
    assertBool "expected Sink label" $
      any ("Sink" `T.isPrefixOf`) ops

----------------------------------------------------------------------
-- 8. Category laws (identity)
----------------------------------------------------------------------

test_category_id_left_right_identity :: TestTree
test_category_id_left_right_identity =
  testCase "'Cat.id . t' and 't . Cat.id' both build identical topologies" $ do
    -- We don't have full equational equality on the GADT (functions
    -- aren't comparable), but the imperative result of compiling
    -- (left-id) / (right-id) should produce the same graph node
    -- ordering as the bare topology, since the 'Category' instance
    -- collapses identity on either side.
    let base :: F.Topology Void ()
        base =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.toUpper
            >>> F.sink "out" textSerde textSerde

        leftId  = Cat.id Cat.. base
        rightId = base Cat.. Cat.id

        ops1 = F.inspect base
        ops2 = F.inspect leftId
        ops3 = F.inspect rightId

    ops1 @?= ops2
    ops1 @?= ops3
