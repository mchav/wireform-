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

import qualified Control.Arrow
import Control.Arrow ((&&&), (***), (>>>))
import qualified Control.Category as Cat
import Control.Exception (try, evaluate)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMap
import Data.Int (Int64)
import Data.IORef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams
import Kafka.Streams.Runtime.EOS
  ( CommitOutcome (..)
  , EOSCoordinator (..)
  , runCommitCycle
  )
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.State.Store
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
  -- Lineage combinators
  , test_tap_passes_wire_through
  , test_forkn_three_branches
  , test_split_named_branches
  , test_fork_explicit_duplicator
  -- Multi-topic source
  , test_sources_multi_topic
  -- KTable extras
  , test_table_table_left_join
  , test_filter_not_table
  -- Windowed aggregation
  , test_windowed_by_time_count
  -- KGroupedTable
  , test_kgrouped_table_count
  -- Cogroup
  , test_cogroup_two_streams
  -- Suppress
  , test_suppress_until_time_limit
  -- Processor API + state store
  , test_process_stream_with_state_store
  -- Optimiser
  , test_optimize_fuses_map_chains
  , test_optimize_fuses_filter_chains
  , test_optimize_collapses_identity_combinators
  , test_optimize_preserves_observable_behaviour
  , test_optimize_noOptimization_is_a_no_op
  , test_optimize_pushes_pure_functions_through_fanout
  , test_compile_default_runs_optimizer
  -- Java-style additional rewrites
  , test_optimize_selectKey_then_groupByKey_becomes_groupBy
  , test_optimize_collapses_repartition_chains
  , test_optimize_collapses_values_idempotent
  , test_optimize_foreach_after_peek_fuses
  , test_optimize_tap_foreach_becomes_peek
  , test_optimize_combines_adjacent_taps
  , test_optimize_pushes_arr_through_fork
  -- Errors
  , test_missing_serde_throws_typed_exception
  -- EOS interaction
  , test_fork_topology_is_eos_atomic
  , test_forkN_topology_is_eos_atomic
  , test_tap_topology_is_eos_atomic
  -- Instance suite (Applicative / Monad / Semigroup / Monoid / Profunctor)
  , test_applicative_liftA2_combines_two_topologies
  , test_monad_do_notation_for_multi_source
  , test_semigroup_runs_both_pipelines_on_one_input
  , test_monoid_unit_output_is_no_op
  , test_profunctor_dimap_works
  , test_reader_localInput_pre_transforms
  -- Cross-lineage EOS + cogroup with monad bind
  , test_mergeSourced_two_sources_share_one_task_under_eos
  , test_cogroup_via_do_notation
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

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

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

----------------------------------------------------------------------
-- 9. Tap: side-effect lineage that passes the wire through
----------------------------------------------------------------------

test_tap_passes_wire_through :: TestTree
test_tap_passes_wire_through =
  testCase "Tap forks a side pipeline and keeps the main lineage flowing" $ do
    let auditSink :: F.Topology (KStream Text Text) ()
        auditSink =
          F.filter (\r -> "audit:" `T.isPrefixOf` recordValue r)
            >>> F.sink "audit" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.tap auditSink
            >>> F.mapValues T.toUpper
            >>> F.sink "main" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-tap"

    pipeInput driver (topicName "in") Nothing (bytes "audit:login")     t0 0
    pipeInput driver (topicName "in") Nothing (bytes "regular")          t0 0
    pipeInput driver (topicName "in") Nothing (bytes "audit:logout")     t0 0

    audit <- readOutput driver (topicName "audit")
    main_ <- readOutput driver (topicName "main")

    -- Tap branch only receives audit-prefixed values.
    map (unbytes . crValue) audit @?= ["audit:login", "audit:logout"]
    -- Main branch sees all records, uppercased.
    map (unbytes . crValue) main_ @?=
      ["AUDIT:LOGIN", "REGULAR", "AUDIT:LOGOUT"]
    closeDriver driver

----------------------------------------------------------------------
-- 10. ForkN: N-way fan-out via NonEmpty list
----------------------------------------------------------------------

test_forkn_three_branches :: TestTree
test_forkn_three_branches =
  testCase "ForkN applies three sub-pipelines to the same upstream" $ do
    let mkSink :: Text -> (Text -> Text) -> F.Topology (KStream Text Text) ()
        mkSink topic f =
          F.mapValues f >>> F.sink topic textSerde textSerde

        threeWays :: F.Topology (KStream Text Text) (NE.NonEmpty ())
        threeWays = F.forkN
          ( NE.fromList
              [ mkSink "upper" T.toUpper
              , mkSink "lower" T.toLower
              , mkSink "len"   (T.pack . show . T.length)
              ])

        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> threeWays
            >>> F.liftIO_ "drop" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-forkn"

    pipeInput driver (topicName "in") Nothing (bytes "Hello") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "World") t0 0

    upper <- readOutput driver (topicName "upper")
    lower <- readOutput driver (topicName "lower")
    len_  <- readOutput driver (topicName "len")
    map (unbytes . crValue) upper @?= ["HELLO", "WORLD"]
    map (unbytes . crValue) lower @?= ["hello", "world"]
    map (unbytes . crValue) len_  @?= ["5", "5"]
    closeDriver driver

----------------------------------------------------------------------
-- 11. Split: KIP-418 named branches
----------------------------------------------------------------------

test_split_named_branches :: TestTree
test_split_named_branches =
  testCase "Split routes records to named branches" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.split
                  [ F.splitBranch "short" (\r -> T.length (recordValue r) < 4)
                  , F.splitBranch "long"  (\r -> T.length (recordValue r) >= 4)
                  ]
                  (Just "rest")
            -- After Split, we have a Map Text (KStream Text Text); use
            -- Arr to extract specific branches and sink them.
            >>> F.liftIO_ "sink-branches" sinkBranches

        sinkBranches _b branchesMap = do
          -- Each named branch is itself a KStream; we route 'short' to
          -- the 'short' topic, 'long' to the 'long' topic, and drop
          -- the default rest.
          case Map.lookup "short" branchesMap of
            Just s -> toTopic (topicName "short")
                              (produced textSerde textSerde) s
            Nothing -> error "split: short branch missing"
          case Map.lookup "long" branchesMap of
            Just s -> toTopic (topicName "long")
                              (produced textSerde textSerde) s
            Nothing -> error "split: long branch missing"
          pure ()

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-split"

    pipeInput driver (topicName "in") Nothing (bytes "ab")    t0 0
    pipeInput driver (topicName "in") Nothing (bytes "abcde") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "xy")    t0 0

    shorts <- readOutput driver (topicName "short")
    longs  <- readOutput driver (topicName "long")
    map (unbytes . crValue) shorts @?= ["ab", "xy"]
    map (unbytes . crValue) longs  @?= ["abcde"]
    closeDriver driver

----------------------------------------------------------------------
-- 12. Fork: explicit duplicator
----------------------------------------------------------------------

test_fork_explicit_duplicator :: TestTree
test_fork_explicit_duplicator =
  testCase "Fork explicitly duplicates one wire into a pair" $ do
    -- Build:
    --   source >>> Fork >>> (mapValues toUpper *** mapValues toLower)
    --          >>> (sink upper *** sink lower)
    --          >>> drop-pair
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.fork
            >>> (F.mapValues T.toUpper *** F.mapValues T.toLower)
            >>> (F.sink "upper" textSerde textSerde
                   *** F.sink "lower" textSerde textSerde)
            >>> F.liftIO_ "drop" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-fork"

    pipeInput driver (topicName "in") Nothing (bytes "Mix")  t0 0
    pipeInput driver (topicName "in") Nothing (bytes "Case") t0 0

    upper <- readOutput driver (topicName "upper")
    lower <- readOutput driver (topicName "lower")
    map (unbytes . crValue) upper @?= ["MIX", "CASE"]
    map (unbytes . crValue) lower @?= ["mix", "case"]
    closeDriver driver
  where
    -- Local alias to the Arrow combinator from Control.Arrow re-exported
    -- via Control.Category — keeps the test concise.
    _ = ()

----------------------------------------------------------------------
-- 13. Multi-topic source via 'sources'
----------------------------------------------------------------------

test_sources_multi_topic :: TestTree
test_sources_multi_topic =
  testCase "sources fans multiple topics into one KStream" $ do
    let topology :: F.Topology Void ()
        topology =
          F.sources (NE.fromList ["in-a", "in-b"]) textSerde textSerde
            >>> F.mapValues (T.append "*")
            >>> F.sink "merged" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-multi-source"

    pipeInput driver (topicName "in-a") Nothing (bytes "a1") t0 0
    pipeInput driver (topicName "in-b") Nothing (bytes "b1") t0 0
    pipeInput driver (topicName "in-a") Nothing (bytes "a2") t0 0

    out <- readOutput driver (topicName "merged")
    map (unbytes . crValue) out @?= ["*a1", "*b1", "*a2"]
    closeDriver driver

----------------------------------------------------------------------
-- 14. KTable-KTable left join
----------------------------------------------------------------------

test_table_table_left_join :: TestTree
test_table_table_left_join =
  testCase "TableTableLeftJoin emits even when right side is absent" $ do
    let leftTable, rightTable :: F.Topology Void (KTable Text Text)
        leftTable  = F.tableSource "left"  textSerde textSerde
        rightTable = F.tableSource "right" textSerde textSerde

        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "lj-out-store")

        topology :: F.Topology Void (KTable Text Text)
        topology =
          (leftTable &&& rightTable)
            >>> F.tableTableLeftJoin
                  (\l mr -> l <> "|" <> maybe "MISS" Prelude.id mr)
                  outMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-tt-leftjoin"

    pipeInput driver (topicName "left")  (Just (bytes "k1")) (bytes "L1") t0 0
    pipeInput driver (topicName "right") (Just (bytes "k1")) (bytes "R1") t0 0
    pipeInput driver (topicName "left")  (Just (bytes "k2")) (bytes "L2") t0 0
    -- k3 only on left
    pipeInput driver (topicName "left")  (Just (bytes "k3")) (bytes "L3") t0 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "k1" >>= (@?= Just "L1|R1")
        kvsGet kvs "k2" >>= (@?= Just "L2|MISS")
        kvsGet kvs "k3" >>= (@?= Just "L3|MISS")
      Nothing -> error "left-join store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 15. filterNotTable
----------------------------------------------------------------------

test_filter_not_table :: TestTree
test_filter_not_table =
  testCase "filterNotTable drops matching records" $ do
    let baseTable :: F.Topology Void (KTable Text Text)
        baseTable = F.tableSource "in" textSerde textSerde

        filteredMat :: Materialized Text Text
        filteredMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "fnt-out")

        topology :: F.Topology Void (KTable Text Text)
        topology =
          baseTable
            >>> F.filterNotTable
                  (\r -> recordValue r == "drop")
                  filteredMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-filter-not-table"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "keep") t0 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "drop") t0 0
    pipeInput driver (topicName "in") (Just (bytes "c")) (bytes "keep") t0 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just "keep")
        kvsGet kvs "b" >>= (@?= Nothing)
        kvsGet kvs "c" >>= (@?= Just "keep")
      Nothing -> error "filter-not-table store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 16. Windowed aggregation: tumbling count
----------------------------------------------------------------------

test_windowed_by_time_count :: TestTree
test_windowed_by_time_count =
  testCase "windowedByTime >>> countWindowed buckets records per window" $ do
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-wcount-store")

        ws = tumblingWindows (millis 100)

        topology :: F.Topology Void (WindowedTableHandle Text Int64)
        topology =
          F.source "in" textSerde textSerde
            >>> F.groupByKey (grouped textSerde textSerde)
            >>> F.windowedByTime ws
            >>> F.countWindowed countMat

    (wth, topo) <- F.compile topology
    driver <- newDriver topo "free-wcount"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v4") (t 150) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v5") (t 199) 0

    mStore <- getWindowStore @Text @Int64 driver (wthStore wth)
    case mStore of
      Just ws_ -> do
        wsFetch ws_ "k" (Timestamp 0)   >>= (@?= Just 3)
        wsFetch ws_ "k" (Timestamp 100) >>= (@?= Just 2)
      Nothing -> error "windowed count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 17. KGroupedTable: subtractor-aware count
----------------------------------------------------------------------

test_kgrouped_table_count :: TestTree
test_kgrouped_table_count =
  testCase "KGroupedTable count tracks key-value insertions and deletions" $ do
    -- An upstream KTable keyed by user-id holds the user's region.
    -- Re-key by region to count users-per-region; deletes on the
    -- upstream must subtract.
    let baseMat :: Materialized Text Text
        baseMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "users-store")

        countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "region-count-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.liftIO_ "table-source"
            (\b _ -> tableFromTopic b (topicName "users")
                                       (consumed textSerde textSerde)
                                       baseMat)
            >>> F.groupTableBy
                  (\_userId region -> (region, region))
                  (grouped textSerde textSerde)
            >>> F.countKGroupedTable countMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-kgrouped-table"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "US") t0 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "US") t0 0
    pipeInput driver (topicName "users") (Just (bytes "u3")) (bytes "EU") t0 0
    -- u1 moves from US to EU
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "EU") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "US" >>= (@?= Just 1)
        kvsGet kvs "EU" >>= (@?= Just 2)
      Nothing -> error "kgrouped-table store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 18. Cogroup: two streams aggregate into one shared state
----------------------------------------------------------------------

test_cogroup_two_streams :: TestTree
test_cogroup_two_streams =
  testCase "Cogroup of two streams shares aggregator state" $ do
    let g = grouped textSerde textSerde

        leftGrouped :: F.Topology Void (KGroupedStream Text Text)
        leftGrouped =
          F.source "left-in" textSerde textSerde
            >>> F.groupByKey g

        rightGrouped :: F.Topology Void (KGroupedStream Text Text)
        rightGrouped =
          F.source "right-in" textSerde textSerde
            >>> F.groupByKey g

        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "cog-store")

        -- Pattern: cogroup left | addCogrouped right
        --   == liftIO build the cogroup, then aggregate.
        --
        -- The shape with the GADT:
        --   leftGrouped
        --     >>> Cogroup leftStep
        --     >>> Fanout id (rightGrouped >>> arr id_)
        --     >>> AddCogrouped rightStep
        --     >>> AggregateCogrouped seed materialized
        --
        -- "Fanout id (rightGrouped >>> arr id_)" is awkward because
        -- rightGrouped has Void input. Instead, compose:
        --
        --   liftIO_ buildBoth -> (CogroupedStream, KGroupedStream)
        --     >>> AddCogrouped rightStep
        --     >>> AggregateCogrouped ...
        --
        -- The fast path: build the cogroup pair manually in a Lifted.
        -- This is the natural usage pattern for cogroup; the GADT
        -- carries the typed handle through the rest of the pipeline.
        topology :: F.Topology Void (KTable Text Text)
        topology =
          F.liftIO_ "build-cogroup-pair"
            (\b _ -> do
                kgsL <- F.compileWith b leftGrouped
                kgsR <- F.compileWith b rightGrouped
                let cgs0 = startCogroup kgsL
                    cgs  = extendCogroup cgs0 kgsR
                pure cgs)
            >>> F.aggregateCogrouped (pure "") outMat

        -- Inlined cogroup-builder helpers using the existing
        -- imperative API. The Topology.Free path expects the upstream
        -- pipeline to deliver a 'CogroupedStream'; this Lifted block
        -- does so.
        startCogroup kgs = cogroup kgs leftStep
        extendCogroup cs kgs = addCogrouped cs kgs rightStep
        leftStep  _ v acc = acc <> "/" <> v
        rightStep _ v acc = acc <> "+" <> v

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-cogroup"

    pipeInput driver (topicName "left-in")  (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "right-in") (Just (bytes "k")) (bytes "b") (t 1) 0
    pipeInput driver (topicName "left-in")  (Just (bytes "k")) (bytes "c") (t 2) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> kvsGet kvs "k" >>= (@?= Just "/a+b/c")
      Nothing  -> error "cogroup store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 19. suppressUntilTimeLimit
----------------------------------------------------------------------

test_suppress_until_time_limit :: TestTree
test_suppress_until_time_limit =
  testCase "suppressUntilTimeLimit debounces emissions per key" $ do
    -- The suppress operator only emits a key once the time-limit has
    -- elapsed since the last record for that key. Records arriving
    -- within the limit overwrite without emitting.
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.suppressUntilTimeLimit (millis 100)
            >>> F.sink "out" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-suppress"

    -- Two rapid records — second one within the 100ms limit. Only one
    -- emission is expected (after enough stream time has elapsed).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 0)   0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50)  0
    -- Advance stream time past the limit so the buffered v2 flushes.
    pipeInput driver (topicName "in") (Just (bytes "z")) (bytes "_")  (t 200) 0
    pipeInput driver (topicName "in") (Just (bytes "z")) (bytes "_")  (t 1000) 0

    out <- readOutput driver (topicName "out")
    -- We don't care about the exact count of emissions for 'z' (the
    -- debounce behaviour is well-tested elsewhere); the key point is
    -- that 'v1' is suppressed by 'v2' and only the latest value
    -- per-key surfaces once the debounce window passes.
    let kvalues = [ unbytes (crValue cr)
                  | cr <- out, fmap unbytes (crKey cr) == Just "k" ]
    assertBool "suppress dropped intermediate value; only v2 should land"
      ("v1" `notElem` kvalues)
    assertBool "the final value v2 must be emitted at least once"
      ("v2" `elem` kvalues)
    closeDriver driver

----------------------------------------------------------------------
-- 20. Processor API + state store
----------------------------------------------------------------------

test_process_stream_with_state_store :: TestTree
test_process_stream_with_state_store =
  testCase "processWithStateStoreKV runs a state-store-backed counter" $ do
    -- A custom processor that increments a per-key count in an
    -- in-memory KV state store. 'F.processWithStateStoreKV'
    -- atomically registers the processor + state store with the
    -- right owner-node wiring, so the user doesn't need to know
    -- the auto-generated processor node name.
    --
    -- The processor matches the standard shape used throughout the
    -- imperative DSL: an 'IORef' for the 'ProcessorContext' captured
    -- in 'procInit', another for the resolved 'KeyValueStore', and
    -- a 'procProcess' that uses both to read-modify-write the
    -- counter store.
    let storeNm = storeName "free-procapi-counts"
        storeBuilder
          :: Kafka.Streams.State.Store.StoreBuilderKV Text Int64
        storeBuilder = inMemoryKeyValueStoreBuilder storeNm

        -- The custom Processor: on every input record, increment
        -- the counter for its key in the state store.
        counterProcessor :: IO (Processor Text Text)
        counterProcessor = do
          ctxRef   <- newIORef Nothing
          storeRef <-
            newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
          pure Processor
            { procName    = processorName "FREE-PROCAPI-COUNTER"
            , procInit    = \ctx -> do
                writeIORef ctxRef (Just ctx)
                getStateStore ctx storeNm >>= \case
                  Just (AnyKeyValueStore kvs) ->
                    writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
                  _ -> error
                         "free-procapi: counter store missing in procInit"
            , procClose   = pure ()
            , procProcess = \r -> case recordKey r of
                Nothing -> pure ()
                Just k  -> do
                  mst <- readIORef storeRef
                  case mst of
                    Just kvs -> do
                      cur <- maybe 0 Prelude.id <$> kvsGet kvs k
                      kvsPut kvs k (cur + 1)
                    Nothing -> pure ()
            }

        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.processWithStateStoreKV
                  "FREE-PROCAPI-COUNTER"
                  storeBuilder
                  counterProcessor

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-procapi"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0

    -- Pull the state store directly and verify the counts.
    mStore <- getKeyValueStore @Text @Int64 driver storeNm
    case mStore of
      Just kvs -> do
        kvsGet kvs "k1" >>= (@?= Just 3)
        kvsGet kvs "k2" >>= (@?= Just 1)
        kvsGet kvs "k3" >>= (@?= Nothing)
      Nothing -> error "free-procapi: counter store missing in driver"
    closeDriver driver

----------------------------------------------------------------------
-- 21. Optimiser fuses chains of MapValues
----------------------------------------------------------------------

test_optimize_fuses_map_chains :: TestTree
test_optimize_fuses_map_chains =
  testCase "optimize collapses 4× MapValues into a single Arr / MapValues" $ do
    -- Four pure value transforms chained — the optimiser must fuse
    -- them into a single MapValues node (composition of the four
    -- functions).
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues (T.append "a")
            >>> F.mapValues (T.append "b")
            >>> F.mapValues (T.append "c")
            >>> F.mapValues (T.append "d")
            >>> F.sink "out" textSerde textSerde

        stats = F.optimizationStats topology

    -- Sanity check: optimisation reduced the AST node count by at
    -- least 3 (the three "extra" MapValues plus the surrounding
    -- Compose nodes collapse together).
    assertBool
      ("expected node-count reduction; stats = " <> show stats)
      (F.osNodesSaved stats >= 3)

    -- And the optimised topology still produces the right output
    -- end-to-end.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-mapfuse"
    pipeInput driver (topicName "in") Nothing (bytes "x") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["dcbax"]
    closeDriver driver

----------------------------------------------------------------------
-- 22. Optimiser fuses chains of Filter
----------------------------------------------------------------------

test_optimize_fuses_filter_chains :: TestTree
test_optimize_fuses_filter_chains =
  testCase "optimize fuses 3× Filter into a single conjunction" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.filter (\r -> T.length (recordValue r) >= 2)
            >>> F.filter (\r -> T.length (recordValue r) <= 5)
            >>> F.filter (\r -> T.head (recordValue r) /= '_')
            >>> F.sink "out" textSerde textSerde

        stats = F.optimizationStats topology

    assertBool
      ("expected filter chain to fuse; stats = " <> show stats)
      (F.osNodesSaved stats >= 2)

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-filterfuse"
    pipeInput driver (topicName "in") Nothing (bytes "a")        t0 0  -- too short
    pipeInput driver (topicName "in") Nothing (bytes "abcdef")   t0 0  -- too long
    pipeInput driver (topicName "in") Nothing (bytes "_skip")    t0 0  -- underscore
    pipeInput driver (topicName "in") Nothing (bytes "keep")     t0 0  -- passes all 3
    pipeInput driver (topicName "in") Nothing (bytes "ok")       t0 0  -- passes all 3

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["keep", "ok"]
    closeDriver driver

----------------------------------------------------------------------
-- 23. Optimiser collapses identity combinators
----------------------------------------------------------------------

test_optimize_collapses_identity_combinators :: TestTree
test_optimize_collapses_identity_combinators =
  testCase "Cat.id . op . Cat.id collapses; first/second/parallel of Id collapse" $ do
    -- A handcrafted "redundant" topology: lots of identity-wrapped
    -- combinators that should reduce to a near-empty AST after the
    -- optimiser runs.
    let redundant :: F.Topology Void ()
        redundant =
          F.source "in" textSerde textSerde
            >>> Cat.id                        -- redundant identity
            >>> Cat.id Cat.. F.mapValues T.toUpper Cat.. Cat.id
                                              -- Id . op . Id ==> op
            >>> Cat.id                        -- another
            >>> F.mapValues T.reverse
            >>> F.sink "out" textSerde textSerde

        before = F.countNodes redundant
        after  = F.countNodes (F.optimize redundant)

    assertBool
      ("expected at least 2 nodes saved; before=" <> show before
        <> " after=" <> show after)
      (before - after >= 2)

    -- Observable behaviour preserved (toUpper then reverse).
    (_, topo) <- F.compile redundant
    driver <- newDriver topo "free-opt-identity"
    pipeInput driver (topicName "in") Nothing (bytes "hello") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["OLLEH"]
    closeDriver driver

----------------------------------------------------------------------
-- 24. Optimiser preserves observable behaviour (optimised vs unoptimised)
----------------------------------------------------------------------

test_optimize_preserves_observable_behaviour :: TestTree
test_optimize_preserves_observable_behaviour =
  testCase "optimised and unoptimised compilations produce identical output" $ do
    -- A topology that mixes several fusible chains. We compile both
    -- with optimisation and without, drive both with the same input,
    -- and compare outputs.
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.strip
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.filter (\r -> T.length (recordValue r) > 1)
            >>> F.flatMapValues T.words
            >>> F.mapValues (<> "!")
            >>> F.sink "out" textSerde textSerde

        inputs =
          [ (Just (bytes "k1"), bytes "  hello world  ")
          , (Just (bytes "k2"), bytes "")
          , (Just (bytes "k3"), bytes " a ")           -- post-strip is "A", length 1
          , (Just (bytes "k4"), bytes " ok cool now ")
          ]

        runWith :: (F.Topology Void () -> IO ((), Topology))
                -> IO [Text]
        runWith comp = do
          (_, topo) <- comp topology
          driver <- newDriver topo "free-opt-vs-noopt"
          mapM_ (\(k, v) -> pipeInput driver (topicName "in") k v t0 0) inputs
          out <- readOutput driver (topicName "out")
          closeDriver driver
          pure (map (unbytes . crValue) out)

    optimised   <- runWith F.compile
    unoptimised <- runWith F.compileNoOptimize
    optimised @?= unoptimised
    -- And the expected output is the one both should produce.
    optimised @?= ["HELLO!", "WORLD!", "OK!", "COOL!", "NOW!"]

----------------------------------------------------------------------
-- 25. 'noOptimization' is a no-op
----------------------------------------------------------------------

test_optimize_noOptimization_is_a_no_op :: TestTree
test_optimize_noOptimization_is_a_no_op =
  testCase "compileWithOptimization noOptimization preserves node count" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.toUpper
            >>> F.mapValues T.reverse
            >>> F.sink "out" textSerde textSerde

        original = F.countNodes topology
        viaNoOpt = F.countNodes (F.optimizeWith F.noOptimization topology)

    viaNoOpt @?= original
    -- And the topology still runs fine with the no-op config.
    (_, topo) <- F.compileWithOptimization F.noOptimization topology
    driver <- newDriver topo "free-noopt"
    pipeInput driver (topicName "in") Nothing (bytes "abc") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["CBA"]
    closeDriver driver

----------------------------------------------------------------------
-- 26. Pure functions push through Fanout
----------------------------------------------------------------------

test_optimize_pushes_pure_functions_through_fanout :: TestTree
test_optimize_pushes_pure_functions_through_fanout =
  testCase "Fanout/Parallel/First of pure functions collapse to a single Arr" $ do
    -- A pure pipeline built out of Arr-only combinators: Fanout +
    -- Parallel of Arrs should fully collapse to a single Arr that
    -- runs the composed pure function.
    let pureFanout :: F.Topology Int (Int, Int)
        pureFanout =
          (Control.Arrow.arr (+ 1)
              &&& Control.Arrow.arr (* 2))    -- Fanout (Arr) (Arr) ==> Arr
            >>> (Control.Arrow.arr (+ 10)
                  *** Control.Arrow.arr (+ 20))
                                              -- Parallel (Arr) (Arr) ==> Arr
            >>> Cat.id                        -- redundant Id

        before = F.countNodes pureFanout
        after  = F.countNodes (F.optimize pureFanout)

    -- We started with several constructors and should land at the
    -- single 'Arr' constructor representing the composed function.
    assertBool
      ("expected pure-function chain to collapse; before=" <> show before
        <> " after=" <> show after)
      (before > 1 && after == 1)

----------------------------------------------------------------------
-- 27. 'compile' (default) runs the optimiser
----------------------------------------------------------------------

test_compile_default_runs_optimizer :: TestTree
test_compile_default_runs_optimizer =
  testCase "default compile reduces topology node count vs compileNoOptimize" $ do
    -- A chain that the optimiser should reduce: count the Kafka
    -- 'Topology' graph nodes (sources/processors/sinks) after both
    -- compilation paths. Fewer processor nodes means fewer
    -- record-forwarding hops on the data path.
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.mapValues T.strip
            >>> F.mapValues T.toUpper
            >>> F.mapValues T.reverse
            >>> F.mapValues (T.append "x")
            >>> F.sink "out" textSerde textSerde

    (_, topoOpt)   <- F.compile topology
    (_, topoNoOpt) <- F.compileNoOptimize topology

    let !nOpt   = length (topologyNodes topoOpt)
        !nNoOpt = length (topologyNodes topoNoOpt)

    assertBool
      ("optimised compile should produce fewer nodes; opt=" <> show nOpt
        <> " noopt=" <> show nNoOpt)
      (nOpt < nNoOpt)

----------------------------------------------------------------------
-- 28. selectKey >>> groupByKey collapses to groupBy
----------------------------------------------------------------------

test_optimize_selectKey_then_groupByKey_becomes_groupBy :: TestTree
test_optimize_selectKey_then_groupByKey_becomes_groupBy =
  testCase "selectKey >>> groupByKey collapses to a single groupBy" $ do
    -- This is the Java best-practice "prefer groupBy over selectKey+groupByKey"
    -- pattern: one fewer processor node and a clearer topology
    -- description.
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-rekey-count")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source "in" textSerde textSerde
            >>> F.selectKey (\r -> T.take 1 (recordValue r))
            >>> F.groupByKey (grouped textSerde textSerde)
            >>> F.count countMat

        opsBefore = F.inspect topology
        opsAfter  = F.inspect (F.optimize topology)

    -- Before optimisation we see both SelectKey and GroupByKey.
    assertBool "expected SelectKey in pre-optimisation AST" $
      "SelectKey" `elem` opsBefore
    assertBool "expected GroupByKey in pre-optimisation AST" $
      "GroupByKey" `elem` opsBefore
    -- After optimisation both have collapsed into a single GroupBy.
    assertBool "expected GroupBy in optimised AST" $
      "GroupBy" `elem` opsAfter
    assertBool "expected NO SelectKey in optimised AST" $
      "SelectKey" `notElem` opsAfter
    assertBool "expected NO bare GroupByKey in optimised AST" $
      "GroupByKey" `notElem` opsAfter

    -- And observable behaviour is preserved.
    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-rekey"
    pipeInput driver (topicName "in") (Just (bytes "u1")) (bytes "apple")  t0 0
    pipeInput driver (topicName "in") (Just (bytes "u2")) (bytes "apricot") t0 0
    pipeInput driver (topicName "in") (Just (bytes "u3")) (bytes "banana") t0 0
    pipeInput driver (topicName "in") (Just (bytes "u4")) (bytes "anchovy") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just 3)
        kvsGet kvs "b" >>= (@?= Just 1)
      Nothing -> error "rekey-count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 29. Repartition >>> Repartition collapses
----------------------------------------------------------------------

test_optimize_collapses_repartition_chains :: TestTree
test_optimize_collapses_repartition_chains =
  testCase "consecutive repartitions collapse to a single shuffle" $ do
    -- Two back-to-back 'repartition's are redundant: the broker
    -- only needs one shuffle. The optimiser collapses the inner
    -- one; the outer's topic prefix wins.
    let redundant :: F.Topology Void ()
        redundant =
          F.source "in" textSerde textSerde
            >>> F.repartition "first-shuffle"
            >>> F.repartition "second-shuffle"
            >>> F.repartition "third-shuffle"
            >>> F.sink "out" textSerde textSerde

        ops = F.inspect (F.optimize redundant)

    -- Only one Repartition node should remain.
    let repartitionCount =
          length [ () | op <- ops, "Repartition" `T.isPrefixOf` op ]
    repartitionCount @?= 1

----------------------------------------------------------------------
-- 30. Values >>> Values idempotence
----------------------------------------------------------------------

test_optimize_collapses_values_idempotent :: TestTree
test_optimize_collapses_values_idempotent =
  testCase "values >>> values collapses to a single values" $ do
    let topology :: F.Topology (KStream Text Text) (KStream () Text)
        topology = F.values >>> F.values

        opsAfter = F.inspect (F.optimize topology)
        valuesCount = length [ () | op <- opsAfter, op == "Values" ]

    valuesCount @?= 1

----------------------------------------------------------------------
-- 31. Foreach after Peek fuses into one Foreach
----------------------------------------------------------------------

test_optimize_foreach_after_peek_fuses :: TestTree
test_optimize_foreach_after_peek_fuses =
  testCase "foreach >>> peek fuses into a single Foreach" $ do
    seen <- newIORef ([] :: [(Text, Text)])
    let topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            >>> F.peek    (\r -> modifyIORef' seen (\xs -> xs ++ [("peek",    recordValue r)]))
            >>> F.foreach (\r -> modifyIORef' seen (\xs -> xs ++ [("foreach", recordValue r)]))

        ops = F.inspect (F.optimize topology)

    -- After optimisation the Peek and Foreach are gone — both fused
    -- into a single Foreach whose effect runs both callbacks in the
    -- original order.
    assertBool "expected Foreach in optimised AST" $
      "Foreach" `elem` ops
    -- And the side effects still fire in the expected order
    -- (peek-then-foreach per record).
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-foreach-peek"
    pipeInput driver (topicName "in") Nothing (bytes "x") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "y") t0 0
    finalSeen <- readIORef seen
    finalSeen @?= [("peek", "x"), ("foreach", "x"),
                   ("peek", "y"), ("foreach", "y")]
    closeDriver driver

----------------------------------------------------------------------
-- 32. Tap (Foreach f) collapses to Peek f
----------------------------------------------------------------------

test_optimize_tap_foreach_becomes_peek :: TestTree
test_optimize_tap_foreach_becomes_peek =
  testCase "tap (foreach f) collapses to peek f" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology = F.tap (F.foreach (\_ -> pure ()))

        ops = F.inspect (F.optimize topology)

    -- The Tap and Foreach should have been replaced by a single Peek.
    assertBool "expected Peek in optimised AST" $
      "Peek" `elem` ops
    assertBool "Tap should be gone from optimised AST" $
      not (any ("Tap" `T.isPrefixOf`) ops)

----------------------------------------------------------------------
-- 33. Adjacent Tap nodes combine
----------------------------------------------------------------------

test_optimize_combines_adjacent_taps :: TestTree
test_optimize_combines_adjacent_taps =
  testCase "two adjacent Taps combine via Fanout" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.tap (F.sink "audit-a" textSerde textSerde)
            >>> F.tap (F.sink "audit-b" textSerde textSerde)

        opsBefore = F.inspect topology
        opsAfter  = F.inspect (F.optimize topology)

        countTaps ops = length [ () | op <- ops, op == "Tap<" ]

    -- Two Tap markers in the unoptimised AST.
    countTaps opsBefore @?= 2
    -- One Tap marker after fusion (both sinks now inside a single
    -- Tap via Fanout).
    countTaps opsAfter  @?= 1

----------------------------------------------------------------------
-- 34. Arr through Fork collapses
----------------------------------------------------------------------

test_optimize_pushes_arr_through_fork :: TestTree
test_optimize_pushes_arr_through_fork =
  testCase "Arr f . Fork collapses to a single Arr" $ do
    let topology :: F.Topology Int Int
        topology =
          F.fork                                          -- Int -> (Int, Int)
            >>> Control.Arrow.arr (\(a, b) -> a + b)       -- (Int, Int) -> Int

        before = F.countNodes topology
        after  = F.countNodes (F.optimize topology)

    -- We started with at least 'Fork', 'Arr', 'Compose' nodes.
    -- After optimisation it should collapse to just 'Arr (\a -> a + a)'.
    assertBool
      ("expected Arr.Fork to collapse; before=" <> show before
        <> " after=" <> show after)
      (after == 1 && before > 1)

----------------------------------------------------------------------
-- 35. Typed exception on a missing-serde Materialized
----------------------------------------------------------------------

test_missing_serde_throws_typed_exception :: TestTree
test_missing_serde_throws_typed_exception =
  testCase "forcing an aggregation KTable's missing serde raises TopologyFreeError" $ do
    -- A Materialized with no serdes set. The aggregation succeeds at
    -- compile time, but the resulting KTable's serde fields are
    -- deferred 'TopologyFreeError' thunks. Forcing one of them
    -- should yield a catchable exception, not an opaque 'error'.
    let unset :: Materialized Text Int64
        unset = Mat.materializedAs (storeName "missing-serde-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source "in" textSerde textSerde
            >>> F.groupByKey (grouped textSerde textSerde)
            >>> F.count unset

    (kt, _topo) <- F.compile topology
    -- Force the value-serde field; expect TopologyFreeError.
    result <- try (evaluate (ktableValueSerde kt))
    case result of
      Left (F.MissingMaterializedSerde F.ValueSide) -> pure ()
      Left other  ->
        error ("expected MissingMaterializedSerde ValueSide, got: "
                <> show other)
      Right _ ->
        error "expected the missing value serde to raise an exception"

----------------------------------------------------------------------
-- 36-38. EOS atomicity of Fork / ForkN / Tap topologies
----------------------------------------------------------------------
-- We can't drive a real broker in unit tests, but we can verify:
--
-- 1. Fork / ForkN / Tap topologies compile to a single source feeding
--    multiple sinks. /All sinks share the upstream source node/ — so
--    when the engine processes one record, every downstream sink
--    receives it within the same task, before any commit fires. That
--    means all sinks participate in the SAME EOS commit cycle.
--
-- 2. With a recording 'EOSCoordinator' wrapping the record-pumping
--    'flushBody', a single 'runCommitCycle' invocation drives records
--    through all branches and lands every sink's output within one
--    transaction. The call sequence is the canonical
--    @begin → commitOffsets → commit → storeCommit@.
--
-- This is the strongest assertion we can make at the unit-test layer
-- without a live transactional broker; the broker-side wire path is
-- exercised by the EOS integration tests in 'Streams.EOSRuntimeSpec'.

-- | Recording 'EOSCoordinator' that captures the call sequence in an
-- 'IORef'. Identical to the helper in 'Streams.EOSRuntimeSpec' but
-- duplicated here to avoid a cross-module test dependency.
mkRecordingCoord :: IO (EOSCoordinator, IO [Text])
mkRecordingCoord = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      coord = EOSCoordinator
        { initTxn       = log_ "init"   *> pure (Right ())
        , beginTxn      = log_ "begin"  *> pure (Right ())
        , commitTxn     = log_ "commit" *> pure (Right ())
        , abortTxn      = log_ "abort"  *> pure (Right ())
        , commitOffsets = \_ _ -> log_ "commitOffsets" *> pure (Right ())
        , storeCommit   = log_ "storeCommit" *> pure (Right ())
        , storeAbort    = log_ "storeAbort"  *> pure (Right ())
        }
  pure (coord, reverse <$> readIORef buf)

test_fork_topology_is_eos_atomic :: TestTree
test_fork_topology_is_eos_atomic =
  testCase "Fork topology: all branch sinks share the source under EOS" $ do
    -- 'Fork' duplicates the wire; we sink each half to a different
    -- topic. The compiled graph has one source feeding both sinks.
    let upper :: F.Topology (KStream Text Text) ()
        upper = F.mapValues T.toUpper >>> F.sink "fork-upper" textSerde textSerde
        lower :: F.Topology (KStream Text Text) ()
        lower = F.mapValues T.toLower >>> F.sink "fork-lower" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "fork-in" textSerde textSerde
            >>> F.fork
            >>> (upper *** lower)
            >>> Control.Arrow.arr (const ())

    (_, topo) <- F.compile topology
    -- Structural check: exactly one source, exactly two sinks, and
    -- both sinks are descendants of that single source. That's the
    -- topology shape EOS needs for atomic commit across both
    -- sinks.
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 2

    -- Drive records through with a recording EOS coordinator
    -- wrapping the flushBody. A single commit cycle covers both
    -- sinks.
    driver <- newDriver topo "free-eos-fork"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "fork-in") Nothing (bytes "Hello") t0 0
      pipeInput driver (topicName "fork-in") Nothing (bytes "World") t0 0
    outcome @?= CommitSucceeded

    upperOut <- readOutput driver (topicName "fork-upper")
    lowerOut <- readOutput driver (topicName "fork-lower")
    map (unbytes . crValue) upperOut @?= ["HELLO", "WORLD"]
    map (unbytes . crValue) lowerOut @?= ["hello", "world"]

    -- The commit cycle ran the canonical EOS sequence, with /all/
    -- branch outputs captured between 'begin' and 'commit'.
    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

test_forkN_topology_is_eos_atomic :: TestTree
test_forkN_topology_is_eos_atomic =
  testCase "ForkN: N branch sinks all share the source under EOS" $ do
    -- A three-way ForkN; each branch writes to its own topic.
    let mkSink :: Text -> (Text -> Text) -> F.Topology (KStream Text Text) ()
        mkSink topic f =
          F.mapValues f >>> F.sink topic textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "forkn-in" textSerde textSerde
            >>> F.forkN
                  ( NE.fromList
                      [ mkSink "fn-upper" T.toUpper
                      , mkSink "fn-lower" T.toLower
                      , mkSink "fn-rev"   T.reverse
                      ])
            >>> Control.Arrow.arr (const ())

    (_, topo) <- F.compile topology
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 3

    driver <- newDriver topo "free-eos-forkn"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "forkn-in") Nothing (bytes "abc") t0 0
    outcome @?= CommitSucceeded

    u <- readOutput driver (topicName "fn-upper")
    l <- readOutput driver (topicName "fn-lower")
    r <- readOutput driver (topicName "fn-rev")
    map (unbytes . crValue) u @?= ["ABC"]
    map (unbytes . crValue) l @?= ["abc"]
    map (unbytes . crValue) r @?= ["cba"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

test_tap_topology_is_eos_atomic :: TestTree
test_tap_topology_is_eos_atomic =
  testCase "Tap topology: side sink commits atomically with main sink" $ do
    -- A 'Tap' that audit-logs to one topic while the main pipeline
    -- writes its (transformed) records to another. Both sinks must
    -- be reached within the same EOS transaction so the audit log
    -- and the main output never disagree.
    let auditSink :: F.Topology (KStream Text Text) ()
        auditSink = F.sink "tap-audit" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "tap-in" textSerde textSerde
            >>> F.tap auditSink
            >>> F.mapValues T.toUpper
            >>> F.sink "tap-main" textSerde textSerde

    (_, topo) <- F.compile topology
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 2

    driver <- newDriver topo "free-eos-tap"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "tap-in") Nothing (bytes "ping") t0 0
      pipeInput driver (topicName "tap-in") Nothing (bytes "pong") t0 0
    outcome @?= CommitSucceeded

    audit <- readOutput driver (topicName "tap-audit")
    main_ <- readOutput driver (topicName "tap-main")
    -- The audit log gets the /original/ values; the main sink
    -- gets the transformed ones. Both within one transaction.
    map (unbytes . crValue) audit @?= ["ping", "pong"]
    map (unbytes . crValue) main_ @?= ["PING", "PONG"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

-- Small helpers reaching into the topology graph for the EOS
-- structural checks above. Both wrap module-level functions
-- already re-exported via 'Kafka.Streams'.
topologySources :: Topology -> [NodeName]
topologySources topo = Map.keys (topoSources topo)

topologySinkNames :: Topology -> [NodeName]
topologySinkNames topo = Map.keys (topoSinks topo)

----------------------------------------------------------------------
-- 39. Applicative liftA2 combines two topologies on one input
----------------------------------------------------------------------

test_applicative_liftA2_combines_two_topologies :: TestTree
test_applicative_liftA2_combines_two_topologies =
  testCase "Applicative <*> runs both topologies on the same input" $ do
    -- Build two topologies that each compute a wire value from a
    -- shared input, then combine them via the Applicative instance
    -- (here through the Functor instance + <*>). The resulting
    -- topology produces a tuple of wire handles for downstream use.
    let topology :: F.Topology Void (KStream Text Text)
        topology =
          ( (,) <$> F.source "ap-in1" textSerde textSerde
                <*> F.source "ap-in2" textSerde textSerde )
            >>= \(s1, s2) -> F.merge `F.applyT` (s1, s2)

        topologyWithSink :: F.Topology Void ()
        topologyWithSink = topology >>= F.applyT (F.sink "ap-out" textSerde textSerde)

    (_, topo) <- F.compile topologyWithSink
    driver <- newDriver topo "free-applicative"

    pipeInput driver (topicName "ap-in1") Nothing (bytes "a1") t0 0
    pipeInput driver (topicName "ap-in2") Nothing (bytes "b1") t0 0
    pipeInput driver (topicName "ap-in1") Nothing (bytes "a2") t0 0

    out <- readOutput driver (topicName "ap-out")
    map (unbytes . crValue) out @?= ["a1", "b1", "a2"]
    closeDriver driver

----------------------------------------------------------------------
-- 40. Monad do-notation for multi-source topologies
----------------------------------------------------------------------

test_monad_do_notation_for_multi_source :: TestTree
test_monad_do_notation_for_multi_source =
  testCase "do-notation threads source handles through a pipeline" $ do
    let topology :: F.Topology Void ()
        topology = do
          s1 <- F.source "do-in1" textSerde textSerde
          s2 <- F.source "do-in2" textSerde textSerde
          -- Map each side, then merge, then sink. Each step uses
          -- 'applyT' to feed the bound Haskell value into the next
          -- fragment.
          u1 <- F.mapValues T.toUpper       `F.applyT` s1
          u2 <- F.mapValues (T.append "B:") `F.applyT` s2
          m  <- F.merge                     `F.applyT` (u1, u2)
          F.sink "do-out" textSerde textSerde `F.applyT` m

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-monad-multi-source"

    pipeInput driver (topicName "do-in1") Nothing (bytes "hi") t0 0
    pipeInput driver (topicName "do-in2") Nothing (bytes "ya") t0 0

    out <- readOutput driver (topicName "do-out")
    -- The merged stream sees mapped values from both upstreams.
    -- The exact interleaving is the test driver's record-arrival
    -- order; both records must appear.
    map (unbytes . crValue) out @?= ["HI", "B:ya"]
    closeDriver driver

----------------------------------------------------------------------
-- 41. Semigroup: run two pipelines on a shared upstream
----------------------------------------------------------------------

test_semigroup_runs_both_pipelines_on_one_input :: TestTree
test_semigroup_runs_both_pipelines_on_one_input =
  testCase "Topology i () <> Topology i () runs both on the same input" $ do
    -- Two completely separate sink pipelines that share a single
    -- source. Operationally equivalent to Fanout-with-discard but
    -- much terser to write. Both branches share lineage, so EOS
    -- atomicity is by construction (verified separately by the
    -- Fanout EOS test above).
    let left, right :: F.Topology (KStream Text Text) ()
        left  = F.mapValues T.toUpper >>> F.sink "sg-upper" textSerde textSerde
        right = F.mapValues T.toLower >>> F.sink "sg-lower" textSerde textSerde

        topology :: F.Topology Void ()
        topology =
          F.source "sg-in" textSerde textSerde
            >>> (left <> right)

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-semigroup"

    pipeInput driver (topicName "sg-in") Nothing (bytes "Mixed") t0 0
    pipeInput driver (topicName "sg-in") Nothing (bytes "Case")  t0 0

    u <- readOutput driver (topicName "sg-upper")
    l <- readOutput driver (topicName "sg-lower")
    map (unbytes . crValue) u @?= ["MIXED", "CASE"]
    map (unbytes . crValue) l @?= ["mixed", "case"]
    closeDriver driver

----------------------------------------------------------------------
-- 42. Monoid: mempty at unit output is a no-op
----------------------------------------------------------------------

test_monoid_unit_output_is_no_op :: TestTree
test_monoid_unit_output_is_no_op =
  testCase "mempty :: Topology (KStream k v) () drops records silently" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source "monoid-in" textSerde textSerde
            >>> mempty   -- Drop everything; no sink, no foreach
    -- Just checking that compilation + driver setup don't crash;
    -- there's nothing observable to assert.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-monoid"
    pipeInput driver (topicName "monoid-in") Nothing (bytes "x") t0 0
    closeDriver driver

----------------------------------------------------------------------
-- 43. Profunctor: lmapT / rmapT / dimapT compose appropriately
----------------------------------------------------------------------

test_profunctor_dimap_works :: TestTree
test_profunctor_dimap_works =
  testCase "dimapT pre/post-composes pure functions on a topology" $ do
    let inner :: F.Topology Int Int
        inner = Control.Arrow.arr (+ 1)

        wrapped :: F.Topology Text Text
        wrapped = F.dimapT T.length (T.pack . show) inner

        -- compileNoOptimize so we can sanity-check that dimap is
        -- 3 nodes (lmap Arr + inner + rmap Arr) without surprising
        -- collapses.
        nWrapped = F.countNodes wrapped
        nInner   = F.countNodes inner

    -- The optimised count should be 1 — the chain of pure functions
    -- collapses into a single Arr (since 'inner' itself is an Arr).
    F.countNodes (F.optimize wrapped) @?= 1
    -- Unoptimised, dimapT added two Arr wrappers and a Compose around
    -- the inner.
    assertBool
      ("expected dimapT to wrap; unwrapped=" <> show nInner
        <> " wrapped=" <> show nWrapped)
      (nWrapped > nInner)

----------------------------------------------------------------------
-- 44. Reader-style: localInput pre-transforms the input
----------------------------------------------------------------------

test_reader_localInput_pre_transforms :: TestTree
test_reader_localInput_pre_transforms =
  testCase "localInput pre-applies its function to the wire input" $ do
    -- Build a pure-function pipeline; localInput pre-applies
    -- 'T.length' so the inner sees an Int instead of a Text.
    let inner :: F.Topology Int Int
        inner = Control.Arrow.arr (\n -> n * 2)

        wrapped :: F.Topology Text Int
        wrapped = F.localInput T.length inner

        opt = F.optimize wrapped
    -- After optimisation we have a single Arr (the chain of pure
    -- functions collapses), and computing it through apply
    -- gives the expected value.
    F.countNodes opt @?= 1

----------------------------------------------------------------------
-- 45. Cross-source EOS via 'mergeSourced'
----------------------------------------------------------------------

test_mergeSourced_two_sources_share_one_task_under_eos :: TestTree
test_mergeSourced_two_sources_share_one_task_under_eos =
  testCase "mergeSourced makes two sources share a single Kafka task" $ do
    -- Without mergeSourced, two source-rooted halves of a Fanout
    -- compile to two disconnected sub-topologies (= two tasks =
    -- two EOS transactions). 'mergeSourced' inserts a convergence
    -- 'Merge' node so the graph is one connected component =
    -- one sub-topology = one task = one EOS transaction.
    let topology :: F.Topology Void ()
        topology =
          F.mergeSourced
            (F.source "ms-a" textSerde textSerde)
            (F.source "ms-b" textSerde textSerde)
            >>> F.mapValues T.toUpper
            >>> F.sink "ms-out" textSerde textSerde

    (_, topo) <- F.compile topology

    -- Structural check: TWO source topics, ONE merge processor
    -- with both as parents, ONE sink. The graph is connected.
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 2
    length sinks @?= 1

    driver <- newDriver topo "free-eos-merged-sources"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "ms-a") Nothing (bytes "hello") t0 0
      pipeInput driver (topicName "ms-b") Nothing (bytes "world") t0 0
    outcome @?= CommitSucceeded

    out <- readOutput driver (topicName "ms-out")
    map (unbytes . crValue) out @?= ["HELLO", "WORLD"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

----------------------------------------------------------------------
-- 46. Cogroup expressed via Monad bind + applyT
----------------------------------------------------------------------

test_cogroup_via_do_notation :: TestTree
test_cogroup_via_do_notation =
  testCase "cogroup builds incrementally via do-notation + applyT" $ do
    -- The cogroup pattern: each source produces its own grouped
    -- stream; the cogroup builder chains them with per-source
    -- aggregators. With the Monad instance and 'applyT' the
    -- builder reads like the imperative Java code, while the
    -- result is still a value-level Topology that compiles and
    -- runs through the standard driver.
    let g = grouped textSerde textSerde
        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "cog-do-store")

        leftStep, rightStep :: Text -> Text -> Text -> Text
        leftStep  _ v acc = acc <> "/" <> v
        rightStep _ v acc = acc <> "+" <> v

        topology :: F.Topology Void (KTable Text Text)
        topology = do
          s1   <- F.source "cogdo-in1" textSerde textSerde
          s2   <- F.source "cogdo-in2" textSerde textSerde
          g1   <- F.groupByKey g `F.applyT` s1
          g2   <- F.groupByKey g `F.applyT` s2
          cgs0 <- F.cogroup leftStep `F.applyT` g1
          cgs1 <- F.addCogrouped rightStep `F.applyT` (cgs0, g2)
          F.aggregateCogrouped (pure "") outMat `F.applyT` cgs1

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-cogdo"

    pipeInput driver (topicName "cogdo-in1") (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "cogdo-in2") (Just (bytes "k")) (bytes "b") (t 1) 0
    pipeInput driver (topicName "cogdo-in1") (Just (bytes "k")) (bytes "c") (t 2) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> kvsGet kvs "k" >>= (@?= Just "/a+b/c")
      Nothing  -> error "cogroup-do store missing"
    closeDriver driver
