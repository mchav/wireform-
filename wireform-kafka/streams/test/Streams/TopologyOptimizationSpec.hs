{-# LANGUAGE OverloadedStrings #-}

module Streams.TopologyOptimizationSpec (tests) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Kafka.Streams.Processor (Processor (..), processorName)
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store
  ( LoggingConfig (..)
  , StoreBuilderKV (..)
  , StoreName
  , storeName
  , withLoggingDisabledKV
  )
import Kafka.Streams.Time (recordTimestampExtractor)
import Kafka.Streams.Topology
  ( AnyStoreBuilder (..)
  , NodeName (..)
  , OptimizationConfig (..)
  , Topology
  , addProcessor
  , addSink
  , addSource
  , addStateStoreKV
  , childrenOf
  , defaultOptimizationConfig
  , emptyTopology
  , optimizeTopology
  , sinkParents
  , topoProcessors
  , topoSinks
  , topoStores
  )
import Kafka.Streams.Types (TopicName, topicName)

tests :: TestTree
tests = testGroup "Topology optimisation"
  [ testGroup "optimizeTopology — REUSE_KTABLE_SOURCE_TOPICS"
      [ testCase "rewrites the table-source store's logging config to its source topic"
          reuse_source_ktable_basic
      , testCase "is a no-op when the toggle is disabled"
          reuse_disabled_noop
      , testCase "skips when the source has multiple topics"
          reuse_skip_multi_topic_source
      , testCase "skips when the processor has multiple parents"
          reuse_skip_multi_parent
      , testCase "skips when the store is shared with another processor"
          reuse_skip_shared_store
      , testCase "skips when the store's logging is already disabled"
          reuse_skip_logging_disabled
      , testCase "skips when another sink writes to the source topic"
          reuse_skip_source_also_sink
      , testCase "is idempotent — applying twice == applying once"
          reuse_idempotent
      ]
  , testGroup "optimizeTopology — MERGE_REPARTITION_TOPICS"
      [ testCase "merges two sibling repartition processors that share a parent + prefix"
          merge_repartition_pair
      , testCase "merges N sibling repartitions and rewires downstream sinks"
          merge_repartition_many
      , testCase "leaves different prefixes alone"
          merge_skip_different_prefixes
      , testCase "leaves repartitions with different parents alone"
          merge_skip_different_parents
      , testCase "is a no-op when the toggle is disabled"
          merge_disabled_noop
      , testCase "is idempotent — applying twice == applying once"
          merge_idempotent
      , testCase "rebuilds topoChildrenIndex consistently after merging"
          merge_rebuilds_children_index
      ]
  , testGroup "optimizeTopology — both rewrites"
      [ testCase "applies both rewrites on the same topology"
          both_rewrites_compose
      ]
  ]

----------------------------------------------------------------------
-- Test helpers
----------------------------------------------------------------------

mkProc :: IO (Processor () ())
mkProc = pure Processor
  { procName    = processorName "P"
  , procInit    = \_ -> pure ()
  , procClose   = pure ()
  , procProcess = \_ -> pure ()
  }

-- | A source-table-style topology: one source on @topic@ feeding a
-- single processor that owns a single in-memory store.
sourceTableTopology
  :: TopicName       -- ^ source topic
  -> NodeName        -- ^ source name
  -> NodeName        -- ^ processor name
  -> StoreName       -- ^ store name
  -> Topology
sourceTableTopology topic srcNm procNm storeNm =
  addStateStoreKV builder [procNm]
    $ addProcessor procNm [srcNm] mkProc
    $ addSource srcNm [topic] textSerde textSerde recordTimestampExtractor
    $ emptyTopology
  where
    -- Pin the type so the existential 'AnyStoreBuilder' inside
    -- the topology doesn't get a free type variable from the
    -- key/value parameter.
    builder :: StoreBuilderKV () ()
    builder = inMemoryKeyValueStoreBuilder storeNm

-- | The 'LoggingConfig' attached to a KV store, for before/after
-- assertions on the reuse rewrite.
storeLogging :: StoreName -> Topology -> LoggingConfig
storeLogging sn t =
  case Map.lookup sn (topoStores t) of
    Nothing -> error ("storeLogging: missing store: " <> show sn)
    Just (AsKeyValueBuilder b) -> sbKvLogging b
    Just _ -> error "storeLogging: unexpected store-builder shape"

-- | Add a sink writing to @topic@ with @parent@ as its single
-- upstream node.
addSinkOnTopic
  :: NodeName -> TopicName -> NodeName -> Topology -> Topology
addSinkOnTopic nm tp parent =
  addSink nm tp textSerde textSerde [parent]

-- | Add a pass-through @KSTREAM-REPARTITION-…@ processor with the
-- supplied node name and single parent. Mirrors the shape produced
-- by 'Kafka.Streams.KStream.repartition'.
addRepartitionNode
  :: Text       -- ^ exact node name
  -> Text       -- ^ parent name
  -> Topology -> Topology
addRepartitionNode nm parent =
  addProcessor (NodeName nm) [NodeName parent] mkProc

----------------------------------------------------------------------
-- REUSE_KTABLE_SOURCE_TOPICS tests
----------------------------------------------------------------------

reuse_source_ktable_basic :: IO ()
reuse_source_ktable_basic = do
  let topic   = topicName "src-topic"
      srcNm   = NodeName "KTABLE-SOURCE-1"
      procNm  = NodeName "KTABLE-SOURCE-PROCESSOR-2"
      storeNm = storeName "ktable-store"
      t       = sourceTableTopology topic srcNm procNm storeNm
  loggingSourceTopic (storeLogging storeNm t) @?= Nothing
  let !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') @?= Just topic
  loggingEnabled    (storeLogging storeNm t') @?= True

reuse_disabled_noop :: IO ()
reuse_disabled_noop = do
  let topic   = topicName "src-topic"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      cfg     = defaultOptimizationConfig { optReuseSourceKTable = False }
      !t'     = optimizeTopology cfg t
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_skip_multi_topic_source :: IO ()
reuse_skip_multi_topic_source = do
  let srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t = addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcNm] mkProc
            $ addSource srcNm
                [topicName "a", topicName "b"]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_skip_multi_parent :: IO ()
reuse_skip_multi_parent = do
  let topic   = topicName "tp"
      srcA    = NodeName "SA"
      srcB    = NodeName "SB"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t = addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcA, srcB] mkProc
            $ addSource srcB [topicName "other"]
                textSerde textSerde recordTimestampExtractor
            $ addSource srcA [topic]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_skip_shared_store :: IO ()
reuse_skip_shared_store = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      proc1   = NodeName "P1"
      proc2   = NodeName "P2"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t = addStateStoreKV builder [proc1, proc2]
            $ addProcessor proc2 [srcNm] mkProc
            $ addProcessor proc1 [srcNm] mkProc
            $ addSource srcNm [topic]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_skip_logging_disabled :: IO ()
reuse_skip_logging_disabled = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = withLoggingDisabledKV (inMemoryKeyValueStoreBuilder storeNm)
      t = addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcNm] mkProc
            $ addSource srcNm [topic]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingEnabled     (storeLogging storeNm t') @?= False
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_skip_source_also_sink :: IO ()
reuse_skip_source_also_sink = do
  let topic   = topicName "shared"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      sinkNm  = NodeName "SINK"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t = addSinkOnTopic sinkNm topic procNm
            $ addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcNm] mkProc
            $ addSource srcNm [topic]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') @?= Nothing

reuse_idempotent :: IO ()
reuse_idempotent = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      !t1     = optimizeTopology defaultOptimizationConfig t
      !t2     = optimizeTopology defaultOptimizationConfig t1
  loggingSourceTopic (storeLogging storeNm t1) @?= Just topic
  loggingSourceTopic (storeLogging storeNm t2) @?= Just topic
  Map.size (topoProcessors t1) @?= Map.size (topoProcessors t2)
  Map.size (topoSinks t1)      @?= Map.size (topoSinks t2)

----------------------------------------------------------------------
-- MERGE_REPARTITION_TOPICS tests
----------------------------------------------------------------------

twoRepartitionSiblings :: Topology
twoRepartitionSiblings =
  addRepartitionNode "KSTREAM-REPARTITION-x-2" "S"
    $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "S"
    $ addSource (NodeName "S") [topicName "in"]
        textSerde textSerde recordTimestampExtractor
    $ emptyTopology

merge_repartition_pair :: IO ()
merge_repartition_pair = do
  let !t = twoRepartitionSiblings
  Map.size (topoProcessors t) @?= 2
  -- 'topoChildrenIndex' stores children in reverse-insertion order
  -- (each new child is prepended); the order here is one
  -- implementation detail we don't bet on. Sort before comparing.
  List.sort (childrenOf t (NodeName "S")) @?=
    [ NodeName "KSTREAM-REPARTITION-x-1"
    , NodeName "KSTREAM-REPARTITION-x-2"
    ]
  let !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') @?= 1
  -- The merge survivor is the lexicographically smallest node name.
  childrenOf t' (NodeName "S") @?= [NodeName "KSTREAM-REPARTITION-x-1"]

merge_repartition_many :: IO ()
merge_repartition_many = do
  let t = addSinkOnTopic (NodeName "sinkB") (topicName "outB")
            (NodeName "KSTREAM-REPARTITION-x-3")
        $ addSinkOnTopic (NodeName "sinkA") (topicName "outA")
            (NodeName "KSTREAM-REPARTITION-x-2")
        $ addRepartitionNode "KSTREAM-REPARTITION-x-3" "S"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-2" "S"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "S"
        $ addSource (NodeName "S") [topicName "in"]
            textSerde textSerde recordTimestampExtractor
        $ emptyTopology
  Map.size (topoProcessors t) @?= 3
  let !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') @?= 1
  -- Both sinks now reference the survivor as their parent.
  let sinkParentsOf nm =
        maybe [] sinkParents (Map.lookup (NodeName nm) (topoSinks t'))
  sinkParentsOf "sinkA" @?= [NodeName "KSTREAM-REPARTITION-x-1"]
  sinkParentsOf "sinkB" @?= [NodeName "KSTREAM-REPARTITION-x-1"]

merge_skip_different_prefixes :: IO ()
merge_skip_different_prefixes = do
  let t = addRepartitionNode "KSTREAM-REPARTITION-y-1" "S"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "S"
        $ addSource (NodeName "S") [topicName "in"]
            textSerde textSerde recordTimestampExtractor
        $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') @?= 2

merge_skip_different_parents :: IO ()
merge_skip_different_parents = do
  let t = addRepartitionNode "KSTREAM-REPARTITION-x-2" "SB"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "SA"
        $ addSource (NodeName "SB") [topicName "in2"]
            textSerde textSerde recordTimestampExtractor
        $ addSource (NodeName "SA") [topicName "in1"]
            textSerde textSerde recordTimestampExtractor
        $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  -- Each parent has one child, so no sibling group is eligible.
  Map.size (topoProcessors t') @?= 2

merge_disabled_noop :: IO ()
merge_disabled_noop = do
  let !t  = twoRepartitionSiblings
      cfg = defaultOptimizationConfig { optMergeRepartitionTopics = False }
      !t' = optimizeTopology cfg t
  Map.size (topoProcessors t') @?= 2

merge_idempotent :: IO ()
merge_idempotent = do
  let !t  = twoRepartitionSiblings
      !t1 = optimizeTopology defaultOptimizationConfig t
      !t2 = optimizeTopology defaultOptimizationConfig t1
  Map.size (topoProcessors t1) @?= 1
  Map.size (topoProcessors t2) @?= 1
  childrenOf t1 (NodeName "S") @?= childrenOf t2 (NodeName "S")

merge_rebuilds_children_index :: IO ()
merge_rebuilds_children_index = do
  let !t  = twoRepartitionSiblings
      !t' = optimizeTopology defaultOptimizationConfig t
  childrenOf t' (NodeName "S") @?= [NodeName "KSTREAM-REPARTITION-x-1"]
  -- The removed node should not have any recorded children either —
  -- it doesn't exist as a parent in the index at all.
  assertBool "removed node has no children entries"
    (null (childrenOf t' (NodeName "KSTREAM-REPARTITION-x-2")))

----------------------------------------------------------------------
-- Both rewrites composed
----------------------------------------------------------------------

both_rewrites_compose :: IO ()
both_rewrites_compose = do
  let tableTopic = topicName "tbl"
      tableSrc   = NodeName "TS"
      tableProc  = NodeName "TP"
      tableStore = storeName "tbl-store"
      streamSrc  = NodeName "SS"
      tableStoreBuilder :: StoreBuilderKV () ()
      tableStoreBuilder = inMemoryKeyValueStoreBuilder tableStore
      t = addRepartitionNode "KSTREAM-REPARTITION-x-2" "SS"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "SS"
        $ addSource streamSrc [topicName "stream-in"]
            textSerde textSerde recordTimestampExtractor
        $ addStateStoreKV tableStoreBuilder [tableProc]
        $ addProcessor tableProc [tableSrc] mkProc
        $ addSource tableSrc [tableTopic]
            textSerde textSerde recordTimestampExtractor
        $ emptyTopology
  let !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging tableStore t') @?= Just tableTopic
  -- One surviving repartition processor + one source-table
  -- processor = two processors total.
  Map.size (topoProcessors t') @?= 2
  -- The repartition merge survivor is the lexicographically
  -- earliest @-1@ suffix.
  childrenOf t' (NodeName "SS") @?= [NodeName "KSTREAM-REPARTITION-x-1"]
  -- Silence "unused" warning on Text import in some GHC configs.
  _ <- pure (T.length "x")
  pure ()
