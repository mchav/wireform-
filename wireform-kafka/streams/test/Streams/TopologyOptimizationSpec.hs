{-# LANGUAGE OverloadedStrings #-}

module Streams.TopologyOptimizationSpec (tests) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import Kafka.Streams.Processor (Processor (..), processorName)
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store
  ( LoggingConfig (..)
  , StoreBuilderKV (..)
  , StoreName
  , storeName
  , withLoggingDisabledKV
  , withSourceTopicChangelogKV
  )
import Kafka.Streams.Time (recordTimestampExtractor)
import Kafka.Streams.Topology
  ( AnyStoreBuilder (..)
  , ChangelogPlanProblem (..)
  , NodeName (..)
  , OptimizationConfig (..)
  , Topology
  , TopologyError (..)
  , addProcessor
  , addSink
  , addSource
  , addStateStoreKV
  , childrenOf
  , defaultOptimizationConfig
  , effectiveChangelogReuse
  , emptyTopology
  , noOptimisations
  , optimizeTopology
  , sinkParents
  , topoChangelogPlan
  , topoProcessors
  , topoSinks
  , topoStores
  , validateChangelogPlan
  )
import qualified Kafka.Streams.Topology
import Kafka.Streams.Types (TopicName, topicName)

tests :: Spec
tests = describe "Topology optimisation" $ sequence_
  [ describe "optimizeTopology — REUSE_KTABLE_SOURCE_TOPICS" $ sequence_
      [ it "populates topoChangelogPlan with the source topic"
          reuse_source_ktable_basic
      , it "is a no-op when the toggle is disabled"
          reuse_disabled_noop
      , it "skips when the source has multiple topics"
          reuse_skip_multi_topic_source
      , it "skips when the processor has multiple parents"
          reuse_skip_multi_parent
      , it "skips when the store is shared with another processor"
          reuse_skip_shared_store
      , it "skips when the store's logging is already disabled"
          reuse_skip_logging_disabled
      , it "skips when another sink writes to the source topic"
          reuse_skip_source_also_sink
      , it "is idempotent — applying twice == applying once"
          reuse_idempotent
      ]
  , describe "optimizeTopology — dynamic-modification safety (REUSE)" $ sequence_
      [ it "adding a competing sink after optimisation, re-running clears the stale plan entry"
          reuse_dynamic_sink_invalidates
      , it "adding a second owner processor after optimisation, re-running clears the entry"
          reuse_dynamic_shared_store_invalidates
      , it "preserves user-explicit withSourceTopicChangelogKV across optimiser runs"
          reuse_dynamic_user_explicit_preserved
      , it "wipes topoChangelogPlan even when the toggle is disabled"
          reuse_dynamic_wipes_when_disabled
      , it "adds a new plan entry when the new graph creates a candidate"
          reuse_dynamic_adds_new_candidate
      ]
  , describe "optimizeTopology — MERGE_REPARTITION_TOPICS" $ sequence_
      [ it "merges two sibling repartition processors that share a parent + prefix"
          merge_repartition_pair
      , it "merges N sibling repartitions and rewires downstream sinks"
          merge_repartition_many
      , it "leaves different prefixes alone"
          merge_skip_different_prefixes
      , it "leaves repartitions with different parents alone"
          merge_skip_different_parents
      , it "is a no-op when the toggle is disabled"
          merge_disabled_noop
      , it "is idempotent — applying twice == applying once"
          merge_idempotent
      , it "rebuilds topoChildrenIndex consistently after merging"
          merge_rebuilds_children_index
      ]
  , describe "optimizeTopology — dynamic-modification safety (MERGE)" $ sequence_
      [ it "adding a third sibling after merge, re-running absorbs it"
          merge_dynamic_new_sibling_absorbed
      , it "adding a sibling with a smaller name after merge, the new one becomes the survivor"
          merge_dynamic_smaller_sibling_takes_over
      , it "adding a sibling under a different parent, re-running leaves both alone"
          merge_dynamic_different_parent_left_alone
      ]
  , describe "optimizeTopology — both rewrites" $ sequence_
      [ it "applies both rewrites on the same topology"
          both_rewrites_compose
      ]
  , describe "validateChangelogPlan" $ sequence_
      [ it "accepts an optimised topology"
          validate_accepts_optimised
      , it "rejects a stale plan entry after adding a competing sink"
          validate_rejects_stale_after_sink
      , it "rejects a stale plan entry after adding a shared owner"
          validate_rejects_stale_after_shared_owner
      , it "rejects an inconsistent user-declared loggingSourceTopic"
          validate_rejects_user_inconsistent
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
  -- Before: no optimisation; the side-table is empty.
  Map.lookup storeNm (topoChangelogPlan t) `shouldBe` Nothing
  effectiveChangelogReuse t storeNm        `shouldBe` Nothing
  let !t' = optimizeTopology defaultOptimizationConfig t
  -- After: optimisation populated the side-table.
  Map.lookup storeNm (topoChangelogPlan t') `shouldBe` Just topic
  effectiveChangelogReuse t' storeNm        `shouldBe` Just topic
  -- The store builder's LoggingConfig is NOT mutated — user
  -- declarations remain authoritative.
  loggingSourceTopic (storeLogging storeNm t')  `shouldBe` Nothing
  loggingEnabled     (storeLogging storeNm t')  `shouldBe` True

reuse_disabled_noop :: IO ()
reuse_disabled_noop = do
  let topic   = topicName "src-topic"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      cfg     = defaultOptimizationConfig { optReuseSourceKTable = False }
      !t'     = optimizeTopology cfg t
  Map.lookup storeNm (topoChangelogPlan t') `shouldBe` Nothing
  effectiveChangelogReuse t' storeNm        `shouldBe` Nothing

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
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing

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
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing

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
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing

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
  loggingEnabled (storeLogging storeNm t')  `shouldBe` False
  effectiveChangelogReuse t' storeNm        `shouldBe` Nothing

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
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing

reuse_idempotent :: IO ()
reuse_idempotent = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      !t1     = optimizeTopology defaultOptimizationConfig t
      !t2     = optimizeTopology defaultOptimizationConfig t1
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  effectiveChangelogReuse t2 storeNm `shouldBe` Just topic
  topoChangelogPlan t1 `shouldBe` topoChangelogPlan t2
  Map.size (topoProcessors t1) `shouldBe` Map.size (topoProcessors t2)
  Map.size (topoSinks t1)      `shouldBe` Map.size (topoSinks t2)

----------------------------------------------------------------------
-- Dynamic-modification safety (REUSE)
----------------------------------------------------------------------

-- | After 'compile' returns its 'Topo.Topology', callers should
-- be able to /modify/ the graph (add sinks, processors,
-- sources, stores) and /re-run/ the optimiser to get a
-- correct fresh plan. These tests pin that contract.

reuse_dynamic_sink_invalidates :: IO ()
reuse_dynamic_sink_invalidates = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      -- 1) Optimise: a candidate, plan entry set.
      !t1     = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
      -- 2) Dynamically add a competing sink that writes to the
      -- same topic. The topic is no longer a clean source-only
      -- changelog target.
  let !t2     = addSinkOnTopic (NodeName "SINK") topic procNm t1
      -- 3) Re-running the optimiser detects the invalidation
      -- and clears the stale plan entry.
      !t3     = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 storeNm `shouldBe` Nothing

reuse_dynamic_shared_store_invalidates :: IO ()
reuse_dynamic_shared_store_invalidates = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      !t1     = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  -- Add a second processor and connect it to the store.
  let proc2   = NodeName "P2"
      !t2     = Kafka.Streams.Topology.connectProcessorAndStateStores
                  proc2 [storeNm]
                $ addProcessor proc2 [srcNm] mkProc t1
      !t3     = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 storeNm `shouldBe` Nothing

reuse_dynamic_user_explicit_preserved :: IO ()
reuse_dynamic_user_explicit_preserved = do
  let topic   = topicName "user-decl"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "user-store"
      builder :: StoreBuilderKV () ()
      builder = withSourceTopicChangelogKV topic
                  (inMemoryKeyValueStoreBuilder storeNm)
      -- User-explicit declaration on the store. The processor /
      -- source pair below DOES match the graph shape — so the
      -- user's intent is consistent with the graph.
      t = addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcNm] mkProc
            $ addSource srcNm [topic]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
  -- 'effectiveChangelogReuse' sees the user declaration even
  -- before optimisation runs.
  effectiveChangelogReuse t storeNm `shouldBe` Just topic
  loggingSourceTopic (storeLogging storeNm t) `shouldBe` Just topic
  -- After optimisation the user declaration is preserved on the
  -- builder; the optimiser does NOT duplicate it into the plan
  -- (the plan filter excludes user-set stores).
  let !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t')  `shouldBe` Just topic
  effectiveChangelogReuse t' storeNm            `shouldBe` Just topic
  Map.lookup storeNm (topoChangelogPlan t')     `shouldBe` Nothing
  -- Re-running optimisation many times leaves the user
  -- declaration alone.
  let !t'' = optimizeTopology defaultOptimizationConfig
               (optimizeTopology defaultOptimizationConfig t')
  loggingSourceTopic (storeLogging storeNm t'') `shouldBe` Just topic

reuse_dynamic_wipes_when_disabled :: IO ()
reuse_dynamic_wipes_when_disabled = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      !t1     = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  -- User flips the toggle off and re-optimises. The plan is
  -- wiped — no stale entries linger.
  let !t2 = optimizeTopology noOptimisations t1
  Map.size (topoChangelogPlan t2) `shouldBe` 0
  effectiveChangelogReuse t2 storeNm `shouldBe` Nothing

reuse_dynamic_adds_new_candidate :: IO ()
reuse_dynamic_adds_new_candidate = do
  -- Topology starts with one source-table; after compile +
  -- optimise, user adds a second source-table-shaped
  -- processor + store on a separate topic. Re-running the
  -- optimiser captures the new candidate in the plan without
  -- disturbing the original.
  let topic1   = topicName "tp1"
      src1     = NodeName "S1"
      proc1    = NodeName "P1"
      store1   = storeName "s1"
      t        = sourceTableTopology topic1 src1 proc1 store1
      !t1      = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 store1 `shouldBe` Just topic1
  let topic2   = topicName "tp2"
      src2     = NodeName "S2"
      proc2    = NodeName "P2"
      store2   = storeName "s2"
      builder2 :: StoreBuilderKV () ()
      builder2 = inMemoryKeyValueStoreBuilder store2
      !t2 = addStateStoreKV builder2 [proc2]
              $ addProcessor proc2 [src2] mkProc
              $ addSource src2 [topic2]
                  textSerde textSerde recordTimestampExtractor t1
      !t3 = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 store1 `shouldBe` Just topic1
  effectiveChangelogReuse t3 store2 `shouldBe` Just topic2
  Map.size (topoChangelogPlan t3)   `shouldBe` 2

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
  Map.size (topoProcessors t) `shouldBe` 2
  -- 'topoChildrenIndex' stores children in reverse-insertion order
  -- (each new child is prepended); the order here is one
  -- implementation detail we don't bet on. Sort before comparing.
  List.sort (childrenOf t (NodeName "S")) `shouldBe`
    [ NodeName "KSTREAM-REPARTITION-x-1"
    , NodeName "KSTREAM-REPARTITION-x-2"
    ]
  let !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') `shouldBe` 1
  -- The merge survivor is the lexicographically smallest node name.
  childrenOf t' (NodeName "S") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]

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
  Map.size (topoProcessors t) `shouldBe` 3
  let !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') `shouldBe` 1
  -- Both sinks now reference the survivor as their parent.
  let sinkParentsOf nm =
        maybe [] sinkParents (Map.lookup (NodeName nm) (topoSinks t'))
  sinkParentsOf "sinkA" `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]
  sinkParentsOf "sinkB" `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]

merge_skip_different_prefixes :: IO ()
merge_skip_different_prefixes = do
  let t = addRepartitionNode "KSTREAM-REPARTITION-y-1" "S"
        $ addRepartitionNode "KSTREAM-REPARTITION-x-1" "S"
        $ addSource (NodeName "S") [topicName "in"]
            textSerde textSerde recordTimestampExtractor
        $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  Map.size (topoProcessors t') `shouldBe` 2

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
  Map.size (topoProcessors t') `shouldBe` 2

merge_disabled_noop :: IO ()
merge_disabled_noop = do
  let !t  = twoRepartitionSiblings
      cfg = defaultOptimizationConfig { optMergeRepartitionTopics = False }
      !t' = optimizeTopology cfg t
  Map.size (topoProcessors t') `shouldBe` 2

merge_idempotent :: IO ()
merge_idempotent = do
  let !t  = twoRepartitionSiblings
      !t1 = optimizeTopology defaultOptimizationConfig t
      !t2 = optimizeTopology defaultOptimizationConfig t1
  Map.size (topoProcessors t1) `shouldBe` 1
  Map.size (topoProcessors t2) `shouldBe` 1
  childrenOf t1 (NodeName "S") `shouldBe` childrenOf t2 (NodeName "S")

merge_rebuilds_children_index :: IO ()
merge_rebuilds_children_index = do
  let !t  = twoRepartitionSiblings
      !t' = optimizeTopology defaultOptimizationConfig t
  childrenOf t' (NodeName "S") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]
  -- The removed node should not have any recorded children either —
  -- it doesn't exist as a parent in the index at all.
  (null (childrenOf t' (NodeName "KSTREAM-REPARTITION-x-2"))) `shouldBe` True

----------------------------------------------------------------------
-- Dynamic-modification safety (MERGE)
----------------------------------------------------------------------

merge_dynamic_new_sibling_absorbed :: IO ()
merge_dynamic_new_sibling_absorbed = do
  -- 1) Two siblings, optimise -> one survivor.
  let !t1 = optimizeTopology defaultOptimizationConfig twoRepartitionSiblings
  Map.size (topoProcessors t1) `shouldBe` 1
  -- 2) Dynamically add a third sibling under the same parent.
  let !t2 = addRepartitionNode "KSTREAM-REPARTITION-x-3" "S" t1
  Map.size (topoProcessors t2) `shouldBe` 2
  -- 3) Re-run -> the new one is absorbed into the survivor.
  let !t3 = optimizeTopology defaultOptimizationConfig t2
  Map.size (topoProcessors t3) `shouldBe` 1
  childrenOf t3 (NodeName "S") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]

merge_dynamic_smaller_sibling_takes_over :: IO ()
merge_dynamic_smaller_sibling_takes_over = do
  let !t1 = optimizeTopology defaultOptimizationConfig twoRepartitionSiblings
  childrenOf t1 (NodeName "S") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]
  -- Add a sibling whose name sorts smaller than the survivor.
  let !t2 = addRepartitionNode "KSTREAM-REPARTITION-x-0" "S" t1
      !t3 = optimizeTopology defaultOptimizationConfig t2
  -- The newly-added smaller node becomes the survivor; the
  -- previous survivor is folded in.
  childrenOf t3 (NodeName "S") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-0"]
  Map.size (topoProcessors t3) `shouldBe` 1

merge_dynamic_different_parent_left_alone :: IO ()
merge_dynamic_different_parent_left_alone = do
  let !t1 = optimizeTopology defaultOptimizationConfig twoRepartitionSiblings
  Map.size (topoProcessors t1) `shouldBe` 1
  -- Add a new source + a repartition under it. Same prefix as
  -- the surviving one but different parent: should NOT merge.
  let !t2 = addRepartitionNode "KSTREAM-REPARTITION-x-99" "S2"
         $ addSource (NodeName "S2") [topicName "other"]
             textSerde textSerde recordTimestampExtractor t1
      !t3 = optimizeTopology defaultOptimizationConfig t2
  Map.size (topoProcessors t3) `shouldBe` 2
  childrenOf t3 (NodeName "S")  `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]
  childrenOf t3 (NodeName "S2") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-99"]

----------------------------------------------------------------------
-- validateChangelogPlan tests
----------------------------------------------------------------------

validate_accepts_optimised :: IO ()
validate_accepts_optimised = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      t       = sourceTableTopology topic srcNm procNm storeNm
      !t'     = optimizeTopology defaultOptimizationConfig t
  validateChangelogPlan t' `shouldBe` Right ()

validate_rejects_stale_after_sink :: IO ()
validate_rejects_stale_after_sink = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      !t1     = optimizeTopology defaultOptimizationConfig
                  (sourceTableTopology topic srcNm procNm storeNm)
      -- Add a competing sink without re-optimising. The plan
      -- entry is now stale.
      !t2     = addSinkOnTopic (NodeName "SINK") topic procNm t1
  validateChangelogPlan t2 `shouldBe`
    Left (StaleChangelogPlan storeNm topic StaleTopicHasSink)
  -- Re-running the optimiser fixes it.
  let !t3 = optimizeTopology defaultOptimizationConfig t2
  validateChangelogPlan t3 `shouldBe` Right ()

validate_rejects_stale_after_shared_owner :: IO ()
validate_rejects_stale_after_shared_owner = do
  let topic   = topicName "tp"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      proc2   = NodeName "P2"
      storeNm = storeName "store"
      !t1     = optimizeTopology defaultOptimizationConfig
                  (sourceTableTopology topic srcNm procNm storeNm)
      !t2     = Kafka.Streams.Topology.connectProcessorAndStateStores
                  proc2 [storeNm]
                $ addProcessor proc2 [srcNm] mkProc t1
  validateChangelogPlan t2 `shouldBe`
    Left (StaleChangelogPlan storeNm topic StaleMultipleOwners)

validate_rejects_user_inconsistent :: IO ()
validate_rejects_user_inconsistent = do
  -- User declares "reuse topic X as changelog" but X isn't the
  -- store's owner's source. The validator catches it.
  let topic   = topicName "wrong-topic"
      srcNm   = NodeName "S"
      procNm  = NodeName "P"
      storeNm = storeName "store"
      actual  = topicName "actual-source"
      builder :: StoreBuilderKV () ()
      builder = withSourceTopicChangelogKV topic
                  (inMemoryKeyValueStoreBuilder storeNm)
      t = addStateStoreKV builder [procNm]
            $ addProcessor procNm [srcNm] mkProc
            $ addSource srcNm [actual]
                textSerde textSerde recordTimestampExtractor
            $ emptyTopology
  validateChangelogPlan t `shouldBe`
    Left (StaleChangelogPlan storeNm topic (StaleWrongTopic actual))

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
  effectiveChangelogReuse t' tableStore `shouldBe` Just tableTopic
  -- One surviving repartition processor + one source-table
  -- processor = two processors total.
  Map.size (topoProcessors t') `shouldBe` 2
  -- The repartition merge survivor is the lexicographically
  -- earliest @-1@ suffix.
  childrenOf t' (NodeName "SS") `shouldBe` [NodeName "KSTREAM-REPARTITION-x-1"]
  -- Silence "unused" warning on Text import in some GHC configs.
  _ <- pure (T.length "x")
  pure ()
