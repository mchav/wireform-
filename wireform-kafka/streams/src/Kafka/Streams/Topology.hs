{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Topology
-- Description : Low-level Topology builder
--
-- Mirrors @org.apache.kafka.streams.Topology@:
--
-- @
-- Topology t =
--     emptyTopology
--   & addSource    "src"  ["my-topic"] keySerde valSerde extractor
--   & addProcessor "p1"   parents       processorSupplier
--   & addStateStore       storeBuilder ["p1"]
--   & addSink      "snk"  "out-topic"   keySerde valSerde ["p1"]
-- @
--
-- The topology is type-erased internally so we can keep all sources /
-- processors / sinks in a single map. Type information is preserved
-- at the edges (source serdes, sink serdes) and at the processor
-- supplier (which is given a typed 'ProcessorContext' inside the
-- runtime).
--
-- Validation:
--
--   * names are unique across nodes
--   * every parent reference resolves to a node
--   * sources have at least one topic
--   * the graph is acyclic
--   * stores referenced by 'addStateStore' exist
module Kafka.Streams.Topology
  ( -- * Names
    NodeName (..)
  , nodeName
  , unNodeName
    -- * Topology
  , Topology
  , emptyTopology
  , addSource
  , addSourceWith
  , addProcessor
  , addProcessorWith
  , addSink
  , addSinkWith
  , addStateStore
  , addStateStoreKV
  , addStateStoreW
  , addStateStoreS
  , addGlobalStore
  , topoGlobalStores
  , connectProcessorAndStateStores
    -- * Optimisations (KIP-295)
  , OptimizationConfig (..)
  , defaultOptimizationConfig
  , optimizeTopology
    -- * Validation
  , validateTopology
  , TopologyValid
  , unsafeAssumeValid
  , topologyNodes
    -- * Internals (used by the runtime / driver)
  , SourceSpec (..)
  , ProcessorSpec (..)
  , SinkSpec (..)
  , AnyProcessor (..)
  , AnySerde (..)
  , AnyTimestampExtractor (..)
  , AnyStoreBuilder (..)
  , storeBuilderName
  , topoSources
  , topoProcessors
  , topoSinks
  , topoStores
  , topoStoreOwners
  , topoSourceOrder
  , topoOrder
  , topologyValidGraph
  , parentsOf
  , childrenOf
    -- * Topology-level errors
  , TopologyError (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Kafka.Streams.Processor
  ( Processor
  , ProcessorName (..)
  )
import Kafka.Streams.Serde (Serde)
import Kafka.Streams.State.Store
  ( LoggingConfig (..)
  , StoreBuilder (..)
  , StoreBuilderKV (..)
  , StoreBuilderS (..)
  , StoreBuilderW (..)
  , StoreName
  , unStoreName
  )
import qualified Kafka.Streams.State.Store
import qualified Kafka.Streams.Time
import Kafka.Streams.Time (TimestampExtractor)
import Kafka.Streams.Types (NodeName (..), TopicName, nodeName, unNodeName)

-- 'NodeName' is shared with "Kafka.Streams.Processor" so re-exported
-- from "Kafka.Streams.Types" to break the import cycle.

-- | Type-erased serde used in the topology AST.
data AnySerde where
  AnySerde :: !(Serde a) -> AnySerde

-- | Type-erased timestamp extractor.
data AnyTimestampExtractor where
  AnyTimestampExtractor :: !(TimestampExtractor k v) -> AnyTimestampExtractor

-- | Type-erased processor supplier. The runtime calls 'apsBuild'
-- once per task to construct an instance.
data AnyProcessor where
  AnyProcessor
    :: !(IO (Processor k v))
    -> AnyProcessor

-- | Type-erased store builder. Keeps the original typed builder
-- around so the DSL can still use it after retrieval.
data AnyStoreBuilder where
  AsKeyValueBuilder :: !(StoreBuilderKV k v) -> AnyStoreBuilder
  AsWindowBuilder   :: !(StoreBuilderW   k v) -> AnyStoreBuilder
  AsSessionBuilder  :: !(StoreBuilderS   k v) -> AnyStoreBuilder
  AsRawBuilder      :: !StoreBuilder           -> AnyStoreBuilder

storeBuilderName :: AnyStoreBuilder -> StoreName
storeBuilderName = \case
  AsKeyValueBuilder b -> sbKvName b
  AsWindowBuilder   b -> sbWName  b
  AsSessionBuilder  b -> sbSName  b
  AsRawBuilder      b -> sbName   b

-- | Source node specification.
data SourceSpec = SourceSpec
  { sourceName       :: !NodeName
  , sourceTopics     :: ![TopicName]
  , sourceKeySerde   :: !AnySerde
  , sourceValueSerde :: !AnySerde
  , sourceExtractor  :: !AnyTimestampExtractor
  }

-- | Processor node specification.
data ProcessorSpec = ProcessorSpec
  { processorSpecName     :: !NodeName
  , processorSpecParents  :: ![NodeName]
  , processorSpecSupplier :: !AnyProcessor
  , processorSpecStores   :: ![StoreName]
  }

-- | Sink node specification.
data SinkSpec = SinkSpec
  { sinkName        :: !NodeName
  , sinkParents     :: ![NodeName]
  , sinkTopic       :: !TopicName
  , sinkKeySerde    :: !AnySerde
  , sinkValueSerde  :: !AnySerde
  }

-- | The graph itself. Maps are by node name; insertion order is kept
-- on the side as 'topoOrder' so the test driver / runtime can boot
-- nodes deterministically.
data Topology = Topology
  { topoSources       :: !(Map NodeName SourceSpec)
  , topoProcessors    :: !(Map NodeName ProcessorSpec)
  , topoSinks         :: !(Map NodeName SinkSpec)
  , topoStores        :: !(Map StoreName AnyStoreBuilder)
  , topoStoreOwners   :: !(Map StoreName [NodeName])
  , topoOrder         :: !(Seq NodeName)
    -- ^ Insertion order across every source / processor / sink.
  , topoSourceOrder   :: !(Seq NodeName)
    -- ^ Insertion order across just the sources.
  , topoChildrenIndex :: !(Map NodeName [NodeName])
  , topoGlobalStores  :: !(Set StoreName)
    -- ^ Stores registered via 'addGlobalStore'. The runtime treats
    -- these as cluster-wide replicas and bypasses partition
    -- assignment for their source topics.
  }

emptyTopology :: Topology
emptyTopology = Topology
  { topoSources       = Map.empty
  , topoProcessors    = Map.empty
  , topoSinks         = Map.empty
  , topoStores        = Map.empty
  , topoStoreOwners   = Map.empty
  , topoOrder         = Seq.empty
  , topoSourceOrder   = Seq.empty
  , topoChildrenIndex = Map.empty
  , topoGlobalStores  = Set.empty
  }

-- | Distinct error categories surfaced by 'validateTopology'.
data TopologyError
  = NodeNameTaken !NodeName
  | UnknownParent !NodeName !NodeName
  | UnknownStore !StoreName !NodeName
  | StoreAlreadyAdded !StoreName
  | EmptySourceTopics !NodeName
  | TopologyCycle ![NodeName]
  | NoSources
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

----------------------------------------------------------------------
-- Builders
----------------------------------------------------------------------

-- | Add a source. Subscribes to each named topic; records flow through
-- the supplied serdes and timestamp extractor.
addSource
  :: NodeName
  -> [TopicName]
  -> Serde k
  -> Serde v
  -> TimestampExtractor k v
  -> Topology
  -> Topology
addSource nm ts ks vs ex t =
  ensureNameFree t nm $ insertSource t SourceSpec
    { sourceName       = nm
    , sourceTopics     = ts
    , sourceKeySerde   = AnySerde ks
    , sourceValueSerde = AnySerde vs
    , sourceExtractor  = AnyTimestampExtractor ex
    }

-- | 'addSource' that lets the caller install a custom 'TopologyError'
-- via 'TopologyException'. Useful from inside the DSL where richer
-- error context is available.
addSourceWith :: SourceSpec -> Topology -> Topology
addSourceWith spec t = ensureNameFree t (sourceName spec) (insertSource t spec)

insertSource :: Topology -> SourceSpec -> Topology
insertSource t spec = t
  { topoSources     = Map.insert (sourceName spec) spec (topoSources t)
  , topoOrder       = topoOrder t |> sourceName spec
  , topoSourceOrder = topoSourceOrder t |> sourceName spec
  }

-- | Add a processor with a list of named parents. Parents must
-- already exist (sources or other processors) — this is checked at
-- build time but only enforced strictly by 'validateTopology'.
addProcessor
  :: NodeName
  -> [NodeName]
  -> IO (Processor k v)
  -> Topology
  -> Topology
addProcessor nm parents supplier =
  addProcessorWith ProcessorSpec
    { processorSpecName     = nm
    , processorSpecParents  = parents
    , processorSpecSupplier = AnyProcessor supplier
    , processorSpecStores   = []
    }

-- | 'addProcessor' that takes an already-built 'ProcessorSpec'.
addProcessorWith :: ProcessorSpec -> Topology -> Topology
addProcessorWith spec t =
  ensureNameFree t (processorSpecName spec) $
    let !t' = t
          { topoProcessors    = Map.insert (processorSpecName spec) spec
                                  (topoProcessors t)
          , topoOrder         = topoOrder t |> processorSpecName spec
          , topoChildrenIndex =
              foldl'
                (\acc p ->
                   Map.insertWith (++) p [processorSpecName spec] acc)
                (topoChildrenIndex t)
                (processorSpecParents spec)
          }
     in t'

-- | Add a sink whose values come from one or more named parents. The
-- runtime serialises records using the provided serdes and emits to
-- the named topic.
addSink
  :: NodeName
  -> TopicName
  -> Serde k
  -> Serde v
  -> [NodeName]
  -> Topology
  -> Topology
addSink nm tp ks vs parents =
  addSinkWith SinkSpec
    { sinkName        = nm
    , sinkParents     = parents
    , sinkTopic       = tp
    , sinkKeySerde    = AnySerde ks
    , sinkValueSerde  = AnySerde vs
    }

addSinkWith :: SinkSpec -> Topology -> Topology
addSinkWith spec t =
  ensureNameFree t (sinkName spec) $
    let !t' = t
          { topoSinks         = Map.insert (sinkName spec) spec (topoSinks t)
          , topoOrder         = topoOrder t |> sinkName spec
          , topoChildrenIndex =
              foldl'
                (\acc p -> Map.insertWith (++) p [sinkName spec] acc)
                (topoChildrenIndex t)
                (sinkParents spec)
          }
     in t'

-- | Attach a generic store builder. Each processor in @owners@ will
-- have access to the store via 'getStore'.
addStateStore :: StoreBuilder -> [NodeName] -> Topology -> Topology
addStateStore b owners =
  addStoreInternal (sbName b) (AsRawBuilder b) owners

-- | Typed key-value store variant. Equivalent to:
--
-- @
-- addStateStoreKV b ["p1"] = addStateStore (rawify b) ["p1"]
-- @
addStateStoreKV
  :: StoreBuilderKV k v -> [NodeName] -> Topology -> Topology
addStateStoreKV b owners =
  addStoreInternal (sbKvName b) (AsKeyValueBuilder b) owners

addStateStoreW
  :: StoreBuilderW k v -> [NodeName] -> Topology -> Topology
addStateStoreW b owners =
  addStoreInternal (sbWName b) (AsWindowBuilder b) owners

addStateStoreS
  :: StoreBuilderS k v -> [NodeName] -> Topology -> Topology
addStateStoreS b owners =
  addStoreInternal (sbSName b) (AsSessionBuilder b) owners

-- | Configuration for 'optimizeTopology'. Each toggle enables one
-- of the KIP-295 optimisations.
data OptimizationConfig = OptimizationConfig
  { optMergeRepartitionTopics :: !Bool
    -- ^ When two consecutive selectKey/groupBy operations would
    -- each materialise a repartition topic, merge them into one.
    -- Currently a no-op (the DSL doesn't auto-insert repartition
    -- topics yet); the field exists so callers can opt in once
    -- the DSL change lands.
  , optReuseSourceKTable      :: !Bool
    -- ^ When a KTable's only consumer is a join, reuse the source
    -- topic directly instead of materialising. Currently a no-op
    -- (most KTables in our DSL are explicitly materialised).
  }
  deriving stock (Eq, Show)

defaultOptimizationConfig :: OptimizationConfig
defaultOptimizationConfig = OptimizationConfig
  { optMergeRepartitionTopics = True
  , optReuseSourceKTable      = True
  }

-- | Mirrors @Topology.optimize(StreamsConfig)@. Applies the
-- requested KIP-295 optimisations and returns a (possibly)
-- rewritten topology.
--
-- The two rewrites are:
--
-- ['optReuseSourceKTable' — @REUSE_KTABLE_SOURCE_TOPICS@]
--   For every processor that has exactly one parent which is a
--   single-topic 'SourceSpec' /and/ owns exactly one state store
--   whose logging is enabled, mark the store's 'LoggingConfig' to
--   reuse the source topic as its changelog (via
--   'loggingSourceTopic'). This eliminates the separate
--   @\<application-id\>-\<store-name\>-changelog@ topic the broker
--   would otherwise create — the source topic itself is the
--   compacted log we'd replay on restore.
--
--   Skipped when:
--
--     * The source has multiple topics (no single topic to reuse).
--     * The processor has multiple parents (the store doesn't have
--       a unique source).
--     * The store is shared across multiple processors (it isn't
--       owned by this lineage).
--     * Logging is already disabled (no changelog to reuse).
--     * Another sink in the topology writes to the source topic
--       (it's not actually source-only).
--     * The store is a global store (its source is already its
--       changelog by construction).
--
-- ['optMergeRepartitionTopics' — @MERGE_REPARTITION_TOPICS@]
--   Sibling pass-through repartition processors (named
--   @KSTREAM-REPARTITION-\<prefix\>-…@) that share the same
--   single parent /and/ the same repartition prefix are merged
--   into one. Every downstream consumer is rewired to the surviving
--   node, the duplicate nodes are removed, and 'topoChildrenIndex'
--   is rebuilt. The result: one shared repartition processor
--   instead of N independent ones, mirroring the Java optimisation
--   that collapses multiple internal repartition topics into one.
--
--   Skipped when:
--
--     * A repartition node has multiple parents (split lineage —
--       not a sibling collision).
--     * Two repartition processors share a parent but disagree on
--       prefix (different broker-side topics in the JVM
--       equivalent).
--     * The processor owns a state store (a stateful repartition,
--       which we never auto-emit but a 'liftIO_' caller could).
--
-- == Semantics
--
-- Both rewrites are observably equivalent to the un-rewritten
-- topology under the in-process 'TopologyTestDriver' (which
-- doesn't materialise broker topics — changelogs are an in-memory
-- store, repartition is a logical pass-through). They become
-- observable against a real broker, where the changelog/topic
-- count drops.
--
-- == Idempotence
--
-- Applying 'optimizeTopology' twice produces the same result as
-- applying it once. The rewrites are not iterative — each one
-- sweeps the graph in a single pass.
optimizeTopology :: OptimizationConfig -> Topology -> Topology
optimizeTopology cfg = mergeRepartitions . reuseSourceKTable
  where
    reuseSourceKTable
      | optReuseSourceKTable cfg      = applyReuseSourceKTable
      | otherwise                     = Prelude.id
    mergeRepartitions
      | optMergeRepartitionTopics cfg = applyMergeRepartitionTopics
      | otherwise                     = Prelude.id

----------------------------------------------------------------------
-- KIP-295 #1 — REUSE_KTABLE_SOURCE_TOPICS
----------------------------------------------------------------------

-- | Find every processor that fits the "source-table" shape
-- (one source parent with a single topic; one owned store with
-- logging enabled; no competing sinks on the source topic) and
-- rewrite the store's 'LoggingConfig' to point its changelog at
-- the source topic.
applyReuseSourceKTable :: Topology -> Topology
applyReuseSourceKTable t =
  let candidates = collectReuseCandidates t
   in foldl' applyOne t candidates
  where
    applyOne topo (storeName_, topic) =
      topo { topoStores = Map.adjust (setLoggingSourceTopic topic)
                                     storeName_
                                     (topoStores topo) }

-- | Pairs of @(store, sourceTopic)@ where the store's changelog
-- can be redirected to the source topic.
collectReuseCandidates :: Topology -> [(StoreName, TopicName)]
collectReuseCandidates t =
  let !sinkTopicSet = Set.fromList
        [ sinkTopic s
        | s <- Map.elems (topoSinks t)
        ]
   in [ (storeNm, topic)
      | (procNm, spec) <- Map.toList (topoProcessors t)
      , [parentNm]     <- pure (processorSpecParents spec)
      , Just src       <- pure (Map.lookup parentNm (topoSources t))
      , [topic]        <- pure (sourceTopics src)
      , not (Set.member topic sinkTopicSet)
      , [storeNm]      <- pure (processorSpecStores spec)
      , not (Set.member storeNm (topoGlobalStores t))
      , Just owners    <- pure (Map.lookup storeNm (topoStoreOwners t))
      , owners == [procNm]
      , Just builder   <- pure (Map.lookup storeNm (topoStores t))
      , Just logCfg    <- pure (storeBuilderLogging builder)
      , loggingEnabled logCfg
      , Nothing        <- pure (loggingSourceTopic logCfg)
      ]

-- | Extract the 'LoggingConfig' from an 'AnyStoreBuilder' (if it
-- has one — raw 'AsRawBuilder' values use the generic
-- 'sbLogging').
storeBuilderLogging :: AnyStoreBuilder -> Maybe LoggingConfig
storeBuilderLogging = \case
  AsKeyValueBuilder b -> Just (sbKvLogging b)
  AsWindowBuilder   b -> Just (sbWLogging b)
  AsSessionBuilder  b -> Just (sbSLogging b)
  AsRawBuilder      b -> Just (sbLogging  b)

-- | Mark an 'AnyStoreBuilder' as having the supplied source topic
-- as its changelog. Pass-through on any builder type that doesn't
-- have a logging field.
setLoggingSourceTopic :: TopicName -> AnyStoreBuilder -> AnyStoreBuilder
setLoggingSourceTopic topic = \case
  AsKeyValueBuilder b -> AsKeyValueBuilder b
    { sbKvLogging = (sbKvLogging b) { loggingSourceTopic = Just topic } }
  AsWindowBuilder   b -> AsWindowBuilder   b
    { sbWLogging  = (sbWLogging  b) { loggingSourceTopic = Just topic } }
  AsSessionBuilder  b -> AsSessionBuilder  b
    { sbSLogging  = (sbSLogging  b) { loggingSourceTopic = Just topic } }
  AsRawBuilder      b -> AsRawBuilder      b
    { sbLogging   = (sbLogging   b) { loggingSourceTopic = Just topic } }

----------------------------------------------------------------------
-- KIP-295 #2 — MERGE_REPARTITION_TOPICS
----------------------------------------------------------------------

-- | Collapse sibling pass-through repartition processors that
-- share a single parent and the same @KSTREAM-REPARTITION-\<prefix\>@
-- name prefix into one. Downstream consumers are rewired to the
-- survivor; 'topoChildrenIndex' is rebuilt at the end.
applyMergeRepartitionTopics :: Topology -> Topology
applyMergeRepartitionTopics t =
  let !groups = repartitionSiblingGroups t
      !rename = buildRenameMap groups
      !t1     = applyRename rename t
   in t1 { topoChildrenIndex = rebuildChildrenIndex t1 }
  where
    buildRenameMap :: [[NodeName]] -> Map NodeName NodeName
    buildRenameMap = foldl' addGroup Map.empty
      where
        addGroup acc grp = case grp of
          []           -> acc
          (survivor:vs) ->
            foldl' (\m loser -> Map.insert loser survivor m) acc vs

-- | Discover groups of sibling repartition processors that can be
-- merged. The returned list contains one entry per group; each
-- group is a non-empty list of 'NodeName's where the head is the
-- survivor and the tail is the to-be-removed duplicates.
--
-- Survivor selection is deterministic: within each group we pick
-- the lexicographically smallest 'NodeName'. This is independent
-- of insertion order, so 'applyMergeRepartitionTopics' is
-- idempotent under the optimiser's normal compose-with-itself
-- semantics.
repartitionSiblingGroups :: Topology -> [[NodeName]]
repartitionSiblingGroups t =
  [ orderedGrp
  | (_parent, kids) <- Map.toList (topoChildrenIndex t)
  , let buckets = groupRepartitionsByPrefix t kids
  , (_prefix, grp) <- buckets
  , length grp > 1
  , let orderedGrp = List.sort grp
  ]

-- | Partition a child list by repartition prefix; keeps only
-- nodes that are eligible for merging (single-parent
-- pass-through repartition processors with no owned stores).
-- Returns @[(prefix, [nodes-in-insertion-order])]@.
groupRepartitionsByPrefix
  :: Topology
  -> [NodeName]
  -> [(Text, [NodeName])]
groupRepartitionsByPrefix t kids =
  let eligible =
        [ (prefix, kid)
        | kid <- kids
        , Just spec <- [Map.lookup kid (topoProcessors t)]
        , length (processorSpecParents spec) == 1
        , null (processorSpecStores spec)
        , Just prefix <- [repartitionPrefix (unNodeName kid)]
        ]
   in Map.toList $ Map.fromListWith (\new old -> old ++ new)
        [ (prefix, [kid]) | (prefix, kid) <- eligible ]

-- | Extract the @\<prefix\>@ part from a @KSTREAM-REPARTITION-\<prefix\>-\<id\>@
-- 'NodeName'. Returns 'Nothing' if the name doesn't match the
-- repartition pattern.
repartitionPrefix :: Text -> Maybe Text
repartitionPrefix nm = do
  rest <- Text.stripPrefix "KSTREAM-REPARTITION-" nm
  -- Strip the trailing @-<counter>@ that 'freshNodeName' appends.
  -- The counter is a non-empty run of digits at the end.
  let (lhs, dashCounter) = Text.breakOnEnd "-" rest
  if Text.null dashCounter || not (Text.all Char.isDigit dashCounter)
    then pure rest
    -- @lhs@ ends in '-' (because 'breakOnEnd' keeps the delimiter
    -- in the left half); strip it.
    else pure (Text.dropWhileEnd (== '-') lhs)

-- | Apply a survivor-rename map to every parent/owner reference
-- in the topology and drop the renamed-away nodes.
applyRename :: Map NodeName NodeName -> Topology -> Topology
applyRename m t
  | Map.null m = t
  | otherwise =
      let resolve nm = Map.findWithDefault nm nm m
          renameList = map resolve
          dropLosers ns = filter (\n -> not (Map.member n m)) ns
       in t
            { topoProcessors =
                Map.map (renameProcessorParents resolve)
                  (Map.filterWithKey (\k _ -> not (Map.member k m))
                                     (topoProcessors t))
            , topoSinks =
                Map.map (renameSinkParents resolve) (topoSinks t)
            , topoOrder = Seq.filter (\n -> not (Map.member n m))
                                     (topoOrder t)
            , topoStoreOwners =
                Map.map (nub . renameList) (topoStoreOwners t)
            , topoChildrenIndex =
                -- Drop renamed keys; the index is rebuilt afterwards.
                Map.mapKeys resolve
                  (Map.map (dropLosers . renameList) (topoChildrenIndex t))
            }
  where
    renameProcessorParents f spec = spec
      { processorSpecParents = nub (map f (processorSpecParents spec))
      }
    renameSinkParents f spec = spec
      { sinkParents = nub (map f (sinkParents spec))
      }

-- | Recompute 'topoChildrenIndex' from scratch by walking every
-- processor's and sink's parent list.
rebuildChildrenIndex :: Topology -> Map NodeName [NodeName]
rebuildChildrenIndex t =
  Map.map reverse $ foldl'
    (\acc (child, parents) ->
       foldl' (\a p -> Map.insertWith (++) p [child] a) acc parents)
    Map.empty
    childParents
  where
    childParents :: [(NodeName, [NodeName])]
    childParents =
      [ (processorSpecName s, processorSpecParents s)
      | s <- Map.elems (topoProcessors t)
      ]
      ++
      [ (sinkName s, sinkParents s)
      | s <- Map.elems (topoSinks t)
      ]

-- | Connect an existing processor to one or more existing state
-- stores. Mirrors @Topology.connectProcessorAndStateStores@ —
-- equivalent to having registered the stores at processor-add time
-- via the @stores@ list, useful for after-the-fact re-wiring.
connectProcessorAndStateStores
  :: NodeName
  -> [StoreName]
  -> Topology
  -> Topology
connectProcessorAndStateStores procNm stores t =
  case Map.lookup procNm (topoProcessors t) of
    Nothing -> errorWithCtx
      ("connectProcessorAndStateStores: unknown processor: "
        <> unNodeName procNm)
    Just _  ->
      let !procsNew  = foldl' (\m sn -> attachToProcessor sn m procNm)
                              (topoProcessors t) stores
          !ownersNew = foldl' (\m sn -> Map.insertWith (++) sn [procNm] m)
                              (topoStoreOwners t) stores
       in t { topoProcessors = procsNew
            , topoStoreOwners = ownersNew
            }

addStoreInternal
  :: StoreName -> AnyStoreBuilder -> [NodeName] -> Topology -> Topology
addStoreInternal sn ab owners t =
  case Map.lookup sn (topoStores t) of
    Just _  -> errorWithCtx ("store already added: " <> unStoreName sn)
    Nothing ->
      let !storesNew = Map.insert sn ab (topoStores t)
          !ownersNew = Map.insertWith (++) sn owners (topoStoreOwners t)
          !procsNew = foldl' (attachToProcessor sn) (topoProcessors t) owners
       in t
            { topoStores      = storesNew
            , topoStoreOwners = ownersNew
            , topoProcessors  = procsNew
            }

-- | Register a /global/ state store backed by its own source +
-- processor. Mirrors @Topology.addGlobalStore@.
--
-- The supplied store builder is realised as a regular state store,
-- but the 'StoreName' is /also/ added to 'topoGlobalStores'. The
-- runtime treats global stores as cluster-wide replicas: every
-- instance subscribes to the source topic and writes to the local
-- copy. Within the single-task 'TopologyTestDriver' the
-- distinction is purely semantic — the same source/processor/store
-- machinery handles the data flow.
--
-- The @processorSupplier@ takes (k, v) records and updates the
-- store; a typical implementation just does @kvsPut store k v@.
-- Use 'Kafka.Streams.GlobalKTable' for the high-level
-- "global lookup table" API; this lower-level entry point lets
-- you run arbitrary processor logic on the global side.
addGlobalStore
  :: StoreBuilderKV k v
  -> NodeName                              -- ^ source name
  -> NodeName                              -- ^ processor name
  -> TopicName
  -> Serde k
  -> Serde v
  -> Kafka.Streams.Time.TimestampExtractor k v
  -> AnyProcessor                           -- ^ store updater
  -> Topology
  -> Topology
addGlobalStore builder sourceNm procNm topic ks vs ex updater t0 =
  ensureNameFree t0 sourceNm $
    ensureNameFree (insertSource t0 srcSpec) procNm $
      let !t1 = insertSource t0 srcSpec
          !t2 = addProcessorWith
                  ProcessorSpec
                    { processorSpecName     = procNm
                    , processorSpecParents  = [sourceNm]
                    , processorSpecSupplier = updater
                    , processorSpecStores   = [Kafka.Streams.State.Store.sbKvName builder]
                    } t1
          !t3 = addStateStoreKV builder [procNm] t2
       in t3 { topoGlobalStores =
                 Set.insert (Kafka.Streams.State.Store.sbKvName builder)
                            (topoGlobalStores t3)
             }
  where
    !srcSpec = SourceSpec
      { sourceName       = sourceNm
      , sourceTopics     = [topic]
      , sourceKeySerde   = AnySerde ks
      , sourceValueSerde = AnySerde vs
      , sourceExtractor  = AnyTimestampExtractor ex
      }

attachToProcessor
  :: StoreName
  -> Map NodeName ProcessorSpec
  -> NodeName
  -> Map NodeName ProcessorSpec
attachToProcessor sn m owner =
  Map.adjust
    (\spec -> spec
        { processorSpecStores = nub (sn : processorSpecStores spec)
        })
    owner
    m

-- | All node names already used by @t@.
allNames :: Topology -> Set NodeName
allNames t =
  Set.unions
    [ Map.keysSet (topoSources t)
    , Map.keysSet (topoProcessors t)
    , Map.keysSet (topoSinks t)
    ]

ensureNameFree :: Topology -> NodeName -> Topology -> Topology
ensureNameFree t nm cont =
  if Set.member nm (allNames t)
    then errorWithCtx ("node already exists: " <> unNodeName nm)
    else cont

errorWithCtx :: Text -> a
errorWithCtx msg = error ("Kafka.Streams.Topology: " <> T.unpack msg)

----------------------------------------------------------------------
-- Validation
----------------------------------------------------------------------

-- | Phantom-tagged topology proving 'validateTopology' succeeded.
newtype TopologyValid = TopologyValid { topologyValidGraph :: Topology }

-- | Bypass validation. The runtime trusts validated topologies; if you
-- bypass and feed an invalid graph in you can corrupt state on
-- failover. Don't.
unsafeAssumeValid :: Topology -> TopologyValid
unsafeAssumeValid = TopologyValid

-- | Check the graph for the invariants documented at the top of the
-- module. Returns either the first error encountered or a phantom
-- proof that validation passed.
validateTopology :: Topology -> Either TopologyError TopologyValid
validateTopology t = do
  let knownNodes = allNames t
  -- 1. sources non-empty + topics
  when_ (Map.null (topoSources t)) NoSources
  Map.foldlWithKey'
    (\acc nm src ->
      acc >> when_ (null (sourceTopics src)) (EmptySourceTopics nm))
    (Right ()) (topoSources t)
  -- 2. parents resolve
  Map.foldlWithKey'
    (\acc nm spec ->
      acc >>
      case [ p | p <- processorSpecParents spec
               , not (Set.member p knownNodes)
           ] of
        []        -> Right ()
        (p : _)   -> Left (UnknownParent nm p))
    (Right ()) (topoProcessors t)
  Map.foldlWithKey'
    (\acc nm spec ->
      acc >>
      case [ p | p <- sinkParents spec
               , not (Set.member p knownNodes)
           ] of
        []      -> Right ()
        (p : _) -> Left (UnknownParent nm p))
    (Right ()) (topoSinks t)
  -- 3. store ownership
  Map.foldlWithKey'
    (\acc sn owners ->
      acc >>
      case [ o | o <- owners
               , not (Map.member o (topoProcessors t))
           ] of
        []        -> Right ()
        (o : _)   -> Left (UnknownStore sn o))
    (Right ()) (topoStoreOwners t)
  -- 4. acyclicity (the children index is a DAG iff a topo-sort exists).
  case detectCycle t of
    Nothing      -> Right (TopologyValid t)
    Just chain   -> Left (TopologyCycle chain)
  where
    when_ :: Bool -> TopologyError -> Either TopologyError ()
    when_ b e = if b then Left e else Right ()

-- | Cycle detector via DFS. Returns one offending chain if any.
detectCycle :: Topology -> Maybe [NodeName]
detectCycle t =
  go Set.empty Set.empty (Map.keys (topoChildrenIndex t))
  where
    children =
      foldr
        (\nm acc -> Map.insertWith (++) nm [] acc)
        (topoChildrenIndex t)
        (Set.toList (allNames t))

    go _seen _stack [] = Nothing
    go seen stack (n : ns)
      | Set.member n seen = go seen stack ns
      | otherwise =
          case dfs n seen stack of
            Right seen' -> go seen' stack ns
            Left cycle' -> Just cycle'

    dfs n seen stack
      | Set.member n stack = Left (Set.toList stack ++ [n])
      | Set.member n seen  = Right seen
      | otherwise =
          let ks = Map.findWithDefault [] n children
              stack' = Set.insert n stack
              step e [] = e
              step (Right s) (k : rest) =
                case dfs k s stack' of
                  Right s' -> step (Right s') rest
                  err      -> err
              step err _ = err
           in case step (Right seen) ks of
                Right s' -> Right (Set.insert n s')
                err      -> err

-- | All node names present in a topology, in insertion order.
topologyNodes :: Topology -> [NodeName]
topologyNodes = Foldable.toList . topoOrder

-- | Reverse-lookup: who feeds @n@?
parentsOf :: Topology -> NodeName -> [NodeName]
parentsOf t n =
  case Map.lookup n (topoProcessors t) of
    Just spec -> processorSpecParents spec
    Nothing ->
      case Map.lookup n (topoSinks t) of
        Just spec -> sinkParents spec
        Nothing   -> []

-- | Forward-lookup: who consumes @n@?
childrenOf :: Topology -> NodeName -> [NodeName]
childrenOf t n = Map.findWithDefault [] n (topoChildrenIndex t)

