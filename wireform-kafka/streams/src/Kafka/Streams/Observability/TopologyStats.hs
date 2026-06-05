{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Observability.TopologyStats
-- Description : Structural metrics about a built topology
--
-- Cheap, pure structural statistics over a validated 'Topo.Topology':
-- node counts by role, store counts, edge count, distinct
-- source/sink topics, and the longest source-to-sink path
-- (a proxy for processing depth). Useful for capacity planning,
-- CI guardrails ("this topology didn't unexpectedly balloon"), and
-- dashboards.
--
-- This complements "Kafka.Streams.Observability.Topology", which
-- emits the full DAG; here we reduce it to a handful of numbers.
module Kafka.Streams.Observability.TopologyStats
  ( TopologyStats (..)
  , topologyStats
  , topologyStatsJson
  , renderTopologyStats
  ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import qualified Kafka.Streams.State.Store as Store
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (NodeName)

----------------------------------------------------------------------
-- Stats
----------------------------------------------------------------------

-- | Structural counts for a topology.
data TopologyStats = TopologyStats
  { statSources      :: !Int
  , statProcessors   :: !Int
  , statSinks        :: !Int
  , statStores       :: !Int
  , statGlobalStores :: !Int
  , statLoggedStores :: !Int
    -- ^ Stores with changelog logging enabled.
  , statEdges        :: !Int
  , statMaxDepth     :: !Int
    -- ^ Number of nodes on the longest path from any source.
  , statSourceTopics :: !Int
    -- ^ Distinct input topics across all sources.
  , statSinkTopics   :: !Int
    -- ^ Distinct output topics across all sinks.
  } deriving stock (Eq, Show)

-- | Compute structural statistics for a topology.
topologyStats :: Topo.Topology -> TopologyStats
topologyStats topo =
  TopologyStats
    { statSources      = Map.size (Topo.topoSources topo)
    , statProcessors   = Map.size (Topo.topoProcessors topo)
    , statSinks        = Map.size (Topo.topoSinks topo)
    , statStores       = Map.size stores
    , statGlobalStores = Set.size (Topo.topoGlobalStores topo)
    , statLoggedStores = loggedCount
    , statEdges        = edgeCount
    , statMaxDepth     = maxDepth
    , statSourceTopics = Set.size sourceTopicSet
    , statSinkTopics   = Set.size sinkTopicSet
    }
  where
    stores = Topo.topoStores topo

    loggedCount =
      Map.foldr
        (\b acc -> if builderLoggingEnabled b then acc + 1 else acc)
        0 stores

    edgeCount =
      List.foldl'
        (\acc n -> acc + length (Topo.childrenOf topo n))
        0 allNodes

    allNodes = foldr (:) [] (Topo.topoOrder topo)

    sourceTopicSet =
      Map.foldr
        (\s acc -> List.foldl' (flip Set.insert) acc (Topo.sourceTopics s))
        Set.empty (Topo.topoSources topo)

    sinkTopicSet =
      Map.foldr
        (\s acc -> Set.insert (Topo.sinkTopic s) acc)
        Set.empty (Topo.topoSinks topo)

    maxDepth =
      Map.foldr max 0 (nodeDepths topo)

-- | Longest path (in node count) ending at each node, memoised. For a
-- DAG this is @1 + max child depth@; sources have no predecessors so
-- their depth is the start of a path. The maximum across all nodes is
-- the topology's processing depth.
nodeDepths :: Topo.Topology -> Map NodeName Int
nodeDepths topo = List.foldl' step Map.empty allNodes
  where
    allNodes = foldr (:) [] (Topo.topoOrder topo)
    step memo n = snd (depthOf memo n)

    depthOf memo n =
      case Map.lookup n memo of
        Just d  -> (d, memo)
        Nothing ->
          let (childMax, memo') =
                List.foldl' visit (0, memo) (Topo.childrenOf topo n)
              d = 1 + childMax
          in (d, Map.insert n d memo')

    visit (acc, memo) c =
      let (dc, memo') = depthOf memo c
      in (max acc dc, memo')

----------------------------------------------------------------------
-- Rendering
----------------------------------------------------------------------

-- | Render stats as a versioned JSON object.
topologyStatsJson :: TopologyStats -> A.Value
topologyStatsJson s = A.object
  [ "version"      .= (1 :: Int)
  , "sources"      .= statSources s
  , "processors"   .= statProcessors s
  , "sinks"        .= statSinks s
  , "stores"       .= statStores s
  , "globalStores" .= statGlobalStores s
  , "loggedStores" .= statLoggedStores s
  , "edges"        .= statEdges s
  , "maxDepth"     .= statMaxDepth s
  , "sourceTopics" .= statSourceTopics s
  , "sinkTopics"   .= statSinkTopics s
  ]

-- | Render stats as a short human-readable block.
renderTopologyStats :: TopologyStats -> Text
renderTopologyStats s =
  T.unlines
    [ line "sources"       (statSources s)
    , line "processors"    (statProcessors s)
    , line "sinks"         (statSinks s)
    , line "stores"        (statStores s)
    , line "globalStores"  (statGlobalStores s)
    , line "loggedStores"  (statLoggedStores s)
    , line "edges"         (statEdges s)
    , line "maxDepth"      (statMaxDepth s)
    , line "sourceTopics"  (statSourceTopics s)
    , line "sinkTopics"    (statSinkTopics s)
    ]
  where
    line label n = T.justifyLeft 14 ' ' (label <> ":") <> T.pack (show n)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

builderLoggingEnabled :: Topo.AnyStoreBuilder -> Bool
builderLoggingEnabled = \case
  Topo.AsKeyValueBuilder b -> Store.loggingEnabled (Store.sbKvLogging b)
  Topo.AsWindowBuilder   b -> Store.loggingEnabled (Store.sbWLogging  b)
  Topo.AsSessionBuilder  b -> Store.loggingEnabled (Store.sbSLogging  b)
  Topo.AsRawBuilder      b -> Store.loggingEnabled (Store.sbLogging   b)
