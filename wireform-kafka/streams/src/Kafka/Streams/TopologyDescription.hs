{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.TopologyDescription
-- Description : Structured + textual introspection of a 'Topology'
--
-- Mirrors @org.apache.kafka.streams.TopologyDescription@ — a tree of
-- 'Subtopology' nodes (each with sources, processors, sinks) plus
-- 'GlobalStore' entries. We keep the structure deliberately simple
-- (no live processor instances, no serdes — just the graph shape and
-- the names that the runtime would reference at startup).
--
-- 'pretty' renders the description in roughly the same shape Java's
-- toString does:
--
-- @
-- Topologies:
--    Sub-topology: 0
--      Source: KSTREAM-SOURCE-0 (topics: [in])
--        --> KSTREAM-MAPVALUES-1
--      Processor: KSTREAM-MAPVALUES-1 (stores: [])
--        \<-- KSTREAM-SOURCE-0
--        --> KSTREAM-SINK-2
--      Sink: KSTREAM-SINK-2 (topic: out)
--        \<-- KSTREAM-MAPVALUES-1
-- @
module Kafka.Streams.TopologyDescription
  ( -- * Description
    TopologyDescription (..)
  , Subtopology (..)
  , NodeDesc (..)
    -- * Build
  , describeTopology
    -- * Render
  , pretty
  ) where

import Data.List (foldl', sort)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Topology (NodeName (..))
import Kafka.Streams.Types (TopicName, unTopicName)
import Kafka.Streams.State.Store (StoreName, unStoreName)

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

data TopologyDescription = TopologyDescription
  { tdSubtopologies :: ![Subtopology]
  , tdStores        :: ![StoreName]
    -- ^ All stores defined in the topology, in any subtopology.
  }
  deriving stock (Eq, Show, Generic)

data Subtopology = Subtopology
  { stIndex :: !Int
  , stNodes :: ![NodeDesc]
  }
  deriving stock (Eq, Show, Generic)

-- | One node in the description: source, processor, or sink.
data NodeDesc
  = SourceDesc
      { ndName     :: !NodeName
      , ndChildren :: ![NodeName]
      , ndTopics   :: ![TopicName]
      }
  | ProcessorDesc
      { ndName     :: !NodeName
      , ndParents  :: ![NodeName]
      , ndChildren :: ![NodeName]
      , ndStores   :: ![StoreName]
      }
  | SinkDesc
      { ndName     :: !NodeName
      , ndParents  :: ![NodeName]
      , ndTopic    :: !TopicName
      }
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- describeTopology
----------------------------------------------------------------------

-- | Build a 'TopologyDescription' from a 'Topology'.
--
-- Sub-topology partitioning: nodes are grouped into sub-topologies
-- by undirected connectivity over the parent/child graph. Sources
-- and sinks that share an internal /repartition topic/ (i.e. a
-- topic the topology both produces to and consumes from) are NOT
-- connected — that's the boundary between sub-topologies.
describeTopology :: Topo.Topology -> TopologyDescription
describeTopology topo =
  let nodes =
          [ srcDesc s
          | s <- Map.elems (Topo.topoSources topo)
          ]
        ++ [ procDesc p
           | p <- Map.elems (Topo.topoProcessors topo)
           ]
        ++ [ sinkDesc k
           | k <- Map.elems (Topo.topoSinks topo)
           ]
      compMap   = computeSubtopologies topo
      grouped   = Map.fromListWith (<>)
        [ (Map.findWithDefault 0 (ndName nd) compMap, [nd])
        | nd <- nodes
        ]
      -- 'topoOrder' is a 'Seq'; pull it through 'Foldable.toList'
      -- once and reuse it for every sub-topology so we don't pay
      -- the conversion per-group.
      !insertionOrderL = Foldable.toList (Topo.topoOrder topo)
      orderedSubs =
        [ Subtopology
            { stIndex = idx
            , stNodes = orderNodes insertionOrderL nds
            }
        | (idx, nds) <- Map.toAscList grouped
        ]
   in TopologyDescription
        { tdSubtopologies = orderedSubs
        , tdStores        = sort (Map.keys (Topo.topoStores topo))
        }
  where
    childrenOf nm = sort (Topo.childrenOf topo nm)
    parentsOf nm  = sort (Topo.parentsOf topo nm)

    srcDesc spec =
      SourceDesc
        { ndName     = Topo.sourceName spec
        , ndChildren = childrenOf (Topo.sourceName spec)
        , ndTopics   = Topo.sourceTopics spec
        }
    procDesc spec =
      ProcessorDesc
        { ndName     = Topo.processorSpecName spec
        , ndParents  = parentsOf (Topo.processorSpecName spec)
        , ndChildren = childrenOf (Topo.processorSpecName spec)
        , ndStores   = sort (Topo.processorSpecStores spec)
        }
    sinkDesc spec =
      SinkDesc
        { ndName    = Topo.sinkName spec
        , ndParents = parentsOf (Topo.sinkName spec)
        , ndTopic   = Topo.sinkTopic spec
        }

----------------------------------------------------------------------
-- Sub-topology partitioning
----------------------------------------------------------------------

-- | Compute, for every node, which sub-topology index it belongs to.
-- Sub-topologies are connected components of the undirected
-- parent/child graph; nodes connected only by an /internal topic/
-- (sink + source pair on the same topic) are NOT considered
-- connected.
computeSubtopologies :: Topo.Topology -> Map.Map NodeName Int
computeSubtopologies topo =
  -- Build parent->children adjacency (just the regular graph).
  -- Walk in insertion order, assigning each unvisited node a fresh
  -- component index and propagating it to all nodes reachable via
  -- /undirected/ parent/child edges.
  let allNodes = Topo.topoOrder topo
      visited = foldl assignComponent Map.empty allNodes
   in visited
  where
    assignComponent acc n
      | Map.member n acc = acc
      | otherwise =
          let !idx = if Map.null acc
                       then 0
                       else 1 + maximum (Map.elems acc)
           in spread idx acc n

    spread !idx acc n
      | Map.member n acc = acc
      | otherwise =
          let acc1     = Map.insert n idx acc
              children = Topo.childrenOf topo n
              parents  = Topo.parentsOf  topo n
              neighbours = children ++ parents
           in foldl (spread idx) acc1 neighbours

-- | Order @nodes@ by their position in @insertionOrder@.
orderNodes :: [NodeName] -> [NodeDesc] -> [NodeDesc]
orderNodes insertionOrder nodes =
  let !rank = Map.fromList (zip insertionOrder [(0 :: Int) ..])
      key nd = Map.findWithDefault maxBound (ndName nd) rank
   in foldr (insertBy key) [] nodes
  where
    insertBy keyF x [] = [x]
    insertBy keyF x (y : ys)
      | keyF x <= keyF y = x : y : ys
      | otherwise       = y : insertBy keyF x ys

----------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------

-- | Render a 'TopologyDescription' as multi-line text suitable for
-- diagnostics / logs.
pretty :: TopologyDescription -> Text
pretty td =
  T.intercalate "\n"
    $ ["Topologies:"]
    <> concatMap renderSub (tdSubtopologies td)
    <> renderStores (tdStores td)
  where
    renderSub st =
      [ "   Sub-topology: " <> T.pack (show (stIndex st)) ]
      <> concatMap renderNode (stNodes st)

    renderNode nd = case nd of
      SourceDesc nm kids topics ->
        [ "    Source: " <> unNodeName nm
            <> " (topics: " <> renderList (map unTopicName topics) <> ")"
        ] <> renderArrows Nothing kids
      ProcessorDesc nm parents kids stores ->
        [ "    Processor: " <> unNodeName nm
            <> " (stores: " <> renderList (map unStoreName stores) <> ")"
        ] <> renderArrows (Just parents) kids
      SinkDesc nm parents topic ->
        [ "    Sink: " <> unNodeName nm
            <> " (topic: " <> unTopicName topic <> ")"
        ] <> renderArrows (Just parents) []

    renderArrows mParents kids =
      let parentLines = case mParents of
            Just ps | not (null ps) ->
              ["      <-- " <> T.intercalate ", " (map unNodeName ps)]
            _ -> []
          childLines = case kids of
            [] -> []
            _  -> ["      --> " <> T.intercalate ", " (map unNodeName kids)]
       in parentLines <> childLines

    renderList xs = "[" <> T.intercalate ", " xs <> "]"

    renderStores [] = []
    renderStores ss =
      [ ""
      , "Stores:"
      ] <> [ "    " <> unStoreName s | s <- ss ]

-- 'Set' kept imported because future descriptions of GlobalStore
-- entries will use it for unique sets; trivial for now.
_keepSet :: Set Int -> Int
_keepSet = Set.size

-- 'foldl' imported above, used by orderNodes; alias to silence unused.
_keepFold :: [a] -> [a]
_keepFold = foldl' (flip (:)) []