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
-- Sub-topology partitioning: in this implementation there is exactly
-- one sub-topology containing every node. A future expansion will
-- partition the topology along repartition boundaries (auto-generated
-- internal topics).
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
      sortedNodes = orderNodes (Topo.topoOrder topo) nodes
   in TopologyDescription
        { tdSubtopologies =
            [ Subtopology { stIndex = 0, stNodes = sortedNodes }
            | not (null sortedNodes)
            ]
        , tdStores =
            sort (Map.keys (Topo.topoStores topo))
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