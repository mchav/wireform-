{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Topology.Free.Graphviz
-- Description : Graphviz DOT visualisers for topologies
--
-- Renders a 'Topology' (AST) or a compiled
-- 'Kafka.Streams.Topology.Topology' (graph) as a
-- <https://graphviz.org/doc/info/lang.html DOT> string, ready to
-- pipe into @dot@ for visualisation:
--
-- @
-- ghci> topo <- F.buildTopologyFrom myTopology
-- ghci> Text.IO.writeFile \"topo.dot\" (topologyDot topo)
-- $ dot -Tsvg topo.dot -o topo.svg
-- @
--
-- == Two flavours
--
-- ['topologyDot']
--   Renders the compiled imperative 'Topo.Topology' graph —
--   the same shape Java's @TopologyDescription@ produces.
--   Sources are rounded blue boxes, processors are grey boxes,
--   sinks are pink inverted trapeziums, and state stores are
--   yellow cylinders with dashed owner edges. This is the view
--   that matches what the Kafka runtime sees and what task
--   assignment / repartition decisions are made from.
--
-- ['astDot']
--   Renders the 'Topology' AST as its constructor tree:
--   'Compose' as the spine, 'Fanout' / 'Parallel' / 'Fork' as
--   diamond branches, leaf primitives as ovals. Useful for
--   debugging the optimiser (does the right-associated chain
--   look the way you expected?) and for confirming that
--   rewrite passes produced the AST shape your tests
--   asserted.
--
-- == Output stability
--
-- The DOT output is intended for /visualisation/, not for
-- parsing. Node IDs in 'astDot' depend on traversal order;
-- 'topologyDot' uses the topology's own 'NodeName's so its
-- IDs are stable across compilations of the same AST. The
-- attribute set on each node (colour, shape, label) may
-- change between releases — use a small layout-renderer
-- script in your tooling if you need rendering stability.
module Kafka.Streams.Topology.Free.Graphviz
  ( -- * Compiled topology
    topologyDot
  , topologyDotWith

    -- * AST
  , astDot
  , astDotWith

    -- * Configuration
  , DotConfig (..)
  , defaultDotConfig
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import qualified Kafka.Streams.State.Store as Store
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Topology.Free
  ( SplitBranch (..)
  , Topology (..)
  )
import Kafka.Streams.Types (NodeName, TopicName, unNodeName, unTopicName)

----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

-- | Rendering options. Pass to 'topologyDotWith' \/ 'astDotWith';
-- use 'topologyDot' \/ 'astDot' for the defaults.
data DotConfig = DotConfig
  { -- | Layout direction. Common values: @"TB"@ (top→bottom),
    -- @"LR"@ (left→right), @"BT"@, @"RL"@.
    dotRankDir       :: !Text

    -- | Show state stores as their own nodes with dashed
    -- "owns" edges from processors. Off by default for AST
    -- output (stores aren't part of the AST); on by default
    -- for compiled-topology output.
  , dotShowStores    :: !Bool

    -- | Show source / sink topic names in the label.
  , dotShowTopics    :: !Bool

    -- | Fill colour for source nodes (Graphviz color names
    -- and @#RRGGBB@ both accepted).
  , dotSourceColor   :: !Text

    -- | Fill colour for sink nodes.
  , dotSinkColor     :: !Text

    -- | Fill colour for processor nodes.
  , dotProcessorColor :: !Text

    -- | Fill colour for state-store nodes.
  , dotStoreColor    :: !Text

    -- | Fill colour for structural AST nodes ('Compose',
    -- 'Fanout', 'Parallel', etc.). Only relevant for
    -- 'astDot' / 'astDotWith'.
  , dotStructColor   :: !Text
  }

-- | Sensible defaults: top-to-bottom layout, stores visible,
-- topic names shown, pastel colour palette.
defaultDotConfig :: DotConfig
defaultDotConfig = DotConfig
  { dotRankDir        = "TB"
  , dotShowStores     = True
  , dotShowTopics     = True
  , dotSourceColor    = "#bcd9ff"     -- light blue
  , dotSinkColor      = "#ffc1cc"     -- light pink
  , dotProcessorColor = "#e6e6e6"     -- light grey
  , dotStoreColor     = "#fff3a8"     -- light yellow
  , dotStructColor    = "#d6e8d6"     -- pale green
  }

----------------------------------------------------------------------
-- Compiled topology -> DOT
----------------------------------------------------------------------

-- | Render a compiled 'Topo.Topology' as a Graphviz DOT graph
-- using the default configuration ('defaultDotConfig').
--
-- The graph contains one node per source / processor / sink
-- and (when 'dotShowStores' is on) one node per state store.
-- Edges follow the topology's @parent → child@ relationships;
-- state-store ownership is shown as dashed edges with an
-- @odot@ arrowhead.
topologyDot :: Topo.Topology -> Text
topologyDot = topologyDotWith defaultDotConfig

-- | 'topologyDot' with an explicit 'DotConfig'.
topologyDotWith :: DotConfig -> Topo.Topology -> Text
topologyDotWith cfg t = render $
     "digraph topology {\n"
  <> "  rankdir=" <> tb (dotRankDir cfg) <> ";\n"
  <> "  node [fontname=\"Helvetica\"];\n"
  <> "  // Sources\n"
  <> foldMapMap (sourceNode cfg) (Topo.topoSources t)
  <> "  // Processors\n"
  <> foldMapMap (procNode cfg) (Topo.topoProcessors t)
  <> "  // Sinks\n"
  <> foldMapMap (sinkNode cfg) (Topo.topoSinks t)
  <> (if dotShowStores cfg
        then  "  // State stores\n"
           <> foldMapMap (storeNode cfg) (Topo.topoStores t)
        else mempty)
  <> "  // Edges (parent -> child)\n"
  <> foldMapMap (\nm spec -> mconcat
        [ edge (Topo.processorSpecParents spec) nm | _ <- [()] ])
       (Topo.topoProcessors t)
  <> foldMapMap (\nm spec -> mconcat
        [ edge (Topo.sinkParents spec) nm | _ <- [()] ])
       (Topo.topoSinks t)
  <> (if dotShowStores cfg
        then  "  // Store ownership\n"
           <> foldMapMap (storeEdges cfg)
                         (Topo.topoStoreOwners t)
        else mempty)
  <> "}\n"

sourceNode :: DotConfig -> NodeName -> Topo.SourceSpec -> TB.Builder
sourceNode cfg nm spec =
  let !label = if dotShowTopics cfg
                 then "SOURCE\n" <> unNodeName nm <> "\n"
                       <> T.intercalate ","
                            (map unTopicName (Topo.sourceTopics spec))
                 else "SOURCE\n" <> unNodeName nm
   in "  " <> nodeId nm
        <> " [shape=box, style=\"filled,rounded\", "
        <> "fillcolor=\"" <> tb (dotSourceColor cfg) <> "\", "
        <> "label=" <> dotString label
        <> "];\n"

procNode :: DotConfig -> NodeName -> Topo.ProcessorSpec -> TB.Builder
procNode cfg nm _spec =
  "  " <> nodeId nm
    <> " [shape=box, style=filled, "
    <> "fillcolor=\"" <> tb (dotProcessorColor cfg) <> "\", "
    <> "label=" <> dotString (unNodeName nm)
    <> "];\n"

sinkNode :: DotConfig -> NodeName -> Topo.SinkSpec -> TB.Builder
sinkNode cfg nm spec =
  let !label = if dotShowTopics cfg
                 then "SINK\n" <> unNodeName nm
                       <> "\ntopic: " <> unTopicName (Topo.sinkTopic spec)
                 else "SINK\n" <> unNodeName nm
   in "  " <> nodeId nm
        <> " [shape=invtrapezium, style=filled, "
        <> "fillcolor=\"" <> tb (dotSinkColor cfg) <> "\", "
        <> "label=" <> dotString label
        <> "];\n"

storeNode
  :: DotConfig -> Store.StoreName -> Topo.AnyStoreBuilder -> TB.Builder
storeNode cfg sn b =
  let !kind = case b of
        Topo.AsKeyValueBuilder _ -> "KV"
        Topo.AsWindowBuilder   _ -> "Window"
        Topo.AsSessionBuilder  _ -> "Session"
        Topo.AsRawBuilder      _ -> "Raw"
      !label = kind <> " store\n" <> Store.unStoreName sn
   in "  " <> storeId sn
        <> " [shape=cylinder, style=\"filled,dashed\", "
        <> "fillcolor=\"" <> tb (dotStoreColor cfg) <> "\", "
        <> "label=" <> dotString label
        <> "];\n"

-- | Edges from a parents list to one child.
edge :: [NodeName] -> NodeName -> TB.Builder
edge parents child =
  foldMap (\p -> "  " <> nodeId p <> " -> " <> nodeId child <> ";\n")
          parents

-- | Owner-processor → state-store edges (dashed, odot arrowhead).
storeEdges
  :: DotConfig -> Store.StoreName -> [NodeName] -> TB.Builder
storeEdges _cfg sn owners =
  foldMap
    (\ow -> "  " <> nodeId ow <> " -> " <> storeId sn
              <> " [style=dashed, arrowhead=odot];\n")
    owners

----------------------------------------------------------------------
-- AST -> DOT
----------------------------------------------------------------------

-- | Render a 'Topology' AST as a DOT graph using the default
-- configuration ('defaultDotConfig'). The graph is the
-- constructor tree of the AST — useful for debugging the
-- optimiser or for confirming the AST shape a test built.
--
-- Note: AST node IDs are traversal-order-dependent. The output
-- is stable for a given AST value but unrelated ASTs may
-- collide on the same numeric prefix.
astDot :: Topology i o -> Text
astDot = astDotWith defaultDotConfig
  { dotShowStores = False  -- AST has no stores to show
  }

-- | 'astDot' with an explicit 'DotConfig'.
astDotWith :: forall i o. DotConfig -> Topology i o -> Text
astDotWith cfg topo =
  let (_, builder) = walkTopAst cfg 0 topo
   in render $
        "digraph ast {\n"
        <> "  rankdir=" <> tb (dotRankDir cfg) <> ";\n"
        <> "  node [fontname=\"Helvetica\"];\n"
        <> builder
        <> "}\n"

-- | Result of a walk: @(nextId, builder)@. The builder
-- accumulates node and edge declarations for the visited
-- sub-tree.
type WalkResult = (Int, TB.Builder)

-- | Walk the AST starting at the supplied ID and emit the
-- corresponding DOT fragment. Returns the next-free ID and
-- the fragment.
walkTopAst :: forall i o. DotConfig -> Int -> Topology i o -> WalkResult
walkTopAst cfg = go
  where
    go :: forall a b. Int -> Topology a b -> WalkResult
    -- Category / Arrow
    go i Id              = leaf i "Id" "ellipse"
    go i (Compose g f)   =
      let !me = i
          (i1, b1) = go (i + 1) f   -- emit children first
          (i2, b2) = go i1 g
       in struct cfg me "Compose" [(i + 1, "L"), (i1, "R")] b1 b2 i2
    go i (Arr _)         = leaf i "Arr" "ellipse"
    go i (First t)       = oneChild i "First"  t
    go i (Second t)      = oneChild i "Second" t
    go i (Parallel p q)  = twoChild i "Parallel" p q
    go i (Fanout p q)    = twoChild i "Fanout"   p q
    go i (LeftT t)       = oneChild i "LeftT"  t
    go i (RightT t)      = oneChild i "RightT" t
    go i (Plus p q)      = twoChild i "Plus"   p q
    go i (Fanin p q)     = twoChild i "Fanin"  p q
    -- Lineage
    go i Fork            = leaf i "Fork" "diamond"
    go i (ForkN ts)      =
      let !me = i
          (iN, bN) = foldl
            (\(curI, bAcc) t ->
                let (ci, cb) = go curI t
                    !ed = "  " <> nodeIdInt me <> " -> "
                            <> nodeIdInt curI <> ";\n"
                 in (ci, bAcc <> cb <> ed))
            (i + 1, mempty)
            (NE.toList ts)
          !meDef = leafNode cfg me "ForkN" "diamond"
       in (iN, meDef <> bN)
    go i (Tap t)         = oneChild i "Tap" t
    go i (Split bs md)   =
      let !lab = "Split\n["
            <> T.intercalate "," (map sbName bs)
            <> maybe "" (\d -> "+default=" <> d) md
            <> "]"
       in leaf i lab "diamond"
    -- Sources
    go i (Source tn _)         = leaf i ("Source\n" <> showTopic tn) "box"
    go i (SourceMulti tns _)
      = leaf i
          ("SourceMulti\n" <>
             T.intercalate ","
               (map showTopic (NE.toList tns)))
          "box"
    go i (TableSource tn _ _)  = leaf i ("TableSource\n"  <> showTopic tn) "box"
    go i (GlobalSource tn _ _) = leaf i ("GlobalSource\n" <> showTopic tn) "box"
    -- Sinks
    go i (Sink tn _)           = leaf i ("Sink\n" <> showTopic tn) "invtrapezium"
    go i (SinkExtracted _ _)   = leaf i "SinkExtracted" "invtrapezium"
    go i (Through tn _)        = leaf i ("Through\n" <> showTopic tn) "box"
    -- Monad bind
    go i (Bind t _)            =
      let !me = i
          (i1, b1) = go (i + 1) t
          !meDef  = leafNode cfg me "Bind\n(opaque continuation)" "octagon"
          !ed     = "  " <> nodeIdInt me <> " -> "
                      <> nodeIdInt (i + 1) <> " [label=\"\\>\\>=\"];\n"
       in (i1, meDef <> b1 <> ed)
    -- Stateless
    go i (MapValues _)         = leaf i "MapValues"         "ellipse"
    go i (MapValuesM _)        = leaf i "MapValuesM"        "ellipse"
    go i (MapKeyValue _)       = leaf i "MapKeyValue"       "ellipse"
    go i (MapKeyValueM _)      = leaf i "MapKeyValueM"      "ellipse"
    go i (Filter _)            = leaf i "Filter"            "ellipse"
    go i (FilterNot _)         = leaf i "FilterNot"         "ellipse"
    go i (FlatMapValues _)     = leaf i "FlatMapValues"     "ellipse"
    go i (FlatMapKeyValue _)   = leaf i "FlatMapKeyValue"   "ellipse"
    go i (Peek _)              = leaf i "Peek"              "ellipse"
    go i (Foreach _)           = leaf i "Foreach"           "invtrapezium"
    go i (SelectKey _)         = leaf i "SelectKey"         "ellipse"
    go i Values                = leaf i "Values"            "ellipse"
    go i (Print nm _)          = leaf i ("Print\n" <> nm)   "invtrapezium"
    -- Composition
    go i Merge                 = leaf i "Merge"             "ellipse"
    go i MergeAll              = leaf i "MergeAll"          "ellipse"
    go i (Branch ps)
      = leaf i ("Branch\n" <> T.pack (show (length ps)) <> " ways") "diamond"
    -- Conversions
    go i (ToTableT _)          = leaf i "ToTable"           "box"
    go i ToStream              = leaf i "ToStream"          "ellipse"
    go i (Repartition pfx)     = leaf i ("Repartition\n" <> pfx) "box"
    go i (RepartitionWith _)   = leaf i "RepartitionWith"   "box"
    -- Aggregation
    go i (GroupByKey _)        = leaf i "GroupByKey"        "ellipse"
    go i (GroupBy _ _)         = leaf i "GroupBy"           "ellipse"
    go i (Count _)             = leaf i "Count"             "box3d"
    go i (Reduce _ _)          = leaf i "Reduce"            "box3d"
    go i (Aggregate _ _ _)     = leaf i "Aggregate"         "box3d"
    -- Windowed
    go i (WindowedByTime _)        = leaf i "WindowedByTime"    "ellipse"
    go i (WindowedBySession _)     = leaf i "WindowedBySession" "ellipse"
    go i (CountWindowed _)         = leaf i "CountWindowed"     "box3d"
    go i (ReduceWindowed _ _)      = leaf i "ReduceWindowed"    "box3d"
    go i (AggregateWindowed _ _ _) = leaf i "AggregateWindowed" "box3d"
    go i (CountSessionWindowed _)  = leaf i "CountSessionWindowed" "box3d"
    go i (AggregateSessionWindowed _ _ _ _)
                                  = leaf i "AggregateSessionWindowed" "box3d"
    -- KGroupedTable
    go i (GroupTableBy _ _)        = leaf i "GroupTableBy"        "ellipse"
    go i (CountKGroupedTable _)    = leaf i "CountKGroupedTable"  "box3d"
    go i (ReduceKGroupedTable _ _ _) = leaf i "ReduceKGroupedTable" "box3d"
    go i (AggregateKGroupedTable _ _ _ _)
                                  = leaf i "AggregateKGroupedTable" "box3d"
    -- Cogroup
    go i (Cogroup _)               = leaf i "Cogroup"             "ellipse"
    go i (AddCogrouped _)          = leaf i "AddCogrouped"        "ellipse"
    go i (AggregateCogrouped _ _)  = leaf i "AggregateCogrouped"  "box3d"
    -- Joins
    go i (StreamTableJoin _ _)        = leaf i "StreamTableJoin"        "hexagon"
    go i (StreamTableLeftJoin _ _)    = leaf i "StreamTableLeftJoin"    "hexagon"
    go i (StreamStreamJoin _ _ _)     = leaf i "StreamStreamJoin"       "hexagon"
    go i (StreamStreamLeftJoin _ _ _) = leaf i "StreamStreamLeftJoin"   "hexagon"
    go i (StreamStreamOuterJoin _ _ _) = leaf i "StreamStreamOuterJoin"  "hexagon"
    go i (TableTableJoin _ _)         = leaf i "TableTableJoin"         "hexagon"
    go i (TableTableLeftJoin _ _)     = leaf i "TableTableLeftJoin"     "hexagon"
    go i (TableTableOuterJoin _ _)    = leaf i "TableTableOuterJoin"    "hexagon"
    go i (ForeignKeyJoin _ _ _)       = leaf i "ForeignKeyJoin"         "hexagon"
    go i (LeftForeignKeyJoin _ _ _)   = leaf i "LeftForeignKeyJoin"     "hexagon"
    go i (StreamGlobalTableJoin _ _)  = leaf i "StreamGlobalTableJoin"  "hexagon"
    go i (StreamGlobalTableLeftJoin _ _) = leaf i "StreamGlobalTableLeftJoin" "hexagon"
    -- KTable
    go i (FilterTable _ _)             = leaf i "FilterTable"           "ellipse"
    go i (FilterNotTable _ _)          = leaf i "FilterNotTable"        "ellipse"
    go i (MapValuesTable _ _)          = leaf i "MapValuesTable"        "ellipse"
    go i (TransformValuesTable nm _ _ _)
      = leaf i ("TransformValuesTable\n" <> nm) "box"
    -- Suppress
    go i (SuppressUntilTimeLimit _)    = leaf i "SuppressUntilTimeLimit" "ellipse"
    go i (SuppressWindowedKS _ _)      = leaf i "SuppressWindowed"       "ellipse"
    -- Processor API
    go i (ProcessStream nm _ _)        = leaf i ("ProcessStream\n" <> nm) "box"
    go i (ProcessValuesStream nm _ _ _)
      = leaf i ("ProcessValuesStream\n" <> nm) "box"
    go i (TransformValuesStreamT nm _ _ _)
      = leaf i ("TransformValuesStream\n" <> nm) "box"
    go i (WithStateStoreKV b _)
      = leaf i ("WithStateStoreKV\n" <> Store.unStoreName (Store.sbKvName b))
              "cylinder"
    go i (WithStateStoreW b _)
      = leaf i ("WithStateStoreW\n" <> Store.unStoreName (Store.sbWName b))
              "cylinder"
    go i (WithStateStoreS b _)
      = leaf i ("WithStateStoreS\n" <> Store.unStoreName (Store.sbSName b))
              "cylinder"
    go i (ProcessWithStateStoreKV nm b _)
      = leaf i ("ProcessWithStateStoreKV\n" <> nm <> "\n"
                  <> Store.unStoreName (Store.sbKvName b)) "box"
    go i (ProcessWithStateStoreW nm b _)
      = leaf i ("ProcessWithStateStoreW\n" <> nm <> "\n"
                  <> Store.unStoreName (Store.sbWName b)) "box"
    go i (ProcessWithStateStoreS nm b _)
      = leaf i ("ProcessWithStateStoreS\n" <> nm <> "\n"
                  <> Store.unStoreName (Store.sbSName b)) "box"
    -- Escape
    go i (Lifted nm _)
      = leaf i ("Lifted\n" <> nm) "octagon"

    -- Helpers (closed over cfg).
    leaf :: Int -> Text -> Text -> WalkResult
    leaf i label shape = (i + 1, leafNode cfg i label shape)

    oneChild
      :: forall a b. Int -> Text -> Topology a b -> WalkResult
    oneChild i lab t =
      let !me = i
          (i1, b1) = go (i + 1) t
          !meDef  = leafNode cfg me lab "diamond"
          !ed     = "  " <> nodeIdInt me <> " -> "
                      <> nodeIdInt (i + 1) <> ";\n"
       in (i1, meDef <> b1 <> ed)

    twoChild
      :: forall a b c d
       . Int -> Text -> Topology a b -> Topology c d -> WalkResult
    twoChild i lab p q =
      let !me = i
          (i1, b1) = go (i + 1) p
          (i2, b2) = go i1 q
          !meDef  = leafNode cfg me lab "diamond"
          !ed1    = "  " <> nodeIdInt me <> " -> "
                      <> nodeIdInt (i + 1) <> ";\n"
          !ed2    = "  " <> nodeIdInt me <> " -> "
                      <> nodeIdInt i1 <> ";\n"
       in (i2, meDef <> b1 <> b2 <> ed1 <> ed2)

leafNode :: DotConfig -> Int -> Text -> Text -> TB.Builder
leafNode cfg i lab shape =
  "  " <> nodeIdInt i
    <> " [shape=" <> tb shape <> ", style=filled, "
    <> "fillcolor=\"" <> tb (dotStructColor cfg) <> "\", "
    <> "label=" <> dotString lab
    <> "];\n"

-- | Emit a structural node and its two-child edges. Useful for
-- compose-like nodes where the order matters (L/R labels on
-- edges). Currently unused — 'twoChild' covers the common case
-- — but kept for symmetry.
struct
  :: DotConfig
  -> Int             -- ^ node id
  -> Text            -- ^ label
  -> [(Int, Text)]   -- ^ children (id, edge-label)
  -> TB.Builder      -- ^ child fragments concatenated
  -> TB.Builder      -- ^ child fragments concatenated (second)
  -> Int
  -> WalkResult
struct cfg me lab children fragL fragR nextI =
  let !meDef = leafNode cfg me lab "diamond"
      !edges = foldMap
        (\(c, el) ->
           "  " <> nodeIdInt me <> " -> " <> nodeIdInt c
             <> " [label=\"" <> tb el <> "\"];\n")
        children
   in (nextI, meDef <> fragL <> fragR <> edges)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Quote and escape a string for DOT's @label="…"@ attribute.
-- Handles backslashes, double quotes, and newlines.
dotString :: Text -> TB.Builder
dotString s =
  "\"" <> tb (T.concatMap esc s) <> "\""
  where
    esc :: Char -> Text
    esc '\\' = "\\\\"
    esc '"'  = "\\\""
    esc '\n' = "\\n"
    esc c    = T.singleton c

-- | Quote a 'NodeName' for use as a DOT identifier.
nodeId :: NodeName -> TB.Builder
nodeId nm = "\"" <> tb (unNodeName nm) <> "\""

-- | DOT identifier for an integer (AST node).
nodeIdInt :: Int -> TB.Builder
nodeIdInt = TB.fromString . ('n' :) . show

-- | Quote a 'Store.StoreName' for use as a DOT identifier. We
-- prefix with @"store:"@ so it can't collide with a 'NodeName'
-- that happens to share the same text.
storeId :: Store.StoreName -> TB.Builder
storeId sn = "\"store:" <> tb (Store.unStoreName sn) <> "\""

-- | Show a 'TopicName' without the @TopicName "@…@"@ wrapper.
showTopic :: TopicName -> Text
showTopic = unTopicName

tb :: Text -> TB.Builder
tb = TB.fromText

render :: TB.Builder -> Text
render = TL.toStrict . TB.toLazyText

-- | 'foldMap' over a 'Map.Map' but giving the callback the
-- key /and/ the value. Used a lot in the topology renderer.
foldMapMap :: Monoid b => (k -> a -> b) -> Map.Map k a -> b
foldMapMap f = Map.foldrWithKey (\k a !acc -> f k a <> acc) mempty
