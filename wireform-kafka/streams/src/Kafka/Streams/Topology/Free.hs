{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Topology.Free
-- Description : First-class GADT \/ free-arrow topology builder
--
-- A 'Topology' value describes a Kafka Streams topology fragment as a
-- /first-class value/. Unlike the imperative builder in
-- "Kafka.Streams.StreamsBuilder" (which mutates an 'IORef' on every
-- DSL call), a 'Topology' is a pure AST that can be inspected,
-- traversed, composed, and finally /compiled/ into the existing
-- 'Kafka.Streams.Topology.Topology' graph via 'compile'.
--
-- The type parameters @i@ and @o@ classify the /wire type/ flowing in
-- and out of the fragment. Concretely they are inhabited by the
-- handle types the DSL already uses:
--
--   * @'Kafka.Streams.KStream' k v@        — a record stream
--   * @'Kafka.Streams.KTable' k v@         — a changelog table
--   * @'Kafka.Streams.KGroupedStream' k v@ — a re-keyed stream
--   * @'Kafka.Streams.KGroupedTable' k v@  — a re-keyed table
--   * @'Kafka.Streams.GlobalKTable' k v@   — cluster-replicated table
--   * @'Kafka.Streams.TimeWindowedKStream' k v@ — time-windowed grouped stream
--   * @'Kafka.Streams.SessionWindowedKStream' k v@ — session-windowed grouped stream
--   * @'Kafka.Streams.Cogroup.CogroupedStream' k a@ — multi-source cogroup
--   * @'Data.Void.Void'@                   — an /open/ input slot
--                                             (only sources fill it)
--   * @()@                                 — a sink terminus
--   * @(a, b)@                             — two independent wires
--                                             (parallel composition)
--   * @'Either' a b@                       — a choice between two
--                                             wires
--   * @'NonEmpty' a@                       — multi-way fan-out
--   * @'Map' 'Text' ('KStream' k v)@        — named branches (KIP-418)
--
-- The DSL surface is reified as constructors. Composition uses the
-- standard category-theoretic vocabulary:
--
-- @
-- import Control.Category ((>>>))
-- import Control.Arrow    ((***), (&&&))
-- import Kafka.Streams.Topology.Free as F
--
-- normalise :: F.Topology Void ()
-- normalise =
--   F.source     \"in\"  textSerde textSerde
--     '>>>' F.mapValues T.toUpper
--     '>>>' F.filter    (\\r -> recordValue r '/=' \"\")
--     '>>>' F.sink      \"out\" textSerde textSerde
-- @
--
-- == Why a GADT?
--
-- The 'Pipeline' newtype over @Kleisli IO@ in "Kafka.Streams.Pipeline"
-- already gives us category + arrow + arrow-choice. What it doesn't
-- give is a /closed AST/ — every fragment is opaquely a function. The
-- GADT form keeps the constructors visible so the same value can be:
--
--   * pretty-printed (the upstream JVM @TopologyDescription@ idiom),
--   * statically optimised (KIP-295 repartition merging, source-KTable
--     reuse, etc — see "Kafka.Streams.Topology.Optimization"),
--   * round-tripped through the StableNames hashing in
--     "Kafka.Streams.Topology.StableNames" without forcing IO, and
--   * lawful w.r.t. the 'Category', 'Arrow', and 'ArrowChoice'
--     instances (free arrow up to the standard equations — the
--     compiler currently materialises every constructor distinctly,
--     so equational rewrites are deferred to an explicit pass).
--
-- == Lineage: 'Parallel', 'Fanout', 'Fork', 'ForkN', 'Tap', 'Split'
--
-- A /lineage/ is what the JVM topology description calls a chain of
-- nodes connected to one another in the directed graph. Two of our
-- combinators introduce a tuple shape on the wire but they mean
-- different things at the lineage level:
--
-- [@'Parallel' p q :: 'Topology' (a, c) (b, d)@]
--   Runs @p@ on the @a@-side and @q@ on the @c@-side. The two
--   subgraphs are /independent/ in the topology graph — nothing
--   connects them except the tuple shape at the call site. Use this
--   when you've brought two unrelated upstream lineages together
--   (typically before a join) and want to apply a different
--   transform to each side.
--
-- [@'Fanout' p q :: 'Topology' a (b, c)@]
--   Feeds a /single/ upstream wire to two sub-fragments. In the
--   compiled topology the upstream node has two children. Use this
--   when you want to fork one lineage into two.
--
-- For more than two siblings, see 'Fork', 'ForkN', 'Tap', and
-- 'Split':
--
-- [@'Fork' :: 'Topology' a (a, a)@]
--   Explicit duplicator. @'Fork' = 'id' '&&&' 'id'@; named for
--   clarity at call sites.
--
-- [@'ForkN' :: 'NonEmpty' ('Topology' a b) -> 'Topology' a ('NonEmpty' b)@]
--   N-way fan-out: apply each sub-pipeline to the same upstream and
--   collect the results in order. Better than nested '&&&' once you
--   have more than two branches.
--
-- [@'Tap' :: 'Topology' a () -> 'Topology' a a@]
--   Run a side-effecting sub-pipeline (typically ending in a 'Sink'
--   or 'Foreach') and pass the upstream wire through unchanged.
--   Operationally equivalent to @id '&&&' t \\>\\>\\> arr fst@ but
--   compiles cleaner and reads better.
--
-- [@'Split' :: ['SplitBranch' k v] -> 'Maybe' 'Text' -> 'Topology' ('KStream' k v) ('Map' 'Text' ('KStream' k v))@]
--   KIP-418 named branches. Each record routes to the first
--   matching branch; records that match none go to the default
--   branch if supplied, otherwise are dropped. The result is a
--   'Map' keyed by branch name, which downstream 'Arr' calls can
--   destructure.
--
-- The combinators compose, so a typical "split a source into N
-- transforms and merge a subset back together" pipeline reads
-- straightforwardly:
--
-- @
--   F.source \"in\" ks vs
--     '>>>' F.tap (F.filter isLog '>>>' F.sink \"audit\" ks vs)
--     '>>>' F.mapValues normalise
--     '>>>' F.sink \"out\" ks vs
-- @
module Kafka.Streams.Topology.Free
  ( -- * The 'Topology' GADT
    Topology (..)
  , SplitBranch (..)

    -- * Compilation
  , compile
  , compileWith
  , apply

    -- * Constants for the type signatures
  , TBuilder
  , buildTopologyFrom

    -- * Sources
  , source
  , sourceWith
  , sources
  , tableSource
  , globalTableSource

    -- * Sinks
  , sink
  , sinkWith
  , sinkExtracted
  , through

    -- * Stateless 'KStream' transforms
  , mapValues
  , mapValuesM
  , mapKeyValue
  , mapKeyValueM
  , filter
  , filterNot
  , flatMapValues
  , flatMapKeyValue
  , peek
  , foreach
  , foreachAsync
  , prints
  , selectKey
  , values

    -- * 'KStream' composition + branching
  , merge
  , mergeAll
  , branch
  , split
  , splitBranch
  , fork
  , forkN
  , tap

    -- * 'KStream' \<-\> 'KTable' conversions
  , toTable
  , toStream
  , repartition
  , repartitionWith

    -- * Grouping + aggregation
  , groupByKey
  , groupBy
  , count
  , reduce
  , aggregate

    -- * Windowed aggregation
  , windowedByTime
  , windowedBySession
  , countWindowed
  , reduceWindowed
  , aggregateWindowed
  , countSessionWindowed
  , aggregateSessionWindowed

    -- * 'KGroupedTable' (subtractor-aware aggregation)
  , groupTableBy
  , countKGroupedTable
  , reduceKGroupedTable
  , aggregateKGroupedTable

    -- * Cogroup
  , cogroup
  , addCogrouped
  , aggregateCogrouped

    -- * Joins
  , streamTableJoin
  , streamTableLeftJoin
  , streamStreamJoin
  , streamStreamLeftJoin
  , streamStreamOuterJoin
  , tableTableJoin
  , tableTableLeftJoin
  , tableTableOuterJoin
  , foreignKeyJoin
  , leftForeignKeyJoin
  , streamGlobalTableJoin
  , streamGlobalTableLeftJoin

    -- * KTable
  , filterTable
  , filterNotTable
  , mapValuesTable
  , transformValuesTable

    -- * Suppress
  , suppressUntilTimeLimit
  , suppressWindowed

    -- * Processor API
  , processStream
  , processValuesStream
  , transformValuesStream
  , withStateStoreKV
  , withStateStoreW
  , withStateStoreS

    -- * Escape hatch + introspection
  , liftIO_
  , inspect
  , prettyPrint
  ) where

import Prelude hiding (id, filter, (.))

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Kafka.Streams.Cogroup (CogroupedStream)
import qualified Kafka.Streams.Cogroup as Cog
import Kafka.Streams.Consumed
  ( Consumed
  , consumed
  , consumedExtractor
  , consumedKeySerde
  , consumedValueSerde
  )
import qualified Kafka.Streams.ForeignKeyJoin as FK
import Kafka.Streams.GlobalKTable (GlobalKTable)
import qualified Kafka.Streams.GlobalKTable as GT
import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.Joined (JoinWindows, Joined)
import qualified Kafka.Streams.KGroupedStream as KGS
import Kafka.Streams.KGroupedStream
  ( KGroupedStream
  , SessionWindowedKStream
  , TimeWindowedKStream
  )
import Kafka.Streams.KGroupedTable (KGroupedTable)
import qualified Kafka.Streams.KGroupedTable as KGT
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.KStream (KStream)
import qualified Kafka.Streams.KTable as KT
import Kafka.Streams.KTable (KTable)
import Kafka.Streams.Materialized (Materialized)
import qualified Kafka.Streams.Materialized as Mat
import Kafka.Streams.Processor (Processor)
import Kafka.Streams.Produced (Produced, produced)
import qualified Kafka.Streams.Repartitioned as Rep
import Kafka.Streams.Serde (Serde)
import qualified Kafka.Streams.SessionWindowedKStream as SWKS
import Kafka.Streams.State.Store
  ( StoreBuilderKV
  , StoreBuilderS
  , StoreBuilderW
  , StoreName
  , WindowedKey
  )
import qualified Kafka.Streams.State.Store as Store
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , buildTopology
  , freshNodeName
  , newStreamsBuilder
  , withTopology_
  )
import qualified Kafka.Streams.Suppress as Suppress
import qualified Kafka.Streams.TimeWindowedKStream as TWKS
import Kafka.Streams.Time (Duration)
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..), TopicName, topicName)
import qualified Kafka.Streams.Window as Win

----------------------------------------------------------------------
-- Splits
----------------------------------------------------------------------

-- | One named branch of a 'Split'. The 'Text' name appears as the
-- 'Data.Map.Strict.Map' key in the result. Mirrors Java's
-- @Branched.as(name).withPredicate(p)@.
data SplitBranch k v = SplitBranch
  { sbName :: !Text
  , sbPred :: !(Record k v -> Bool)
  }

----------------------------------------------------------------------
-- The GADT
----------------------------------------------------------------------

-- | A composable topology fragment from input wire type @i@ to output
-- wire type @o@.
--
-- The first group of constructors are the category- and arrow-style
-- combinators; the rest are the reified DSL primitives. Each
-- constructor has a precise type that prevents nonsense
-- compositions like piping a 'KStream' into a 'KTable'-shaped
-- operation.
data Topology i o where
  -- ------------------------------------------------------------------
  -- Category / Arrow combinators
  --
  -- These cover the laws-respecting combinators of 'Category',
  -- 'Arrow', and 'ArrowChoice'.
  -- ------------------------------------------------------------------

  -- | 'Control.Category.id'
  Id      :: Topology a a

  -- | 'Control.Category.>>>' / '.' (right-to-left composition).
  -- @'Compose' g f@ runs @f@ then @g@.
  Compose :: Topology b c -> Topology a b -> Topology a c

  -- | 'Control.Arrow.arr': lift a pure function. The function runs
  -- /at compile time/ — it doesn't add a node to the topology.
  -- Useful for plumbing tuples around the AST.
  Arr     :: (a -> b) -> Topology a b

  -- | 'Control.Arrow.first': run a sub-fragment on the left of a
  -- pair, leaving the right untouched.
  First   :: Topology a b -> Topology (a, c) (b, c)

  -- | 'Control.Arrow.second': mirror of 'First'.
  Second  :: Topology a b -> Topology (c, a) (c, b)

  -- | 'Control.Arrow.***': two independent sub-fragments in parallel
  -- on a tuple input. /Independent/ in the topology graph — there
  -- are no edges between the two subgraphs except the tuple shape
  -- at the call site. See the module-level note on lineage.
  Parallel :: Topology a b -> Topology c d -> Topology (a, c) (b, d)

  -- | 'Control.Arrow.&&&': feed one upstream into two sub-fragments
  -- and pair the outputs. The compiler reuses the same upstream
  -- node as parent for both sub-graphs — this is the "one node,
  -- two children" pattern that fans a single lineage into two.
  Fanout  :: Topology a b -> Topology a c -> Topology a (b, c)

  -- | 'Control.Arrow.left' for sum-typed wires.
  LeftT   :: Topology a b -> Topology (Either a c) (Either b c)

  -- | 'Control.Arrow.right'.
  RightT  :: Topology a b -> Topology (Either c a) (Either c b)

  -- | 'Control.Arrow.+++'.
  Plus    :: Topology a b -> Topology c d -> Topology (Either a c) (Either b d)

  -- | 'Control.Arrow.|||': collapse a sum into a single output.
  Fanin   :: Topology a c -> Topology b c -> Topology (Either a b) c

  -- ------------------------------------------------------------------
  -- Lineage combinators
  --
  -- These cover the topology-graph "one source, many children"
  -- patterns that come up so often in Kafka Streams. They're all
  -- expressible as a combination of 'Fanout' and 'Arr', but having
  -- them as first-class constructors keeps the AST legible and
  -- makes optimization passes easier.
  -- ------------------------------------------------------------------

  -- | Explicit duplicator. Equivalent to @'Id' '&&&' 'Id'@ but
  -- compiles to a single AST node and reads cleaner at call sites.
  Fork    :: Topology a (a, a)

  -- | Apply N sub-fragments to the same upstream and collect the
  -- results in order. Better than chained '&&&' once you have more
  -- than two branches — the resulting topology has one upstream
  -- node with N children.
  ForkN   :: !(NonEmpty (Topology a b)) -> Topology a (NonEmpty b)

  -- | Run a side-effecting sub-pipeline (typically ending in a
  -- 'Sink' or 'Foreach') and pass the upstream wire through
  -- unchanged. Mirrors how the JVM @KStream.peek@ + @KStream.to@
  -- pair is often used: tap a stream into an audit log without
  -- changing the main pipeline.
  Tap     :: Topology a () -> Topology a a

  -- | KIP-418 named branches. Each input record routes to the first
  -- branch whose predicate matches. Records that match none go to
  -- the default branch if supplied, otherwise are dropped. The
  -- output is a 'Map' from branch name to its 'KStream', which
  -- downstream 'Arr' calls can destructure.
  Split   :: ![SplitBranch k v]
          -> !(Maybe Text)
          -> Topology (KStream k v) (Map Text (KStream k v))

  -- ------------------------------------------------------------------
  -- Sources
  --
  -- Sources have 'Void' on the input side. Composing anything to the
  -- left of a 'Source' is statically impossible; the only way to
  -- close the input is to put a source there.
  -- ------------------------------------------------------------------

  Source       :: !TopicName -> !(Consumed k v)
               -> Topology Void (KStream k v)

  -- | Multi-topic source: subscribe to several topics, fan-in into
  -- a single 'KStream'. Mirrors @StreamsBuilder.stream(Collection<String>)@.
  SourceMulti  :: !(NonEmpty TopicName) -> !(Consumed k v)
               -> Topology Void (KStream k v)

  -- | Source materialised straight into a 'KTable'.
  TableSource  :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Topology Void (KTable k v)

  -- | Source materialised into a 'GlobalKTable'.
  GlobalSource :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Topology Void (GlobalKTable k v)

  -- ------------------------------------------------------------------
  -- Sinks
  -- ------------------------------------------------------------------

  -- | Publish the stream to a topic. The result wire is @()@ — the
  -- pipeline is closed on this branch.
  Sink         :: !TopicName -> !(Produced k v)
               -> Topology (KStream k v) ()

  -- | Per-record dynamic-topic sink. Mirrors
  -- @KStream.to(TopicNameExtractor, Produced)@.
  SinkExtracted
               :: !(KS.TopicNameExtractor k v) -> !(Produced k v)
               -> Topology (KStream k v) ()

  -- | Sink + immediate re-subscribe; mirrors @KStream.through@.
  Through      :: !TopicName -> !(Produced k v)
               -> Topology (KStream k v) (KStream k v)

  -- ------------------------------------------------------------------
  -- Stateless 'KStream' transforms
  -- ------------------------------------------------------------------

  MapValues       :: (v -> v')
                  -> Topology (KStream k v) (KStream k v')
  MapValuesM      :: (v -> IO v')
                  -> Topology (KStream k v) (KStream k v')
  MapKeyValue     :: (k -> v -> (k', v'))
                  -> Topology (KStream k v) (KStream k' v')
  MapKeyValueM    :: (k -> v -> IO (k', v'))
                  -> Topology (KStream k v) (KStream k' v')
  Filter          :: (Record k v -> Bool)
                  -> Topology (KStream k v) (KStream k v)
  FilterNot       :: (Record k v -> Bool)
                  -> Topology (KStream k v) (KStream k v)
  FlatMapValues   :: (v -> [v'])
                  -> Topology (KStream k v) (KStream k v')
  FlatMapKeyValue :: (k -> v -> [(k', v')])
                  -> Topology (KStream k v) (KStream k' v')
  Peek            :: (Record k v -> IO ())
                  -> Topology (KStream k v) (KStream k v)
  Foreach         :: (Record k v -> IO ())
                  -> Topology (KStream k v) ()
  ForeachAsync    :: (Record k v -> IO ())
                  -> Topology (KStream k v) ()
  SelectKey       :: (Record k v -> k')
                  -> Topology (KStream k v) (KStream k' v)
  Values          :: Topology (KStream k v) (KStream () v)
  Print           :: (Show k, Show v)
                  => !Text                  -- ^ label
                  -> !(String -> IO ())     -- ^ line writer
                  -> Topology (KStream k v) ()

  -- ------------------------------------------------------------------
  -- 'KStream' composition
  -- ------------------------------------------------------------------

  -- | Binary 'KStream.merge'. Use 'Fanout' or 'Parallel' to bring
  -- two streams into the tuple-shape this constructor consumes.
  Merge    :: Topology (KStream k v, KStream k v) (KStream k v)
  -- | N-ary merge.
  MergeAll :: Topology [KStream k v] (KStream k v)

  -- | Predicate-routed split (pre-KIP-418 shape). Records that
  -- match no predicate are dropped. Mirrors @KStream.branch@.
  Branch   :: ![Record k v -> Bool]
           -> Topology (KStream k v) [KStream k v]

  -- ------------------------------------------------------------------
  -- Conversions
  -- ------------------------------------------------------------------

  ToTableT        :: Ord k
                  => !(Materialized k v)
                  -> Topology (KStream k v) (KTable k v)
  ToStream        :: Topology (KTable k v) (KStream k v)
  Repartition     :: !Text
                  -> Topology (KStream k v) (KStream k v)
  RepartitionWith :: !(Rep.Repartitioned k v)
                  -> Topology (KStream k v) (KStream k v)

  -- ------------------------------------------------------------------
  -- Grouping + aggregation
  -- ------------------------------------------------------------------

  GroupByKey   :: !(Grouped k v)
               -> Topology (KStream k v) (KGroupedStream k v)
  GroupBy      :: (Record k v -> k') -> !(Grouped k' v)
               -> Topology (KStream k v) (KGroupedStream k' v)
  Count        :: Ord k
               => !(Materialized k Int64)
               -> Topology (KGroupedStream k v) (KTable k Int64)
  Reduce       :: Ord k
               => (v -> v -> v) -> !(Materialized k v)
               -> Topology (KGroupedStream k v) (KTable k v)
  Aggregate    :: Ord k
               => !(IO agg)
               -> (k -> v -> agg -> agg)
               -> !(Materialized k agg)
               -> Topology (KGroupedStream k v) (KTable k agg)

  -- ------------------------------------------------------------------
  -- Windowed aggregation
  -- ------------------------------------------------------------------

  WindowedByTime
    :: !Win.Windows
    -> Topology (KGroupedStream k v) (TimeWindowedKStream k v)
  WindowedBySession
    :: !Win.SessionWindows
    -> Topology (KGroupedStream k v) (SessionWindowedKStream k v)

  CountWindowed
    :: Ord k
    => !(Materialized k Int64)
    -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k Int64)
  ReduceWindowed
    :: Ord k
    => (v -> v -> v) -> !(Materialized k v)
    -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k v)
  AggregateWindowed
    :: Ord k
    => !(IO agg) -> (k -> v -> agg -> agg) -> !(Materialized k agg)
    -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k agg)

  CountSessionWindowed
    :: Ord k
    => !(Materialized k Int64)
    -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k Int64)
  AggregateSessionWindowed
    :: Ord k
    => !(IO agg)
    -> (k -> v -> agg -> agg)
    -> (k -> agg -> agg -> agg)            -- ^ session-merger
    -> !(Materialized k agg)
    -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k agg)

  -- ------------------------------------------------------------------
  -- KGroupedTable (subtractor-aware aggregation)
  -- ------------------------------------------------------------------

  GroupTableBy
    :: (Ord k, Ord k')
    => (k -> v -> (k', v')) -> !(Grouped k' v')
    -> Topology (KTable k v) (KGroupedTable k' v')
  CountKGroupedTable
    :: Ord k
    => !(Materialized k Int64)
    -> Topology (KGroupedTable k v) (KTable k Int64)
  ReduceKGroupedTable
    :: Ord k
    => (v -> v -> v)                          -- ^ adder
    -> (v -> v -> v)                          -- ^ subtractor
    -> !(Materialized k v)
    -> Topology (KGroupedTable k v) (KTable k v)
  AggregateKGroupedTable
    :: Ord k
    => !(IO agg)
    -> (k -> v -> agg -> agg)                 -- ^ adder
    -> (k -> v -> agg -> agg)                 -- ^ subtractor
    -> !(Materialized k agg)
    -> Topology (KGroupedTable k v) (KTable k agg)

  -- ------------------------------------------------------------------
  -- Cogroup
  -- ------------------------------------------------------------------

  -- | Start a cogroup from one source.
  Cogroup
    :: (k -> v -> a -> a)
    -> Topology (KGroupedStream k v) (CogroupedStream k a)
  -- | Add another source to an in-progress cogroup. Bring the
  -- second source in via 'Fanout' / 'Parallel'.
  AddCogrouped
    :: (k -> v -> a -> a)
    -> Topology (CogroupedStream k a, KGroupedStream k v) (CogroupedStream k a)
  AggregateCogrouped
    :: Ord k
    => !(IO a) -> !(Materialized k a)
    -> Topology (CogroupedStream k a) (KTable k a)

  -- ------------------------------------------------------------------
  -- Joins
  --
  -- Joins take the two participants on the input side as a tuple. To
  -- thread two named upstream sources into a join, build them with
  -- 'Parallel' / 'Fanout' so they land in the same pair.
  -- ------------------------------------------------------------------

  StreamTableJoin
    :: Ord k
    => (v -> vt -> v') -> !(Joined k v vt)
    -> Topology (KStream k v, KTable k vt) (KStream k v')
  StreamTableLeftJoin
    :: Ord k
    => (v -> Maybe vt -> v') -> !(Joined k v vt)
    -> Topology (KStream k v, KTable k vt) (KStream k v')

  StreamStreamJoin
    :: Ord k
    => (v1 -> v2 -> v')
    -> !JoinWindows -> !(Joined k v1 v2)
    -> Topology (KStream k v1, KStream k v2) (KStream k v')
  StreamStreamLeftJoin
    :: Ord k
    => (v1 -> Maybe v2 -> v')
    -> !JoinWindows -> !(Joined k v1 v2)
    -> Topology (KStream k v1, KStream k v2) (KStream k v')
  StreamStreamOuterJoin
    :: Ord k
    => (Maybe v1 -> Maybe v2 -> v')
    -> !JoinWindows -> !(Joined k v1 v2)
    -> Topology (KStream k v1, KStream k v2) (KStream k v')

  TableTableJoin
    :: Ord k
    => (v1 -> v2 -> v') -> !(Materialized k v')
    -> Topology (KTable k v1, KTable k v2) (KTable k v')
  TableTableLeftJoin
    :: Ord k
    => (v1 -> Maybe v2 -> v') -> !(Materialized k v')
    -> Topology (KTable k v1, KTable k v2) (KTable k v')
  TableTableOuterJoin
    :: Ord k
    => (Maybe v1 -> Maybe v2 -> v') -> !(Materialized k v')
    -> Topology (KTable k v1, KTable k v2) (KTable k v')

  ForeignKeyJoin
    :: (Ord k, Ord fk, Hashable v)
    => (v -> fk)                             -- ^ FK extractor
    -> (v -> vr -> v')                       -- ^ joiner
    -> !(Materialized k v')
    -> Topology (KTable k v, KTable fk vr) (KTable k v')
  LeftForeignKeyJoin
    :: (Ord k, Ord fk, Hashable v)
    => (v -> fk)
    -> (v -> Maybe vr -> v')
    -> !(Materialized k v')
    -> Topology (KTable k v, KTable fk vr) (KTable k v')

  StreamGlobalTableJoin
    :: Ord kg
    => (k -> v -> kg)                        -- ^ key mapper
    -> (v -> vg -> v')                       -- ^ joiner
    -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
  StreamGlobalTableLeftJoin
    :: Ord kg
    => (k -> v -> kg)
    -> (v -> Maybe vg -> v')
    -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')

  -- ------------------------------------------------------------------
  -- KTable surface
  -- ------------------------------------------------------------------

  FilterTable
    :: Ord k
    => (Record k v -> Bool) -> !(Materialized k v)
    -> Topology (KTable k v) (KTable k v)
  FilterNotTable
    :: Ord k
    => (Record k v -> Bool) -> !(Materialized k v)
    -> Topology (KTable k v) (KTable k v)
  MapValuesTable
    :: Ord k
    => (v -> v') -> !(Materialized k v')
    -> Topology (KTable k v) (KTable k v')
  TransformValuesTable
    :: Ord k
    => !Text                                  -- ^ name prefix
    -> !(IO (Processor k v))                  -- ^ processor supplier
    -> ![StoreName]                           -- ^ external stores
    -> !(Materialized k v')
    -> Topology (KTable k v) (KTable k v')

  -- ------------------------------------------------------------------
  -- Suppress
  -- ------------------------------------------------------------------

  SuppressUntilTimeLimit
    :: Ord k
    => !Duration
    -> Topology (KStream k v) (KStream k v)
  SuppressWindowedKS
    :: Ord k
    => !Duration                              -- ^ grace
    -> !Int64                                 -- ^ window size (ms)
    -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)

  -- ------------------------------------------------------------------
  -- Processor API
  -- ------------------------------------------------------------------

  ProcessStream
    :: !Text                                  -- ^ name prefix
    -> ![StoreName]                           -- ^ attached stores
    -> !(IO (Processor k v))
    -> Topology (KStream k v) ()
  ProcessValuesStream
    :: !Text
    -> ![StoreName]
    -> !(IO (Processor k v))
    -> !(Serde v')
    -> Topology (KStream k v) (KStream k v')
  TransformValuesStreamT
    :: !Text
    -> ![Topo.NodeName]                       -- ^ store names as NodeNames
                                              -- (matches existing imperative
                                              -- 'transformValuesStream')
    -> !(IO (Processor k v))
    -> !(Serde v')
    -> Topology (KStream k v) (KStream k v')

  -- | Register a 'StoreBuilderKV' against the topology graph. The
  -- 'NodeName' list is the set of processors granted read/write
  -- access; the constructor is a pass-through on the wire (it
  -- doesn't change the @i ~ o@ shape).
  WithStateStoreKV
    :: !(StoreBuilderKV k v)
    -> ![Topo.NodeName]
    -> Topology x x
  WithStateStoreW
    :: !(StoreBuilderW k v)
    -> ![Topo.NodeName]
    -> Topology x x
  WithStateStoreS
    :: !(StoreBuilderS k v)
    -> ![Topo.NodeName]
    -> Topology x x

  -- ------------------------------------------------------------------
  -- Escape hatch
  --
  -- Lets callers splice in any topology-mutating IO action. Use
  -- whenever a custom processor or upstream-library operation
  -- isn't reachable through the dedicated constructors above. The
  -- escape is typed: the caller still has to declare the input /
  -- output wire types.
  -- ------------------------------------------------------------------
  Lifted
    :: !Text                                  -- ^ pretty name for traces
    -> (StreamsBuilder -> i -> IO o)
    -> Topology i o

----------------------------------------------------------------------
-- Category / Arrow / ArrowChoice instances
----------------------------------------------------------------------

instance Category Topology where
  id :: Topology a a
  id = Id

  (.) :: Topology b c -> Topology a b -> Topology a c
  -- Local Cat-law simplifications: cancel 'Id' on either side so
  -- iterated composition doesn't accumulate identity noise.
  Id . f  = f
  g  . Id = g
  g  . f  = Compose g f

instance Arrow Topology where
  arr :: (a -> b) -> Topology a b
  arr = Arr

  first :: Topology a b -> Topology (a, c) (b, c)
  first = First

  second :: Topology a b -> Topology (c, a) (c, b)
  second = Second

  (***) :: Topology a b -> Topology c d -> Topology (a, c) (b, d)
  (***) = Parallel

  (&&&) :: Topology a b -> Topology a c -> Topology a (b, c)
  (&&&) = Fanout

instance ArrowChoice Topology where
  left :: Topology a b -> Topology (Either a c) (Either b c)
  left = LeftT

  right :: Topology a b -> Topology (Either c a) (Either c b)
  right = RightT

  (+++) :: Topology a b -> Topology c d -> Topology (Either a c) (Either b d)
  (+++) = Plus

  (|||) :: Topology a c -> Topology b c -> Topology (Either a b) c
  (|||) = Fanin

-- | A 'Topology' is a 'Functor' over its output type via
-- post-composition with 'arr'.
instance Functor (Topology a) where
  fmap f t = Arr f `Compose` t

----------------------------------------------------------------------
-- Compilation
----------------------------------------------------------------------

-- | Type alias just to make signatures shorter at call sites.
type TBuilder = StreamsBuilder

-- | Apply a topology to an input wire value against a builder. This
-- is the open-ended interpreter — the same function the
-- closed-input 'compile' uses internally, exposed so users can
-- splice a 'Topology' into a hand-rolled imperative builder.
apply :: forall i o. Topology i o -> TBuilder -> i -> IO o
apply t b = go t
  where
    go :: forall x y. Topology x y -> x -> IO y
    -- Category / Arrow
    go Id            = pure
    go (Compose g f) = go f >=> go g
    go (Arr f)       = pure . f
    go (First t')    = \(a, c) -> do
      !x <- go t' a
      pure (x, c)
    go (Second t')   = \(c, a) -> do
      !x <- go t' a
      pure (c, x)
    go (Parallel p q) = \(a, c) -> do
      !x <- go p a
      !y <- go q c
      pure (x, y)
    go (Fanout p q)   = \a -> do
      !x <- go p a
      !y <- go q a
      pure (x, y)
    go (LeftT t')    = \case
      Left a  -> Left  <$> go t' a
      Right c -> pure (Right c)
    go (RightT t')   = \case
      Left c  -> pure (Left c)
      Right a -> Right <$> go t' a
    go (Plus p q)    = \case
      Left a  -> Left  <$> go p a
      Right c -> Right <$> go q c
    go (Fanin p q)   = \case
      Left a  -> go p a
      Right c -> go q c

    -- Lineage combinators
    go Fork          = \a -> pure (a, a)
    go (ForkN ts)    = \a -> traverse (`go` a) ts
    go (Tap t')      = \a -> do
      !() <- go t' a
      pure a
    go (Split branches mDefault) = \s -> do
      let toBranched (SplitBranch nm p) = KS.branchedFrom nm p
      KS.splitStream (Prelude.map toBranched branches) mDefault s

    -- Sources (the Void input is never inspected)
    go (Source topic c)         = \_void -> KS.streamFromTopic b topic c
    go (SourceMulti topics c)   = \_void -> sourceMultiCompile b (NE.toList topics) c
    go (TableSource topic c m)  = \_void -> KT.tableFromTopic b topic c m
    go (GlobalSource topic c m) = \_void -> GT.globalTable b topic c m

    -- Sinks
    go (Sink topic p)        = KS.toTopic topic p
    go (SinkExtracted e p)   = KS.toExtracted e p
    go (Through topic p)     = KS.throughTopic topic p

    -- Stateless
    go (MapValues f)         = KS.mapValues f
    go (MapValuesM f)        = KS.mapValuesM f
    go (MapKeyValue f)       = KS.mapKeyValue f
    go (MapKeyValueM f)      = KS.mapKeyValueM f
    go (Filter p)            = KS.filterStream p
    go (FilterNot p)         = KS.filterNotStream p
    go (FlatMapValues f)     = KS.flatMapValues f
    go (FlatMapKeyValue f)   = KS.flatMapKeyValue f
    go (Peek f)              = KS.peekStream f
    go (Foreach f)           = KS.foreachStream f
    go (ForeachAsync f)      = KS.foreachStreamAsync f
    go (SelectKey f)         = KS.selectKey f
    go Values                = KS.valuesStream
    go (Print label putLine) = KS.printToHandle label putLine

    -- Composition
    go Merge                 = \(s1, s2) -> KS.mergeStreams s1 s2
    go MergeAll              = KS.mergeStreamsN
    go (Branch ps)           = KS.branchStream ps

    -- Conversions
    go (ToTableT m)            = KS.toTable m
    go ToStream                = KS.toKStreamFromKTable
    go (Repartition prefix)    = KS.repartition prefix
    go (RepartitionWith cfg)   = KS.repartitionWith cfg

    -- Grouping + aggregation
    go (GroupByKey g)        = pure . KGS.groupByKey g
    go (GroupBy f g)         = KGS.groupByStream f g
    go (Count m)             = \g -> do
      h <- KGS.countStream m g
      pure (handleToKTable m h)
    go (Reduce f m)          = \g -> do
      h <- KGS.reduceStream f m g
      pure (handleToKTable m h)
    go (Aggregate seed step m) = \g -> do
      h <- KGS.aggregateStream seed step m g
      pure (handleToKTable m h)

    -- Windowed aggregation
    go (WindowedByTime ws)     = pure . KGS.windowedByTime ws
    go (WindowedBySession sw)  = pure . KGS.windowedBySession sw
    go (CountWindowed m)       = TWKS.countWindowed m
    go (ReduceWindowed f m)    = TWKS.reduceWindowed f m
    go (AggregateWindowed seed step m)
                               = TWKS.aggregateWindowed seed step m
    go (CountSessionWindowed m)
                               = SWKS.countSessionWindowed m
    go (AggregateSessionWindowed seed step merger m)
                               = SWKS.aggregateSessionWindowed seed step merger m

    -- KGroupedTable
    go (GroupTableBy f g)      = pure . KGT.groupTableBy f g
    go (CountKGroupedTable m)  = \kgt -> do
      h <- KGT.countKGroupedTable m kgt
      pure (handleToKTable m h)
    go (ReduceKGroupedTable add sub m) = \kgt -> do
      h <- KGT.reduceKGroupedTable add sub m kgt
      pure (handleToKTable m h)
    go (AggregateKGroupedTable seed add sub m) = \kgt -> do
      h <- KGT.aggregateKGroupedTable seed add sub m kgt
      pure (handleToKTable m h)

    -- Cogroup
    go (Cogroup step)              = pure . (\g -> Cog.cogroup g step)
    go (AddCogrouped step)         = \(cs, g) -> pure (Cog.addCogrouped cs g step)
    go (AggregateCogrouped seed m) = \cs -> do
      h <- Cog.aggregateCogrouped seed m cs
      pure (handleToKTable m h)

    -- Joins
    go (StreamTableJoin j jo)
      = \(s, t') -> KS.joinKStreamKTable j jo s t'
    go (StreamTableLeftJoin j jo)
      = \(s, t') -> KS.leftJoinKStreamKTable j jo s t'
    go (StreamStreamJoin j w jo)
      = \(s1, s2) -> KS.joinKStreamKStream j w jo s1 s2
    go (StreamStreamLeftJoin j w jo)
      = \(s1, s2) -> KS.leftJoinKStreamKStream j w jo s1 s2
    go (StreamStreamOuterJoin j w jo)
      = \(s1, s2) -> KS.outerJoinKStreamKStream j w jo s1 s2
    go (TableTableJoin j m)
      = \(t1, t2) -> KT.joinKTableKTable j m t1 t2
    go (TableTableLeftJoin j m)
      = \(t1, t2) -> KT.leftJoinKTableKTable j m t1 t2
    go (TableTableOuterJoin j m)
      = \(t1, t2) -> KT.outerJoinKTableKTable j m t1 t2
    go (ForeignKeyJoin ext j m)
      = \(t1, t2) -> FK.foreignKeyJoinKTable ext j m t1 t2
    go (LeftForeignKeyJoin ext j m)
      = \(t1, t2) -> FK.leftForeignKeyJoinKTable ext j m t1 t2
    go (StreamGlobalTableJoin km j)
      = \(s, g) -> GT.joinKStreamGlobalKTable km j s g
    go (StreamGlobalTableLeftJoin km j)
      = \(s, g) -> GT.leftJoinKStreamGlobalKTable km j s g

    -- KTable surface
    go (FilterTable p m)             = KT.filterTable p m
    go (FilterNotTable p m)          = KT.filterNotTable p m
    go (MapValuesTable f m)          = KT.mapValuesTable f m
    go (TransformValuesTable _nm sup stores m)
                                     = KT.transformValuesTable sup stores m

    -- Suppress
    go (SuppressUntilTimeLimit lim)  = Suppress.suppressUntilTimeLimit lim
    go (SuppressWindowedKS grace sz) = Suppress.suppressWindowed grace sz

    -- Processor API
    go (ProcessStream nm stores sup)
                                     = KS.processStream nm stores sup
    go (ProcessValuesStream nm stores sup vs)
                                     = KS.processValuesStream nm stores sup vs
    go (TransformValuesStreamT nm stores sup vs)
                                     = KS.transformValuesStream nm stores sup vs

    go (WithStateStoreKV builder owners) = \x -> do
      withTopology_ b (Topo.addStateStoreKV builder owners)
      pure x
    go (WithStateStoreW builder owners)  = \x -> do
      withTopology_ b (Topo.addStateStoreW builder owners)
      pure x
    go (WithStateStoreS builder owners)  = \x -> do
      withTopology_ b (Topo.addStateStoreS builder owners)
      pure x

    -- Escape
    go (Lifted _ act)        = act b

-- | Multi-topic source helper. The existing
-- 'Kafka.Streams.KStream.streamFromTopic' takes a single 'TopicName';
-- the multi-topic variant calls the low-level
-- 'Kafka.Streams.Topology.addSource' directly and constructs the
-- same 'KStream' handle.
--
-- The runtime's source-topic set is a single list per source node, so
-- the multi-topic case is the natural shape; the single-topic
-- 'streamFromTopic' is just sugar over it.
sourceMultiCompile
  :: TBuilder
  -> [TopicName]
  -> Consumed k v
  -> IO (KStream k v)
sourceMultiCompile _ [] _ =
  error
    "Kafka.Streams.Topology.Free.sources: multi-topic source needs \
    \at least one topic; pass a NonEmpty in via the smart constructor."
sourceMultiCompile b ts c = do
  nm <- freshNodeName b "KSTREAM-SOURCE-MULTI"
  withTopology_ b $
    Topo.addSource nm
                   ts
                   (consumedKeySerde c)
                   (consumedValueSerde c)
                   (consumedExtractor c)
  pure (KS.KStream
          { KS.kstreamBuilder    = b
          , KS.kstreamParent     = nm
          , KS.kstreamKeySerde   = consumedKeySerde c
          , KS.kstreamValueSerde = consumedValueSerde c
          })

-- The 'CountedTableLocal' / windowed handle / session handle returned
-- by the existing aggregation DSL is a thin wrapper around a 'KTable'
-- — it just doesn't carry the serdes. We promote it to a real
-- 'KTable' by extracting the serdes from the supplied 'Materialized'.
-- If they're absent the corresponding 'KTable' field becomes a
-- deferred error (matching the existing 'KTable' lazy-serde behaviour
-- after @mapValues@ etc); a downstream @to@ or @join@ will then
-- report the missing serde at the use site rather than here.
handleToKTable
  :: Materialized k v
  -> KGS.CountedTableLocal k v
  -> KTable k v
handleToKTable m h = KT.KTable
  { KT.ktableNode       = KGS.ctlNode h
  , KT.ktableStore      = KGS.ctlStore h
  , KT.ktableBuilder    = KGS.ctlBuilder h
  , KT.ktableKeySerde   =
      case Mat.matKeySerde m of
        Just s  -> s
        Nothing ->
          error
            "Kafka.Streams.Topology.Free: aggregation result KTable has \
            \no key serde; supply one via Materialized.withKeySerde"
  , KT.ktableValueSerde =
      case Mat.matValueSerde m of
        Just s  -> s
        Nothing ->
          error
            "Kafka.Streams.Topology.Free: aggregation result KTable has \
            \no value serde; supply one via Materialized.withValueSerde"
  }

-- | Compile a closed-input topology into the existing
-- 'Kafka.Streams.Topology.Topology' graph. The output @o@ is
-- whatever the AST says — typically @()@ for a sink-closed
-- pipeline, but a freshly-compiled 'KStream' / 'KTable' is also
-- fine if the caller wants to keep wiring around the imperative
-- DSL.
compile :: Topology Void o -> IO (o, Topo.Topology)
compile t = do
  b <- newStreamsBuilder
  o <- apply t b voidInput
  topo <- buildTopology b
  pure (o, topo)
  where
    voidInput :: Void
    voidInput =
      error
        "Kafka.Streams.Topology.Free.compile: Source primitive inspected its Void input — \
        \this is a library bug. Please report."

-- | 'compile' against a pre-existing 'StreamsBuilder'. Useful when
-- splicing a 'Topology' into a topology being built imperatively
-- through "Kafka.Streams.StreamsBuilder".
compileWith :: TBuilder -> Topology Void o -> IO o
compileWith b t = apply t b voidInput
  where
    voidInput :: Void
    voidInput =
      error "Kafka.Streams.Topology.Free.compileWith: void input forced"

-- | Compile a closed topology and return only the resulting
-- 'Topo.Topology'. Convenience wrapper around 'compile'.
buildTopologyFrom :: Topology Void () -> IO Topo.Topology
buildTopologyFrom = fmap snd . compile

----------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------

-- * Sources --------------------------------------------------------

-- | Subscribe to a topic with the default 'Consumed' (record
-- timestamp, earliest offset reset, no explicit node name).
source :: Text -> Serde k -> Serde v -> Topology Void (KStream k v)
source t ks vs = sourceWith (topicName t) (consumed ks vs)

-- | Subscribe to a topic with a fully-specified 'Consumed'.
sourceWith :: TopicName -> Consumed k v -> Topology Void (KStream k v)
sourceWith = Source

-- | Subscribe to N topics, fanning them into a single 'KStream'.
sources
  :: NonEmpty Text -> Serde k -> Serde v -> Topology Void (KStream k v)
sources ts ks vs = SourceMulti (fmap topicName ts) (consumed ks vs)

-- | Materialise a topic as a 'KTable' (default in-memory store).
tableSource
  :: Ord k => Text -> Serde k -> Serde v -> Topology Void (KTable k v)
tableSource t ks vs =
  TableSource (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

-- | Cluster-replicated 'GlobalKTable'.
globalTableSource
  :: Ord k => Text -> Serde k -> Serde v -> Topology Void (GlobalKTable k v)
globalTableSource t ks vs =
  GlobalSource (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

-- * Sinks ----------------------------------------------------------

-- | Publish records to a topic.
sink :: Text -> Serde k -> Serde v -> Topology (KStream k v) ()
sink t ks vs = sinkWith (topicName t) (produced ks vs)

-- | 'sink' with a fully-specified 'Produced'.
sinkWith :: TopicName -> Produced k v -> Topology (KStream k v) ()
sinkWith = Sink

-- | Per-record dynamic-topic sink.
sinkExtracted
  :: KS.TopicNameExtractor k v -> Produced k v -> Topology (KStream k v) ()
sinkExtracted = SinkExtracted

-- | Sink to a topic and re-subscribe from it. Mirrors
-- @KStream.through@.
through :: Text -> Serde k -> Serde v -> Topology (KStream k v) (KStream k v)
through t ks vs = Through (topicName t) (produced ks vs)

-- * Stateless 'KStream' transforms --------------------------------

mapValues :: (v -> v') -> Topology (KStream k v) (KStream k v')
mapValues = MapValues

mapValuesM :: (v -> IO v') -> Topology (KStream k v) (KStream k v')
mapValuesM = MapValuesM

mapKeyValue
  :: (k -> v -> (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValue = MapKeyValue

mapKeyValueM
  :: (k -> v -> IO (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValueM = MapKeyValueM

filter :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filter = Filter

filterNot :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filterNot = FilterNot

flatMapValues :: (v -> [v']) -> Topology (KStream k v) (KStream k v')
flatMapValues = FlatMapValues

flatMapKeyValue
  :: (k -> v -> [(k', v')]) -> Topology (KStream k v) (KStream k' v')
flatMapKeyValue = FlatMapKeyValue

peek :: (Record k v -> IO ()) -> Topology (KStream k v) (KStream k v)
peek = Peek

foreach :: (Record k v -> IO ()) -> Topology (KStream k v) ()
foreach = Foreach

-- | Non-blocking 'foreach' — each effect runs in its own
-- 'Control.Concurrent.Async' so the stream thread doesn't block on
-- IO latency. Same per-task-order caveats as
-- 'Kafka.Streams.KStream.foreachStreamAsync'.
foreachAsync :: (Record k v -> IO ()) -> Topology (KStream k v) ()
foreachAsync = ForeachAsync

selectKey :: (Record k v -> k') -> Topology (KStream k v) (KStream k' v)
selectKey = SelectKey

values :: Topology (KStream k v) (KStream () v)
values = Values

-- | Show-debugging sink. The 'Text' label is prepended to every
-- printed line; the @line writer@ is typically 'putStrLn' or a
-- logger callback. Named 'prints' (with an @s@) to avoid clashing
-- with @Prelude.print@.
prints
  :: (Show k, Show v)
  => Text -> (String -> IO ()) -> Topology (KStream k v) ()
prints = Print

-- * 'KStream' composition + branching -----------------------------

merge :: Topology (KStream k v, KStream k v) (KStream k v)
merge = Merge

mergeAll :: Topology [KStream k v] (KStream k v)
mergeAll = MergeAll

branch :: [Record k v -> Bool] -> Topology (KStream k v) [KStream k v]
branch = Branch

-- | KIP-418 named branches. Each branch routes records matching its
-- predicate; the optional default branch catches the rest.
split
  :: [SplitBranch k v]
  -> Maybe Text
  -> Topology (KStream k v) (Map Text (KStream k v))
split = Split

-- | Helper for assembling a 'SplitBranch'. Reads as
-- @F.splitBranch \"low\" (\\r -> recordValue r < 10)@.
splitBranch :: Text -> (Record k v -> Bool) -> SplitBranch k v
splitBranch = SplitBranch

-- | Explicit wire duplicator. Same as @id '&&&' id@ but reads better.
fork :: Topology a (a, a)
fork = Fork

-- | N-way fan-out: apply each sub-fragment to the same upstream and
-- collect the results in input order.
forkN :: NonEmpty (Topology a b) -> Topology a (NonEmpty b)
forkN = ForkN

-- | Run a side-effecting sub-pipeline (typically ending in a 'Sink'
-- or 'Foreach') and pass the upstream wire through unchanged.
tap :: Topology a () -> Topology a a
tap = Tap

-- * Conversions ---------------------------------------------------

toTable :: Ord k => Materialized k v -> Topology (KStream k v) (KTable k v)
toTable = ToTableT

toStream :: Topology (KTable k v) (KStream k v)
toStream = ToStream

repartition :: Text -> Topology (KStream k v) (KStream k v)
repartition = Repartition

repartitionWith
  :: Rep.Repartitioned k v -> Topology (KStream k v) (KStream k v)
repartitionWith = RepartitionWith

-- * Grouping + aggregation ----------------------------------------

groupByKey :: Grouped k v -> Topology (KStream k v) (KGroupedStream k v)
groupByKey = GroupByKey

groupBy
  :: (Record k v -> k') -> Grouped k' v
  -> Topology (KStream k v) (KGroupedStream k' v)
groupBy = GroupBy

count
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedStream k v) (KTable k Int64)
count = Count

reduce
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedStream k v) (KTable k v)
reduce = Reduce

aggregate
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedStream k v) (KTable k agg)
aggregate = Aggregate

-- * Windowed aggregation ------------------------------------------

windowedByTime
  :: Win.Windows
  -> Topology (KGroupedStream k v) (TimeWindowedKStream k v)
windowedByTime = WindowedByTime

windowedBySession
  :: Win.SessionWindows
  -> Topology (KGroupedStream k v) (SessionWindowedKStream k v)
windowedBySession = WindowedBySession

countWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k Int64)
countWindowed = CountWindowed

reduceWindowed
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k v)
reduceWindowed = ReduceWindowed

aggregateWindowed
  :: Ord k
  => IO agg -> (k -> v -> agg -> agg) -> Materialized k agg
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k agg)
aggregateWindowed = AggregateWindowed

countSessionWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k Int64)
countSessionWindowed = CountSessionWindowed

aggregateSessionWindowed
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> agg -> agg -> agg)
  -> Materialized k agg
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k agg)
aggregateSessionWindowed = AggregateSessionWindowed

-- * KGroupedTable -------------------------------------------------

groupTableBy
  :: (Ord k, Ord k')
  => (k -> v -> (k', v')) -> Grouped k' v'
  -> Topology (KTable k v) (KGroupedTable k' v')
groupTableBy = GroupTableBy

countKGroupedTable
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedTable k v) (KTable k Int64)
countKGroupedTable = CountKGroupedTable

reduceKGroupedTable
  :: Ord k
  => (v -> v -> v) -> (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedTable k v) (KTable k v)
reduceKGroupedTable = ReduceKGroupedTable

aggregateKGroupedTable
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedTable k v) (KTable k agg)
aggregateKGroupedTable = AggregateKGroupedTable

-- * Cogroup -------------------------------------------------------

cogroup
  :: (k -> v -> a -> a)
  -> Topology (KGroupedStream k v) (CogroupedStream k a)
cogroup = Cogroup

addCogrouped
  :: (k -> v -> a -> a)
  -> Topology (CogroupedStream k a, KGroupedStream k v) (CogroupedStream k a)
addCogrouped = AddCogrouped

aggregateCogrouped
  :: Ord k
  => IO a -> Materialized k a
  -> Topology (CogroupedStream k a) (KTable k a)
aggregateCogrouped = AggregateCogrouped

-- * Joins ---------------------------------------------------------

streamTableJoin
  :: Ord k
  => (v -> vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableJoin = StreamTableJoin

streamTableLeftJoin
  :: Ord k
  => (v -> Maybe vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableLeftJoin = StreamTableLeftJoin

streamStreamJoin
  :: Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamJoin = StreamStreamJoin

streamStreamLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamLeftJoin = StreamStreamLeftJoin

streamStreamOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamOuterJoin = StreamStreamOuterJoin

tableTableJoin
  :: Ord k
  => (v1 -> v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableJoin = TableTableJoin

tableTableLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableLeftJoin = TableTableLeftJoin

tableTableOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableOuterJoin = TableTableOuterJoin

foreignKeyJoin
  :: (Ord k, Ord fk, Hashable v)
  => (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
foreignKeyJoin = ForeignKeyJoin

leftForeignKeyJoin
  :: (Ord k, Ord fk, Hashable v)
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
leftForeignKeyJoin = LeftForeignKeyJoin

streamGlobalTableJoin
  :: Ord kg
  => (k -> v -> kg)
  -> (v -> vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableJoin = StreamGlobalTableJoin

streamGlobalTableLeftJoin
  :: Ord kg
  => (k -> v -> kg)
  -> (v -> Maybe vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableLeftJoin = StreamGlobalTableLeftJoin

-- * KTable surface ------------------------------------------------

filterTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterTable = FilterTable

filterNotTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterNotTable = FilterNotTable

mapValuesTable
  :: Ord k
  => (v -> v') -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
mapValuesTable = MapValuesTable

transformValuesTable
  :: Ord k
  => Text
  -> IO (Processor k v)
  -> [StoreName]
  -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
transformValuesTable = TransformValuesTable

-- * Suppress ------------------------------------------------------

suppressUntilTimeLimit
  :: Ord k => Duration -> Topology (KStream k v) (KStream k v)
suppressUntilTimeLimit = SuppressUntilTimeLimit

suppressWindowed
  :: Ord k
  => Duration -> Int64
  -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)
suppressWindowed = SuppressWindowedKS

-- * Processor API -------------------------------------------------

processStream
  :: Text -> [StoreName] -> IO (Processor k v)
  -> Topology (KStream k v) ()
processStream = ProcessStream

processValuesStream
  :: Text -> [StoreName] -> IO (Processor k v) -> Serde v'
  -> Topology (KStream k v) (KStream k v')
processValuesStream = ProcessValuesStream

transformValuesStream
  :: Text
  -> [Topo.NodeName]
  -> IO (Processor k v)
  -> Serde v'
  -> Topology (KStream k v) (KStream k v')
transformValuesStream = TransformValuesStreamT

-- | Register a 'StoreBuilderKV' against the topology graph and
-- attach it to the named owner processors. Pass-through on the
-- wire — composes cleanly anywhere via '>>>'.
withStateStoreKV
  :: StoreBuilderKV k v -> [Topo.NodeName] -> Topology x x
withStateStoreKV = WithStateStoreKV

withStateStoreW
  :: StoreBuilderW k v -> [Topo.NodeName] -> Topology x x
withStateStoreW = WithStateStoreW

withStateStoreS
  :: StoreBuilderS k v -> [Topo.NodeName] -> Topology x x
withStateStoreS = WithStateStoreS

----------------------------------------------------------------------
-- Escape hatch + introspection
----------------------------------------------------------------------

-- | Splice in a hand-rolled builder action. Use when an operator
-- isn't yet a dedicated constructor.
liftIO_
  :: Text
  -> (StreamsBuilder -> i -> IO o)
  -> Topology i o
liftIO_ = Lifted

-- | Walk the AST and collect a top-level operator-name listing.
-- This is the same shape Java's
-- @org.apache.kafka.streams.TopologyDescription@ surfaces — useful
-- for tests and golden-file comparisons.
inspect :: Topology i o -> [Text]
inspect = go
  where
    go :: Topology x y -> [Text]
    -- Category / Arrow
    go Id              = ["Id"]
    go (Compose g f)   = go f ++ go g
    go (Arr _)         = ["Arr"]
    go (First t)       = "First<" : go t ++ [">"]
    go (Second t)      = "Second<" : go t ++ [">"]
    go (Parallel p q)  = "Parallel<" : go p ++ "|" : go q ++ [">"]
    go (Fanout p q)    = "Fanout<"   : go p ++ "|" : go q ++ [">"]
    go (LeftT t)       = "Left<"     : go t ++ [">"]
    go (RightT t)      = "Right<"    : go t ++ [">"]
    go (Plus p q)      = "Plus<"     : go p ++ "|" : go q ++ [">"]
    go (Fanin p q)     = "Fanin<"    : go p ++ "|" : go q ++ [">"]
    -- Lineage
    go Fork            = ["Fork"]
    go (ForkN ts)      =
      "ForkN<" : concatMap go (NE.toList ts) ++ [">"]
    go (Tap t)         = "Tap<" : go t ++ [">"]
    go (Split bs md)
      = [ "Split(" <> T.intercalate "|" (Prelude.map sbName bs)
        <> maybe "" (\d -> "+default=" <> d) md
        <> ")" ]
    -- Sources
    go (Source t _)         = ["Source(" <> showTopic t <> ")"]
    go (SourceMulti ts _)
      = [ "SourceMulti("
          <> T.intercalate "," (Prelude.map showTopic (NE.toList ts))
          <> ")" ]
    go (TableSource t _ _)  = ["TableSource(" <> showTopic t <> ")"]
    go (GlobalSource t _ _) = ["GlobalSource(" <> showTopic t <> ")"]
    -- Sinks
    go (Sink t _)           = ["Sink(" <> showTopic t <> ")"]
    go (SinkExtracted _ _)  = ["SinkExtracted"]
    go (Through t _)        = ["Through(" <> showTopic t <> ")"]
    -- Stateless
    go (MapValues _)        = ["MapValues"]
    go (MapValuesM _)       = ["MapValuesM"]
    go (MapKeyValue _)      = ["MapKeyValue"]
    go (MapKeyValueM _)     = ["MapKeyValueM"]
    go (Filter _)           = ["Filter"]
    go (FilterNot _)        = ["FilterNot"]
    go (FlatMapValues _)    = ["FlatMapValues"]
    go (FlatMapKeyValue _)  = ["FlatMapKeyValue"]
    go (Peek _)             = ["Peek"]
    go (Foreach _)          = ["Foreach"]
    go (ForeachAsync _)     = ["ForeachAsync"]
    go (SelectKey _)        = ["SelectKey"]
    go Values               = ["Values"]
    go (Print nm _)         = ["Print(" <> nm <> ")"]
    -- Composition
    go Merge                = ["Merge"]
    go MergeAll             = ["MergeAll"]
    go (Branch ps)
      = ["Branch(" <> T.pack (show (length ps)) <> ")"]
    -- Conversions
    go (ToTableT _)         = ["ToTable"]
    go ToStream             = ["ToStream"]
    go (Repartition pfx)    = ["Repartition(" <> pfx <> ")"]
    go (RepartitionWith _)  = ["RepartitionWith"]
    -- Aggregation
    go (GroupByKey _)       = ["GroupByKey"]
    go (GroupBy _ _)        = ["GroupBy"]
    go (Count _)            = ["Count"]
    go (Reduce _ _)         = ["Reduce"]
    go (Aggregate _ _ _)    = ["Aggregate"]
    -- Windowed
    go (WindowedByTime _)        = ["WindowedByTime"]
    go (WindowedBySession _)     = ["WindowedBySession"]
    go (CountWindowed _)         = ["CountWindowed"]
    go (ReduceWindowed _ _)      = ["ReduceWindowed"]
    go (AggregateWindowed _ _ _) = ["AggregateWindowed"]
    go (CountSessionWindowed _)  = ["CountSessionWindowed"]
    go (AggregateSessionWindowed _ _ _ _) = ["AggregateSessionWindowed"]
    -- KGroupedTable
    go (GroupTableBy _ _)             = ["GroupTableBy"]
    go (CountKGroupedTable _)         = ["CountKGroupedTable"]
    go (ReduceKGroupedTable _ _ _)    = ["ReduceKGroupedTable"]
    go (AggregateKGroupedTable _ _ _ _) = ["AggregateKGroupedTable"]
    -- Cogroup
    go (Cogroup _)                    = ["Cogroup"]
    go (AddCogrouped _)               = ["AddCogrouped"]
    go (AggregateCogrouped _ _)       = ["AggregateCogrouped"]
    -- Joins
    go (StreamTableJoin _ _)          = ["StreamTableJoin"]
    go (StreamTableLeftJoin _ _)      = ["StreamTableLeftJoin"]
    go (StreamStreamJoin _ _ _)       = ["StreamStreamJoin"]
    go (StreamStreamLeftJoin _ _ _)   = ["StreamStreamLeftJoin"]
    go (StreamStreamOuterJoin _ _ _)  = ["StreamStreamOuterJoin"]
    go (TableTableJoin _ _)           = ["TableTableJoin"]
    go (TableTableLeftJoin _ _)       = ["TableTableLeftJoin"]
    go (TableTableOuterJoin _ _)      = ["TableTableOuterJoin"]
    go (ForeignKeyJoin _ _ _)         = ["ForeignKeyJoin"]
    go (LeftForeignKeyJoin _ _ _)     = ["LeftForeignKeyJoin"]
    go (StreamGlobalTableJoin _ _)    = ["StreamGlobalTableJoin"]
    go (StreamGlobalTableLeftJoin _ _) = ["StreamGlobalTableLeftJoin"]
    -- KTable
    go (FilterTable _ _)              = ["FilterTable"]
    go (FilterNotTable _ _)           = ["FilterNotTable"]
    go (MapValuesTable _ _)           = ["MapValuesTable"]
    go (TransformValuesTable nm _ _ _)
      = ["TransformValuesTable(" <> nm <> ")"]
    -- Suppress
    go (SuppressUntilTimeLimit _)     = ["SuppressUntilTimeLimit"]
    go (SuppressWindowedKS _ _)       = ["SuppressWindowed"]
    -- Processor API
    go (ProcessStream nm _ _)         = ["ProcessStream(" <> nm <> ")"]
    go (ProcessValuesStream nm _ _ _) = ["ProcessValuesStream(" <> nm <> ")"]
    go (TransformValuesStreamT nm _ _ _)
      = ["TransformValuesStream(" <> nm <> ")"]
    go (WithStateStoreKV b _)
      = ["WithStateStoreKV(" <> Store.unStoreName (Store.sbKvName b) <> ")"]
    go (WithStateStoreW b _)
      = ["WithStateStoreW("  <> Store.unStoreName (Store.sbWName  b) <> ")"]
    go (WithStateStoreS b _)
      = ["WithStateStoreS("  <> Store.unStoreName (Store.sbSName  b) <> ")"]
    -- Escape
    go (Lifted nm _)                  = ["Lifted(" <> nm <> ")"]

    showTopic :: TopicName -> Text
    showTopic = T.pack . show

-- | Render an AST as a single-line description. Useful at the REPL
-- and in test failure messages. The output is /not/ a stable
-- format — use it for debugging, not for parsing.
prettyPrint :: Topology i o -> Text
prettyPrint = T.intercalate " " . inspect
