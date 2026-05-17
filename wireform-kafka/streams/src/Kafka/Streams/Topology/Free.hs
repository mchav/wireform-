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
--
-- == Lineage and exactly-once semantics
--
-- Kafka Streams partitions a topology into /sub-topologies/ — the
-- connected components of the graph — and assigns one task per
-- sub-topology per partition. EOS-v2 commits transactionally
-- /per task/, so two processors share an EOS transaction iff
-- they're in the same sub-topology.
--
-- That has implications for how our lineage combinators interact
-- with EOS:
--
--   * 'Fanout' / 'Fork' / 'ForkN' / 'Tap' / 'Split' all branch
--     from /one/ upstream, so every branch lands in the
--     /same/ sub-topology as that upstream. EOS atomicity
--     extends to every sink in every branch.
--   * 'Parallel' over two source-rooted halves (the typical
--     "two unrelated pipelines combined into one
--     @Topology Void ()@") /does not/ share lineage: the two
--     halves compile to two disconnected sub-topologies and
--     therefore run as two tasks with independent EOS
--     transactions.
--   * Use 'mergeSourced' (or any other convergence point —
--     stream-stream join, shared 'Merge', etc.) to make two
--     source-rooted halves share a task.
--   * Alternative cross-task lineage: register the same state
--     store against both halves via 'withStateStoreKV' — the
--     task assigner treats co-owned state stores as a shared
--     lineage edge.
--
-- The 'Semigroup' / 'Monoid' instances over @'Topology' i ()@
-- run two pipelines on the /same/ upstream input, so they
-- share lineage by construction (single-source, multi-sink) —
-- another way to build EOS-atomic multi-output topologies.
--
-- == On side effects
--
-- 'Foreach' is /synchronous/: the supplied callback runs on the
-- stream-processing thread for every record before the engine
-- moves on. This is intentional and matches the JVM
-- @KStream.foreach@ contract. The reasons we do /not/ ship a
-- @foreachAsync@ constructor on top:
--
--   * No backpressure — fire-and-forget asyncs accumulate when the
--     handler is slower than the poll rate; the runtime OOMs
--     before the stream thread ever felt the slowness.
--   * Silent error swallowing — discarded async handles drop
--     failures on the floor.
--   * EOS-incompatibility — the async escapes the transactional
--     commit cycle; an abort can't roll it back.
--   * Out-of-order completion within a partition — Kafka Streams
--     guarantees per-key ordering on the stream thread, but
--     @async@ destroys that guarantee.
--
-- The JVM API doesn't ship an async @foreach@ either; the canonical
-- patterns for non-blocking work are:
--
-- [\"Sink + downstream consumer\"]
--   Publish to a side topic with 'sink', then run a separate
--   consumer that processes the side topic. The intermediate
--   topic provides durability, ordering, EOS, and natural
--   backpressure.
--
-- [\"Custom processor with bounded async pool\"]
--   Reach for 'processStream' / 'processWithStateStoreKV' and
--   build your own bounded queue + worker pool inside the
--   processor. The 'Processor' lifecycle hooks
--   ('procInit' \/ 'procClose') give you a place to allocate
--   and clean up. This keeps the slow work off the stream
--   thread /and/ caps the in-flight work.
--
-- [\"liftIO_ escape hatch\"]
--   For genuinely best-effort fire-and-forget work that's
--   acceptable to lose on a restart, use 'liftIO_' with an
--   explicit 'Control.Concurrent.Async.async'. Making the
--   user reach for 'liftIO_' is the deliberate friction: the
--   ergonomic GADT path doesn't lead callers into the footgun.
--
-- The underlying imperative
-- 'Kafka.Streams.KStream.foreachStreamAsync' from the original
-- DSL is still available for callers who explicitly want it;
-- this module just doesn't promote it to a first-class
-- 'Topology' constructor.
--
-- == Monad bind for incrementally-built topologies
--
-- The 'Monad' instance lets you bind a wire value to a
-- Haskell-level name and use it across multiple downstream
-- fragments. This is especially handy for the
-- /cogroup-shaped/ Kafka Streams operations where the JVM
-- API is incremental (each @addCogrouped@ call extends a
-- builder). 'applyT' threads a bound value into a downstream
-- fragment:
--
-- @
-- topology :: F.Topology Void (KTable Text Text)
-- topology = do
--   s1  <- F.source \"in1\" textSerde textSerde
--   s2  <- F.source \"in2\" textSerde textSerde
--   g1  <- F.groupByKey grp   \`F.applyT\` s1
--   g2  <- F.groupByKey grp   \`F.applyT\` s2
--   cg0 <- F.cogroup adder1   \`F.applyT\` g1
--   cg1 <- F.addCogrouped a2  \`F.applyT\` (cg0, g2)
--   F.aggregateCogrouped (pure \"\") mat \`F.applyT\` cg1
-- @
module Kafka.Streams.Topology.Free
  ( -- * The 'Topology' GADT
    Topology (..)
  , SplitBranch (..)

    -- * Compilation
  --
  -- 'compile' applies a curated set of /semantics-preserving/
  -- rewrites to the topology before walking it (see
  -- "Kafka.Streams.Topology.Free.Optimize"). Use
  -- 'compileNoOptimize' for golden-file or rewrite-debugging tests
  -- where you want the AST untouched. 'compileWithOptimization'
  -- lets you pick which rewrite families fire.
  , compile
  , compileNoOptimize
  , compileWithOptimization
  , compileInBuilder
  , compileWith
  , apply

    -- * Optimisation
  --
  -- See "Kafka.Streams.Topology.Free.Optimize" for the rewrite
  -- list and configuration. 'optimize' is also re-exported from
  -- "Kafka.Streams.Topology.Free.Optimize" so callers can pick
  -- either module without changing import lists.
  , optimize
  , optimizeWith
  , OptimizeConfig (..)
  , defaultOptimizeConfig
  , noOptimization
  , countNodes
  , OptimizationStats (..)
  , optimizationStats

    -- * Constants for the type signatures
  , TBuilder
  , buildTopologyFrom

    -- * Sources
  , source
  , sourceWith
  , sources
  , tableSource
  , globalTableSource
  , mergeSourced

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
  , processWithStateStoreKV
  , processWithStateStoreW
  , processWithStateStoreS

    -- * Escape hatch + introspection
  , liftIO_
  , inspect
  , prettyPrint

    -- * Profunctor- and Reader-shaped helpers
    --
    -- 'Topology' is a profunctor (contravariant in the input,
    -- covariant in the output). The methods are exposed as
    -- standalone functions rather than 'Data.Profunctor.Profunctor'
    -- typeclass instances to keep this package's dependency
    -- closure minimal. Users who want the typeclasses can write
    -- orphan instances in their own modules.
  , lmapT
  , rmapT
  , dimapT
  , askInput
  , localInput
  , applyT

    -- * Errors
  --
  -- Lazy errors the builder can surface when a downstream
  -- operation forces a field that wasn't fully populated
  -- upstream (the only situations where the AST is itself well-
  -- typed but partial). 'TopologyFreeError' is an 'Exception'
  -- so callers can 'Control.Exception.try' the offending action.
  , TopologyFreeError (..)
  , SerdeSide (..)
  ) where

import Prelude hiding (id, filter, (.))

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import qualified Control.Exception as Exception
import Control.Monad ((>=>))
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)

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
  -- on a tuple input.
  --
  -- /Independent/ at the topology graph level — there are no edges
  -- between the two subgraphs except the tuple shape at the call
  -- site. If both subgraphs are source-rooted (each compiles its
  -- own source node), the runtime task assigner will see two
  -- disconnected sub-topologies and assign them to /separate
  -- Kafka tasks/ — each with its own EOS transaction. To get
  -- cross-source EOS atomicity, use 'mergeSourced' (or insert a
  -- convergence node yourself with 'Merge') so both halves end
  -- up in one connected sub-topology.
  --
  -- See the module-level "Lineage" note for the full story.
  Parallel :: Topology a b -> Topology c d -> Topology (a, c) (b, d)

  -- | 'Control.Arrow.&&&': feed one upstream into two sub-fragments
  -- and pair the outputs. The compiler reuses the same upstream
  -- node as parent for both sub-graphs — this is the "one node,
  -- two children" pattern that fans a single lineage into two.
  --
  -- /Both children share the same Kafka task as the upstream/, so
  -- 'Fanout' on a single source is EOS-atomic across both
  -- branches by construction. (Contrast with 'Parallel' over two
  -- /different/ sources, which the runtime treats as two
  -- separate sub-topologies.)
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
  -- Monad bind
  --
  -- @'Bind' t k@ runs @t@ on the input to get a wire value, then
  -- invokes @k@ on that value to produce the continuation
  -- topology, which is then run on /the same/ input. Powers the
  -- 'Monad' instance for @'Topology' i@.
  --
  -- Unlike the rest of the GADT, @Bind@ has an /opaque/
  -- continuation function: the optimiser and 'inspect' can see
  -- the left side but not into @k@. Use applicative-style
  -- combinators ('Fanout', 'Parallel', '<*>') when you want a
  -- fully-static AST; reach for @Bind@ / do-notation when the
  -- ergonomic win justifies losing inspectability past the bind.
  -- ------------------------------------------------------------------
  Bind :: Topology i a -> (a -> Topology i b) -> Topology i b

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
  -- | Terminal side-effect sink. The callback runs synchronously
  -- on the stream-processing thread for every record. /No
  -- async variant is exposed/ — see the module-level note on
  -- side effects below for the rationale and the recommended
  -- patterns for non-blocking work.
  Foreach         :: (Record k v -> IO ())
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

  -- | Atomically register a processor /and/ a 'StoreBuilderKV' the
  -- processor depends on. The compiler generates one fresh
  -- 'Topo.NodeName' from the prefix, attaches the processor with
  -- the store's name in its @processorSpecStores@ list, and
  -- registers the store with that generated node as its owner —
  -- so the user doesn't need to (and can't predict the generated
  -- name). This is the recommended way to combine 'processStream'
  -- with 'withStateStoreKV' for typical custom-processor use.
  ProcessWithStateStoreKV
    :: !Text                                  -- ^ prefix
    -> !(StoreBuilderKV stk stv)
    -> !(IO (Processor k v))
    -> Topology (KStream k v) ()
  -- | 'ProcessWithStateStoreKV' for a window store.
  ProcessWithStateStoreW
    :: !Text
    -> !(StoreBuilderW stk stv)
    -> !(IO (Processor k v))
    -> Topology (KStream k v) ()
  -- | 'ProcessWithStateStoreKV' for a session store.
  ProcessWithStateStoreS
    :: !Text
    -> !(StoreBuilderS stk stv)
    -> !(IO (Processor k v))
    -> Topology (KStream k v) ()

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

-- | A 'Topology' is an 'Applicative' over its output type:
--
-- [@pure x@]
--   @'Arr' ('const' x)@ — a topology that ignores its input and
--   produces @x@.
-- [@tf '<*>' tx@]
--   Run both @tf@ and @tx@ on the same input via 'Fanout', then
--   apply. The resulting AST is entirely static — every node is
--   visible to 'inspect' and the optimiser.
--
-- Used internally by the 'Monoid'\/'Semigroup' instances below
-- and by 'liftA2' / 'liftA3' from "Control.Applicative".
instance Applicative (Topology i) where
  pure x = Arr (const x)
  tf <*> tx = Compose (Arr (uncurry ($))) (Fanout tf tx)

-- | A 'Topology' is a /reader/ 'Monad' over its input type: the
-- input is the environment threaded into every bind.
--
-- @t '>>=' k@ runs @t@ on the input to obtain a value @a@, then
-- runs @k a@ on the /same/ input. This is the same shape as
-- @ReaderT i (Topology i)@ collapsed into the GADT itself.
--
-- The bind continuation is /opaque/: 'inspect' and the optimiser
-- see the left side of the bind but not into the function.
-- Prefer applicative-style combinators ('Fanout', '<*>',
-- 'Parallel', 'Fork') when full inspectability matters. Reach
-- for the monad when the do-notation ergonomics outweigh the
-- inspectability loss — typically when assembling a topology
-- from several upstream sources whose handles you want to bind
-- as Haskell-level names:
--
-- @
-- combined :: F.Topology Void ()
-- combined = do
--   s1 <- F.source \"in1\" textSerde textSerde
--   s2 <- F.source \"in2\" textSerde textSerde
--   pure (s1, s2)
--   '>>>' F.merge
--   '>>>' F.sink  \"out\" textSerde textSerde
-- @
instance Monad (Topology i) where
  return = pure
  (>>=) = Bind

-- | Pointwise 'Semigroup' over the output. @t1 '<>' t2@ runs both
-- topologies on the same input via 'Fanout' and combines their
-- outputs with @('<>')@. Especially useful at @o = ()@ where it
-- gives a natural \"run several closed pipelines on one input\"
-- combinator — but works for any 'Semigroup' output (e.g. lists
-- of records, maps of named branches).
instance Semigroup o => Semigroup (Topology i o) where
  t1 <> t2 = Compose (Arr (uncurry (<>))) (Fanout t1 t2)

-- | 'mempty' is the topology that ignores its input and produces
-- 'mempty'. At @o = ()@ this is the no-op pipeline; at
-- @o = [a]@ it's the empty-record stream; etc.
instance Monoid o => Monoid (Topology i o) where
  mempty = Arr (const mempty)

----------------------------------------------------------------------
-- Profunctor- and Reader-shaped helpers
----------------------------------------------------------------------
--
-- 'Topology' is a 'Data.Profunctor.Profunctor', a
-- 'Data.Profunctor.Strong' profunctor (via 'First' / 'Second'),
-- and a 'Data.Profunctor.Choice' profunctor (via 'LeftT' /
-- 'RightT'). Rather than depending on the @profunctors@ package
-- for a one-line instance, we expose the methods as standalone
-- functions. Users who want the typeclasses can write orphan
-- instances in their own modules.

-- | Pre-compose with a pure function (contravariant in the input).
-- Equivalent to 'Data.Profunctor.lmap'.
lmapT :: (a -> b) -> Topology b c -> Topology a c
lmapT f t = Compose t (Arr f)

-- | Post-compose with a pure function (covariant in the output).
-- Equivalent to 'Data.Profunctor.rmap' and to 'fmap' for the
-- 'Functor' instance.
rmapT :: (c -> d) -> Topology b c -> Topology b d
rmapT g t = Compose (Arr g) t

-- | Pre- and post-compose with pure functions. Equivalent to
-- 'Data.Profunctor.dimap'.
dimapT :: (a -> b) -> (c -> d) -> Topology b c -> Topology a d
dimapT f g t = rmapT g (lmapT f t)

-- | The input wire is the \"environment\" of the 'Topology'
-- reader. @askInput@ is just 'Cat.id'; the alias exists so
-- do-notation reads naturally:
--
-- @
-- duplicate :: F.Topology a (a, a)
-- duplicate = do
--   x \<- askInput
--   pure (x, x)
-- @
askInput :: Topology i i
askInput = Id

-- | Run a topology with the input transformed by @f@. The
-- profunctor analogue of @MonadReader@'s 'Control.Monad.Reader.local'.
-- Slightly more general than the @mtl@ shape: @f@'s codomain
-- doesn't have to match the original input type — the
-- transformed-input value just has to fit the topology being run.
localInput :: (i' -> i) -> Topology i a -> Topology i' a
localInput f t = Compose t (Arr f)

-- | Pre-feed a fixed value into a topology, producing a
-- 'Topology i b' from a 'Topology a b' regardless of the
-- outer input type @i@. Useful inside do-notation: you've
-- bound a wire value as a Haskell name and want to thread it
-- into a downstream operator that expects an @a@ input.
--
-- @
-- combinedSources :: F.Topology Void (KStream Text Text)
-- combinedSources = do
--   s1 <- F.source "in1" textSerde textSerde
--   s2 <- F.source "in2" textSerde textSerde
--   -- 'merge' expects @(KStream, KStream)@ as its input.
--   F.merge \`F.applyT\` (s1, s2)
-- @
--
-- Operationally identical to @'Compose' t ('Arr' ('const' a))@,
-- which is also @'pure' a '>>>' t@ via the 'Applicative' instance.
applyT :: Topology a b -> a -> Topology i b
applyT t a = Compose t (Arr (const a))

----------------------------------------------------------------------
-- Compilation
----------------------------------------------------------------------

-- | Type alias just to make signatures shorter at call sites.
type TBuilder = StreamsBuilder

-- | The open-ended AST interpreter. Walks a 'Topology' bottom-up,
-- compiling each leaf primitive against the shared
-- 'StreamsBuilder' (which holds the in-progress
-- 'Topo.Topology' graph) and threading the wire value @i@ down
-- the chain.
--
-- This is the function 'compile' uses internally after running
-- the optimiser. Exposed publicly so users can splice a
-- 'Topology' fragment into a hand-rolled imperative builder
-- chain — e.g. when migrating from the imperative DSL to the
-- typed GADT incrementally.
--
-- == Contract
--
--   * The 'StreamsBuilder' is the shared builder state — every
--     leaf primitive registers nodes / sinks / stores against
--     it via 'withTopology_'.
--   * The wire value @i@ is what feeds the AST. For
--     @Topology Void o@, @i@ is 'Data.Void.Void' and the
--     interpreter discharges it with a thunk that aborts
--     ('TopologyFreeError' / 'VoidInputForced') if any
--     Source primitive tries to inspect it — which by
--     construction they don't.
--   * The result @o@ is whatever the AST produces — a
--     'KStream', a 'KTable', a tuple, '()', etc.
--
-- /Does not/ run the optimiser. Apply 'optimize' yourself
-- before calling 'apply' if you want the rewrites.
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

    -- Sources (the Void input is never inspected — neither the
    -- 'Source', 'SourceMulti', 'TableSource', nor 'GlobalSource'
    -- pattern-match on it).
    go (Source topic c)         = \_void -> KS.streamFromTopic b topic c
    go (SourceMulti topics c)   = \_void -> sourceMultiCompile b topics c
    go (TableSource topic c m)  = \_void -> KT.tableFromTopic b topic c m
    go (GlobalSource topic c m) = \_void -> GT.globalTable b topic c m

    -- Sinks
    go (Sink topic p)        = KS.toTopic topic p
    go (SinkExtracted e p)   = KS.toExtracted e p
    go (Through topic p)     = KS.throughTopic topic p

    -- Monad bind: run @t@ to get the wire value, then run the
    -- continuation @k a@ on the /same/ input. Both sub-topologies
    -- compile against the shared builder, so any source / sink /
    -- store registration each one performs lands in the same
    -- graph.
    go (Bind t k)            = \i -> do
      !a <- go t i
      go (k a) i

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

    go (ProcessWithStateStoreKV prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbKvName builder)
        (Topo.addStateStoreKV builder)
    go (ProcessWithStateStoreW prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbWName builder)
        (Topo.addStateStoreW builder)
    go (ProcessWithStateStoreS prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbSName builder)
        (Topo.addStateStoreS builder)

    -- Escape
    go (Lifted _ act)        = act b

-- | Attach a custom 'Processor' to a 'KStream' while atomically
-- registering the state store it owns. The fresh processor node
-- name is the only thing that gets generated; the store is
-- registered against that same node so the validator's
-- store-ownership check passes without callers needing to know
-- the generated name.
--
-- The @attachStore@ function is whichever of
-- 'Topo.addStateStoreKV' / 'Topo.addStateStoreW' /
-- 'Topo.addStateStoreS' is appropriate. The @storeNm@ is the
-- store's name (used to populate 'processorSpecStores').
attachProcessorWithStore
  :: TBuilder
  -> KStream k v
  -> Text                                            -- ^ prefix
  -> IO (Processor k v)                              -- ^ supplier
  -> StoreName                                       -- ^ store name
  -> ([Topo.NodeName] -> Topo.Topology -> Topo.Topology)
  -- ^ topology-level @addStateStore*@ for the store's type
  -> IO ()
attachProcessorWithStore b s prefix supplier storeNm attachStore = do
  nm <- freshNodeName b prefix
  withTopology_ b $ \topo ->
    let !topo1 = Topo.addProcessorWith
                   Topo.ProcessorSpec
                     { Topo.processorSpecName     = nm
                     , Topo.processorSpecParents  = [KS.kstreamParent s]
                     , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
                     , Topo.processorSpecStores   = [storeNm]
                     }
                   topo
        !topo2 = attachStore [nm] topo1
     in topo2

-- | Multi-topic source helper. The existing
-- 'Kafka.Streams.KStream.streamFromTopic' takes a single 'TopicName';
-- the multi-topic variant calls the low-level
-- 'Kafka.Streams.Topology.addSource' directly and constructs the
-- same 'KStream' handle.
--
-- The runtime's source-topic set is a single list per source node, so
-- the multi-topic case is the natural shape; the single-topic
-- 'streamFromTopic' is just sugar over it. The helper takes a
-- 'NonEmpty' rather than '[]' so the empty-list case isn't even
-- representable — calling this with an empty 'NonEmpty' is a type
-- error, eliminating the partial-function pitfall.
sourceMultiCompile
  :: TBuilder
  -> NonEmpty TopicName
  -> Consumed k v
  -> IO (KStream k v)
sourceMultiCompile b ts c = do
  nm <- freshNodeName b "KSTREAM-SOURCE-MULTI"
  withTopology_ b $
    Topo.addSource nm
                   (NE.toList ts)
                   (consumedKeySerde c)
                   (consumedValueSerde c)
                   (consumedExtractor c)
  pure (KS.KStream
          { KS.kstreamBuilder    = b
          , KS.kstreamParent     = nm
          , KS.kstreamKeySerde   = consumedKeySerde c
          , KS.kstreamValueSerde = consumedValueSerde c
          })

-- | Promote an aggregation handle to a real 'KTable'.
--
-- The 'CountedTableLocal' handle the existing DSL returns from
-- aggregations is a thin wrapper around a 'KTable' — it just
-- doesn't carry the serdes. We extract the serdes from the
-- supplied 'Materialized'. If they're absent the corresponding
-- 'KTable' field becomes a /deferred/ 'TopologyFreeError' —
-- matching the existing 'KTable' lazy-serde behaviour after
-- @mapValues@ etc. Downstream @to@\/@join@ that forces the
-- serde will raise the exception at the actual use site.
--
-- Unlike a bare 'error', the deferred thunk uses
-- 'TopologyFreeError' so callers who want to surface friendlier
-- diagnostics can catch it (via 'Control.Exception.evaluate' on
-- the thunk, or by wrapping the downstream operation in
-- 'Control.Exception.try').
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
        Nothing -> Exception.throw (MissingMaterializedSerde KeySide)
  , KT.ktableValueSerde =
      case Mat.matValueSerde m of
        Just s  -> s
        Nothing -> Exception.throw (MissingMaterializedSerde ValueSide)
  }

-- | Whether the missing serde was the key or value side of the
-- 'Materialized'.
data SerdeSide = KeySide | ValueSide
  deriving stock (Eq, Show, Generic)

-- | The (small) set of partial conditions the Free topology builder
-- can land in. All currently land /lazily/: they aren't raised at
-- AST-construction time, only when a downstream operation forces
-- the affected field (typically @to@, @join@, or @repartition@
-- asking for a serde, or the runtime asking for a Void input on a
-- topology that bypassed the smart constructors).
--
-- Because 'TopologyFreeError' is an 'Exception.Exception' callers
-- can catch it via 'Control.Exception.try' \/
-- 'Control.Exception.evaluate', rather than the bare 'error' calls
-- that preceded this type. Pattern-match on the constructor to
-- decide whether the user can fix the call site
-- ('MissingMaterializedSerde') or it's a library bug
-- ('VoidInputForced').
data TopologyFreeError
  = -- | An aggregation result was promoted to a 'KTable' but the
    -- supplied 'Materialized' didn't carry the named serde. Supply
    -- one via @'Mat.withKeySerde'@ or @'Mat.withValueSerde'@ when
    -- constructing the 'Materialized', /or/ ensure the downstream
    -- operation that forced the serde gets it through @Produced@
    -- (for sinks) \/ @Joined@ (for joins) instead of relying on
    -- the @KTable@'s embedded serde.
    MissingMaterializedSerde !SerdeSide
  | -- | A 'Source' (or any closed-input) primitive inspected its
    -- 'Void' input. By construction sources never look at the
    -- void; if you see this exception, please file a bug. The
    -- 'Text' is the location label from the compile entry point.
    VoidInputForced !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception.Exception)

-- | Compile a closed-input topology into the existing
-- 'Kafka.Streams.Topology.Topology' graph.
--
-- 'compile' is the canonical "AST → runnable topology" entry
-- point. It:
--
--   1. Runs the supplied AST through the default rewrite passes
--      ('defaultOptimizeConfig') — see "Kafka.Streams.Topology.Free.Optimize"
--      for the full list. The rewrites are semantics-preserving;
--      the result has fewer processor nodes but identical
--      observable behaviour.
--   2. Creates a fresh 'StreamsBuilder' and walks the optimised
--      AST, registering each leaf primitive (sources, sinks,
--      transforms, joins, aggregators, state stores) against
--      the builder.
--   3. Snapshots the builder into a 'Topo.Topology' graph and
--      returns it alongside the AST's output wire value.
--
-- The output @o@ is whatever the AST says it is — typically @()@
-- for a sink-closed pipeline, but a freshly-compiled 'KStream'
-- / 'KTable' / 'CogroupedStream' is also valid (useful when
-- you want to splice the resulting wire value into a
-- hand-rolled imperative chain afterwards).
--
-- == Variants
--
--   * 'compileNoOptimize' — same shape but skips the rewrite
--     passes. Use for golden-file tests where you need
--     node-for-node correspondence between the AST and the
--     compiled graph.
--   * 'compileWithOptimization' — explicit 'OptimizeConfig' for
--     per-pass control (toggle individual rewrite families).
--   * 'compileInBuilder' — compile against a pre-existing
--     'StreamsBuilder' for splicing into an imperative
--     pipeline.
compile :: Topology Void o -> IO (o, Topo.Topology)
compile = compileWithOptimization defaultOptimizeConfig

-- | Like 'compile' but skips the rewrite passes entirely
-- (applies 'noOptimization'). Use when:
--
--   * You need node-for-node correspondence between the
--     'Topology' AST and the compiled 'Topo.Topology' graph
--     (golden-file tests).
--   * You're debugging an unexpected optimiser interaction
--     and want to confirm the un-optimised path behaves
--     correctly.
--   * You're benchmarking the un-optimised path to
--     quantify the optimiser's win.
--
-- The /default/ should be 'compile'. Reach for
-- 'compileNoOptimize' only when one of the above applies.
compileNoOptimize :: Topology Void o -> IO (o, Topo.Topology)
compileNoOptimize = compileWithOptimization noOptimization

-- | 'compile' with an explicit 'OptimizeConfig'. Use when:
--
--   * You want to enable /one/ rewrite family for an A\/B
--     comparison (toggle individual @optFuse*@ flags).
--   * You want to /disable/ a specific rewrite family that's
--     interacting with caller-side assumptions (e.g. a
--     bespoke optimiser pass downstream that wants
--     un-fused @Filter@ nodes).
--   * You want to extend 'optMaxPasses' for an unusually
--     deep AST.
--
-- For the default everything-enabled or everything-disabled
-- cases, use 'compile' or 'compileNoOptimize'.
compileWithOptimization
  :: OptimizeConfig -> Topology Void o -> IO (o, Topo.Topology)
compileWithOptimization cfg t = do
  b <- newStreamsBuilder
  let !t' = optimizeWith cfg t
  o <- apply t' b voidInput
  topo <- buildTopology b
  pure (o, topo)
  where
    -- By construction every 'Topology Void _' is rooted in one of
    -- the source constructors, and none of them inspect the input
    -- value. The thunk below is therefore unreachable code; if it
    -- ever fires it's because someone bypassed the smart
    -- constructors (e.g. via 'unsafeCoerce' or a custom 'Lifted'
    -- handler that destructures Void). We throw a typed
    -- 'TopologyFreeError' rather than a bare 'error' so callers
    -- can catch it and report at their preferred granularity.
    voidInput :: Void
    voidInput = Exception.throw (VoidInputForced "compileWithOptimization")

-- | Compile a closed-input 'Topology' against a pre-existing
-- 'StreamsBuilder'. Useful when splicing a 'Topology' into a
-- pipeline being built imperatively through
-- "Kafka.Streams.StreamsBuilder" — the imperative chain holds
-- the builder, and 'compileInBuilder' adds the AST's nodes to
-- the same builder.
--
-- /Does not/ run the optimiser — when splicing into an
-- existing imperative graph callers usually want node-for-node
-- correspondence between the 'Topology' AST and the resulting
-- builder nodes (the imperative graph has its own naming
-- conventions; fusion would change those).
--
-- Returns just the AST output value (not the topology graph)
-- because the caller is managing the builder externally.
compileInBuilder :: TBuilder -> Topology Void o -> IO o
compileInBuilder b t = apply t b voidInput
  where
    voidInput :: Void
    voidInput = Exception.throw (VoidInputForced "compileInBuilder")

-- | Deprecated alias for 'compileInBuilder'. Kept so existing
-- call sites don't break; new code should use
-- 'compileInBuilder' directly.
compileWith :: TBuilder -> Topology Void o -> IO o
compileWith = compileInBuilder

-- | Compile a closed-output topology and return only the
-- resulting 'Topo.Topology' graph, discarding the AST output
-- (which is @()@ for sink-closed pipelines).
--
-- Convenience wrapper around 'compile' for the common case of
-- "I just want the topology graph, ready to feed into a
-- 'newDriver' or 'KafkaStreams' runtime."
buildTopologyFrom :: Topology Void () -> IO Topo.Topology
buildTopologyFrom = fmap snd . compile

----------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------

-- * Sources --------------------------------------------------------
--
-- Every closed 'Topology' starts with at least one source: a
-- pipeline that doesn't start with a 'Source' (or one of the
-- table / global variants) can't be closed on the input side,
-- because all source primitives have @'Void'@ as their input
-- type — the only way to discharge the open @Void@ slot is to
-- pin a source there. (Composing a source on the right
-- automatically lifts the whole pipeline to @Topology Void o@.)

-- | Subscribe to a Kafka topic and emit records as a
-- 'KStream'. Uses the default 'Consumed' configuration:
-- /record-timestamp/ timestamp extractor, /earliest/
-- offset-reset, no explicit node name.
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic)@.
--
-- The 'Serde's deserialise records as the runtime polls
-- them; downstream operators see the typed @'KStream' k v@.
-- For non-default 'Consumed' (e.g. custom timestamp
-- extractor, latest offset reset) use 'sourceWith'. For
-- multi-topic sources, see 'sources'.
source :: Text -> Serde k -> Serde v -> Topology Void (KStream k v)
source t ks vs = sourceWith (topicName t) (consumed ks vs)

-- | Subscribe to a Kafka topic with a fully-specified
-- 'Consumed'.
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic, Consumed)@.
--
-- The 'Consumed' carries the key and value serdes, the
-- timestamp extractor (use 'Consumed.withTimestampExtractor'
-- to override), and the offset-reset policy. The default
-- 'Consumed' (via 'source') uses record-timestamp +
-- earliest-offset; reach for 'sourceWith' when you need to
-- override either of those, or to pin an explicit
-- source-node name via 'Consumed.withName'.
sourceWith :: TopicName -> Consumed k v -> Topology Void (KStream k v)
sourceWith = Source

-- | Subscribe to N topics simultaneously, fanning them into
-- a single 'KStream'. All topics must use the same key and
-- value serdes (records from different topics are
-- indistinguishable downstream — use 'tap' / 'peek' on
-- 'recordHeaders' if you need to track which topic a record
-- came from).
--
-- /JVM equivalent:/ @StreamsBuilder.stream(Collection<String>)@.
--
-- Use this for fan-in patterns where multiple upstream
-- producers feed a single logical topic — e.g. per-region
-- topics merged into one regionless pipeline. For two
-- /different/ source-rooted pipelines that should share EOS,
-- use 'mergeSourced' instead (which keeps the value types
-- separate up to the merge point).
sources
  :: NonEmpty Text -> Serde k -> Serde v -> Topology Void (KStream k v)
sources ts ks vs = SourceMulti (fmap topicName ts) (consumed ks vs)

-- | Subscribe to a Kafka topic and materialise it as a
-- 'KTable' (latest-value-per-key) backed by an in-memory
-- state store.
--
-- /JVM equivalent:/ @StreamsBuilder.table(topic)@.
--
-- Tombstone records (records with null value) delete the
-- key from the store; non-tombstones insert or update. The
-- resulting 'KTable' can be queried via the standard
-- Interactive Queries API, joined against, or converted
-- back into a 'KStream' via 'toStream'.
--
-- For finer control over the materialised store (named
-- store, custom serdes, RocksDB backend, etc.) call the
-- 'TableSource' constructor directly or fall back to
-- 'liftIO_' with the imperative @tableFromTopic@.
tableSource
  :: Ord k => Text -> Serde k -> Serde v -> Topology Void (KTable k v)
tableSource t ks vs =
  TableSource (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

-- | Subscribe to a Kafka topic and materialise it as a
-- /global/ 'GlobalKTable' — a cluster-wide replicated table.
--
-- /JVM equivalent:/ @StreamsBuilder.globalTable(topic)@.
--
-- Unlike 'tableSource', a 'GlobalKTable' is /not/
-- partitioned: every Kafka Streams instance maintains its
-- own full copy of the table. That makes it ideal for
-- "lookup table" patterns — see 'streamGlobalTableJoin' /
-- 'streamGlobalTableLeftJoin' for the join shape, which
-- /doesn't/ require co-partitioning.
--
-- The memory cost is proportional to the full table size on
-- every instance; use only for small-to-moderate reference
-- data (customer details, product catalogs, feature flags)
-- rather than high-volume transactional data.
globalTableSource
  :: Ord k => Text -> Serde k -> Serde v -> Topology Void (GlobalKTable k v)
globalTableSource t ks vs =
  GlobalSource (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

-- | Merge two source-rooted streams into a single downstream lineage
-- so they share a Kafka task (and therefore an EOS transaction).
--
-- Operationally identical to:
--
-- @
-- (left '&&&' right) '>>>' 'merge'
-- @
--
-- but named for the use case it enables: cross-source
-- exactly-once atomicity. Without the convergence at 'merge',
-- the Kafka task assigner sees two disconnected sub-topologies
-- and groups them into /separate/ tasks — each with its own EOS
-- coordinator and its own commit cycle.
--
-- == When to use this
--
-- Use 'mergeSourced' whenever you want two independent source
-- topics to be processed in a single transaction. The merged
-- output is a single 'KStream' carrying records from both — feed
-- it into your downstream pipeline as usual.
--
-- For sources with /different/ value types, merge after a value
-- map that brings them to a common shape, e.g.:
--
-- @
-- mergeSourced
--   (F.source "a" ks vsA '>>>' F.mapValues 'Left')
--   (F.source "b" ks vsB '>>>' F.mapValues 'Right')
-- @
--
-- For shared-state EOS without value merging (each source still
-- writes to its own output topic but they share an EOS
-- transaction because they share a state store), use
-- 'withStateStoreKV' with the same store registered against
-- both subgraphs' processors.
mergeSourced
  :: Topology Void (KStream k v)
  -> Topology Void (KStream k v)
  -> Topology Void (KStream k v)
mergeSourced left right = Compose Merge (Fanout left right)

-- * Sinks ----------------------------------------------------------
--
-- A sink closes off a 'KStream' lineage by sending its records
-- to a Kafka topic. The output wire is @()@ — there's no
-- downstream continuation. If you need to /both/ sink the
-- records and continue processing them, use 'through' (which
-- sinks and re-subscribes from the broker) or 'tap' (which
-- runs the sink as a side pipeline without leaving the
-- in-process lineage).

-- | Publish every record on the stream to the given topic.
-- The 'Serde's serialise the key and value as records are
-- produced.
--
-- /JVM equivalent:/ @KStream.to(topic, Produced.with(ks, vs))@.
--
-- This is the canonical pipeline-closing operation. Under
-- EOS the writes go through the bound transactional
-- producer and commit atomically with the task's offset
-- commit. Use 'sinkWith' if you need to override the
-- partitioner or the auto-generated sink-node name.
sink :: Text -> Serde k -> Serde v -> Topology (KStream k v) ()
sink t ks vs = sinkWith (topicName t) (produced ks vs)

-- | 'sink' with a fully-specified 'Produced'. Use this when you
-- need to override the default sink-node name, supply a custom
-- partitioner, or otherwise reach into the lower-level
-- @Produced@ configuration. For the common case of "publish
-- with these serdes", prefer 'sink'.
sinkWith :: TopicName -> Produced k v -> Topology (KStream k v) ()
sinkWith = Sink

-- | Per-record dynamic-topic sink (KIP-303). The supplied
-- 'KS.TopicNameExtractor' is consulted for every record and
-- decides which topic that record lands in. The wire is closed
-- ('()' output).
--
-- /JVM equivalent:/ @KStream.to(TopicNameExtractor, Produced)@.
--
-- Use when you're routing records to one of many topics based
-- on record content — e.g. per-tenant topics, per-region topics
-- — without enumerating them in the topology itself.
sinkExtracted
  :: KS.TopicNameExtractor k v -> Produced k v -> Topology (KStream k v) ()
sinkExtracted = SinkExtracted

-- | Publish to a topic /and/ immediately re-subscribe from it,
-- returning a fresh upstream-equivalent 'KStream' that flows
-- through the broker. Mirrors @KStream.through@.
--
-- == When to use
--
-- 'through' is the canonical way to introduce a /task boundary/:
-- the records get serialised into the topic, durably stored on
-- the broker, and re-fetched on the downstream side. Use it
-- when you want:
--
--   * a re-partition with explicit broker storage in between,
--   * a fault-tolerance boundary between expensive upstream and
--     downstream work,
--   * to expose an intermediate result as a real Kafka topic
--     that other consumers can observe.
--
-- Note that 'through' is a /sink + source pair/, so it costs
-- one round-trip through the broker. If you don't need the
-- durability, use 'repartition' (logical re-partition, broker
-- still involved but no extra read) or stay in-process.
through :: Text -> Serde k -> Serde v -> Topology (KStream k v) (KStream k v)
through t ks vs = Through (topicName t) (produced ks vs)

-- * Stateless 'KStream' transforms --------------------------------
--
-- The stateless transforms apply a pure (or @IO@) function to
-- every record and produce a fresh 'KStream' downstream. None of
-- them require a state store, none of them block on broker
-- I/O, and all are individually exposed as their own processor
-- nodes in the compiled topology (subject to the optimiser
-- fusing adjacent same-shape transforms — see
-- 'OptimizeConfig').
--
-- == Repartition implications
--
-- Transforms that change the /key/ ('mapKeyValue', 'mapKeyValueM',
-- 'flatMapKeyValue', 'selectKey') mark the stream as
-- /needing repartition/ for any subsequent stateful operation
-- (groupBy, join, aggregate). Use 'repartition' explicitly when
-- you want to force the shuffle at a particular point — Kafka's
-- broker-side partitioning is what guarantees per-key ordering,
-- so re-keying without repartitioning would put records for the
-- same key on different tasks.

-- | Apply a pure function to the value side of every record,
-- preserving the key, timestamp, and headers.
--
-- /JVM equivalent:/ @KStream.mapValues(ValueMapper)@.
--
-- Use 'mapValues' over 'mapKeyValue' whenever you don't need to
-- change the key — keeping the key stable means downstream
-- stateful operations don't trigger a repartition.
--
-- The optimiser fuses chains of 'mapValues' into one
-- @MapValues (compose ...)@ node, so writing small,
-- single-responsibility maps is cheap at run time.
mapValues :: (v -> v') -> Topology (KStream k v) (KStream k v')
mapValues = MapValues

-- | Effectful 'mapValues': the value-transform runs in 'IO',
-- so it can read from external resources, emit metrics, or
-- otherwise side-effect per record. The 'IO' action runs
-- /synchronously on the stream thread/ — slow handlers
-- backpressure the broker poll loop.
--
-- /JVM equivalent:/ no direct match; closest is
-- @ValueMapper@ + an out-of-band side channel. We model the
-- @IO@ in the type so the cost is visible at the call site.
--
-- For purely-stateless effects (logging, metrics) prefer
-- 'peek' (which doesn't mutate the value). For effects that
-- need exactly-once semantics, route through a state store
-- and gate on an idempotency token (the same caveat applies
-- to JVM @transformValues@).
mapValuesM :: (v -> IO v') -> Topology (KStream k v) (KStream k v')
mapValuesM = MapValuesM

-- | Re-key /and/ re-value every record with a pure function.
--
-- /JVM equivalent:/ @KStream.map(KeyValueMapper)@.
--
-- == Repartition warning
--
-- 'mapKeyValue' changes the record key, which means downstream
-- stateful operations (groupBy, join, aggregate) need a
-- 'repartition' to re-shuffle records onto the correct
-- partitions. The runtime doesn't auto-insert this — call
-- 'repartition' explicitly when the next stage is stateful,
-- /unless/ you've maintained the partition assignment (i.e.
-- the new key has the same hash as the old).
--
-- If you only need to change the /key/ and not the value,
-- prefer 'selectKey'. If you only need to change the value,
-- prefer 'mapValues' (avoids the repartition flag).
mapKeyValue
  :: (k -> v -> (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValue = MapKeyValue

-- | Effectful 'mapKeyValue'. Same trade-offs as 'mapValuesM'
-- (synchronous on the stream thread, slow handlers
-- backpressure) plus the repartition implications of
-- 'mapKeyValue'.
mapKeyValueM
  :: (k -> v -> IO (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValueM = MapKeyValueM

-- | Keep records for which the predicate returns 'True'; drop
-- the rest.
--
-- /JVM equivalent:/ @KStream.filter(Predicate)@.
--
-- The predicate sees the entire 'Record', not just the value,
-- so you can filter on key, timestamp, or headers as well.
-- Chains of 'filter' fuse into a single @Filter (conj ...)@
-- node automatically.
filter :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filter = Filter

-- | Inverse of 'filter': drop records for which the predicate
-- returns 'True'. Same fusion characteristics as 'filter'
-- (chains collapse via de Morgan).
--
-- /JVM equivalent:/ @KStream.filterNot(Predicate)@.
filterNot :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filterNot = FilterNot

-- | Expand each record into zero or more output records,
-- changing only the value (key + timestamp + headers all
-- inherited from the input record).
--
-- /JVM equivalent:/ @KStream.flatMapValues(ValueMapper)@.
--
-- Useful for record-splitting (e.g. line → words) and
-- conditional emission (e.g. @Just x -> [x]; Nothing -> []@).
-- An empty list drops the input record entirely.
flatMapValues :: (v -> [v']) -> Topology (KStream k v) (KStream k v')
flatMapValues = FlatMapValues

-- | Expand each record into zero or more output records,
-- potentially changing both key and value.
--
-- /JVM equivalent:/ @KStream.flatMap(KeyValueMapper)@.
--
-- Like 'mapKeyValue', this marks the stream as needing a
-- repartition for downstream stateful ops; insert
-- 'repartition' before any stateful stage if you're changing
-- the partitioning.
flatMapKeyValue
  :: (k -> v -> [(k', v')]) -> Topology (KStream k v) (KStream k' v')
flatMapKeyValue = FlatMapKeyValue

-- | Run a side-effecting observer per record and pass the
-- record through unchanged. Useful for metrics, logging, or
-- debug taps that shouldn't alter the data path.
--
-- /JVM equivalent:/ @KStream.peek(ForeachAction)@.
--
-- 'peek' runs synchronously on the stream thread — same
-- backpressure caveat as 'mapValuesM' / 'foreach'. For a
-- non-data-altering "tap to a side topic", use 'tap' with a
-- sink as the side pipeline.
peek :: (Record k v -> IO ()) -> Topology (KStream k v) (KStream k v)
peek = Peek

-- | Terminal side-effect sink. The callback runs /synchronously/
-- on the stream-processing thread for every record. The wire
-- is closed (@()@ output) — there's no downstream
-- continuation.
--
-- /JVM equivalent:/ @KStream.foreach(ForeachAction)@.
--
-- There is /no/ @foreachAsync@ in this module on purpose. See
-- the module-level note "On side effects" for the rationale
-- and the recommended patterns for non-blocking work.
foreach :: (Record k v -> IO ()) -> Topology (KStream k v) ()
foreach = Foreach

-- | Re-key the stream using a function of the full 'Record'.
-- The value is preserved unchanged.
--
-- /JVM equivalent:/ @KStream.selectKey(KeyValueMapper)@.
--
-- 'selectKey' marks the stream as needing a repartition for
-- downstream stateful operations. Prefer it over
-- 'mapKeyValue' when you don't need to change the value —
-- the optimiser pairs @selectKey >>> groupByKey@ into a
-- single @groupBy@ node which matches the Apache docs'
-- recommended pattern.
selectKey :: (Record k v -> k') -> Topology (KStream k v) (KStream k' v)
selectKey = SelectKey

-- | Drop the key — the resulting stream is keyed by @()@.
--
-- /JVM equivalent:/ @KStream.values()@.
--
-- Idempotent — chained 'values' calls collapse into one. Use
-- when you want to disable per-key partitioning for a
-- subsequent operation that doesn't need the key (e.g. a
-- global counter via 'count').
values :: Topology (KStream k v) (KStream () v)
values = Values

-- | Show-debugging sink: pretty-print each record via the
-- supplied line writer. The 'Text' label is prepended to
-- every printed line; @line writer@ is typically 'putStrLn',
-- 'hPutStrLn' against an open file handle, or a structured
-- logger callback.
--
-- /JVM equivalent:/ @KStream.print(Printed)@.
--
-- Named 'prints' (with an @s@) to avoid clashing with
-- 'Prelude.print'. For production-grade structured logging,
-- use 'peek' / 'foreach' against your logger; 'prints' is
-- optimised for REPL exploration and ad-hoc debugging.
prints
  :: (Show k, Show v)
  => Text -> (String -> IO ()) -> Topology (KStream k v) ()
prints = Print

-- * 'KStream' composition + branching -----------------------------

-- | Binary merge of two streams of the same shape into one. The
-- output stream interleaves records from both inputs in
-- per-partition arrival order; per-key ordering is preserved
-- /within/ each input but the relative order across inputs is
-- not specified.
--
-- /JVM equivalent:/ @KStream.merge(KStream)@.
--
-- The input is a tuple, so chain with 'Fanout' / @'&&&'@ /
-- 'mergeSourced' to get the two streams in place. For more
-- than two streams use 'mergeAll'.
merge :: Topology (KStream k v, KStream k v) (KStream k v)
merge = Merge

-- | N-ary merge: a list of streams collapses to one. The list
-- is consumed at AST-application time, so the count is
-- bounded at compile time per call site. An empty list is a
-- run-time error (matches the imperative
-- 'Kafka.Streams.KStream.mergeStreamsN' semantics).
--
-- /JVM equivalent:/ chained @merge@ calls.
mergeAll :: Topology [KStream k v] (KStream k v)
mergeAll = MergeAll

-- | Predicate-routed split into N sub-streams.
--
-- Each input record is routed to the first sub-stream whose
-- predicate returns 'True'. Records that match no predicate
-- are dropped. The output is a /list/ of streams, one per
-- predicate, in the order the predicates were supplied.
--
-- /JVM equivalent:/ the pre-KIP-418 @KStream.branch(Predicate...)@.
--
-- For KIP-418 /named/ branches (returning a 'Data.Map.Strict.Map'
-- keyed by branch name) use 'split' instead — it's the
-- recommended modern API.
branch :: [Record k v -> Bool] -> Topology (KStream k v) [KStream k v]
branch = Branch

-- | KIP-418 named-branch split. Each input record is routed to
-- the first branch whose predicate matches; records that
-- match no branch go to the optional default-branch name (or
-- are dropped if 'Nothing'). The output is a 'Map' from
-- branch name to its sub-stream, so downstream code can
-- destructure by name rather than by list position.
--
-- /JVM equivalent:/ @KStream.split().branch(Branched.as("name").withPredicate(...))@.
--
-- @
-- F.source \"in\" ks vs
--   '>>>' F.split
--           [ F.splitBranch \"errors\"   (\\r -> recordValue r > threshold)
--           , F.splitBranch \"warnings\" (\\r -> recordValue r > 0)
--           ]
--           ('Just' \"info\")
--   '>>>' F.liftIO_ \"route-branches\" (\\b m -> do
--           let Just errs = Map.lookup \"errors\"   m
--               Just wrns = Map.lookup \"warnings\" m
--               Just info = Map.lookup \"info\"     m
--           -- ... sink each branch to a different topic ...
--           pure ())
-- @
split
  :: [SplitBranch k v]
  -> Maybe Text
  -> Topology (KStream k v) (Map Text (KStream k v))
split = Split

-- | Helper for assembling a 'SplitBranch'. The 'Text' is the
-- branch name (appears as the result-'Map' key); the function
-- is the predicate.
--
-- @
-- F.splitBranch \"low\" (\\r -> recordValue r < 10)
-- @
splitBranch :: Text -> (Record k v -> Bool) -> SplitBranch k v
splitBranch = SplitBranch

-- | Explicit wire duplicator: @a -> (a, a)@.
--
-- Equivalent to @'Cat.id' '&&&' 'Cat.id'@, but named for the
-- common use case where you want to thread the same wire
-- value into two downstream paths. The optimiser pushes
-- adjacent pure functions ('Arr') through 'fork' so
-- @fork >>> arr f@ collapses to @Arr (\\a -> f (a, a))@.
--
-- See also 'forkN' for N-way duplication.
fork :: Topology a (a, a)
fork = Fork

-- | N-way fan-out: apply each sub-fragment to the /same/
-- upstream value and collect the results in input order.
-- Generalises 'fork' / @'&&&'@ to any positive arity.
--
-- All sub-fragments share the upstream node in the compiled
-- topology — every branch's lineage descends from the same
-- source, so the EOS task assigner groups them together
-- (every branch's sinks commit in one transaction).
--
-- @
-- forkN ( mkSink \"upper\" T.toUpper
--      'NE.:|' [ mkSink \"lower\" T.toLower
--           , mkSink \"len\"   (T.pack . show . T.length)
--           ])
-- @
forkN :: NonEmpty (Topology a b) -> Topology a (NonEmpty b)
forkN = ForkN

-- | Run a side-effecting sub-pipeline and pass the upstream
-- wire through unchanged.
--
-- Operationally equivalent to @'Cat.id' '&&&' t '>>>' 'arr'
-- 'fst'@ but reads better at the call site and lets the
-- optimiser apply @Tap-specific@ rewrites (e.g.
-- @'Tap' ('Foreach' f) === 'Peek' f@).
--
-- The side pipeline @t@ must be closed (return @()@) — it's
-- typically a 'Sink', 'Foreach', or a small chain ending in
-- one. Use 'tap' for "audit-log this stream without
-- disturbing the main path" patterns:
--
-- @
-- F.source \"in\" ks vs
--   '>>>' F.tap (F.filter isUrgent '>>>' F.sink \"alerts\" ks vs)
--   '>>>' F.mapValues normalise
--   '>>>' F.sink \"out\" ks vs
-- @
tap :: Topology a () -> Topology a a
tap = Tap

-- * Conversions ---------------------------------------------------

-- | Materialise the latest value-per-key into a state-backed
-- 'KTable'. Every input record either inserts or updates the
-- store; tombstones (records with @Nothing@-typed value
-- representations) are still inserted as such — apply
-- 'mapValues' to drop them upstream if you want delete
-- semantics.
--
-- /JVM equivalent:/ @KStream.toTable(Materialized)@.
--
-- The 'Materialized' carries the store name, optional key /
-- value serdes (downstream operations that need them will
-- raise 'MissingMaterializedSerde' if absent), and the
-- changelog config. The resulting 'KTable' shares its state
-- store with any other operation referencing the same
-- 'StoreName'.
toTable :: Ord k => Materialized k v -> Topology (KStream k v) (KTable k v)
toTable = ToTableT

-- | Convert a 'KTable' back to a 'KStream' carrying every
-- changelog event. The output stream emits one record for
-- every update (including tombstones) that flowed into the
-- table.
--
-- /JVM equivalent:/ @KTable.toStream@.
--
-- 'toStream' is /not/ idempotent — feeding the same KTable
-- twice produces two separate change-emitting processors and
-- the records appear on the resulting streams in (the same)
-- per-table order, but each invocation is its own node.
toStream :: Topology (KTable k v) (KStream k v)
toStream = ToStream

-- | Force a repartition through an internal topic, using the
-- supplied prefix to name the topic. The topic is created
-- with the same partition count as the upstream and the
-- records re-keyed by the current 'KStream' key.
--
-- /JVM equivalent:/ @KStream.repartition(name)@.
--
-- == When to call this
--
-- Insert 'repartition' between a key-changing op (a
-- 'selectKey' / 'mapKeyValue' / 'flatMapKeyValue') and a
-- subsequent stateful op (a 'groupBy' / 'aggregate' / 'join')
-- to make the broker re-shuffle records so per-key ordering
-- holds.
--
-- Chained 'repartition' calls collapse to one shuffle via
-- the 'optCollapseRepartition' optimisation — the broker
-- would otherwise pay for the extra topic round-trip with
-- no benefit.
repartition :: Text -> Topology (KStream k v) (KStream k v)
repartition = Repartition

-- | 'repartition' with a fully-specified 'Rep.Repartitioned'
-- config: caller controls the partition count, custom
-- partitioner, and override serdes. Use this when the
-- default partition count doesn't match what downstream
-- joins or external consumers need.
--
-- /JVM equivalent:/ @KStream.repartition(Repartitioned)@.
repartitionWith
  :: Rep.Repartitioned k v -> Topology (KStream k v) (KStream k v)
repartitionWith = RepartitionWith

-- * Grouping + aggregation ----------------------------------------
--
-- Aggregation in Kafka Streams is a /per-key/ reduction that runs
-- inside a stateful processor backed by a state store. The
-- pattern is always three stages:
--
--   1. /Group/ the stream by some key (this stage is pure — no
--      processor node is added; it just changes the wire type to
--      'KGroupedStream' so the type system enforces that the
--      next stage is an aggregation).
--   2. /Aggregate/ via 'count', 'reduce', or 'aggregate' — this
--      adds the processor + state store and produces a 'KTable'.
--   3. Optionally consume the resulting 'KTable' downstream
--      ('toStream', joins, sinks, …).
--
-- For session-windowed or time-windowed aggregations insert a
-- 'windowedByTime' / 'windowedBySession' step between the
-- grouping and the aggregation, and use the @*Windowed@
-- aggregators that consume 'TimeWindowedKStream' /
-- 'SessionWindowedKStream'.

-- | Group records by their existing key. The wire becomes a
-- 'KGroupedStream'; the next stage must be a 'count' /
-- 'reduce' / 'aggregate' / 'windowedByTime' / 'windowedBySession'.
--
-- /JVM equivalent:/ @KStream.groupByKey(Grouped)@.
--
-- 'groupByKey' is /pure/: no processor node, no repartition
-- needed if the upstream key hasn't been altered. The
-- 'Grouped' carries the serdes the downstream aggregator
-- needs to read/write the state store.
--
-- If the upstream had a key-changing op ('selectKey',
-- 'mapKeyValue', 'flatMapKeyValue') without an explicit
-- 'repartition' in between, the broker partition layout no
-- longer matches the new key and the aggregation will silently
-- produce wrong results. Either insert 'repartition' or use
-- 'groupBy' (which the optimiser fuses with an upstream
-- 'selectKey').
groupByKey :: Grouped k v -> Topology (KStream k v) (KGroupedStream k v)
groupByKey = GroupByKey

-- | Group by a derived key. The supplied function picks a new
-- key per 'Record'; the upstream value is preserved.
--
-- /JVM equivalent:/ @KStream.groupBy(KeyValueMapper, Grouped)@.
--
-- Prefer 'groupBy' over @selectKey >>> groupByKey@ —
-- the optimiser collapses the latter pair into the former, but
-- writing it directly is clearer and skips a node.
--
-- Like any key-changing op, 'groupBy' marks the stream as
-- needing a repartition for downstream stateful operations.
-- Callers usually want a 'repartition' between 'groupBy' and
-- the aggregation if the partition layout matters; in the
-- in-process test driver this is a no-op, but against a real
-- broker it's what guarantees per-key ordering.
groupBy
  :: (Record k v -> k') -> Grouped k' v
  -> Topology (KStream k v) (KGroupedStream k' v)
groupBy = GroupBy

-- | Count records per key. The resulting 'KTable' maps each
-- key to the number of records seen for it.
--
-- /JVM equivalent:/ @KGroupedStream.count(Materialized)@.
--
-- The 'Materialized' names the state store and carries the
-- key serde (the value serde is implicit — counts are
-- 'Int64'). Pass 'Mat.materializedAs' for a named store, or
-- 'Mat.materialized' for an auto-named one.
count
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedStream k v) (KTable k Int64)
count = Count

-- | Combine values per key with a binary reducer of the same
-- shape. The accumulator is initialised with the first value
-- seen for each key; subsequent values are combined via the
-- supplied function.
--
-- /JVM equivalent:/ @KGroupedStream.reduce(Reducer, Materialized)@.
--
-- The reducer must be associative for the result to be
-- deterministic across replays / failovers — Kafka may
-- replay records on restart, and a non-associative reducer
-- would produce different results.
--
-- For aggregations whose accumulator type differs from the
-- input value type, use 'aggregate'.
reduce
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedStream k v) (KTable k v)
reduce = Reduce

-- | General-shape per-key aggregator: the accumulator can be a
-- different type from the value, the initialiser runs once
-- per key (lazily, on first record), and the step function
-- folds in each new value.
--
-- /JVM equivalent:/ @KGroupedStream.aggregate(Initializer, Aggregator, Materialized)@.
--
-- The initialiser is an 'IO' to mirror Java's
-- @Initializer<A>@: callers who want a pure seed pass
-- @'pure' x@. Stateful initialisers (e.g. allocating a
-- mutable buffer) are also valid.
--
-- == Determinism note
--
-- As with 'reduce', the @(k -> v -> agg -> agg)@ step must be
-- associative if Kafka may replay records on failover —
-- otherwise different replay orderings produce different
-- final values.
aggregate
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedStream k v) (KTable k agg)
aggregate = Aggregate

-- * Windowed aggregation ------------------------------------------
--
-- Windowed aggregations bucket records by /time/ before
-- aggregating, so the output is one value per (key, window)
-- pair instead of one value per key. There are two windowing
-- shapes:
--
--   * Time windows ('Win.Windows' / 'windowedByTime'): fixed-size
--     buckets — tumbling (no overlap), hopping (advance < size,
--     overlapping), sliding. Records fall into one or more
--     windows based on their timestamp.
--
--   * Session windows ('Win.SessionWindows' / 'windowedBySession'):
--     gap-based — a window stays open as long as records keep
--     arriving within the gap; a long enough silence closes the
--     window. Useful for activity-style aggregations.
--
-- After windowing, the wire becomes 'TimeWindowedKStream' /
-- 'SessionWindowedKStream' and the only legal next stage is one
-- of the @*Windowed@ aggregators. The result is a
-- 'TWKS.WindowedTableHandle' / 'SWKS.SessionWindowedTableHandle'
-- which downstream can read directly (via Interactive Queries)
-- or convert into a 'KStream' via the windowed-key suppress
-- machinery.

-- | Bucket the grouped stream into time windows. The 'Win.Windows'
-- carries the window size, advance interval (for hopping),
-- and retention. Used immediately upstream of 'countWindowed' /
-- 'reduceWindowed' / 'aggregateWindowed'.
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(TimeWindows)@.
--
-- Pure — no processor node, just a wire-type change.
windowedByTime
  :: Win.Windows
  -> Topology (KGroupedStream k v) (TimeWindowedKStream k v)
windowedByTime = WindowedByTime

-- | Bucket the grouped stream by /session/: a window stays open
-- as long as new records arrive within the configured gap.
-- When a record arrives more than the gap after the previous
-- record (for the same key), a new session window starts.
-- Adjacent sessions whose records' timestamps overlap (after
-- a late record arrives) are /merged/.
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(SessionWindows)@.
--
-- Use immediately upstream of 'countSessionWindowed' /
-- 'aggregateSessionWindowed' — the session aggregator takes a
-- /merger/ in addition to the step function, used when two
-- sessions get merged into one.
windowedBySession
  :: Win.SessionWindows
  -> Topology (KGroupedStream k v) (SessionWindowedKStream k v)
windowedBySession = WindowedBySession

-- | Count records per (key, window) pair. Mirrors 'count' for
-- the time-windowed case.
--
-- /JVM equivalent:/ @TimeWindowedKStream.count(Materialized)@.
countWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k Int64)
countWindowed = CountWindowed

-- | Per-(key, window) reducer; the value type is unchanged.
-- See 'reduce' for the associativity caveat.
--
-- /JVM equivalent:/ @TimeWindowedKStream.reduce(Reducer, Materialized)@.
reduceWindowed
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k v)
reduceWindowed = ReduceWindowed

-- | General-shape per-(key, window) aggregator with an
-- accumulator type that may differ from the input value.
-- Mirrors 'aggregate' for the time-windowed case.
--
-- /JVM equivalent:/ @TimeWindowedKStream.aggregate(Initializer, Aggregator, Materialized)@.
aggregateWindowed
  :: Ord k
  => IO agg -> (k -> v -> agg -> agg) -> Materialized k agg
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k agg)
aggregateWindowed = AggregateWindowed

-- | Count records per (key, session). Mirrors 'count' for the
-- session-windowed case; no merger needed since the
-- accumulator is just @Int64@.
--
-- /JVM equivalent:/ @SessionWindowedKStream.count(Materialized)@.
countSessionWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k Int64)
countSessionWindowed = CountSessionWindowed

-- | Session-windowed aggregator with an explicit merger.
-- When two adjacent sessions are merged (because a late
-- record bridged them), the merger combines their two
-- accumulators into one — this is the extra argument
-- compared to the time-windowed 'aggregateWindowed'.
--
-- /JVM equivalent:/ @SessionWindowedKStream.aggregate(Initializer, Aggregator, Merger, Materialized)@.
--
-- The merger must be associative + commutative if Kafka may
-- replay records on failover.
aggregateSessionWindowed
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> agg -> agg -> agg)
  -> Materialized k agg
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k agg)
aggregateSessionWindowed = AggregateSessionWindowed

-- * KGroupedTable -------------------------------------------------
--
-- A 'KGroupedTable' aggregates a /changelog/ — the input is a
-- 'KTable' whose records are inserts/updates/deletes per key,
-- and we want to aggregate them under a derived key.
--
-- Unlike 'KGroupedStream' aggregations, a 'KGroupedTable'
-- aggregator needs BOTH an /adder/ (combine the new value
-- into the accumulator) AND a /subtractor/ (remove the
-- previous value's contribution before adding the new one).
-- Without the subtractor, every update would double-count the
-- previous contribution.
--
-- See 'groupTableBy' to enter this regime from a 'KTable'.

-- | Re-key a 'KTable' and group the resulting changelog for
-- subtractor-aware aggregation. The function picks a new
-- (key, value) per record; downstream aggregators see that
-- new key.
--
-- /JVM equivalent:/ @KTable.groupBy(KeyValueMapper, Grouped)@.
groupTableBy
  :: (Ord k, Ord k')
  => (k -> v -> (k', v')) -> Grouped k' v'
  -> Topology (KTable k v) (KGroupedTable k' v')
groupTableBy = GroupTableBy

-- | Count grouped-table records per derived key. The
-- subtractor is implicit (decrement) and the adder is
-- implicit (increment), so no functions are needed beyond
-- the materialised result.
--
-- /JVM equivalent:/ @KGroupedTable.count(Materialized)@.
countKGroupedTable
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedTable k v) (KTable k Int64)
countKGroupedTable = CountKGroupedTable

-- | Reduce grouped-table records per derived key with explicit
-- adder + subtractor functions. The subtractor is applied
-- first (to remove the previous value's contribution) and
-- then the adder is applied (to fold in the new value);
-- inserts skip the subtractor, deletes skip the adder.
--
-- /JVM equivalent:/ @KGroupedTable.reduce(Adder, Subtractor, Materialized)@.
--
-- Both adder and subtractor must be associative for the
-- result to be deterministic across replays.
reduceKGroupedTable
  :: Ord k
  => (v -> v -> v) -> (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedTable k v) (KTable k v)
reduceKGroupedTable = ReduceKGroupedTable

-- | General-shape subtractor-aware aggregator. The
-- accumulator may differ from the input value; both adder
-- and subtractor take the current accumulator, the value
-- being added/removed, and the key.
--
-- /JVM equivalent:/ @KGroupedTable.aggregate(Initializer, Adder, Subtractor, Materialized)@.
aggregateKGroupedTable
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedTable k v) (KTable k agg)
aggregateKGroupedTable = AggregateKGroupedTable

-- * Cogroup -------------------------------------------------------
--
-- Cogroup aggregates N /independent/ grouped streams that share
-- the same key type and accumulator type. Each grouped stream
-- contributes records via its own per-source aggregator; all
-- of them update the same shared state.
--
-- The natural way to build a cogroup is /incrementally/, one
-- source at a time. With the 'Monad' instance + 'applyT' this
-- maps onto do-notation cleanly — see the module-level
-- "Monad bind for incrementally-built topologies" example.

-- | Start a cogroup with one grouped-stream source plus its
-- per-source aggregator. The resulting wire is a
-- 'CogroupedStream' that further sources can be added to via
-- 'addCogrouped'.
--
-- /JVM equivalent:/ @KGroupedStream.cogroup(Aggregator)@.
cogroup
  :: (k -> v -> a -> a)
  -> Topology (KGroupedStream k v) (CogroupedStream k a)
cogroup = Cogroup

-- | Extend a cogroup with another grouped-stream source. The
-- input is a tuple: bring the existing cogroup builder and
-- the new grouped stream together via 'Fanout' / @'&&&'@ /
-- 'applyT'.
--
-- /JVM equivalent:/ @CogroupedKStream.cogroup(KGroupedStream, Aggregator)@.
addCogrouped
  :: (k -> v -> a -> a)
  -> Topology (CogroupedStream k a, KGroupedStream k v) (CogroupedStream k a)
addCogrouped = AddCogrouped

-- | Close out a cogroup builder and emit its result as a
-- 'KTable'. The initialiser allocates the accumulator (once
-- per key, lazily); the 'Materialized' names the resulting
-- store.
--
-- /JVM equivalent:/ @CogroupedKStream.aggregate(Initializer, Materialized)@.
aggregateCogrouped
  :: Ord k
  => IO a -> Materialized k a
  -> Topology (CogroupedStream k a) (KTable k a)
aggregateCogrouped = AggregateCogrouped

-- * Joins ---------------------------------------------------------
--
-- All join constructors take their two participants as a /tuple/
-- on the input side; bring two streams / tables together with
-- 'Fanout' / @'&&&'@ / 'mergeSourced' / @'applyT'@ so they
-- land in the tuple shape.
--
-- == Co-partitioning requirement
--
-- Kafka Streams joins require the two input topics to be
-- /co-partitioned/: same partition count, same partitioning
-- function. If you've re-keyed an input via 'selectKey' /
-- 'mapKeyValue' upstream of a join, you'll need a 'repartition'
-- before the join so the broker's partition layout matches.
-- (For stream-table joins of a 'globalTableSource', the
-- co-partitioning requirement is relaxed — the global table is
-- replicated across all tasks.)

-- | Stream-table /inner/ join: emit @joiner v vt@ whenever a
-- stream record's key matches a table entry. Stream records
-- whose key has no table match are dropped.
--
-- /JVM equivalent:/ @KStream.join(KTable, ValueJoiner, Joined)@.
streamTableJoin
  :: Ord k
  => (v -> vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableJoin = StreamTableJoin

-- | Stream-table /left/ join: emit @joiner v (Just vt)@ when
-- the key matches, @joiner v Nothing@ when it doesn't. Every
-- stream record produces exactly one output; the join
-- function decides what to do with the @Nothing@ case.
--
-- /JVM equivalent:/ @KStream.leftJoin(KTable, ValueJoiner, Joined)@.
streamTableLeftJoin
  :: Ord k
  => (v -> Maybe vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableLeftJoin = StreamTableLeftJoin

-- | Stream-stream /windowed inner/ join. Two stream records
-- match if their keys are equal and their timestamps are
-- within the 'JoinWindows' (asymmetric before/after offsets,
-- plus an optional grace period for late arrivals).
--
-- /JVM equivalent:/ @KStream.join(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
--
-- Both sides are buffered in per-side window stores for the
-- 'JoinWindows' duration; downstream emission happens
-- per match.
streamStreamJoin
  :: Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamJoin = StreamStreamJoin

-- | Stream-stream /windowed left/ join. Every left record
-- emits at least once: with @'Just' v2@ for each match, or
-- with @'Nothing'@ if no match is found within the join
-- window before its grace period expires. Right records
-- only contribute matches.
--
-- /JVM equivalent:/ @KStream.leftJoin(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
streamStreamLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamLeftJoin = StreamStreamLeftJoin

-- | Stream-stream /windowed outer/ join. Both sides emit at
-- least once; the joiner takes 'Maybe' on both sides because
-- either may be absent.
--
-- /JVM equivalent:/ @KStream.outerJoin(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
streamStreamOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamOuterJoin = StreamStreamOuterJoin

-- | Table-table /inner/ join. The output is a 'KTable' that
-- updates whenever either side updates. Materialised via the
-- supplied 'Materialized'.
--
-- /JVM equivalent:/ @KTable.join(KTable, ValueJoiner, Materialized)@.
tableTableJoin
  :: Ord k
  => (v1 -> v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableJoin = TableTableJoin

-- | Table-table /left/ join: emit even when the right side
-- has no value (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KTable.leftJoin(KTable, ValueJoiner, Materialized)@.
tableTableLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableLeftJoin = TableTableLeftJoin

-- | Table-table /outer/ join: emit whenever either side has a
-- value (joiner sees 'Maybe' on both sides).
--
-- /JVM equivalent:/ @KTable.outerJoin(KTable, ValueJoiner, Materialized)@.
tableTableOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableOuterJoin = TableTableOuterJoin

-- | Foreign-key KTable-KTable /inner/ join (KIP-213). The
-- /left/ table is keyed by @k@; the /right/ table is keyed by
-- @fk@. The foreign-key extractor pulls an @fk@ out of each
-- left value, used to look up the right table. Records on
-- the left whose extracted foreign key has no right entry
-- are dropped.
--
-- /JVM equivalent:/ @KTable.join(KTable, fkExtractor, ValueJoiner, Materialized)@.
--
-- The 'Hashable' constraint on @v@ is the foreign-key
-- subscription token mechanism used by the implementation to
-- detect which left rows are affected by a right update.
foreignKeyJoin
  :: (Ord k, Ord fk, Hashable v)
  => (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
foreignKeyJoin = ForeignKeyJoin

-- | Foreign-key KTable-KTable /left/ join: emit even when the
-- right table has no entry for the extracted foreign key
-- (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KTable.leftJoin(KTable, fkExtractor, ValueJoiner, Materialized)@.
leftForeignKeyJoin
  :: (Ord k, Ord fk, Hashable v)
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
leftForeignKeyJoin = LeftForeignKeyJoin

-- | Stream-globalTable /inner/ join. The key-mapper picks a
-- foreign key out of each stream record; the global table is
-- looked up by that foreign key; the joiner produces the
-- result.
--
-- /JVM equivalent:/ @KStream.join(GlobalKTable, KeyValueMapper, ValueJoiner)@.
--
-- Unlike stream-table joins, /no co-partitioning is
-- required/: a 'GlobalKTable' is cluster-replicated, so
-- every task has the full table available locally.
streamGlobalTableJoin
  :: Ord kg
  => (k -> v -> kg)
  -> (v -> vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableJoin = StreamGlobalTableJoin

-- | Stream-globalTable /left/ join: emit even when the
-- global-table lookup misses (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KStream.leftJoin(GlobalKTable, KeyValueMapper, ValueJoiner)@.
streamGlobalTableLeftJoin
  :: Ord kg
  => (k -> v -> kg)
  -> (v -> Maybe vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableLeftJoin = StreamGlobalTableLeftJoin

-- * KTable surface ------------------------------------------------
--
-- These mirror the stream-side 'filter' / 'mapValues' family but
-- operate on a changelog table — the implementation maintains
-- a new materialised store of the filtered / transformed view.
-- That's why every constructor here takes a 'Materialized': the
-- output 'KTable' is its own state store.

-- | Filter a 'KTable'. Records where the predicate returns
-- 'False' are stored as tombstones (deletes) in the output
-- table — so the output is the @True@-subset view.
--
-- /JVM equivalent:/ @KTable.filter(Predicate, Materialized)@.
filterTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterTable = FilterTable

-- | Inverse of 'filterTable': records matching the predicate
-- are tombstoned out. Implemented as
-- @'filterTable' (not . p)@ but exposed as its own
-- constructor for parity with the JVM API.
--
-- /JVM equivalent:/ @KTable.filterNot(Predicate, Materialized)@.
filterNotTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterNotTable = FilterNotTable

-- | Map values in a 'KTable'. The key is preserved; the value
-- type may change. A new state store is created for the
-- mapped view.
--
-- /JVM equivalent:/ @KTable.mapValues(ValueMapper, Materialized)@.
mapValuesTable
  :: Ord k
  => (v -> v') -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
mapValuesTable = MapValuesTable

-- | Custom-processor transform of a 'KTable'. The processor
-- receives table-update records and can read/write the
-- declared 'StoreName's. Useful when 'mapValuesTable' isn't
-- enough — e.g. you need access to other state stores during
-- the value transformation.
--
-- /JVM equivalent:/ @KTable.transformValues(TransformerSupplier, Materialized, storeNames)@.
transformValuesTable
  :: Ord k
  => Text
  -> IO (Processor k v)
  -> [StoreName]
  -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
transformValuesTable = TransformValuesTable

-- * Suppress ------------------------------------------------------

-- | Debounce-style suppression: hold the latest value per key
-- in a buffer for up to the supplied 'Duration' before
-- emitting. Successive updates within the time limit
-- overwrite the buffered value; the buffered value is
-- emitted once the time limit has elapsed since the first
-- record of the current window.
--
-- /JVM equivalent:/ @Suppressed.untilTimeLimit(Duration, BufferConfig)@.
--
-- Useful for reducing downstream noise from rapid updates on
-- the same key — only the latest value lands after the
-- quiet period.
suppressUntilTimeLimit
  :: Ord k => Duration -> Topology (KStream k v) (KStream k v)
suppressUntilTimeLimit = SuppressUntilTimeLimit

-- | Suppress all updates to a windowed key until the window
-- has closed (i.e. stream time has advanced past
-- @windowEnd + gracePeriod@). Each (window, key) pair emits
-- exactly once with its final value.
--
-- /JVM equivalent:/ @Suppressed.untilWindowCloses(BufferConfig)@.
--
-- Operates on streams keyed by 'WindowedKey' — typically the
-- output of @streamFromWindowedHandle@ or a similar windowed
-- conversion. The 'Int64' is the window size in milliseconds;
-- the 'Duration' is the grace period for late records.
suppressWindowed
  :: Ord k
  => Duration -> Int64
  -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)
suppressWindowed = SuppressWindowedKS

-- * Processor API -------------------------------------------------
--
-- The Processor API gives the most flexibility (and the most
-- responsibility): you supply a full 'Processor' value with
-- 'procInit' / 'procProcess' / 'procClose' callbacks. The
-- callbacks have full 'IO' and can interact with declared
-- state stores via 'getStateStore'.
--
-- For the common pattern of "one processor with one state store
-- it owns exclusively" reach for 'processWithStateStoreKV' /
-- 'processWithStateStoreW' / 'processWithStateStoreS' — they
-- atomically register the processor and the store with the
-- right owner-node wiring. The lower-level 'processStream' +
-- 'withStateStoreKV' pair is for multi-owner stores and other
-- advanced cases.

-- | Attach a custom processor as a terminal sink. The processor
-- consumes records and writes only to declared stores or out
-- of band; it doesn't forward downstream (the @()@ output
-- reflects that). For a processor that DOES produce a
-- downstream value, use 'processValuesStream' or
-- 'transformValuesStream'.
--
-- /JVM equivalent:/ @KStream.process(ProcessorSupplier, storeNames)@.
--
-- The 'Text' prefix names the processor in the topology
-- graph (with a fresh suffix appended). The @['StoreName']@
-- list declares which state stores the processor will access
-- — you must separately register each named store via
-- 'withStateStoreKV' / @W@ / @S@, with this processor's
-- generated 'NodeName' as an owner. /Because the generated
-- name isn't visible/, the common case of "one processor +
-- one store" is much easier with 'processWithStateStoreKV'.
processStream
  :: Text -> [StoreName] -> IO (Processor k v)
  -> Topology (KStream k v) ()
processStream = ProcessStream

-- | Attach a custom processor that produces a typed
-- downstream 'KStream'. The output value type and serde
-- come from the explicit 'Serde' argument.
--
-- /JVM equivalent:/ @KStream.processValues(ProcessorSupplier, storeNames)@.
--
-- Same store-ownership caveat as 'processStream'; for the
-- single-store common case use 'processWithStateStoreKV' or
-- its windowed / session counterparts.
processValuesStream
  :: Text -> [StoreName] -> IO (Processor k v) -> Serde v'
  -> Topology (KStream k v) (KStream k v')
processValuesStream = ProcessValuesStream

-- | Legacy /transformValues/ wiring: same as
-- 'processValuesStream' but the store list is typed as
-- @['Topo.NodeName']@ for compatibility with the original
-- imperative DSL. Prefer 'processValuesStream' for new code.
transformValuesStream
  :: Text
  -> [Topo.NodeName]
  -> IO (Processor k v)
  -> Serde v'
  -> Topology (KStream k v) (KStream k v')
transformValuesStream = TransformValuesStreamT

-- | Register a 'StoreBuilderKV' against the topology graph
-- and grant access to the named owner processors. Wire is
-- unchanged — compose anywhere via @'>>>'@.
--
-- The @['Topo.NodeName']@ list is the set of processors that
-- can read/write the store. Auto-generated names from
-- 'processStream' aren't easy to predict, so for the common
-- single-owner case use 'processWithStateStoreKV' instead
-- (it generates the name and registers the store atomically).
--
-- == Use cases for the standalone form
--
--   * Multi-owner stores shared across N processors (rare).
--   * Stores attached after-the-fact via
--     @connectProcessorAndStateStores@.
--   * Hand-rolled imperative wiring spliced in via
--     'liftIO_' with known node names.
withStateStoreKV
  :: StoreBuilderKV k v -> [Topo.NodeName] -> Topology x x
withStateStoreKV = WithStateStoreKV

-- | 'withStateStoreKV' for a /window/ state store. Same
-- semantics: register the store, grant access to the named
-- owner processors, wire passes through unchanged.
withStateStoreW
  :: StoreBuilderW k v -> [Topo.NodeName] -> Topology x x
withStateStoreW = WithStateStoreW

-- | 'withStateStoreKV' for a /session/ state store.
withStateStoreS
  :: StoreBuilderS k v -> [Topo.NodeName] -> Topology x x
withStateStoreS = WithStateStoreS

-- | Atomically attach a custom 'Processor' /and/ its
-- 'StoreBuilderKV' to the upstream 'KStream'. The compiler
-- generates the processor's 'Topo.NodeName' from the prefix,
-- attaches the store's name to its @processorSpecStores@ list,
-- and registers the store with that generated node as its
-- owner — so callers don't need to coordinate node names manually
-- between 'processStream' and 'withStateStoreKV'.
--
-- Use this whenever your processor reads or writes a state store
-- that no other processor shares (the common case). For
-- multi-owner state stores, stay with the
-- 'processStream' + 'withStateStoreKV' split and pass an
-- explicit 'Topo.NodeName' yourself (currently requires the
-- 'liftIO_' escape; a dedicated /named/ variant is on the
-- follow-up list).
processWithStateStoreKV
  :: Text
  -> StoreBuilderKV stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreKV = ProcessWithStateStoreKV

processWithStateStoreW
  :: Text
  -> StoreBuilderW stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreW = ProcessWithStateStoreW

processWithStateStoreS
  :: Text
  -> StoreBuilderS stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreS = ProcessWithStateStoreS

----------------------------------------------------------------------
-- Escape hatch + introspection
----------------------------------------------------------------------

-- | Escape hatch: splice in a hand-rolled
-- 'StreamsBuilder'-mutating action as a topology fragment.
--
-- The supplied function receives the shared 'StreamsBuilder'
-- and the upstream wire value, performs whatever imperative
-- operations it likes (registering nodes, attaching stores,
-- calling into the original DSL), and returns the downstream
-- wire value.
--
-- == When to use
--
-- 'liftIO_' is the catch-all for operations that aren't yet
-- a dedicated GADT constructor. Typical cases:
--
--   * Calling into the imperative @Kafka.Streams.KStream@ /
--     @Kafka.Streams.KTable@ DSL for an operator we haven't
--     promoted yet.
--   * One-off custom processors that don't fit
--     'processStream' (e.g. needing explicit node-name
--     control).
--   * Bridging Layer-4 ('Topology') and Layer-1 ('StreamsBuilder')
--     during a gradual migration.
--
-- The supplied 'Text' is the label used by 'inspect' /
-- 'prettyPrint' so the AST remains legible. Keep it
-- descriptive (e.g. @\"custom-windowed-suppress\"@ rather
-- than @\"helper\"@); the optimiser and inspection passes
-- can't see past 'liftIO_', so the label is your only
-- breadcrumb.
--
-- == Implications
--
--   * The optimiser cannot rewrite across or into a
--     'liftIO_' boundary. If you have repeated
--     'liftIO_'-wrapped fragments, fusion opportunities are
--     lost.
--   * Whatever the action does at run time is opaque to
--     'compile' — caller-asserted correctness.
--   * EOS atomicity still holds /if/ the action only mutates
--     the shared topology graph: any processors / sinks it
--     registers go through the same transactional producer.
--     Out-of-band side channels (e.g. an action that
--     writes to a database) are NOT part of the EOS
--     transaction.
liftIO_
  :: Text
  -> (StreamsBuilder -> i -> IO o)
  -> Topology i o
liftIO_ = Lifted

-- | Walk the AST and collect a per-node textual label
-- listing.
--
-- The output is a list of operator-shape tokens in roughly
-- the order they'd appear in a topology-description string —
-- compose two via @'>>>'@ and the resulting 'inspect' has
-- the left side's tokens followed by the right side's
-- tokens.
--
-- /Comparable to:/ Java's
-- @org.apache.kafka.streams.TopologyDescription@. Our output
-- is a simpler tokenised form, not the full graph
-- description; it's intended for tests, golden-file
-- comparisons, and assertions on AST shape.
--
-- == What you can rely on
--
--   * Every constructor produces at least one token.
--   * Structural constructors ('Compose', 'First', 'Parallel',
--     etc.) bracket their children with angle-bracket tokens
--     so nesting is visible.
--   * The labels are stable for any given operator name in
--     the AST; 'inspect' is the basis of the introspection
--     tests in @Streams.TopologyFreeSpec@.
--
-- == What you should NOT rely on
--
--   * The exact token format is /not/ a stable public API —
--     use 'inspect' for debugging and golden-file tests, not
--     for parsing.
--   * The token list is /not/ a topological order of the
--     compiled-graph nodes; it follows the AST structure,
--     not the runtime processor graph.
--   * The continuation of a 'Bind' is opaque — 'inspect'
--     shows the left side and a single @"…>"@ marker.
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
    -- Monad bind — opaque continuation
    go (Bind t _)           = "Bind<" : go t ++ ["…>"]
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
    go (ProcessWithStateStoreKV nm b _)
      = [ "ProcessWithStateStoreKV(" <> nm <> "," <>
            Store.unStoreName (Store.sbKvName b) <> ")" ]
    go (ProcessWithStateStoreW nm b _)
      = [ "ProcessWithStateStoreW(" <> nm <> "," <>
            Store.unStoreName (Store.sbWName b) <> ")" ]
    go (ProcessWithStateStoreS nm b _)
      = [ "ProcessWithStateStoreS(" <> nm <> "," <>
            Store.unStoreName (Store.sbSName b) <> ")" ]
    -- Escape
    go (Lifted nm _)                  = ["Lifted(" <> nm <> ")"]

    showTopic :: TopicName -> Text
    showTopic = T.pack . show

-- | Render an AST as a single-line whitespace-separated
-- description. Built by 'T.intercalate'-ing the output of
-- 'inspect'.
--
-- Useful for REPL exploration, error messages, and at-a-glance
-- comparison of AST shapes. The output is /not/ a stable
-- format — use it for debugging, not for parsing or contract
-- tests.
--
-- @
-- ghci> F.prettyPrint (F.source \"in\" textSerde textSerde
--                       '>>>' F.mapValues T.toUpper
--                       '>>>' F.sink \"out\" textSerde textSerde)
-- "Source(...) MapValues Sink(...)"
-- @
--
-- For more structured introspection (e.g. counting operator
-- occurrences, walking the AST) use 'inspect' directly — it
-- returns a list of tokens you can pattern-match on.
prettyPrint :: Topology i o -> Text
prettyPrint = T.intercalate " " . inspect

----------------------------------------------------------------------
-- Optimisation
--
-- Walks a 'Topology' and applies a curated set of /semantics-
-- preserving/ rewrites that fuse adjacent operators, collapse
-- identity nodes, and right-associate composition into a canonical
-- form. The result is an AST that compiles to fewer topology graph
-- nodes and therefore fewer record-forwarding hops at run time.
--
-- The rewrite set covers Category\/Arrow laws (Id collapse, Arr
-- fusion, push-pure-through-First\/Second\/Parallel\/Fanout,
-- right-associate Compose), KStream operator fusion
-- (MapValues\/MapValuesM\/MapKeyValue\/FlatMapValues\/Filter\/
-- FilterNot\/SelectKey\/Peek), and structural identity collapse
-- (Tap Id, First Id, Parallel Id Id, etc).
--
-- What we deliberately /don't/ rewrite: reordering across
-- @Filter '>>>' MapValues@ (the filter observes the pre-map
-- record), global topic-level optimisations (those live in
-- "Kafka.Streams.Topology.Optimization"), and removing observable
-- effects ('Peek', 'Foreach' are preserved verbatim).
----------------------------------------------------------------------

-- | Toggles for the rewrite passes. Each flag controls one /family/
-- of rewrites. Defaults enable everything.
--
-- == Relationship to Java's @topology.optimization@ knob
--
-- Java's Kafka Streams ships two formal optimisations, both at the
-- /topic/ level (see "Kafka.Streams.Topology.Optimization"):
--
--   * @REUSE_KTABLE_SOURCE_TOPICS@ — reuse a 'TableSource' topic as
--     its KTable's changelog instead of an internal one. /Not yet
--     implemented here/ — it's a flag on the materialised store's
--     changelog config, not an AST rewrite.
--   * @MERGE_REPARTITION_TOPICS@ — collapse repartition topics that
--     descend from the same key-changing operation via multiple
--     downstream stateful ops. /Not yet implemented here/ — needs
--     the runtime to auto-insert repartitions on stateful ops,
--     which we don't currently do (callers insert 'Repartition'
--     explicitly).
--
-- The GADT-level rewrites this module ships are /additional/ to
-- those: they fuse adjacent operators (saving processor nodes /
-- record-forwarding hops) and collapse Cat\/Arrow identity
-- combinators. Java's DSL doesn't expose an equivalent because its
-- builder eagerly constructs the physical plan rather than holding
-- a reified AST. Operationally the result is the same shape Java
-- programmers achieve by hand-writing
--
-- @
-- stream.mapValues(f).mapValues(g)   -- two processors
-- @
--
-- as
--
-- @
-- stream.mapValues(g . f)            -- one processor
-- @
--
-- We do that fusion automatically.
data OptimizeConfig = OptimizeConfig
  { -- | Right-associate 'Compose' chains so adjacent operators
    -- surface along the right spine for the fusion pass.
    optAssociateCompose :: !Bool

    -- | Fuse adjacent 'Arr' applications and push pure functions
    -- through 'First', 'Second', 'Parallel', 'Fanout', 'Fork'.
  , optFusePureFunctions :: !Bool

    -- | Fuse adjacent 'MapValues' \/ 'MapValuesM' \/ 'MapKeyValue'.
  , optFuseMaps          :: !Bool

    -- | Fuse adjacent 'Filter' \/ 'FilterNot'.
  , optFuseFilters       :: !Bool

    -- | Fuse 'FlatMapValues' with adjacent 'MapValues' and itself.
  , optFuseFlatMaps      :: !Bool

    -- | Fuse adjacent 'SelectKey'.
  , optFuseSelectKeys    :: !Bool

    -- | Fuse adjacent 'Peek' callbacks. Also fuses
    -- @'Foreach' '.' 'Peek'@ and collapses
    -- @'Tap' ('Foreach' f) = 'Peek' f@.
  , optFusePeeks         :: !Bool

    -- | Collapse @'Tap' 'Id'@, @'First' 'Id'@, @'Parallel' 'Id'
    -- 'Id'@, etc. to 'Id'.
  , optCollapseIdentity  :: !Bool

    -- | Java-style grouping rewrite:
    -- @'GroupByKey' g '.' 'SelectKey' f = 'GroupBy' f g@.
    -- Matches the Apache docs' guidance to prefer @groupBy@ over
    -- @selectKey + groupByKey@ for fewer nodes and clearer
    -- topology descriptions.
  , optFuseSelectKeyIntoGroupBy :: !Bool

    -- | Eliminate redundant repartitions: a second 'Repartition'
    -- after a first one with no key-changing op in between is
    -- pure waste (two shuffles produce the same data). The
    -- outer wins (its prefix is the one named in the broker
    -- topic).
  , optCollapseRepartition :: !Bool

    -- | @'Values' '.' 'Values' = 'Values'@ — idempotent key
    -- drop.
  , optCollapseValues      :: !Bool

    -- | Combine adjacent 'Tap' nodes into one by 'Fanout'-ing the
    -- side pipelines:
    -- @'Tap' t1 '.' 'Tap' t2 = 'Tap' ('Fanout' t2 t1 '>>>' Arr (const ()))@.
  , optFuseTaps            :: !Bool

    -- | Upper bound on rewrite passes. Each iteration must reduce
    -- node count or termination is forced; this is a belt-and-
    -- braces guard against pathological inputs.
  , optMaxPasses         :: !Int
  }
  deriving stock (Show)

defaultOptimizeConfig :: OptimizeConfig
defaultOptimizeConfig = OptimizeConfig
  { optAssociateCompose         = True
  , optFusePureFunctions        = True
  , optFuseMaps                 = True
  , optFuseFilters              = True
  , optFuseFlatMaps             = True
  , optFuseSelectKeys           = True
  , optFusePeeks                = True
  , optCollapseIdentity         = True
  , optFuseSelectKeyIntoGroupBy = True
  , optCollapseRepartition      = True
  , optCollapseValues           = True
  , optFuseTaps                 = True
  , optMaxPasses                = 12
  }

-- | Everything off — the AST passes through unchanged. Useful for
-- A\/B comparisons in tests.
noOptimization :: OptimizeConfig
noOptimization = OptimizeConfig
  { optAssociateCompose         = False
  , optFusePureFunctions        = False
  , optFuseMaps                 = False
  , optFuseFilters              = False
  , optFuseFlatMaps             = False
  , optFuseSelectKeys           = False
  , optFusePeeks                = False
  , optCollapseIdentity         = False
  , optFuseSelectKeyIntoGroupBy = False
  , optCollapseRepartition      = False
  , optCollapseValues           = False
  , optFuseTaps                 = False
  , optMaxPasses                = 0
  }

-- | Apply the default rewrite passes ('defaultOptimizeConfig') to
-- a 'Topology' AST, returning a semantically-equivalent but
-- structurally-minimal version.
--
-- The default config enables every rewrite family in the
-- module (operator fusion, identity collapse, right-associate
-- Compose, push pure functions through Fanout/Parallel/Fork,
-- Java-aligned rewrites like @selectKey >>> groupByKey =>
-- groupBy@). Run-time semantics are preserved by construction;
-- only the node count and AST shape change.
--
-- This is the function 'compile' applies automatically before
-- walking the AST — calling 'optimize' yourself is useful when:
--
--   * comparing the pre- and post-optimisation shape via
--     'inspect' / 'countNodes' / 'optimizationStats',
--   * running the optimiser separately from compilation,
--   * caching an optimised AST for repeated compilation.
optimize :: Topology i o -> Topology i o
optimize = optimizeWith defaultOptimizeConfig

-- | 'optimize' with a custom 'OptimizeConfig'. Use to enable a
-- subset of the rewrite families (e.g. only collapse identity
-- without fusing maps), to disable all rewrites
-- ('noOptimization'), or to bump 'optMaxPasses' for
-- pathological inputs.
--
-- == Fixed-point semantics
--
-- The optimiser runs rewrite passes in a loop, each pass a
-- bottom-up traversal that applies all enabled rewrite
-- families. The loop terminates when:
--
--   * a pass produces strictly fewer nodes than the previous
--     (continue), /or/
--   * a pass produces the same (or more) nodes (stop — we've
--     reached a fixed point), /or/
--   * 'optMaxPasses' has been exhausted (stop — belt-and-
--     braces against pathological loops).
--
-- All rewrites are strictly node-count-reducing or
-- node-count-preserving, so the fixed point exists and is
-- reached in linearly-many passes.
optimizeWith :: OptimizeConfig -> Topology i o -> Topology i o
optimizeWith cfg = loop (optMaxPasses cfg)
  where
    loop :: Int -> Topology i o -> Topology i o
    loop 0 t = t
    loop n t =
      let !t' = optimizePass cfg t
          !before = countNodes t
          !after  = countNodes t'
       in if after < before then loop (n - 1) t' else t'

-- | One bottom-up rewrite pass.
optimizePass :: OptimizeConfig -> Topology i o -> Topology i o
optimizePass cfg = go
  where
    go :: forall a b. Topology a b -> Topology a b
    go (Compose g f) = smartCompose cfg (go g) (go f)
    go (First t)     = collapseFirst cfg (go t)
    go (Second t)    = collapseSecond cfg (go t)
    go (Parallel p q)= collapseParallel cfg (go p) (go q)
    go (Fanout p q)  = collapseFanout cfg (go p) (go q)
    go (LeftT t)     = collapseLeft cfg (go t)
    go (RightT t)    = collapseRight cfg (go t)
    go (Plus p q)    = collapsePlus cfg (go p) (go q)
    go (Fanin p q)   = Fanin (go p) (go q)
    go (ForkN ts)    = ForkN (NE.map go ts)
    go (Tap t)       = collapseTap cfg (go t)
    -- Optimise the LHS of a 'Bind'; the continuation is opaque so
    -- we leave it untouched. Branches threaded out of @k@ will
    -- still be re-optimised by the recursive 'apply' walk at
    -- interpretation time.
    go (Bind t k)    = Bind (go t) k
    -- Leaf operators are unchanged; nothing to recurse into.
    go x             = x

----------------------------------------------------------------------
-- Identity collapses on structural combinators
----------------------------------------------------------------------

collapseFirst :: OptimizeConfig -> Topology a b -> Topology (a, c) (b, c)
collapseFirst cfg Id      | optCollapseIdentity  cfg = Id
collapseFirst cfg (Arr f) | optFusePureFunctions cfg =
  Arr (\(a, c) -> (f a, c))
collapseFirst _ t = First t

collapseSecond :: OptimizeConfig -> Topology a b -> Topology (c, a) (c, b)
collapseSecond cfg Id      | optCollapseIdentity  cfg = Id
collapseSecond cfg (Arr f) | optFusePureFunctions cfg =
  Arr (\(c, a) -> (c, f a))
collapseSecond _ t = Second t

collapseParallel
  :: OptimizeConfig
  -> Topology a b -> Topology c d -> Topology (a, c) (b, d)
collapseParallel cfg Id      Id | optCollapseIdentity  cfg = Id
collapseParallel cfg (Arr f) (Arr g) | optFusePureFunctions cfg =
  Arr (\(a, c) -> (f a, g c))
collapseParallel _ p q = Parallel p q

collapseFanout
  :: OptimizeConfig
  -> Topology a b -> Topology a c -> Topology a (b, c)
collapseFanout cfg (Arr f) (Arr g) | optFusePureFunctions cfg =
  Arr (\a -> (f a, g a))
collapseFanout _ p q = Fanout p q

collapseLeft
  :: OptimizeConfig
  -> Topology a b -> Topology (Either a c) (Either b c)
collapseLeft cfg Id | optCollapseIdentity cfg = Id
collapseLeft _ t = LeftT t

collapseRight
  :: OptimizeConfig
  -> Topology a b -> Topology (Either c a) (Either c b)
collapseRight cfg Id | optCollapseIdentity cfg = Id
collapseRight _ t = RightT t

collapsePlus
  :: OptimizeConfig
  -> Topology a b -> Topology c d -> Topology (Either a c) (Either b d)
collapsePlus cfg Id Id | optCollapseIdentity cfg = Id
collapsePlus _ p q = Plus p q

collapseTap :: OptimizeConfig -> Topology a () -> Topology a a
collapseTap cfg Id                | optCollapseIdentity cfg = Id
-- 'Tap (Foreach f)' is exactly 'Peek f' — both run an effect on
-- every record and pass the wire through. We recognise this
-- standalone form here in addition to the @Compose@ boundary
-- form in 'fuseStep' so single-node @Tap (Foreach f)@ also
-- collapses.
collapseTap cfg (Foreach act)     | optFusePeeks cfg = Peek act
collapseTap _ t = Tap t

----------------------------------------------------------------------
-- Smart 'Compose': right-associate + fuse along the right spine
----------------------------------------------------------------------

smartCompose
  :: forall a b c. OptimizeConfig -> Topology b c -> Topology a b -> Topology a c
smartCompose cfg g f =
  case fuseStep cfg g f of
    Just gf -> gf
    Nothing ->
      case f of
        -- If 'f' is itself a Compose, attempt to fuse 'g' with its
        -- head 'h' along the right spine.
        Compose h i ->
          case fuseStep cfg g h of
            Just gh -> smartCompose cfg gh i
            Nothing -> Compose g (Compose h i)
        _ -> case g of
          Compose h i | optAssociateCompose cfg ->
            -- Right-associate: (h . i) . f -> h . (i . f)
            smartCompose cfg h (smartCompose cfg i f)
          _ -> Compose g f

----------------------------------------------------------------------
-- The fuse rule table
----------------------------------------------------------------------

-- | Return the fused operator if @g '.' f@ matches one of the
-- recognised patterns. The result must be /semantically equivalent/
-- to running @f@ then @g@ in sequence; the only thing it changes is
-- the number of topology graph nodes.
fuseStep
  :: forall a b c
   . OptimizeConfig
  -> Topology b c
  -> Topology a b
  -> Maybe (Topology a c)
fuseStep cfg g f = case (g, f) of
  -- Identity collapses
  (Id, _) | optCollapseIdentity cfg -> Just f
  (_, Id) | optCollapseIdentity cfg -> Just g

  -- Pure-function fusion
  (Arr g', Arr f') | optFusePureFunctions cfg -> Just (Arr (g' . f'))

  -- MapValues family
  (MapValues g', MapValues f') | optFuseMaps cfg ->
    Just (MapValues (g' . f'))
  (MapValuesM g', MapValues f') | optFuseMaps cfg ->
    Just (MapValuesM (g' . f'))
  (MapValues g', MapValuesM f') | optFuseMaps cfg ->
    Just (MapValuesM (\v -> g' <$> f' v))
  (MapValuesM g', MapValuesM f') | optFuseMaps cfg ->
    Just (MapValuesM (\v -> f' v >>= g'))

  -- MapKeyValue family
  (MapKeyValue g', MapKeyValue f') | optFuseMaps cfg ->
    Just (MapKeyValue (\k v ->
                         let (!k', !v') = f' k v
                          in g' k' v'))
  (MapKeyValueM g', MapKeyValue f') | optFuseMaps cfg ->
    Just (MapKeyValueM (\k v ->
                           let (!k', !v') = f' k v
                            in g' k' v'))
  (MapKeyValue g', MapKeyValueM f') | optFuseMaps cfg ->
    Just (MapKeyValueM (\k v -> do
                           (!k', !v') <- f' k v
                           pure (g' k' v')))
  (MapKeyValueM g', MapKeyValueM f') | optFuseMaps cfg ->
    Just (MapKeyValueM (\k v -> do
                            (!k', !v') <- f' k v
                            g' k' v'))

  -- FlatMap fusion
  (FlatMapValues g', MapValues f') | optFuseFlatMaps cfg ->
    Just (FlatMapValues (g' . f'))
  (MapValues g', FlatMapValues f') | optFuseFlatMaps cfg ->
    Just (FlatMapValues (fmap g' . f'))
  (FlatMapValues g', FlatMapValues f') | optFuseFlatMaps cfg ->
    Just (FlatMapValues (\v -> concatMap g' (f' v)))

  -- Filter / FilterNot fusion. 'f' runs first, so the conjunction's
  -- left-hand side is the inner predicate.
  (Filter p2, Filter p1) | optFuseFilters cfg ->
    Just (Filter (\r -> p1 r && p2 r))
  (FilterNot p2, FilterNot p1) | optFuseFilters cfg ->
    Just (FilterNot (\r -> p1 r || p2 r))
  (Filter p, FilterNot q) | optFuseFilters cfg ->
    Just (Filter (\r -> not (q r) && p r))
  (FilterNot q, Filter p) | optFuseFilters cfg ->
    Just (Filter (\r -> p r && not (q r)))

  -- SelectKey fusion. The outer 'SelectKey' sees the re-keyed
  -- record; we synthesise the intermediate 'Record k' v' for it.
  (SelectKey g', SelectKey f') | optFuseSelectKeys cfg ->
    Just (SelectKey (\r ->
            let !k' = f' r
                !r' = r { recordKey = Just k' }
             in g' r'))

  -- Peek fusion. 'inner' runs first, then 'outer' — both observe
  -- the same record, no order change.
  (Peek g', Peek f') | optFusePeeks cfg ->
    Just (Peek (\r -> f' r >> g' r))

  -- Foreach after Peek: the peek's side-effect ran, then the
  -- foreach's side effect runs on the same record and drops the
  -- wire. Fuse into a single 'Foreach' that runs both in order.
  (Foreach g', Peek f') | optFusePeeks cfg ->
    Just (Foreach (\r -> f' r >> g' r))

  -- 'GroupBy' subsumes 'SelectKey >>> GroupByKey': re-key + group
  -- in a single processor. Mirrors Java's guidance to prefer
  -- @groupBy@ over @selectKey + groupByKey@. The 'Grouped' carries
  -- the new key serde so it stays on 'GroupBy'.
  (GroupByKey g', SelectKey f') | optFuseSelectKeyIntoGroupBy cfg ->
    Just (GroupBy f' g')

  -- Repartition idempotence: a second 'Repartition' with no
  -- key-change between it and the first is a redundant shuffle.
  -- The outer wins because its prefix is the one the broker
  -- topic will be named after.
  (Repartition pfx2, Repartition _pfx1) | optCollapseRepartition cfg ->
    Just (Repartition pfx2)
  (RepartitionWith cfg2, Repartition _) | optCollapseRepartition cfg ->
    Just (RepartitionWith cfg2)
  (Repartition pfx, RepartitionWith _) | optCollapseRepartition cfg ->
    Just (Repartition pfx)
  (RepartitionWith cfg2, RepartitionWith _) | optCollapseRepartition cfg ->
    Just (RepartitionWith cfg2)

  -- 'Values' is idempotent on a key-less stream.
  (Values, Values) | optCollapseValues cfg -> Just Values

  -- Adjacent 'Tap's combine into one by Fanout-ing the side
  -- pipelines. Side effects of both still run on each record;
  -- the wire still passes through unchanged. Saves a constructor
  -- but more importantly keeps the AST flat for downstream
  -- inspection. 'inner' runs first then 'outer'.
  (Tap t1, Tap t2) | optFuseTaps cfg ->
    Just (Tap (Compose (Arr (const ())) (Fanout t2 t1)))

  -- 'Tap (Foreach f)' is exactly 'Peek f' — both run an effect
  -- on each record and pass the wire through. Collapses one
  -- 'Tap' node into a 'Peek'.
  (_, _) | Just t <- tapForeachToPeek cfg g f -> Just t

  -- Push pure functions through 'Fork' so the resulting 'Arr'
  -- can fuse with adjacent 'Arr's. @'Arr' f '.' 'Fork'@ is just
  -- the pure function applied to the duplicated input.
  (Arr g', Fork) | optFusePureFunctions cfg ->
    Just (Arr (\a -> g' (a, a)))

  _ -> Nothing

-- | Detect the pattern @'Tap' ('Foreach' f)@ which is equivalent
-- to @'Peek' f@. The pattern is on the /inner/ position; the
-- outer is some unrelated combinator that the caller wants to
-- chain afterwards (it's just passed through).
--
-- We match on the @f@ argument of 'fuseStep' (i.e. the inner
-- topology) and use a tiny helper rather than a full extra
-- 'fuseStep' clause because the type juggling for the case is
-- a bit verbose.
tapForeachToPeek
  :: forall a b c
   . OptimizeConfig
  -> Topology b c
  -> Topology a b
  -> Maybe (Topology a c)
tapForeachToPeek cfg g f
  | optFusePeeks cfg
  , Tap inner <- f
  , Foreach act <- inner
  -- Re-fuse 'g' on top of the new 'Peek' so this rewrite plays
  -- well with the surrounding pass.
  = case fuseStep cfg g (Peek act) of
      Just t  -> Just t
      Nothing -> Just (Compose g (Peek act))
  | otherwise = Nothing

----------------------------------------------------------------------
-- Node counting and optimization statistics
----------------------------------------------------------------------

-- | Count the constructors in a 'Topology' AST.
--
-- Every leaf (sources / sinks / transforms / aggregators /
-- 'Arr' / 'Id' / 'Lifted' / etc.) counts as 1; every structural
-- combinator ('Compose', 'First', 'Second', 'Parallel',
-- 'Fanout', 'LeftT', 'RightT', 'Plus', 'Fanin', 'Tap',
-- 'ForkN', 'Bind') also counts 1 and adds the counts of its
-- children.
--
-- This is the basis of the optimiser's fixed-point criterion
-- ('optimizeWith' loops until 'countNodes' stops decreasing)
-- and of 'optimizationStats' for visibility into how much
-- the rewriter shrank a topology.
--
-- == What it doesn't count
--
--   * The compiled Kafka 'Topo.Topology' graph nodes. AST
--     nodes and Kafka processor nodes don't have a 1:1
--     mapping — pure 'Arr' nodes compile to nothing, several
--     leaf constructors compile to multiple processors, and
--     the optimiser collapses Arr chains into one 'Arr'
--     constructor. Use 'topologyNodes' on the compiled graph
--     to count run-time nodes.
--   * The /continuation/ of a 'Bind' — the function returning
--     a continuation 'Topology' is opaque, so we count it as
--     a single placeholder.
countNodes :: Topology i o -> Int
countNodes = go
  where
    go :: forall a b. Topology a b -> Int
    go Id              = 1
    go (Compose g f)   = 1 + go g + go f
    go (Arr _)         = 1
    go (First t)       = 1 + go t
    go (Second t)      = 1 + go t
    go (Parallel p q)  = 1 + go p + go q
    go (Fanout p q)    = 1 + go p + go q
    go (LeftT t)       = 1 + go t
    go (RightT t)      = 1 + go t
    go (Plus p q)      = 1 + go p + go q
    go (Fanin p q)     = 1 + go p + go q
    go Fork            = 1
    go (ForkN ts)      = 1 + sum (NE.map go ts)
    go (Tap t)         = 1 + go t
    go (Split _ _)     = 1
    -- Monad bind: the left side is visible, the continuation is
    -- opaque (we count it as 1 placeholder node).
    go (Bind t _)      = 1 + go t
    -- All remaining leaves: source/sink/transform/aggregator etc.
    go _               = 1

-- | Before / after node counts plus the absolute and relative
-- reduction the default rewrite passes achieve on a given AST.
--
-- Useful in tests, benchmarks, and golden-file outputs as a
-- sanity check on the optimiser — assert
-- @osNodesSaved > 0@ to confirm a representative pipeline is
-- actually being shrunk.
--
-- The fields:
--
--   * 'osBefore' — node count of the input AST (via
--     'countNodes' before rewriting).
--   * 'osAfter'  — node count after applying 'optimize'.
--   * 'osNodesSaved' — the difference, never negative
--     because every rewrite is strictly non-expanding.
--   * 'osPercent' — @100 * osNodesSaved / osBefore@; @0@ for
--     the (degenerate) empty-AST case.
data OptimizationStats = OptimizationStats
  { osBefore     :: !Int
  , osAfter      :: !Int
  , osNodesSaved :: !Int
  , osPercent    :: !Double
  }
  deriving stock (Eq, Show)

-- | Compute 'OptimizationStats' for applying the default
-- 'optimize' to @t@. Equivalent to
--
-- @
-- let before = 'countNodes' t
--     after  = 'countNodes' ('optimize' t)
-- in OptimizationStats before after (before - after) percent
-- @
--
-- but bundled into one call for ergonomics. Useful as the
-- @<>@-side of a 'show' / 'putStrLn' for quick optimiser
-- inspection at the REPL.
optimizationStats :: Topology i o -> OptimizationStats
optimizationStats t =
  let !before = countNodes t
      !after  = countNodes (optimize t)
      !saved  = before - after
      !pct    = if before == 0
                  then 0
                  else fromIntegral saved * 100 / fromIntegral before
   in OptimizationStats
        { osBefore     = before
        , osAfter      = after
        , osNodesSaved = saved
        , osPercent    = pct
        }
