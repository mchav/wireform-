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
--   * @'Kafka.Streams.GlobalKTable' k v@   — cluster-replicated table
--   * @'Data.Void.Void'@                   — an /open/ input slot
--                                             (only sources fill it)
--   * @()@                                 — a sink terminus
--   * @(a, b)@                             — two independent wires
--                                             (parallel composition)
--   * @'Either' a b@                       — a choice between two
--                                             wires
--
-- The full DSL surface is reified as constructors. Composition uses
-- the standard category-theoretic vocabulary:
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
-- == The "parallel" caveat
--
-- @'Parallel' p q :: 'Topology' (a, c) (b, d)@ runs @p@ on the @a@
-- side and @q@ on the @c@ side. Each side is an /independent/
-- subgraph: nothing connects them at the topology level except the
-- pair value at composition boundaries. This corresponds exactly to
-- how the Java builder treats two independent @KStream@ pipelines —
-- the broker partition layout is what enforces parallelism at run
-- time; the topology graph just has two disconnected lineages.
--
-- @'Fanout' p q :: 'Topology' a (b, c)@ is different: a single
-- upstream wire is fed to /both/ sub-fragments. In Kafka Streams
-- this is the "one processor with two children" pattern; the
-- compiler reuses the same upstream node as the parent of both
-- sub-graphs, mirroring how the imperative DSL handles a
-- @KStream@ used in two downstream operations.
module Kafka.Streams.Topology.Free
  ( -- * The 'Topology' GADT
    Topology (..)

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
  , selectKey
  , values

    -- * 'KStream' composition
  , merge
  , mergeAll
  , branch

    -- * 'KStream' \<-\> 'KTable' conversions
  , toTable
  , toStream
  , repartition

    -- * Grouping + aggregation
  , groupByKey
  , groupBy
  , count
  , reduce
  , aggregate

    -- * Joins
  , streamTableJoin
  , streamTableLeftJoin
  , streamStreamJoin
  , streamStreamLeftJoin
  , streamStreamOuterJoin
  , tableTableJoin

    -- * KTable
  , filterTable
  , mapValuesTable

    -- * Escape hatch + introspection
  , liftIO_
  , inspect
  , prettyPrint
  ) where

import Prelude hiding (id, filter, (.))

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Kafka.Streams.Consumed (Consumed, consumed)
import Kafka.Streams.GlobalKTable (GlobalKTable)
import qualified Kafka.Streams.GlobalKTable as GT
import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.Joined (JoinWindows, Joined)
import qualified Kafka.Streams.KGroupedStream as KGS
import Kafka.Streams.KGroupedStream (KGroupedStream)
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.KStream (KStream)
import qualified Kafka.Streams.KTable as KT
import Kafka.Streams.KTable (KTable)
import Kafka.Streams.Materialized (Materialized)
import qualified Kafka.Streams.Materialized as Mat
import Kafka.Streams.Produced (Produced, produced)
import Kafka.Streams.Serde (Serde)
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , buildTopology
  , newStreamsBuilder
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..), TopicName, topicName)

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
  -- * Category / Arrow combinators
  --
  -- These cover the laws-respecting combinators of 'Category',
  -- 'Arrow', and 'ArrowChoice'.

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

  -- | 'Control.Arrow.***': two independent sub-fragments in parallel.
  -- See the module-level note on the "parallel" semantics.
  Parallel :: Topology a b -> Topology c d -> Topology (a, c) (b, d)

  -- | 'Control.Arrow.&&&': feed one upstream into two sub-fragments
  -- and pair the outputs. The compiler reuses the same upstream
  -- node as parent for both sub-graphs.
  Fanout  :: Topology a b -> Topology a c -> Topology a (b, c)

  -- | 'Control.Arrow.left' for sum-typed wires.
  LeftT   :: Topology a b -> Topology (Either a c) (Either b c)

  -- | 'Control.Arrow.right'.
  RightT  :: Topology a b -> Topology (Either c a) (Either c b)

  -- | 'Control.Arrow.+++'.
  Plus    :: Topology a b -> Topology c d -> Topology (Either a c) (Either b d)

  -- | 'Control.Arrow.|||': collapse a sum into a single output.
  Fanin   :: Topology a c -> Topology b c -> Topology (Either a b) c

  -- * Sources
  --
  -- Sources have 'Void' on the input side. Composing anything to the
  -- left of a 'Source' is statically impossible; the only way to
  -- close the input is to put a source there.

  Source       :: !TopicName -> !(Consumed k v)
               -> Topology Void (KStream k v)

  -- | Source materialised straight into a 'KTable'.
  TableSource  :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Topology Void (KTable k v)

  -- | Source materialised into a 'GlobalKTable'.
  GlobalSource :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Topology Void (GlobalKTable k v)

  -- * Sinks

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

  -- * Stateless 'KStream' transforms

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
  SelectKey       :: (Record k v -> k')
                  -> Topology (KStream k v) (KStream k' v)
  Values          :: Topology (KStream k v) (KStream () v)

  -- * 'KStream' composition

  -- | Binary 'KStream.merge'. Use 'Fanout' or 'Parallel' to bring two
  -- streams into the tuple-shape this constructor consumes.
  Merge   :: Topology (KStream k v, KStream k v) (KStream k v)
  -- | N-ary merge. The runtime fold is implemented in
  -- 'Kafka.Streams.KStream.mergeStreamsN' — empty lists are
  -- rejected at compile-of-AST time by requiring a non-empty list
  -- through the constructor's type.
  MergeAll :: Topology [KStream k v] (KStream k v)

  -- | Predicate-routed split. Records that match no predicate are
  -- dropped. Mirrors the pre-KIP-418 @KStream.branch@ shape.
  Branch  :: ![Record k v -> Bool]
          -> Topology (KStream k v) [KStream k v]

  -- * Conversions

  ToTableT     :: Ord k
               => !(Materialized k v)
               -> Topology (KStream k v) (KTable k v)
  ToStream     :: Topology (KTable k v) (KStream k v)
  Repartition  :: !Text
               -> Topology (KStream k v) (KStream k v)

  -- * Grouping + aggregation

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

  -- * Joins
  --
  -- Joins take the two participants on the input side as a tuple. To
  -- thread two named upstream sources into a join, build them with
  -- 'Parallel' / 'Fanout' so they land in the same pair.

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

  -- * KTable surface

  FilterTable
    :: Ord k
    => (Record k v -> Bool) -> !(Materialized k v)
    -> Topology (KTable k v) (KTable k v)
  MapValuesTable
    :: Ord k
    => (v -> v') -> !(Materialized k v')
    -> Topology (KTable k v) (KTable k v')

  -- * Escape hatch
  --
  -- Lets callers splice in any topology-mutating IO action. Used
  -- internally by 'liftIO_' and for any operator that hasn't yet
  -- earned a dedicated constructor (custom processors, foreign-key
  -- joins, suppress, cogroup, …). The escape is typed: the
  -- caller still has to declare the input/output wire types.
  Lifted
    :: !Text                        -- ^ pretty name for traces
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
--
-- The output type @o@ is whatever the AST says it is — a 'KStream',
-- 'KTable', '()' for a closed sink, or a tuple\/'Either' of any of
-- those.
apply :: forall i o. Topology i o -> TBuilder -> i -> IO o
apply t b = go t
  where
    go :: forall x y. Topology x y -> x -> IO y
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

    -- Sources discharge their 'Void' input without inspecting it.
    -- The compiler caller is responsible for never feeding a real
    -- 'Void' through; in practice 'compile' supplies a thunk that
    -- aborts if ever forced.
    go (Source topic c)         = \_void -> KS.streamFromTopic b topic c
    go (TableSource topic c m)  = \_void -> KT.tableFromTopic b topic c m
    go (GlobalSource topic c m) = \_void -> GT.globalTable b topic c m

    go (Sink topic p)        = KS.toTopic topic p
    go (SinkExtracted e p)   = KS.toExtracted e p
    go (Through topic p)     = KS.throughTopic topic p

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

    go Merge                 = \(s1, s2) -> KS.mergeStreams s1 s2
    go MergeAll              = KS.mergeStreamsN
    go (Branch ps)           = KS.branchStream ps

    go (ToTableT m)          = KS.toTable m
    go ToStream              = KS.toKStreamFromKTable
    go (Repartition prefix)  = KS.repartition prefix

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

    go (FilterTable p m)
      = KT.filterTable p m
    go (MapValuesTable f m)
      = KT.mapValuesTable f m

    go (Lifted _ act)        = act b

-- | The 'CountedTableLocal' handle the existing DSL returns from
-- aggregations is a thin wrapper around a 'KTable' — it just doesn't
-- carry the serdes. We promote it to a real 'KTable' by extracting
-- the serdes from the supplied 'Materialized'. If they're absent
-- the corresponding 'KTable' field becomes a deferred error
-- (matching the existing 'KTable' lazy-serde behaviour after
-- @mapValues@ etc); a downstream @to@ or @join@ will then report
-- the missing serde at the use site rather than here.
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

-- | Publish records to a topic. The resulting wire is @()@ — the
-- pipeline is closed on this branch.
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

-- | Pure value-only map; mirrors @KStream.mapValues@.
mapValues :: (v -> v') -> Topology (KStream k v) (KStream k v')
mapValues = MapValues

-- | Effectful value-only map.
mapValuesM :: (v -> IO v') -> Topology (KStream k v) (KStream k v')
mapValuesM = MapValuesM

-- | Pure key+value map; mirrors @KStream.map@.
mapKeyValue
  :: (k -> v -> (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValue = MapKeyValue

-- | Effectful key+value map.
mapKeyValueM
  :: (k -> v -> IO (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValueM = MapKeyValueM

-- | Keep records satisfying the predicate.
filter :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filter = Filter

-- | Drop records satisfying the predicate.
filterNot :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filterNot = FilterNot

-- | One-to-many value expansion.
flatMapValues :: (v -> [v']) -> Topology (KStream k v) (KStream k v')
flatMapValues = FlatMapValues

-- | One-to-many key+value expansion.
flatMapKeyValue
  :: (k -> v -> [(k', v')]) -> Topology (KStream k v) (KStream k' v')
flatMapKeyValue = FlatMapKeyValue

-- | Side-effecting observer; record is unchanged.
peek :: (Record k v -> IO ()) -> Topology (KStream k v) (KStream k v)
peek = Peek

-- | Terminal side-effect. The result wire is @()@.
foreach :: (Record k v -> IO ()) -> Topology (KStream k v) ()
foreach = Foreach

-- | Re-key the stream from the full record.
selectKey :: (Record k v -> k') -> Topology (KStream k v) (KStream k' v)
selectKey = SelectKey

-- | Drop the key.
values :: Topology (KStream k v) (KStream () v)
values = Values

-- * 'KStream' composition -----------------------------------------

-- | Binary 'KStream.merge'.
merge :: Topology (KStream k v, KStream k v) (KStream k v)
merge = Merge

-- | N-ary merge.
mergeAll :: Topology [KStream k v] (KStream k v)
mergeAll = MergeAll

-- | Predicate-routed split.
branch :: [Record k v -> Bool] -> Topology (KStream k v) [KStream k v]
branch = Branch

-- * Conversions ---------------------------------------------------

-- | Materialise the latest value per key into a 'KTable'.
toTable :: Ord k => Materialized k v -> Topology (KStream k v) (KTable k v)
toTable = ToTableT

-- | Convert a 'KTable' to its changelog 'KStream'.
toStream :: Topology (KTable k v) (KStream k v)
toStream = ToStream

-- | Force a repartition with the given topic-name prefix.
repartition :: Text -> Topology (KStream k v) (KStream k v)
repartition = Repartition

-- * Grouping + aggregation ----------------------------------------

-- | Group by the existing key.
groupByKey :: Grouped k v -> Topology (KStream k v) (KGroupedStream k v)
groupByKey = GroupByKey

-- | Group by a derived key.
groupBy
  :: (Record k v -> k') -> Grouped k' v
  -> Topology (KStream k v) (KGroupedStream k' v)
groupBy = GroupBy

-- | Cardinality per key.
count
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedStream k v) (KTable k Int64)
count = Count

-- | Same-shape reducer.
reduce
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedStream k v) (KTable k v)
reduce = Reduce

-- | General-shape aggregator. The initialiser is @IO agg@ to mirror
-- Java's @Initializer<A>@ (a fresh accumulator per key).
aggregate
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedStream k v) (KTable k agg)
aggregate = Aggregate

-- * Joins ---------------------------------------------------------

-- | Stream-table inner join.
streamTableJoin
  :: Ord k
  => (v -> vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableJoin = StreamTableJoin

-- | Stream-table left join.
streamTableLeftJoin
  :: Ord k
  => (v -> Maybe vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableLeftJoin = StreamTableLeftJoin

-- | Stream-stream window inner join.
streamStreamJoin
  :: Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamJoin = StreamStreamJoin

-- | Stream-stream window left join.
streamStreamLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamLeftJoin = StreamStreamLeftJoin

-- | Stream-stream window outer join.
streamStreamOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamOuterJoin = StreamStreamOuterJoin

-- | KTable-KTable inner join.
tableTableJoin
  :: Ord k
  => (v1 -> v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableJoin = TableTableJoin

-- * KTable surface ------------------------------------------------

-- | Filter a 'KTable' by a predicate. Tombstones are forwarded as
-- 'KTable.filter' does upstream.
filterTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterTable = FilterTable

-- | Pure value-only map on a 'KTable'.
mapValuesTable
  :: Ord k
  => (v -> v') -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
mapValuesTable = MapValuesTable

----------------------------------------------------------------------
-- Escape hatch + introspection
----------------------------------------------------------------------

-- | Splice in a hand-rolled builder action. Use when an operator
-- isn't yet a dedicated constructor (custom processors, suppress,
-- cogroup, foreign-key joins, …). The 'Text' name is purely for
-- 'prettyPrint' / debug output.
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
    go (Source t _)    = ["Source(" <> showTopic t <> ")"]
    go (TableSource t _ _)  = ["TableSource(" <> showTopic t <> ")"]
    go (GlobalSource t _ _) = ["GlobalSource(" <> showTopic t <> ")"]
    go (Sink t _)      = ["Sink(" <> showTopic t <> ")"]
    go (SinkExtracted _ _) = ["SinkExtracted"]
    go (Through t _)   = ["Through(" <> showTopic t <> ")"]
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
    go Merge                = ["Merge"]
    go MergeAll             = ["MergeAll"]
    go (Branch ps)
      = ["Branch(" <> T.pack (show (length ps)) <> ")"]
    go (ToTableT _)         = ["ToTable"]
    go ToStream             = ["ToStream"]
    go (Repartition pfx)    = ["Repartition(" <> pfx <> ")"]
    go (GroupByKey _)       = ["GroupByKey"]
    go (GroupBy _ _)        = ["GroupBy"]
    go (Count _)            = ["Count"]
    go (Reduce _ _)         = ["Reduce"]
    go (Aggregate _ _ _)    = ["Aggregate"]
    go (StreamTableJoin _ _)     = ["StreamTableJoin"]
    go (StreamTableLeftJoin _ _) = ["StreamTableLeftJoin"]
    go (StreamStreamJoin _ _ _)      = ["StreamStreamJoin"]
    go (StreamStreamLeftJoin _ _ _)  = ["StreamStreamLeftJoin"]
    go (StreamStreamOuterJoin _ _ _) = ["StreamStreamOuterJoin"]
    go (TableTableJoin _ _)          = ["TableTableJoin"]
    go (FilterTable _ _)             = ["FilterTable"]
    go (MapValuesTable _ _)          = ["MapValuesTable"]
    go (Lifted nm _)                 = ["Lifted(" <> nm <> ")"]

    showTopic :: TopicName -> Text
    showTopic = T.pack . show

-- | Render an AST as a single-line description. Useful at the REPL
-- and in test failure messages. The output is /not/ a stable
-- format — use it for debugging, not for parsing.
prettyPrint :: Topology i o -> Text
prettyPrint = T.intercalate " " . inspect
