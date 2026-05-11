-- |
-- Module      : Kafka.Streams
-- Description : Public API umbrella for the Kafka Streams port
--
-- Re-exports the entire Streams DSL surface in one place so user
-- code only needs @import Kafka.Streams@.
--
-- = Shape of the DSL
--
-- The DSL is plain Haskell IO. You build a topology by
-- creating a 'StreamsBuilder', threading 'KStream' / 'KTable'
-- values through the combinators, and finishing with
-- 'buildTopology'. The 'KStream' value carries its builder
-- internally, so only the source operations (@streamFromTopic@,
-- @tableFromTopic@) need the builder threaded explicitly:
--
-- @
-- topology :: IO Topology
-- topology = do
--   b   <- newStreamsBuilder
--   src <- streamFromTopic b (topicName \"in\")
--             (consumed textSerde textSerde)
--   out <- mapValues T.toUpper src
--      >>= filterStream (\\r -> recordValue r /\= \"\")
--   toTopic (topicName \"out\") (produced textSerde textSerde) out
--   buildTopology b
-- @
--
-- Each combinator's Haddock lists the JVM-DSL method it
-- mirrors so the cross-reference is one click away.
--
-- == Reusable transformation fragments
--
-- For transformations you want to /name/ and /reuse/ across
-- topologies, use 'Kafka.Streams.DSL.Pipeline'. A
-- @Pipeline a b@ is a thin newtype over @a -\> IO b@ with a
-- 'Control.Category.Category' instance, so fragments compose
-- with @('Control.Category.>>>')@ exactly like ordinary
-- functions. Equivalent JVM idiom: extracting a helper method
-- that takes a @KStream@ and returns a transformed @KStream@.
--
-- == Modules not re-exported here
--
--   * @Kafka.Streams.Internal@ is an implementation detail.
--   * @Kafka.Streams.Stores@ shadows each per-backend factory
--     name (the @Stores.persistentKeyValueStore@ /
--     @Stores.inMemoryWindowStore@ shape mirrors the JVM
--     @org.apache.kafka.streams.state.Stores@ class). Import
--     it qualified instead:
--
-- @
-- import qualified Kafka.Streams.Stores as Stores
-- @
module Kafka.Streams
  ( -- * Topology, Processor API
    module Kafka.Streams.Types
  , module Kafka.Streams.Time
  , module Kafka.Streams.Errors
  , module Kafka.Streams.Serde
  , module Kafka.Streams.Window
  , module Kafka.Streams.Processor
  , module Kafka.Streams.Topology
  , module Kafka.Streams.TopologyDescription
  , module Kafka.Streams.Metrics
  , module Kafka.Streams.Query
  , module Kafka.Streams.Discovery
  , module Kafka.Streams.Config
    -- * State stores
  , module Kafka.Streams.State.Store
  , module Kafka.Streams.State.KeyValue.InMemory
  , module Kafka.Streams.State.KeyValue.Persistent
  , module Kafka.Streams.State.KeyValue.Caching
  , module Kafka.Streams.State.KeyValue.Timestamped
  , module Kafka.Streams.State.KeyValue.Versioned
  , module Kafka.Streams.State.Window.InMemory
  , module Kafka.Streams.State.Session.InMemory
    -- * DSL
  , module Kafka.Streams.DSL.Consumed
  , module Kafka.Streams.DSL.Produced
  , module Kafka.Streams.DSL.Repartitioned
  , module Kafka.Streams.DSL.Grouped
  , module Kafka.Streams.DSL.Joined
  , module Kafka.Streams.DSL.Named
  , module Kafka.Streams.DSL.Materialized
  , module Kafka.Streams.DSL.StreamsBuilder
  , module Kafka.Streams.DSL.KStream
  , module Kafka.Streams.DSL.KGroupedStream
  , module Kafka.Streams.DSL.KGroupedTable
  , module Kafka.Streams.DSL.KTable
  , module Kafka.Streams.DSL.TimeWindowedKStream
  , module Kafka.Streams.DSL.SessionWindowedKStream
  , module Kafka.Streams.DSL.GlobalKTable
  , module Kafka.Streams.DSL.ForeignKeyJoin
  , module Kafka.Streams.DSL.Cogroup
  , module Kafka.Streams.DSL.Suppress
    -- * Driver / Runtime / IQ
  , module Kafka.Streams.Driver
  , module Kafka.Streams.Runtime
  , module Kafka.Streams.InteractiveQueries
  ) where

import Kafka.Streams.Config
import Kafka.Streams.DSL.Consumed
import Kafka.Streams.DSL.Grouped
import Kafka.Streams.DSL.Joined
import Kafka.Streams.DSL.Named
import Kafka.Streams.DSL.KGroupedStream
import Kafka.Streams.DSL.KGroupedTable
import Kafka.Streams.DSL.KStream
import Kafka.Streams.DSL.KTable
import Kafka.Streams.DSL.Materialized
import Kafka.Streams.DSL.Produced
import Kafka.Streams.DSL.Repartitioned
import Kafka.Streams.DSL.Cogroup
import Kafka.Streams.DSL.Suppress
import Kafka.Streams.DSL.ForeignKeyJoin
import Kafka.Streams.DSL.GlobalKTable
import Kafka.Streams.DSL.SessionWindowedKStream
import Kafka.Streams.DSL.StreamsBuilder
import Kafka.Streams.DSL.TimeWindowedKStream
import Kafka.Streams.Driver
import Kafka.Streams.Errors
import Kafka.Streams.InteractiveQueries
import Kafka.Streams.Processor
import Kafka.Streams.Runtime
import Kafka.Streams.Serde
import Kafka.Streams.State.KeyValue.Caching
import Kafka.Streams.State.KeyValue.InMemory
import Kafka.Streams.State.KeyValue.Persistent
import Kafka.Streams.State.KeyValue.Timestamped
import Kafka.Streams.State.KeyValue.Versioned
import Kafka.Streams.State.Session.InMemory
import Kafka.Streams.State.Store
import Kafka.Streams.State.Window.InMemory
import Kafka.Streams.Time
import Kafka.Streams.Topology
import Kafka.Streams.Discovery
import Kafka.Streams.Metrics
import Kafka.Streams.Query
import Kafka.Streams.TopologyDescription
import Kafka.Streams.Types
import Kafka.Streams.Window
