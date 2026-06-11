{- |
Module      : Kafka.Streams.Imperative
Description : Imperative-first umbrella (pre-Free public API)

This module re-exports the imperative DSL surface that
@import Kafka.Streams@ used to expose before the public API
pivoted to "Kafka.Streams.Topology.Free". It bundles the
@KStream -> IO KStream@ flavoured combinators alongside all
the value-type and runtime modules.

Use this module when:

  * You're maintaining an internal subsystem (the streams
    library's own tests, conformance fixtures, the
    "Kafka.Streams" Free compiler's interpreter implementation)
    that interacts with the imperative DSL directly.
  * You're migrating a downstream project from the old
    umbrella shape; updating @import Kafka.Streams@ to
    @import Kafka.Streams.Imperative@ preserves the previous
    semantics one-shot.

New code should prefer "Kafka.Streams" (Free-first); the Free
DSL composes the same primitives without the implicit
'StreamsBuilder' threading and yields a first-class
inspectable\/optimisable 'F.Topology' value. The two surfaces
live side by side so existing imperative code keeps working.
-}
module Kafka.Streams.Imperative (
  -- * Topology, Processor API
  module Kafka.Streams.Types,
  module Kafka.Streams.Time,
  module Kafka.Streams.Errors,
  module Kafka.Streams.Serde,
  module Kafka.Streams.Window,
  module Kafka.Streams.Processor,
  module Kafka.Streams.Topology,
  module Kafka.Streams.TopologyDescription,
  module Kafka.Streams.Metrics,
  module Kafka.Streams.Query,
  module Kafka.Streams.Discovery,
  module Kafka.Streams.Config,

  -- * State stores
  module Kafka.Streams.State.Store,
  module Kafka.Streams.State.KeyValue.InMemory,
  module Kafka.Streams.State.KeyValue.Persistent,
  module Kafka.Streams.State.KeyValue.Caching,
  module Kafka.Streams.State.KeyValue.Timestamped,
  module Kafka.Streams.State.KeyValue.Versioned,
  module Kafka.Streams.State.Window.InMemory,
  module Kafka.Streams.State.Session.InMemory,

  -- * DSL
  module Kafka.Streams.Consumed,
  module Kafka.Streams.Produced,
  module Kafka.Streams.Repartitioned,
  module Kafka.Streams.Grouped,
  module Kafka.Streams.Joined,
  module Kafka.Streams.Named,
  module Kafka.Streams.Materialized,
  module Kafka.Streams.StreamsBuilder,
  module Kafka.Streams.KStream,
  module Kafka.Streams.KGroupedStream,
  module Kafka.Streams.KGroupedTable,
  module Kafka.Streams.KTable,
  module Kafka.Streams.TimeWindowedKStream,
  module Kafka.Streams.SessionWindowedKStream,
  module Kafka.Streams.GlobalKTable,
  module Kafka.Streams.ForeignKeyJoin,
  module Kafka.Streams.Cogroup,
  module Kafka.Streams.Suppress,

  -- * Driver / Runtime / IQ
  module Kafka.Streams.Driver,
  module Kafka.Streams.Runtime,
  module Kafka.Streams.InteractiveQueries,
) where

import Kafka.Streams.Cogroup
import Kafka.Streams.Config
import Kafka.Streams.Consumed
import Kafka.Streams.Discovery
import Kafka.Streams.Driver
import Kafka.Streams.Errors
import Kafka.Streams.ForeignKeyJoin
import Kafka.Streams.GlobalKTable
import Kafka.Streams.Grouped
import Kafka.Streams.InteractiveQueries
import Kafka.Streams.Joined
import Kafka.Streams.KGroupedStream
import Kafka.Streams.KGroupedTable
import Kafka.Streams.KStream
import Kafka.Streams.KTable
import Kafka.Streams.Materialized
import Kafka.Streams.Metrics
import Kafka.Streams.Named
import Kafka.Streams.Processor
import Kafka.Streams.Produced
import Kafka.Streams.Query
import Kafka.Streams.Repartitioned
import Kafka.Streams.Runtime
import Kafka.Streams.Serde
import Kafka.Streams.SessionWindowedKStream
import Kafka.Streams.State.KeyValue.Caching
import Kafka.Streams.State.KeyValue.InMemory
import Kafka.Streams.State.KeyValue.Persistent
import Kafka.Streams.State.KeyValue.Timestamped
import Kafka.Streams.State.KeyValue.Versioned
import Kafka.Streams.State.Session.InMemory
import Kafka.Streams.State.Store
import Kafka.Streams.State.Window.InMemory
import Kafka.Streams.StreamsBuilder
import Kafka.Streams.Suppress
import Kafka.Streams.Time
import Kafka.Streams.TimeWindowedKStream
import Kafka.Streams.Topology
import Kafka.Streams.TopologyDescription
import Kafka.Streams.Types
import Kafka.Streams.Window

