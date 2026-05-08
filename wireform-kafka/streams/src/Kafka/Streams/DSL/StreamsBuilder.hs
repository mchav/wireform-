{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Kafka.Streams.DSL.StreamsBuilder
-- Description : Mutable DSL builder
--
-- 'StreamsBuilder' is the stateful façade users construct topologies
-- through. Mirrors @org.apache.kafka.streams.StreamsBuilder@.
--
-- The builder is a mutable handle: every DSL operation (@mapValues@,
-- @filter@, @to@, @groupByKey@, …) mutates the embedded 'Topology'
-- via 'IORef' and returns a fresh 'KStream' / 'KTable' value pinned
-- to the new node. This trades a tiny amount of impurity for the
-- ability to mirror the Java fluent API one-to-one.
module Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , newStreamsBuilder
  , buildTopology
    -- * Internals (used by KStream/KTable)
  , freshNodeName
  , freshStoreName
  , withTopology
  , readTopology
  , withTopology_
  ) where

import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)

import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.State.Store (StoreName, storeName)

-- | Mutable DSL builder.
data StreamsBuilder = StreamsBuilder
  { sbTopology  :: !(IORef Topo.Topology)
  , sbNodeCount :: !(IORef Int)
  , sbStoreCount :: !(IORef Int)
  }

-- | Create a fresh, empty builder.
newStreamsBuilder :: IO StreamsBuilder
newStreamsBuilder = do
  t <- newIORef Topo.emptyTopology
  n <- newIORef 0
  s <- newIORef 0
  pure StreamsBuilder { sbTopology = t, sbNodeCount = n, sbStoreCount = s }

-- | Snapshot the builder into a 'Topology'.
buildTopology :: StreamsBuilder -> IO Topo.Topology
buildTopology = readIORef . sbTopology

readTopology :: StreamsBuilder -> IO Topo.Topology
readTopology = readIORef . sbTopology

-- | Run a transformation over the embedded topology, returning a
-- result the caller wants alongside the new topology.
withTopology
  :: StreamsBuilder
  -> (Topo.Topology -> (Topo.Topology, a))
  -> IO a
withTopology b f = atomicModifyIORef' (sbTopology b) f

withTopology_
  :: StreamsBuilder -> (Topo.Topology -> Topo.Topology) -> IO ()
withTopology_ b f =
  atomicModifyIORef' (sbTopology b) (\t -> (f t, ()))

-- | Synthesise a unique node name with the given prefix.
freshNodeName :: StreamsBuilder -> Text -> IO Topo.NodeName
freshNodeName b prefix = do
  n <- atomicModifyIORef' (sbNodeCount b) (\i -> (i + 1, i))
  pure (Topo.nodeName (prefix <> "-" <> T.pack (show n)))

-- | Synthesise a unique store name with the given prefix.
freshStoreName :: StreamsBuilder -> Text -> IO StoreName
freshStoreName b prefix = do
  n <- atomicModifyIORef' (sbStoreCount b) (\i -> (i + 1, i))
  pure (storeName (prefix <> "-" <> T.pack (show n)))
