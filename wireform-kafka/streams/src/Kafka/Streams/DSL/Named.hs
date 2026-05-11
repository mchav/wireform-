{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.DSL.Named
-- Description : 'Named' configuration for DSL operators
--
-- Mirrors Java's @org.apache.kafka.streams.kstream.Named@. Every
-- DSL operator accepts an optional 'Named' that sets the topology
-- node name (otherwise the builder auto-generates one of the form
-- @KSTREAM-MAPVALUES-0000000007@).
--
-- Named names show up in 'TopologyDescription' / 'pretty' output
-- and in the metrics namespace, so they're useful for diagnostics
-- when you want stable identifiers across topology builds.
module Kafka.Streams.DSL.Named
  ( Named (..)
  , named
  , noName
  , namedOr
  ) where

import Data.Text (Text)

import Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  )
import qualified Kafka.Streams.Topology as Topo

-- | An optional explicit name for a DSL operator's topology node.
-- 'Nothing' means "let the builder generate one".
newtype Named = Named { unNamed :: Maybe Text }
  deriving stock (Eq, Show)

-- | Lift a 'Text' into a 'Named'.
named :: Text -> Named
named = Named . Just

-- | The "no name" marker — every operator that takes 'Named' uses
-- this when the caller doesn't provide one.
noName :: Named
noName = Named Nothing

-- | Resolve a 'Named' into a concrete 'NodeName', falling back to
-- 'freshNodeName' with the given prefix when no name is set.
namedOr :: StreamsBuilder -> Named -> Text -> IO Topo.NodeName
namedOr _ (Named (Just n)) _      = pure (Topo.NodeName n)
namedOr b (Named Nothing)  prefix = freshNodeName b prefix
