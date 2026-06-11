{- |
Module      : Kafka.Streams.Topology.Free.Optimize
Description : Re-export of the AST optimiser for 'Topology'

The optimisation passes live in "Kafka.Streams.Topology.Free"
alongside the GADT to avoid an import cycle (the optimiser
pattern-matches on every 'Topology' constructor). This thin
re-export keeps the conventional @.Free.Optimize@ module path
working so callers can do

@
import qualified Kafka.Streams.Topology.Free.Optimize as Opt

topo' :: F.Topology i o -> F.Topology i o
topo' = Opt.optimizeWith (Opt.defaultOptimizeConfig
                           { Opt.optFuseConcatMaps = False })
@

without having to know that the implementation lives one module
up. The default 'Kafka.Streams.Topology.Free.compile' already
applies 'optimize' automatically — this module is for callers
who want fine-grained control.
-}
module Kafka.Streams.Topology.Free.Optimize (
  -- * Optimisation passes
  optimize,
  optimizeWith,
  OptimizeConfig (..),
  defaultOptimizeConfig,
  noOptimization,

  -- * Statistics
  countNodes,
  OptimizationStats (..),
  optimizationStats,
) where

import Kafka.Streams.Topology.Free (
  OptimizationStats (..),
  OptimizeConfig (..),
  countNodes,
  defaultOptimizeConfig,
  noOptimization,
  optimizationStats,
  optimize,
  optimizeWith,
 )

