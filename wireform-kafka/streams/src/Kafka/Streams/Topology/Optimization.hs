{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Kafka.Streams.Topology.Optimization
Description : KIP-295 / @StreamsConfig.topology.optimization@ toggles

The Java client lets users opt into a small set of topology
rewrites with @Topology.optimize(StreamsBuilder.OPTIMIZE)@:

  * @REUSE_KTABLE_SOURCE_TOPICS@   — drop the per-source-table
    repartition topic when the source topic already has a
    suitable key.
  * @MERGE_REPARTITION_TOPICS@     — collapse adjacent repartitions
    that share a key.
  * @SINGLE_STORE_SELF_JOIN@       — when both sides of a stream-stream
    self-join read from the same source, hold one window-store
    instead of two.

This module is the pure decision layer: 'TopologyOptimizationLevel'
+ 'optimizationFlags' translate the user-facing knob into an
internal set of booleans the topology builder consults. The actual
rewrites would call 'shouldReuseSourceTopics' etc. when assembling
the topology DAG.
-}
module Kafka.Streams.Topology.Optimization (
  TopologyOptimizationLevel (..),
  OptimizationFlags (..),
  noOptimizations,
  optimizationFlags,
  parseOptimizationLevel,
  optimizationLevelText,
) where

import Data.Text (Text)
import GHC.Generics (Generic)


{- | The user-visible @topology.optimization@ knob. Mirrors the
Java enum verbatim.
-}
data TopologyOptimizationLevel
  = OptimizeNone
  | OptimizeReuseKtableSourceTopics
  | OptimizeMergeRepartitionTopics
  | OptimizeSingleStoreSelfJoin
  | OptimizeAll
  deriving stock (Eq, Show, Generic)


{- | Internal projection of the user's knob into the set of
rewriter passes that should run.
-}
data OptimizationFlags = OptimizationFlags
  { flagReuseSourceTopics :: !Bool
  , flagMergeRepartitionTopics :: !Bool
  , flagSingleStoreSelfJoin :: !Bool
  }
  deriving stock (Eq, Show, Generic)


noOptimizations :: OptimizationFlags
noOptimizations = OptimizationFlags False False False


optimizationFlags :: TopologyOptimizationLevel -> OptimizationFlags
optimizationFlags = \case
  OptimizeNone -> noOptimizations
  OptimizeReuseKtableSourceTopics ->
    noOptimizations {flagReuseSourceTopics = True}
  OptimizeMergeRepartitionTopics ->
    noOptimizations {flagMergeRepartitionTopics = True}
  OptimizeSingleStoreSelfJoin ->
    noOptimizations {flagSingleStoreSelfJoin = True}
  OptimizeAll -> OptimizationFlags True True True


{- | Parse the textual config value users put in their
@StreamsConfig@ properties. Mirrors the Java enum's @valueOf@.
-}
parseOptimizationLevel :: Text -> Maybe TopologyOptimizationLevel
parseOptimizationLevel = \case
  "none" -> Just OptimizeNone
  "reuse.ktable.source.topics" -> Just OptimizeReuseKtableSourceTopics
  "merge.repartition.topics" -> Just OptimizeMergeRepartitionTopics
  "single.store.self.join" -> Just OptimizeSingleStoreSelfJoin
  "all" -> Just OptimizeAll
  _ -> Nothing


optimizationLevelText :: TopologyOptimizationLevel -> Text
optimizationLevelText = \case
  OptimizeNone -> "none"
  OptimizeReuseKtableSourceTopics -> "reuse.ktable.source.topics"
  OptimizeMergeRepartitionTopics -> "merge.repartition.topics"
  OptimizeSingleStoreSelfJoin -> "single.store.self.join"
  OptimizeAll -> "all"
