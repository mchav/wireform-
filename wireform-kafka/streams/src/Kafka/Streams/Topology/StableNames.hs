{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Kafka.Streams.Topology.StableNames
Description : KIP-307 stable name generator for unnamed DSL nodes

KIP-307 lets every DSL combinator take an explicit @Named@ value
that becomes part of the generated processor name (and any
internally-created changelog / repartition topic). Java users
that rely on @application.id@ portability across builds care
because Kafka uses processor names to derive the changelog topic
names that store user state — if those names drift, restoration
breaks.

This module provides the deterministic name generator the engine
uses when the user /didn't/ supply an explicit name. It mirrors
the Java client's @StreamsBuilder.Internal.NodeName@ shape:

  * Names are of the form @KSTREAM-MAPVALUES-0000000007@.
  * The numeric suffix is a per-topology, per-operator-class
    monotonically increasing counter padded to 10 digits.
  * Two builds of the same topology — i.e. the same operator
    sequence — produce identical names regardless of run-time
    timing.
-}
module Kafka.Streams.Topology.StableNames (
  OperatorClass (..),
  operatorPrefix,
  StableNameSeed,
  newStableNameSeed,
  nextName,

  -- * Pure (testable) interface
  generateNames,
) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)


{- | The operator classes that need stable names. Order matters
because the prefix is what users see in @TopologyDescription@.
-}
data OperatorClass
  = OpSource
  | OpSink
  | OpFilter
  | OpMap
  | OpMapValues
  | OpFlatMap
  | OpFlatMapValues
  | OpSelectKey
  | OpProcess
  | OpTransform
  | OpTransformValues
  | OpAggregate
  | OpReduce
  | OpCount
  | OpJoin
  | OpLeftJoin
  | OpOuterJoin
  | OpForeignKeyJoin
  | OpRepartition
  | OpKtable
  | OpToStream
  | OpThrough
  | OpBranch
  | OpMerge
  deriving stock (Eq, Ord, Show, Generic)


operatorPrefix :: OperatorClass -> Text
operatorPrefix = \case
  OpSource -> "KSTREAM-SOURCE-"
  OpSink -> "KSTREAM-SINK-"
  OpFilter -> "KSTREAM-FILTER-"
  OpMap -> "KSTREAM-MAP-"
  OpMapValues -> "KSTREAM-MAPVALUES-"
  OpFlatMap -> "KSTREAM-FLATMAP-"
  OpFlatMapValues -> "KSTREAM-FLATMAPVALUES-"
  OpSelectKey -> "KSTREAM-KEY-SELECT-"
  OpProcess -> "KSTREAM-PROCESSOR-"
  OpTransform -> "KSTREAM-TRANSFORM-"
  OpTransformValues -> "KSTREAM-TRANSFORMVALUES-"
  OpAggregate -> "KSTREAM-AGGREGATE-"
  OpReduce -> "KSTREAM-REDUCE-"
  OpCount -> "KSTREAM-COUNT-"
  OpJoin -> "KSTREAM-JOIN-"
  OpLeftJoin -> "KSTREAM-LEFTJOIN-"
  OpOuterJoin -> "KSTREAM-OUTERJOIN-"
  OpForeignKeyJoin -> "KTABLE-FK-JOIN-"
  OpRepartition -> "KSTREAM-REPARTITION-"
  OpKtable -> "KTABLE-"
  OpToStream -> "KSTREAM-TOSTREAM-"
  OpThrough -> "KSTREAM-THROUGH-"
  OpBranch -> "KSTREAM-BRANCH-"
  OpMerge -> "KSTREAM-MERGE-"


{- | Per-topology, per-operator counter. Mirrors what the Java
@InternalTopologyBuilder@ holds; we keep one counter for each
'OperatorClass' so consecutive maps don't burn through the same
counter and unrelated operators stay independent.
-}
newtype StableNameSeed = StableNameSeed (Map OperatorClass Int)
  deriving stock (Eq, Show)


newStableNameSeed :: StableNameSeed
newStableNameSeed = StableNameSeed Map.empty


{- | Allocate the next stable name for the given operator class.
Returns the name plus the updated seed.
-}
nextName :: OperatorClass -> StableNameSeed -> (Text, StableNameSeed)
nextName op (StableNameSeed m) =
  let !cur = Map.findWithDefault 0 op m
      !next = cur + 1
      !name = operatorPrefix op <> padDigits 10 cur
  in (name, StableNameSeed (Map.insert op next m))


padDigits :: Int -> Int -> Text
padDigits width n =
  let !s = T.pack (show n)
      !pad = max 0 (width - T.length s)
  in T.replicate pad "0" <> s


{- | Pure batch helper: for each 'OperatorClass' in the input list
(in order), allocate a stable name. Used by tests.
-}
generateNames :: [OperatorClass] -> [Text]
generateNames =
  reverse . fst . foldl' step ([], newStableNameSeed)
  where
    step (acc, seed) op =
      let (name, seed') = nextName op seed
      in (name : acc, seed')
