{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Client.RackAware
Description : KIP-881 rack-aware partition assignment for consumers

The classic 'Kafka.Streams.Runtime.Assignor' assigns
partitions purely by load. KIP-881 lets the assignor consult
each member's @client.rack@ and prefer placing partitions on
members that share a rack with the partition's leader — fewer
cross-AZ bytes in steady state.

Pure decision layer:

  * 'RackAwareInputs' bundles each member's rack id, every
    partition's preferred-replica rack list, and the desired
    target load per member.
  * 'rackAwareAssignment' computes a placement that maximises
    rack-affinity while still staying within @ceil(N/M)@ load.

The streams runtime would call this from the cooperative-sticky
assignor when @client.rack@ is set; tests for the math live in
'Streams.RackAwareSpec'.
-}
module Kafka.Client.RackAware
  ( -- * Inputs
    RackId (..)
  , PartitionRackInfo (..)
  , RackAwareInputs (..)
    -- * Decision
  , rackAwareAssignment
  , preferLocalRack
  , rackAffinityScore
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype RackId = RackId { unRackId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

data PartitionRackInfo p = PartitionRackInfo
  { priPartition       :: !p
  , priLeaderRack      :: !(Maybe RackId)
  , priReplicaRacks    :: ![RackId]
    -- ^ Sorted by replica preference (leader first).
  }
  deriving stock (Eq, Show, Generic)

data RackAwareInputs m p = RackAwareInputs
  { raiMembers     :: !(Map m (Maybe RackId))
  , raiPartitions  :: ![PartitionRackInfo p]
  , raiTargetLoad  :: !Int
    -- ^ ceil(numPartitions / numMembers) — the load cap.
  }
  deriving stock (Eq, Show, Generic)

-- | Score how much a member's rack matches a partition's
-- replica racks. 100 for "leader matches", 50 for "replica
-- matches but not leader", 0 otherwise. Higher scores come
-- first when sorting placement candidates.
rackAffinityScore
  :: Maybe RackId               -- ^ member rack
  -> PartitionRackInfo p
  -> Int
rackAffinityScore memberRack pri = case memberRack of
  Nothing -> 0
  Just mr
    | Just mr == priLeaderRack pri -> 100
    | mr `elem` priReplicaRacks pri -> 50
    | otherwise                     -> 0

-- | Place partitions on members preferring rack affinity but
-- respecting 'raiTargetLoad'. The returned 'Map' is the new
-- assignment.
rackAwareAssignment
  :: forall m p
   . (Ord m, Ord p)
  => RackAwareInputs m p
  -> Map m [p]
rackAwareAssignment RackAwareInputs{..} =
  let memberList = Map.keys raiMembers
      empty0     = Map.fromList [(m, []) | m <- memberList]
      placed     = L.foldl' place empty0 (sortByAffinity raiPartitions)
  in placed
  where
    place !acc pri =
      let -- Score every member by affinity then current load.
          loads     = [ ( -(rackAffinityScore (Map.findWithDefault Nothing m raiMembers) pri)
                        , length (Map.findWithDefault [] m acc)
                        , m
                        )
                      | m <- Map.keys raiMembers
                      ]
          -- Filter out at-cap members unless every member is at cap.
          underCap = filter (\(_, l, _) -> l < raiTargetLoad) loads
          pool     = if null underCap then loads else underCap
          (_, _, chosen) = L.minimum pool
      in Map.adjust (++ [priPartition pri]) chosen acc

    sortByAffinity =
      L.sortBy (\a b ->
        compare (negate (maxAffinity a))
                (negate (maxAffinity b)))
    maxAffinity pri =
      maximum (0 : [rackAffinityScore (Map.findWithDefault Nothing m raiMembers) pri
                    | m <- Map.keys raiMembers])

-- | Convenience: among the partitions a member is /already/
-- assigned, return only those whose leader-rack matches the
-- member's. Useful for the consumer's preferred-replica fetch
-- (KIP-392).
preferLocalRack
  :: Eq p
  => Maybe RackId
  -> [PartitionRackInfo p]
  -> [p]
preferLocalRack memberRack pris =
  [ priPartition pri
  | pri <- pris
  , case memberRack of
      Just mr -> Just mr == priLeaderRack pri
      Nothing -> False
  ]
