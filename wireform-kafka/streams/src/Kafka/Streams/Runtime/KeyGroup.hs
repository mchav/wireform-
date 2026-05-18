{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.KeyGroup
-- Description : Key-group routing primitive (Riffle §6, Phase 2)
--
-- Today, parallelism in 'Kafka.Streams.Runtime.WorkerPool' is
-- pinned to the number of /partitions/ on the source topic. That
-- ceiling is fine for most topologies but bites in two cases:
--
--   * Stateful operators on a low-partition topic that need to
--     scale beyond the partition count.
--   * Pre-existing topics whose partition count was chosen years
--     ago for a different workload.
--
-- Flink decouples parallelism from partitions by hashing each
-- record onto one of a /fixed/ number of \"key-groups\" and then
-- assigning key-groups to workers. The number of key-groups is
-- a deployment constant (typically @128@ or a power of two
-- around the maximum anticipated parallelism); the assignor
-- moves key-groups, not partitions.
--
-- This module defines the routing primitive. The assignor
-- integration lives in
-- 'Kafka.Streams.Runtime.Assignor.assignKeyGroups' and the
-- runtime wiring is deferred to a follow-up.
module Kafka.Streams.Runtime.KeyGroup
  ( -- * Identity
    KeyGroupId (..)
  , KeyGroupCount (..)
    -- * Routing
  , keyGroupOf
  , keyGroupOfHash
  , keyGroupRangeOf
    -- * Membership
  , KeyGroupRange (..)
  , inKeyGroupRange
  , rangeFromList
  , rangeToList
  ) where

import Data.Hashable (Hashable, hash)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (sort)
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- Identity
----------------------------------------------------------------------

-- | Identifier of one key-group within the topology. Stable for
-- the lifetime of the deployment.
newtype KeyGroupId = KeyGroupId { unKeyGroupId :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

-- | Total number of key-groups in the topology. The runtime
-- treats this as immutable for the lifetime of the application;
-- changing it requires the same kind of state-store migration
-- you'd run for a re-partition.
newtype KeyGroupCount = KeyGroupCount { unKeyGroupCount :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- Routing
----------------------------------------------------------------------

-- | Hash a key onto its key-group. The hash function is the
-- default 'Data.Hashable' one; downstream operators that need
-- byte-identical behaviour with the JVM Streams' @groupBy@
-- partitioner should override this with their own
-- @keyGroupOfHash@ call.
keyGroupOf
  :: Hashable k
  => KeyGroupCount
  -> k
  -> KeyGroupId
keyGroupOf (KeyGroupCount n) k =
  KeyGroupId (abs (hash k) `mod` max 1 n)

-- | Like 'keyGroupOf' but takes a pre-computed hash. Use when
-- the runtime has already hashed the key for partition routing
-- and you don't want to hash twice.
keyGroupOfHash
  :: KeyGroupCount
  -> Int
  -> KeyGroupId
keyGroupOfHash (KeyGroupCount n) h =
  KeyGroupId (abs h `mod` max 1 n)

-- | Convert a key-group id into the partition it routes to. The
-- mapping is fixed: @key-group i@ → @partition (i mod
-- partitionCount)@. This matches Flink's KeyGroupRangeAssignment
-- and ensures key-group co-location with the upstream Kafka
-- partition.
keyGroupRangeOf
  :: KeyGroupCount
  -> Int                     -- ^ partition count
  -> KeyGroupId
  -> Int                     -- ^ partition number
keyGroupRangeOf (KeyGroupCount _) parts (KeyGroupId k) =
  k `mod` max 1 parts

----------------------------------------------------------------------
-- KeyGroupRange (a contiguous set of key-groups)
----------------------------------------------------------------------

-- | A set of key-groups owned by one worker / member. Internally
-- an 'IntSet' so membership checks are O(log n); the runtime
-- routes records through a per-worker @KeyGroupRange@ on the hot
-- path.
newtype KeyGroupRange = KeyGroupRange { unKeyGroupRange :: IntSet }
  deriving stock (Eq, Show, Generic)

inKeyGroupRange :: KeyGroupRange -> KeyGroupId -> Bool
inKeyGroupRange (KeyGroupRange s) (KeyGroupId k) = IntSet.member k s

rangeFromList :: [KeyGroupId] -> KeyGroupRange
rangeFromList = KeyGroupRange . IntSet.fromList . map unKeyGroupId

rangeToList :: KeyGroupRange -> [KeyGroupId]
rangeToList (KeyGroupRange s) =
  map KeyGroupId (sort (IntSet.toList s))
