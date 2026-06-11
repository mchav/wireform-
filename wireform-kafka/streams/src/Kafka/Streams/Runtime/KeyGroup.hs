{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.KeyGroup
Description : Key-group routing primitive (Riffle §6, Phase 2)

Today, parallelism in 'Kafka.Streams.Runtime.WorkerPool' is
pinned to the number of /partitions/ on the source topic. That
ceiling is fine for most topologies but bites in two cases:

  * Stateful operators on a low-partition topic that need to
    scale beyond the partition count.
  * Pre-existing topics whose partition count was chosen years
    ago for a different workload.

Flink decouples parallelism from partitions by hashing each
record onto one of a /fixed/ number of \"key-groups\" and then
assigning key-groups to workers. The number of key-groups is
a deployment constant (typically @128@ or a power of two
around the maximum anticipated parallelism); the assignor
moves key-groups, not partitions.

This module defines the routing primitive. The assignor
integration lives in
'Kafka.Streams.Runtime.Assignor.assignKeyGroups' and the
runtime wiring is deferred to a follow-up.
-}
module Kafka.Streams.Runtime.KeyGroup (
  -- * Identity
  KeyGroupId (..),
  KeyGroupCount (..),

  -- * Config
  KeyGroupConfig (..),
  defaultKeyGroupConfig,

  -- * Assignment
  KeyGroupAssignment (..),
  WarmupProgress (..),
  emptyAssignment,
  assignedToKeyGroupRange,

  -- * Routing
  keyGroupOf,
  keyGroupOfHash,
  keyGroupOfBytes,
  keyGroupRangeOf,

  -- * Membership
  KeyGroupRange (..),
  inKeyGroupRange,
  rangeFromList,
  rangeToList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Hashable (Hashable, hash)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)


----------------------------------------------------------------------
-- Identity
----------------------------------------------------------------------

{- | Identifier of one key-group within the topology. Stable for
the lifetime of the deployment.
-}
newtype KeyGroupId = KeyGroupId {unKeyGroupId :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


{- | Total number of key-groups in the topology. The runtime
treats this as immutable for the lifetime of the application;
changing it requires the same kind of state-store migration
you'd run for a re-partition.
-}
newtype KeyGroupCount = KeyGroupCount {unKeyGroupCount :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

{- | Topology-wide key-group configuration. The runtime treats
'kgcTotal' as immutable for the lifetime of the application;
changing it requires a key-space migration analogous to a
topic repartition.

'kgcHash' is the function the runtime uses to hash a record's
raw key bytes to an 'Int'. The default (`xxHash64`-style; we
use 'Data.Hashable' here as a stand-in) is sufficient for
most workloads; override only when byte-identical behaviour
with a JVM partitioner is required.
-}
data KeyGroupConfig = KeyGroupConfig
  { kgcTotal :: !KeyGroupCount
  , kgcHash :: !(ByteString -> Int)
  }


{- | Default config: 128 key-groups (matches Flink's
@setDefaultMaxParallelism@), 'Data.Hashable' over the key
bytes.
-}
defaultKeyGroupConfig :: KeyGroupConfig
defaultKeyGroupConfig =
  KeyGroupConfig
    { kgcTotal = KeyGroupCount 128
    , kgcHash = hash . BS.unpack
    }


----------------------------------------------------------------------
-- Assignment
----------------------------------------------------------------------

{- | Per-instance view of which key-groups this runtime owns
right now, plus the warm-up state for key-groups it has
agreed to take over but hasn't yet finished restoring.
-}
data KeyGroupAssignment = KeyGroupAssignment
  { kgaOwned :: !(Set KeyGroupId)
  , kgaWarming :: !(Map KeyGroupId WarmupProgress)
  }
  deriving stock (Eq, Show, Generic)


{- | Progress of a warm-up sweep for one key-group. The runtime
compares 'wpReplayedOffset' against 'wpEndOffset' to decide
when the standby is caught up enough to promote.
-}
data WarmupProgress = WarmupProgress
  { wpReplayedOffset :: !Int
  , wpEndOffset :: !Int
  }
  deriving stock (Eq, Show, Generic)


{- | The empty assignment: own nothing, warm nothing. The
runtime starts here and the assignor pushes the live
assignment in as a side-effect.
-}
emptyAssignment :: KeyGroupAssignment
emptyAssignment = KeyGroupAssignment Set.empty Map.empty


{- | Project an assignment to a 'KeyGroupRange' that the routing
hot path consults via 'inKeyGroupRange'.
-}
assignedToKeyGroupRange :: KeyGroupAssignment -> KeyGroupRange
assignedToKeyGroupRange = rangeFromList . Set.toAscList . kgaOwned


----------------------------------------------------------------------
-- Routing
----------------------------------------------------------------------

{- | Hash a key onto its key-group. The hash function is the
default 'Data.Hashable' one; downstream operators that need
byte-identical behaviour with the JVM Streams' @groupBy@
partitioner should override this with their own
@keyGroupOfHash@ call.
-}
keyGroupOf
  :: Hashable k
  => KeyGroupCount
  -> k
  -> KeyGroupId
keyGroupOf (KeyGroupCount n) k =
  KeyGroupId (abs (hash k) `mod` max 1 n)


{- | Like 'keyGroupOf' but takes a pre-computed hash. Use when
the runtime has already hashed the key for partition routing
and you don't want to hash twice.
-}
keyGroupOfHash
  :: KeyGroupCount
  -> Int
  -> KeyGroupId
keyGroupOfHash (KeyGroupCount n) h =
  KeyGroupId (abs h `mod` max 1 n)


{- | Hash a raw 'ByteString' key onto a key-group using a
'KeyGroupConfig'. This is the runtime's hot-path call: the
engine has the serialised key bytes already and the config
pre-built.
-}
keyGroupOfBytes
  :: KeyGroupConfig
  -> ByteString
  -> KeyGroupId
keyGroupOfBytes cfg kb =
  let !h = kgcHash cfg kb
      KeyGroupCount n = kgcTotal cfg
  in KeyGroupId (abs h `mod` max 1 n)


{- | Convert a key-group id into the partition it routes to. The
mapping is fixed: @key-group i@ → @partition (i mod
partitionCount)@. This matches Flink's KeyGroupRangeAssignment
and ensures key-group co-location with the upstream Kafka
partition.
-}
keyGroupRangeOf
  :: KeyGroupCount
  -> Int
  -- ^ partition count
  -> KeyGroupId
  -> Int
  -- ^ partition number
keyGroupRangeOf (KeyGroupCount _) parts (KeyGroupId k) =
  k `mod` max 1 parts


----------------------------------------------------------------------
-- KeyGroupRange (a contiguous set of key-groups)
----------------------------------------------------------------------

{- | A set of key-groups owned by one worker / member. Internally
an 'IntSet' so membership checks are O(log n); the runtime
routes records through a per-worker @KeyGroupRange@ on the hot
path.
-}
newtype KeyGroupRange = KeyGroupRange {unKeyGroupRange :: IntSet}
  deriving stock (Eq, Show, Generic)


inKeyGroupRange :: KeyGroupRange -> KeyGroupId -> Bool
inKeyGroupRange (KeyGroupRange s) (KeyGroupId k) = IntSet.member k s


rangeFromList :: [KeyGroupId] -> KeyGroupRange
rangeFromList = KeyGroupRange . IntSet.fromList . map unKeyGroupId


rangeToList :: KeyGroupRange -> [KeyGroupId]
rangeToList (KeyGroupRange s) =
  map KeyGroupId (sort (IntSet.toList s))
