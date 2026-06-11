{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure table-maintenance operations.

- 'expireSnapshots' drops snapshots that fail the retention rules
  (max age, min snapshots to keep) and returns the snapshots that
  would be removed plus the new 'TableMetadata'.

- 'orphanFileCandidates' computes the set of file paths referenced
  by a 'TableMetadata' (manifest list locations, manifest file
  paths, statistics file paths). The caller compares it against an
  I\/O-supplied directory listing to determine which files in the
  warehouse are unreferenced.

Both follow the same shape as Java's @ExpireSnapshots@ and
@DeleteOrphanFiles@ but stay pure: I\/O (deleting object-store
entries) is the caller's responsibility.
-}
module Iceberg.Maintenance (
  -- * Snapshot expiration
  ExpiryPolicy (..),
  defaultExpiryPolicy,
  ExpirationResult (..),
  expireSnapshots,

  -- * Orphan-file detection
  referencedFilePaths,
  orphanFileCandidates,
) where

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Iceberg.Snapshot qualified as Snap
import Iceberg.Types


-- ============================================================
-- Snapshot expiration
-- ============================================================

{- | Snapshot retention policy. Mirrors the table properties Iceberg's
ExpireSnapshots reads from:

* @history.expire.max-snapshot-age-ms@
* @history.expire.min-snapshots-to-keep@
* @history.expire.max-ref-age-ms@ (per ref override; not modelled here)
-}
data ExpiryPolicy = ExpiryPolicy
  { epMaxAgeMs :: !(Maybe Int64)
  {- ^ Drop snapshots older than @nowMs - this@. Defaults to 5 days
  (432_000_000 ms) when unset, matching the Iceberg default.
  -}
  , epMinSnapshots :: !Int
  {- ^ Always keep at least this many snapshots regardless of age.
  Defaults to 1 (the current snapshot itself).
  -}
  , epRetainSnapshots :: !(Set Int64)
  {- ^ Snapshot ids that must be retained even if otherwise eligible
  for expiration (e.g. tag targets, branch HEADs the caller wants
  to keep beyond the policy default).
  -}
  }
  deriving (Show, Eq)


defaultExpiryPolicy :: ExpiryPolicy
defaultExpiryPolicy =
  ExpiryPolicy
    { epMaxAgeMs = Just (5 * 24 * 60 * 60 * 1000) -- 5 days, Iceberg default
    , epMinSnapshots = 1
    , epRetainSnapshots = Set.empty
    }


-- | Result of an expiration pass.
data ExpirationResult = ExpirationResult
  { exNewMetadata :: !TableMetadata
  , exExpiredSnapshots :: ![Snapshot]
  {- ^ Snapshots removed. Their @manifest_list@ files should be
  deleted from the object store, plus any manifest entries those
  lists pointed at that aren't reachable from any retained
  snapshot.
  -}
  }
  deriving (Show, Eq)


{- | Expire snapshots according to 'ExpiryPolicy'.

The policy ANDs with the table's @snapshot-refs@: any snapshot id
referenced by a branch or tag is retained automatically (this is
what Iceberg's @ExpireSnapshots@ does by default).
-}
expireSnapshots
  :: Int64
  -- ^ Current wall-clock time in ms.
  -> ExpiryPolicy
  -> TableMetadata
  -> ExpirationResult
expireSnapshots nowMs policy tm =
  let !snaps = V.toList (tmSnapshots tm)
      !retained = retainedIds tm policy
      !ageCutoff = case epMaxAgeMs policy of
        Just age -> nowMs - age
        Nothing -> minBound
      !sortedNewest = reverseSortOn snapTimestampMs snaps
      -- Always keep at least 'minSnapshots' newest snapshots
      !alwaysKeep =
        Set.fromList
          (map snapId (take (epMinSnapshots policy) sortedNewest))
      keepSet = retained `Set.union` alwaysKeep
      shouldKeep s =
        Set.member (snapId s) keepSet
          || snapTimestampMs s >= ageCutoff
      (kept, expired) = partitionList shouldKeep snaps
      keepIds = Set.fromList (map snapId kept)
      newRefs =
        Map.filter
          (\r -> Set.member (srSnapshotId r) keepIds)
          (tmSnapshotRefs tm)
      newCurrent = case tmCurrentSnapshotId tm of
        Just sid | Set.notMember sid keepIds -> Nothing
        x -> x
      newLog =
        V.filter
          (\e -> Set.member (sleSnapshotId e) keepIds)
          (tmSnapshotLog tm)
  in ExpirationResult
       { exNewMetadata =
           tm
             { tmSnapshots = V.fromList kept
             , tmSnapshotRefs = newRefs
             , tmCurrentSnapshotId = newCurrent
             , tmSnapshotLog = newLog
             }
       , exExpiredSnapshots = expired
       }


retainedIds :: TableMetadata -> ExpiryPolicy -> Set Int64
retainedIds tm policy =
  let refIds =
        Set.fromList
          (map srSnapshotId (Map.elems (tmSnapshotRefs tm)))
      currIds = case tmCurrentSnapshotId tm of
        Just s -> Set.singleton s
        Nothing -> Set.empty
  in foldr Set.union (epRetainSnapshots policy) [refIds, currIds]


reverseSortOn :: Ord b => (a -> b) -> [a] -> [a]
reverseSortOn f = reverse . sortOn f


partitionList :: (a -> Bool) -> [a] -> ([a], [a])
partitionList p = foldr (\x (l, r) -> if p x then (x : l, r) else (l, x : r)) ([], [])


-- ============================================================
-- Orphan file detection
-- ============================================================

{- | All file paths referenced by a 'TableMetadata' tree:
snapshot manifest lists, statistics files, partition statistics
files, and metadata-log entries. Manifest /entry/-level data file
paths are not included here (they require reading every manifest);
callers can union them in by piping the manifest entries through
their reader.
-}
referencedFilePaths :: TableMetadata -> Set Text
referencedFilePaths tm =
  Set.unions
    [ Set.fromList (V.toList (V.map snapManifestList (tmSnapshots tm)))
    , Set.fromList (V.toList (V.map mleMetadataFile (tmMetadataLog tm)))
    , Set.fromList (V.toList (V.map sfsStatPath (tmStatistics tm)))
    , Set.fromList (V.toList (V.map psfPath (tmPartitionStatistics tm)))
    ]


{- | Given a 'TableMetadata' and a directory listing, return the file
paths that exist in the listing but aren't referenced by metadata.
The caller is expected to apply the Iceberg "older than" guard
before deleting (e.g. drop only paths whose modification time is at
least @history.expire.orphan-file-min-age@ ago) - we don't have a
modification time on each path.
-}
orphanFileCandidates
  :: TableMetadata
  -> Set Text
  -- ^ All paths discovered in the warehouse.
  -> Set Text
orphanFileCandidates tm allPaths =
  allPaths `Set.difference` referencedFilePaths tm


-- We import Iceberg.Snapshot for future helpers (e.g. ancestry-checked
-- expiration). Keep the qualified import alive so the library always
-- compiles cleanly.
_unused :: TableMetadata -> Maybe Snapshot
_unused = Snap.currentSnapshot
