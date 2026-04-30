-- | Snapshot and partition spec operations on Iceberg table metadata.
--
-- These are pure lookup functions over the 'TableMetadata' structure.
-- Snapshot traversal ('snapshotParentChain') walks the parent-ID chain
-- to reconstruct history. Partition pruning ('evaluatePartitionFilter')
-- is a simplified predicate-based filter over manifest entries.
module Iceberg.Snapshot
  ( -- * Snapshot operations
    currentSnapshot
  , snapshotById
  , snapshotByRef
  , snapshotParentChain
  , snapshotManifestListPath
  , snapshotAsOfTime
    -- * Snapshot history
  , ancestorsOf
  , currentAncestors
  , snapshotsBetween
  , isAncestor
    -- * Partition spec operations
  , currentPartitionSpec
  , partitionSpecById
  , evaluatePartitionFilter
    -- * Sequence number filtering
  , filterBySequenceNumber
  , applicableDeletes
  ) where

import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Vector as V

import qualified Avro.Value as AV
import Iceberg.Types

-- | The snapshot matching 'tmCurrentSnapshotId', or 'Nothing' if there
-- is no current snapshot or the ID doesn't match any snapshot in the list.
currentSnapshot :: TableMetadata -> Maybe Snapshot
currentSnapshot tm = do
  sid <- tmCurrentSnapshotId tm
  snapshotById tm sid

-- | Find a snapshot by its ID.
snapshotById :: TableMetadata -> Int64 -> Maybe Snapshot
snapshotById tm sid = V.find (\s -> snapId s == sid) (tmSnapshots tm)

-- | Resolve a named branch or tag to its target 'Snapshot'.
snapshotByRef :: TableMetadata -> Text -> Maybe Snapshot
snapshotByRef tm name =
  case Map.lookup name (tmSnapshotRefs tm) of
    Just r  -> snapshotById tm (srSnapshotId r)
    Nothing -> Nothing

-- | The most recent snapshot whose @timestamp_ms@ does not exceed the
-- supplied target; @Nothing@ if there is no such snapshot.
snapshotAsOfTime :: TableMetadata -> Int64 -> Maybe Snapshot
snapshotAsOfTime tm target =
  let candidates = V.filter (\s -> snapTimestampMs s <= target) (tmSnapshots tm)
   in if V.null candidates
        then Nothing
        else Just $ V.maximumBy
               (\a b -> compare (snapTimestampMs a) (snapTimestampMs b))
               candidates

-- | Walk 'snapParentId' backwards to produce the snapshot history chain
-- (excluding the given snapshot itself). Stops when no parent is found
-- or the parent ID doesn't match any snapshot.
snapshotParentChain :: TableMetadata -> Snapshot -> [Snapshot]
snapshotParentChain tm = go
  where
    go snap = case snapParentId snap >>= snapshotById tm of
      Nothing     -> []
      Just parent -> parent : go parent

-- | The given snapshot followed by its ancestors, oldest-last. Mirrors
-- Java's @SnapshotUtil.ancestorsOf@.
ancestorsOf :: TableMetadata -> Int64 -> [Snapshot]
ancestorsOf tm sid = case snapshotById tm sid of
  Nothing -> []
  Just s  -> s : snapshotParentChain tm s

-- | The current snapshot's ancestor chain, oldest-last.
currentAncestors :: TableMetadata -> [Snapshot]
currentAncestors tm = case currentSnapshot tm of
  Nothing -> []
  Just s  -> ancestorsOf tm (snapId s)

-- | All snapshots between two ids on the same ancestry, exclusive at @from@,
-- inclusive at @to@. Returns 'Nothing' if @to@ is not a descendant of @from@
-- (or if either id does not exist).
snapshotsBetween :: TableMetadata -> Int64 -> Int64 -> Maybe [Snapshot]
snapshotsBetween tm fromId toId = case snapshotById tm toId of
  Nothing -> Nothing
  Just toS ->
    let chain = toS : snapshotParentChain tm toS
        (before, atFrom) = break (\s -> snapId s == fromId) chain
     in if null atFrom then Nothing else Just (reverse before)

-- | True if @ancestor@ appears in @descendant@'s parent chain (or is the
-- snapshot itself).
isAncestor :: TableMetadata -> Int64 {- ancestor -} -> Int64 {- descendant -} -> Bool
isAncestor tm ancestor descendant = any (\s -> snapId s == ancestor) (ancestorsOf tm descendant)

-- | Extract the manifest-list path from a snapshot. Returns 'Nothing'
-- only if the path is empty (shouldn't happen for valid snapshots).
snapshotManifestListPath :: Snapshot -> Maybe Text
snapshotManifestListPath snap =
  let p = snapManifestList snap
  in if T.null p then Nothing else Just p

-- | The partition spec matching 'tmDefaultSpecId'.
currentPartitionSpec :: TableMetadata -> Maybe PartitionSpec
currentPartitionSpec tm = partitionSpecById tm (tmDefaultSpecId tm)

-- | Look up a partition spec by its ID.
partitionSpecById :: TableMetadata -> Int -> Maybe PartitionSpec
partitionSpecById tm specId =
  V.find (\ps -> psSpecId ps == specId) (tmPartitionSpecs tm)

-- | Simplified partition pruning: given a predicate on
-- @(source_field_name, partition_value_as_text)@, keep only manifest
-- entries where the predicate returns 'True' for every partition field.
--
-- The source field name is looked up from the 'Schema' via
-- 'pfSourceId'. Partition values are converted to 'Text' for
-- string, int, long, and bool Avro values; other types yield 'Nothing'.
evaluatePartitionFilter
  :: PartitionSpec
  -> Schema
  -> (Text -> Maybe Text -> Bool)
  -> V.Vector ManifestEntry
  -> V.Vector ManifestEntry
evaluatePartitionFilter spec schema predicate =
  V.filter matchesFilter
  where
    partFlds = psFields spec

    matchesFilter entry =
      let partVals = mePartition entry
          n = min (V.length partFlds) (V.length partVals)
      in checkAll 0 n partVals

    checkAll :: Int -> Int -> V.Vector (Maybe AV.Value) -> Bool
    checkAll !i n vals
      | i >= n = True
      | otherwise =
          let pf = V.unsafeIndex partFlds i
              mv = V.unsafeIndex vals i
              fieldName = lookupFieldName (pfSourceId pf)
              valText = mv >>= avroValueToText
          in predicate fieldName valText && checkAll (i + 1) n vals

    lookupFieldName srcId =
      case V.find (\sf -> sfId sf == srcId) (schemaFields schema) of
        Just sf -> sfName sf
        Nothing -> T.empty

-- | Keep only manifest entries whose data sequence number is at most @maxSeq@.
-- Entries without a sequence number are always included.
filterBySequenceNumber :: Int64 -> V.Vector ManifestEntry -> V.Vector ManifestEntry
filterBySequenceNumber maxSeq = V.filter $ \me ->
  case meSequenceNumber me of
    Just n  -> n <= maxSeq
    Nothing -> True

-- | Delete manifests applicable to a snapshot: content is 'DeletesContent' and
-- sequence number does not exceed the snapshot's sequence number.
applicableDeletes :: Snapshot -> V.Vector ManifestFile -> V.Vector ManifestFile
applicableDeletes snap = V.filter $ \mf ->
  mfContent mf == DeletesContent && mfSequenceNumber mf <= snapSequenceNumber snap

avroValueToText :: AV.Value -> Maybe Text
avroValueToText = \case
  AV.String t -> Just t
  AV.Int n    -> Just (int32ToText n)
  AV.Long n   -> Just (int64ToText n)
  AV.Bool b   -> Just (if b then "true" else "false")
  _           -> Nothing

int32ToText :: Int32 -> Text
int32ToText = TL.toStrict . toLazyText . decimal

int64ToText :: Int64 -> Text
int64ToText = TL.toStrict . toLazyText . decimal
