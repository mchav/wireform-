{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Pure 'TableMetadata' update operations: snapshot creation, branch and
-- tag management, schema evolution, and partition spec management.
--
-- These functions return a new 'TableMetadata' value; they do not perform
-- any I/O. The caller is responsible for atomically swapping the metadata
-- pointer at the catalog. This matches the semantics of the Java
-- @TableOperations.commit@ contract.
module Iceberg.Update
  ( -- * Snapshot creation
    AppendFiles(..)
  , appendFiles
  , OverwriteFiles(..)
  , overwriteFiles
  , RowDelta(..)
  , rowDelta
  , addSnapshot
    -- * Branch / tag management
  , createBranch
  , createTag
  , removeRef
  , fastForwardBranch
  , setCurrentSnapshot
    -- * Schema / partition / sort updates
  , addSchema
  , addPartitionSpec
  , addSortOrder
  , setCurrentSchema
    -- * Metadata log
  , recordMetadataLogEntry
  ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import Iceberg.Snapshot (snapshotById)
import Iceberg.Types

-- ============================================================
-- Append / overwrite
-- ============================================================

-- | Description of an append operation.
data AppendFiles = AppendFiles
  { apfNewManifestList :: !Text
    -- ^ Location of the manifest list file just written for this snapshot.
  , apfTimestampMs     :: !Int64
  , apfSummary         :: !(Map.Map Text Text)
  , apfSchemaId        :: !(Maybe Int)
  } deriving (Show, Eq)

-- | Description of an overwrite operation.
data OverwriteFiles = OverwriteFiles
  { ovfNewManifestList :: !Text
  , ovfTimestampMs     :: !Int64
  , ovfSummary         :: !(Map.Map Text Text)
  , ovfSchemaId        :: !(Maybe Int)
  } deriving (Show, Eq)

-- | Description of a row-delta operation (mixed appends + delete files).
data RowDelta = RowDelta
  { rdNewManifestList :: !Text
  , rdTimestampMs     :: !Int64
  , rdSummary         :: !(Map.Map Text Text)
  , rdSchemaId        :: !(Maybe Int)
  } deriving (Show, Eq)

-- | Append snapshot. Increments the table's sequence number and pushes the
-- new snapshot onto the @snapshots@ list, snapshot log, and the @main@
-- branch ref.
appendFiles :: TableMetadata -> AppendFiles -> TableMetadata
appendFiles tm AppendFiles{..} = addSnapshot tm
  apfTimestampMs apfNewManifestList apfSummary apfSchemaId "append"

overwriteFiles :: TableMetadata -> OverwriteFiles -> TableMetadata
overwriteFiles tm OverwriteFiles{..} = addSnapshot tm
  ovfTimestampMs ovfNewManifestList ovfSummary ovfSchemaId "overwrite"

rowDelta :: TableMetadata -> RowDelta -> TableMetadata
rowDelta tm RowDelta{..} = addSnapshot tm
  rdTimestampMs rdNewManifestList rdSummary rdSchemaId "delete"

-- | Lower-level helper: record a new snapshot with the given operation summary
-- entry, pointing it at @manifestListPath@. The new snapshot is placed at
-- the head of @main@ if @main@ already references the previous snapshot
-- (otherwise the caller can use 'createBranch' to manage its own ref).
addSnapshot
  :: TableMetadata
  -> Int64           -- ^ Timestamp.
  -> Text            -- ^ Manifest list path.
  -> Map.Map Text Text   -- ^ User summary fields.
  -> Maybe Int       -- ^ Schema id used for this snapshot.
  -> Text            -- ^ Operation, written into the @operation@ summary key.
  -> TableMetadata
addSnapshot tm ts mlPath userSummary schId op =
  let parent = tmCurrentSnapshotId tm
      newSeq = tmLastSequenceNumber tm + 1
      newId  = nextSnapshotId tm
      summary = Map.insert "operation" op userSummary
      snap = Snapshot
        { snapId             = newId
        , snapParentId       = parent
        , snapSequenceNumber = newSeq
        , snapTimestampMs    = ts
        , snapManifestList   = mlPath
        , snapSummary        = summary
        , snapSchemaId       = schId
        , snapFirstRowId     = Nothing
        , snapKeyId          = Nothing
        }
      mainRef = SnapshotRef
        { srSnapshotId         = newId
        , srType               = "branch"
        , srMaxRefAgeMs        = Nothing
        , srMaxSnapshotAgeMs   = Nothing
        , srMinSnapshotsToKeep = Nothing
        }
   in tm
      { tmLastSequenceNumber = newSeq
      , tmLastUpdatedMs      = ts
      , tmCurrentSnapshotId  = Just newId
      , tmSnapshots          = V.snoc (tmSnapshots tm) snap
      , tmSnapshotLog        = V.snoc (tmSnapshotLog tm) (SnapshotLogEntry ts newId)
      , tmSnapshotRefs       = Map.insert "main" mainRef (tmSnapshotRefs tm)
      }

-- | Allocate a fresh snapshot id (largest existing + 1, starting from 1).
nextSnapshotId :: TableMetadata -> Int64
nextSnapshotId tm =
  let xs = V.map snapId (tmSnapshots tm)
   in if V.null xs then 1 else V.maximum xs + 1

-- ============================================================
-- Branch / tag management
-- ============================================================

-- | Create a new branch ref pointing at the given snapshot id.
createBranch
  :: Text            -- ^ Branch name (e.g. \"audit-2025-04\").
  -> Int64           -- ^ Snapshot id this branch starts from.
  -> Maybe Int64     -- ^ Optional max-ref-age-ms.
  -> Maybe Int64     -- ^ Optional max-snapshot-age-ms.
  -> Maybe Int       -- ^ Optional min-snapshots-to-keep.
  -> TableMetadata
  -> TableMetadata
createBranch name sid mra msa msk tm =
  let ref = SnapshotRef
        { srSnapshotId         = sid
        , srType               = "branch"
        , srMaxRefAgeMs        = mra
        , srMaxSnapshotAgeMs   = msa
        , srMinSnapshotsToKeep = fmap fromIntegral msk
        }
   in tm { tmSnapshotRefs = Map.insert name ref (tmSnapshotRefs tm) }

-- | Create a new tag ref pointing at the given snapshot id.
createTag :: Text -> Int64 -> Maybe Int64 -> TableMetadata -> TableMetadata
createTag name sid mra tm =
  let ref = SnapshotRef
        { srSnapshotId         = sid
        , srType               = "tag"
        , srMaxRefAgeMs        = mra
        , srMaxSnapshotAgeMs   = Nothing
        , srMinSnapshotsToKeep = Nothing
        }
   in tm { tmSnapshotRefs = Map.insert name ref (tmSnapshotRefs tm) }

-- | Remove a branch or tag ref by name. Removing @main@ is a no-op (the
-- spec requires @main@ to always exist on a non-empty table).
removeRef :: Text -> TableMetadata -> TableMetadata
removeRef name tm
  | name == "main" = tm
  | otherwise = tm { tmSnapshotRefs = Map.delete name (tmSnapshotRefs tm) }

-- | Move @branch@ to point to a later snapshot in its history. Fails (returns
-- the unchanged metadata) if the new snapshot is not in the existing history
-- of the branch's current target.
fastForwardBranch :: Text -> Int64 -> TableMetadata -> TableMetadata
fastForwardBranch name newSid tm = case Map.lookup name (tmSnapshotRefs tm) of
  Nothing -> tm
  Just ref ->
    let history = ancestors tm newSid
        srcId   = srSnapshotId ref
     in if elem srcId (map snapId history)
        then tm { tmSnapshotRefs = Map.insert name (ref { srSnapshotId = newSid })
                                                  (tmSnapshotRefs tm) }
        else tm

ancestors :: TableMetadata -> Int64 -> [Snapshot]
ancestors tm sid = case snapshotById tm sid of
  Nothing -> []
  Just s  -> s : maybe [] (ancestors tm) (snapParentId s)

-- | Set the current snapshot pointer (and the @main@ branch ref) to a
-- specific historical snapshot id. Used for time-travel rollbacks.
setCurrentSnapshot :: Int64 -> TableMetadata -> TableMetadata
setCurrentSnapshot sid tm = case snapshotById tm sid of
  Nothing -> tm
  Just _  ->
    let mainRef = case Map.lookup "main" (tmSnapshotRefs tm) of
          Just r  -> r { srSnapshotId = sid }
          Nothing -> SnapshotRef sid "branch" Nothing Nothing Nothing
     in tm
        { tmCurrentSnapshotId = Just sid
        , tmSnapshotRefs      = Map.insert "main" mainRef (tmSnapshotRefs tm)
        }

-- ============================================================
-- Schema / partition / sort updates
-- ============================================================

-- | Append a schema and (optionally) make it the current schema.
addSchema :: Schema -> Bool {- make current -} -> TableMetadata -> TableMetadata
addSchema schema makeCurrent tm =
  let sid  = schemaId schema
      nextCol = max (tmLastColumnId tm) (highestFieldId schema)
      withSchema = tm
        { tmSchemas      = V.snoc (tmSchemas tm) schema
        , tmLastColumnId = nextCol
        }
   in if makeCurrent
      then withSchema { tmCurrentSchemaId = sid }
      else withSchema

setCurrentSchema :: Int -> TableMetadata -> TableMetadata
setCurrentSchema sid tm = tm { tmCurrentSchemaId = sid }

addPartitionSpec :: PartitionSpec -> Bool -> TableMetadata -> TableMetadata
addPartitionSpec ps makeDefault tm =
  let withSpec = tm
        { tmPartitionSpecs  = V.snoc (tmPartitionSpecs tm) ps
        , tmLastPartitionId = max (tmLastPartitionId tm) (highestPartitionFieldId ps)
        }
   in if makeDefault
      then withSpec { tmDefaultSpecId = psSpecId ps }
      else withSpec

addSortOrder :: SortOrder -> Bool -> TableMetadata -> TableMetadata
addSortOrder so makeDefault tm =
  let withSO = tm { tmSortOrders = V.snoc (tmSortOrders tm) so }
   in if makeDefault
      then withSO { tmDefaultSortOrderId = soOrderId so }
      else withSO

highestFieldId :: Schema -> Int
highestFieldId Schema{schemaFields = fs} = V.foldl' (\acc sf -> max acc (sfId sf)) 0 fs

highestPartitionFieldId :: PartitionSpec -> Int
highestPartitionFieldId PartitionSpec{psFields = fs} =
  V.foldl' (\acc pf -> max acc (pfFieldId pf)) 0 fs

-- ============================================================
-- Metadata log
-- ============================================================

-- | Record the location of the previous metadata file before swapping in a
-- new one. Per the spec, the metadata log is a bounded ring; the table
-- property @write.metadata.previous-versions-max@ limits how many
-- entries are kept (defaulting to 100).
recordMetadataLogEntry :: Int64 -> Text -> TableMetadata -> TableMetadata
recordMetadataLogEntry ts path tm =
  let entry = MetadataLogEntry { mleTimestampMs = ts, mleMetadataFile = path }
      maxEntries = case Map.lookup "write.metadata.previous-versions-max"
                                  (tmProperties tm) of
        Just t  -> case TR.decimal t of
          Right (n, rest) | T.null rest -> n
          _                              -> defaultMaxEntries
        Nothing -> defaultMaxEntries
      log' = V.snoc (tmMetadataLog tm) entry
      trimmed
        | V.length log' > maxEntries = V.drop (V.length log' - maxEntries) log'
        | otherwise = log'
   in tm { tmMetadataLog = trimmed }
  where
    defaultMaxEntries = 100
