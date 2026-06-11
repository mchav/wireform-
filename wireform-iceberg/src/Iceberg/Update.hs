{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

{- | Pure 'TableMetadata' update operations: snapshot creation, branch and
tag management, schema evolution, and partition spec management.

These functions return a new 'TableMetadata' value; they do not perform
any I/O. The caller is responsible for atomically swapping the metadata
pointer at the catalog. This matches the semantics of the Java
@TableOperations.commit@ contract.
-}
module Iceberg.Update (
  -- * Snapshot creation
  AppendFiles (..),
  appendFiles,
  OverwriteFiles (..),
  overwriteFiles,
  RowDelta (..),
  rowDelta,
  addSnapshot,

  -- * Snapshot summary helpers
  autoSummary,
  SnapshotStats (..),
  emptySnapshotStats,
  statsFromManifestEntry,

  -- * Branch / tag management
  createBranch,
  createTag,
  removeRef,
  fastForwardBranch,
  setCurrentSnapshot,
  rollbackToSnapshot,

  -- * Schema / partition / sort updates
  addSchema,
  addPartitionSpec,
  addSortOrder,
  setCurrentSchema,

  -- * Metadata log
  recordMetadataLogEntry,
) where

import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Iceberg.Snapshot (isAncestor, snapshotById)
import Iceberg.Types


-- ============================================================
-- Snapshot summary
-- ============================================================
--
-- The Iceberg spec defines well-known summary keys (added-data-files,
-- added-records, total-data-files, total-records, ...). Java/PyIceberg
-- compute these automatically when committing. 'SnapshotStats' is a small
-- accumulator that callers populate from the manifest entries they are
-- about to commit; 'autoSummary' converts a stats record into the
-- canonical text-keyed map.

data SnapshotStats = SnapshotStats
  { ssAddedDataFiles :: !Int
  , ssAddedRecords :: !Int64
  , ssAddedFilesSize :: !Int64
  , ssRemovedDataFiles :: !Int
  , ssRemovedRecords :: !Int64
  , ssRemovedFilesSize :: !Int64
  , ssAddedDeleteFiles :: !Int
  , ssAddedPositionDeletes :: !Int64
  , ssAddedEqualityDeletes :: !Int64
  , ssTotalDataFiles :: !Int
  , ssTotalRecords :: !Int64
  , ssTotalFilesSize :: !Int64
  , ssTotalDeleteFiles :: !Int
  , ssTotalPositionDeletes :: !Int64
  , ssTotalEqualityDeletes :: !Int64
  }
  deriving (Show, Eq)


emptySnapshotStats :: SnapshotStats
emptySnapshotStats = SnapshotStats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0


{- | Summarise a single manifest entry's contribution to the new-files side
of a snapshot. Intended to be 'mappend'ed into a 'SnapshotStats' fold.
-}
statsFromManifestEntry :: ManifestEntry -> SnapshotStats
statsFromManifestEntry me =
  let isDelete = case meDataFile me of
        Just df -> dataFileContent df /= DataContent
        Nothing -> False
      eq = case meDataFile me of
        Just df
          | dataFileContent df == DeletesContent ->
              if V.null (dataFileEqualityIds df) then False else True
        _ -> False
      pos = isDelete && not eq
  in emptySnapshotStats
       { ssAddedDataFiles = if isDelete then 0 else 1
       , ssAddedRecords = if isDelete then 0 else meRecordCount me
       , ssAddedFilesSize = meFileSizeBytes me
       , ssAddedDeleteFiles = if isDelete then 1 else 0
       , ssAddedPositionDeletes = if pos then meRecordCount me else 0
       , ssAddedEqualityDeletes = if eq then meRecordCount me else 0
       }


-- | Canonical Iceberg summary keys. Zero-valued counts are omitted.
autoSummary :: SnapshotStats -> Map.Map Text Text
autoSummary s =
  Map.fromList $
    textPair "added-data-files" (ssAddedDataFiles s)
      ++ textPair64 "added-records" (ssAddedRecords s)
      ++ textPair64 "added-files-size" (ssAddedFilesSize s)
      ++ textPair "removed-data-files" (ssRemovedDataFiles s)
      ++ textPair64 "removed-records" (ssRemovedRecords s)
      ++ textPair64 "removed-files-size" (ssRemovedFilesSize s)
      ++ textPair "added-delete-files" (ssAddedDeleteFiles s)
      ++ textPair64 "added-position-deletes" (ssAddedPositionDeletes s)
      ++ textPair64 "added-equality-deletes" (ssAddedEqualityDeletes s)
      ++ textPair "total-data-files" (ssTotalDataFiles s)
      ++ textPair64 "total-records" (ssTotalRecords s)
      ++ textPair64 "total-files-size" (ssTotalFilesSize s)
      ++ textPair "total-delete-files" (ssTotalDeleteFiles s)
      ++ textPair64 "total-position-deletes" (ssTotalPositionDeletes s)
      ++ textPair64 "total-equality-deletes" (ssTotalEqualityDeletes s)


textPair :: Text -> Int -> [(Text, Text)]
textPair k v
  | v == 0 = []
  | otherwise = [(k, T.pack (show v))]


textPair64 :: Text -> Int64 -> [(Text, Text)]
textPair64 k v
  | v == 0 = []
  | otherwise = [(k, T.pack (show v))]


-- ============================================================
-- Append / overwrite
-- ============================================================

{- | Description of an append operation. Callers can either supply the full
summary they want recorded ('apfSummary') or supply a 'SnapshotStats'
record via 'apfStats' which 'appendFiles' will turn into the canonical
summary keys (added-data-files, added-records, ...).
-}
data AppendFiles = AppendFiles
  { apfNewManifestList :: !Text
  -- ^ Location of the manifest list file just written for this snapshot.
  , apfTimestampMs :: !Int64
  , apfSummary :: !(Map.Map Text Text)
  {- ^ Free-form summary entries supplied by the caller (e.g. dictionary
  of engine name, write hints, etc).
  -}
  , apfStats :: !(Maybe SnapshotStats)
  {- ^ When set, populates the canonical Iceberg summary keys via
  'autoSummary' before merging with 'apfSummary'.
  -}
  , apfSchemaId :: !(Maybe Int)
  }
  deriving (Show, Eq)


-- | Description of an overwrite operation.
data OverwriteFiles = OverwriteFiles
  { ovfNewManifestList :: !Text
  , ovfTimestampMs :: !Int64
  , ovfSummary :: !(Map.Map Text Text)
  , ovfStats :: !(Maybe SnapshotStats)
  , ovfSchemaId :: !(Maybe Int)
  }
  deriving (Show, Eq)


-- | Description of a row-delta operation (mixed appends + delete files).
data RowDelta = RowDelta
  { rdNewManifestList :: !Text
  , rdTimestampMs :: !Int64
  , rdSummary :: !(Map.Map Text Text)
  , rdStats :: !(Maybe SnapshotStats)
  , rdSchemaId :: !(Maybe Int)
  }
  deriving (Show, Eq)


{- | Append snapshot. Increments the table's sequence number and pushes the
new snapshot onto the @snapshots@ list, snapshot log, and the @main@
branch ref. When 'apfStats' is provided, the canonical Iceberg summary
keys are added first so that user-supplied entries in 'apfSummary' can
override them.
-}
appendFiles :: TableMetadata -> AppendFiles -> TableMetadata
appendFiles tm AppendFiles {..} =
  addSnapshot
    tm
    apfTimestampMs
    apfNewManifestList
    (mergeSummary apfStats apfSummary)
    apfSchemaId
    "append"


overwriteFiles :: TableMetadata -> OverwriteFiles -> TableMetadata
overwriteFiles tm OverwriteFiles {..} =
  addSnapshot
    tm
    ovfTimestampMs
    ovfNewManifestList
    (mergeSummary ovfStats ovfSummary)
    ovfSchemaId
    "overwrite"


rowDelta :: TableMetadata -> RowDelta -> TableMetadata
rowDelta tm RowDelta {..} =
  addSnapshot
    tm
    rdTimestampMs
    rdNewManifestList
    (mergeSummary rdStats rdSummary)
    rdSchemaId
    "delete"


{- | Combine an optional 'SnapshotStats' with caller-supplied summary
entries. Auto-derived keys come first; explicit user entries take
precedence on conflicts.
-}
mergeSummary :: Maybe SnapshotStats -> Map.Map Text Text -> Map.Map Text Text
mergeSummary Nothing user = user
mergeSummary (Just s) user = Map.union user (autoSummary s)


{- | Lower-level helper: record a new snapshot with the given operation summary
entry, pointing it at @manifestListPath@. The new snapshot is placed at
the head of @main@ if @main@ already references the previous snapshot
(otherwise the caller can use 'createBranch' to manage its own ref).
-}
addSnapshot
  :: TableMetadata
  -> Int64
  -- ^ Timestamp.
  -> Text
  -- ^ Manifest list path.
  -> Map.Map Text Text
  -- ^ User summary fields.
  -> Maybe Int
  -- ^ Schema id used for this snapshot.
  -> Text
  -- ^ Operation, written into the @operation@ summary key.
  -> TableMetadata
addSnapshot tm ts mlPath userSummary schId op =
  let parent = tmCurrentSnapshotId tm
      newSeq = tmLastSequenceNumber tm + 1
      newId = nextSnapshotId tm
      summary = Map.insert "operation" op userSummary
      snap =
        Snapshot
          { snapId = newId
          , snapParentId = parent
          , snapSequenceNumber = newSeq
          , snapTimestampMs = ts
          , snapManifestList = mlPath
          , snapSummary = summary
          , snapSchemaId = schId
          , snapFirstRowId = Nothing
          , snapKeyId = Nothing
          }
      mainRef =
        SnapshotRef
          { srSnapshotId = newId
          , srType = "branch"
          , srMaxRefAgeMs = Nothing
          , srMaxSnapshotAgeMs = Nothing
          , srMinSnapshotsToKeep = Nothing
          }
  in tm
       { tmLastSequenceNumber = newSeq
       , tmLastUpdatedMs = ts
       , tmCurrentSnapshotId = Just newId
       , tmSnapshots = V.snoc (tmSnapshots tm) snap
       , tmSnapshotLog = V.snoc (tmSnapshotLog tm) (SnapshotLogEntry ts newId)
       , tmSnapshotRefs = Map.insert "main" mainRef (tmSnapshotRefs tm)
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
  :: Text
  -- ^ Branch name (e.g. \"audit-2025-04\").
  -> Int64
  -- ^ Snapshot id this branch starts from.
  -> Maybe Int64
  -- ^ Optional max-ref-age-ms.
  -> Maybe Int64
  -- ^ Optional max-snapshot-age-ms.
  -> Maybe Int
  -- ^ Optional min-snapshots-to-keep.
  -> TableMetadata
  -> TableMetadata
createBranch name sid mra msa msk tm =
  let ref =
        SnapshotRef
          { srSnapshotId = sid
          , srType = "branch"
          , srMaxRefAgeMs = mra
          , srMaxSnapshotAgeMs = msa
          , srMinSnapshotsToKeep = fmap fromIntegral msk
          }
  in tm {tmSnapshotRefs = Map.insert name ref (tmSnapshotRefs tm)}


-- | Create a new tag ref pointing at the given snapshot id.
createTag :: Text -> Int64 -> Maybe Int64 -> TableMetadata -> TableMetadata
createTag name sid mra tm =
  let ref =
        SnapshotRef
          { srSnapshotId = sid
          , srType = "tag"
          , srMaxRefAgeMs = mra
          , srMaxSnapshotAgeMs = Nothing
          , srMinSnapshotsToKeep = Nothing
          }
  in tm {tmSnapshotRefs = Map.insert name ref (tmSnapshotRefs tm)}


{- | Remove a branch or tag ref by name. Removing @main@ is a no-op (the
spec requires @main@ to always exist on a non-empty table).
-}
removeRef :: Text -> TableMetadata -> TableMetadata
removeRef name tm
  | name == "main" = tm
  | otherwise = tm {tmSnapshotRefs = Map.delete name (tmSnapshotRefs tm)}


{- | Move @branch@ to point to a later snapshot in its history. Fails (returns
the unchanged metadata) if the new snapshot is not in the existing history
of the branch's current target.
-}
fastForwardBranch :: Text -> Int64 -> TableMetadata -> TableMetadata
fastForwardBranch name newSid tm = case Map.lookup name (tmSnapshotRefs tm) of
  Nothing -> tm
  Just ref ->
    let history = ancestors tm newSid
        srcId = srSnapshotId ref
    in if elem srcId (map snapId history)
         then
           tm
             { tmSnapshotRefs =
                 Map.insert
                   name
                   (ref {srSnapshotId = newSid})
                   (tmSnapshotRefs tm)
             }
         else tm


ancestors :: TableMetadata -> Int64 -> [Snapshot]
ancestors tm sid = case snapshotById tm sid of
  Nothing -> []
  Just s -> s : maybe [] (ancestors tm) (snapParentId s)


{- | Set the current snapshot pointer (and the @main@ branch ref) to a
specific historical snapshot id. Used for time-travel rollbacks.
-}
setCurrentSnapshot :: Int64 -> TableMetadata -> TableMetadata
setCurrentSnapshot sid tm = case snapshotById tm sid of
  Nothing -> tm
  Just _ ->
    let mainRef = case Map.lookup "main" (tmSnapshotRefs tm) of
          Just r -> r {srSnapshotId = sid}
          Nothing -> SnapshotRef sid "branch" Nothing Nothing Nothing
    in tm
         { tmCurrentSnapshotId = Just sid
         , tmSnapshotRefs = Map.insert "main" mainRef (tmSnapshotRefs tm)
         }


{- | Iceberg's "rollback" semantics: only succeeds if @sid@ is an ancestor
of the current snapshot. Mirrors @ManageSnapshots#rollbackTo@.

Returns @Left@ explaining the violation (snapshot missing or not an
ancestor) without mutating @tm@; otherwise returns the new metadata.
-}
rollbackToSnapshot :: Int64 -> TableMetadata -> Either String TableMetadata
rollbackToSnapshot sid tm = case tmCurrentSnapshotId tm of
  Nothing -> Left "rollbackToSnapshot: table has no current snapshot"
  Just curSid
    | sid == curSid -> Right tm
    | not (isAncestor tm sid curSid) ->
        Left $
          "rollbackToSnapshot: snapshot "
            ++ show sid
            ++ " is not an ancestor of the current snapshot "
            ++ show curSid
    | otherwise -> case snapshotById tm sid of
        Nothing -> Left $ "rollbackToSnapshot: no such snapshot " ++ show sid
        Just _ -> Right (setCurrentSnapshot sid tm)


-- ============================================================
-- Schema / partition / sort updates
-- ============================================================

-- | Append a schema and (optionally) make it the current schema.
addSchema :: Schema -> Bool {- make current -} -> TableMetadata -> TableMetadata
addSchema schema makeCurrent tm =
  let sid = schemaId schema
      nextCol = max (tmLastColumnId tm) (highestFieldId schema)
      withSchema =
        tm
          { tmSchemas = V.snoc (tmSchemas tm) schema
          , tmLastColumnId = nextCol
          }
  in if makeCurrent
       then withSchema {tmCurrentSchemaId = sid}
       else withSchema


setCurrentSchema :: Int -> TableMetadata -> TableMetadata
setCurrentSchema sid tm = tm {tmCurrentSchemaId = sid}


addPartitionSpec :: PartitionSpec -> Bool -> TableMetadata -> TableMetadata
addPartitionSpec ps makeDefault tm =
  let withSpec =
        tm
          { tmPartitionSpecs = V.snoc (tmPartitionSpecs tm) ps
          , tmLastPartitionId = max (tmLastPartitionId tm) (highestPartitionFieldId ps)
          }
  in if makeDefault
       then withSpec {tmDefaultSpecId = psSpecId ps}
       else withSpec


addSortOrder :: SortOrder -> Bool -> TableMetadata -> TableMetadata
addSortOrder so makeDefault tm =
  let withSO = tm {tmSortOrders = V.snoc (tmSortOrders tm) so}
  in if makeDefault
       then withSO {tmDefaultSortOrderId = soOrderId so}
       else withSO


highestFieldId :: Schema -> Int
highestFieldId Schema {schemaFields = fs} = V.foldl' (\acc sf -> max acc (sfId sf)) 0 fs


highestPartitionFieldId :: PartitionSpec -> Int
highestPartitionFieldId PartitionSpec {psFields = fs} =
  V.foldl' (\acc pf -> max acc (pfFieldId pf)) 0 fs


-- ============================================================
-- Metadata log
-- ============================================================

{- | Record the location of the previous metadata file before swapping in a
new one. Per the spec, the metadata log is a bounded ring; the table
property @write.metadata.previous-versions-max@ limits how many
entries are kept (defaulting to 100).
-}
recordMetadataLogEntry :: Int64 -> Text -> TableMetadata -> TableMetadata
recordMetadataLogEntry ts path tm =
  let entry = MetadataLogEntry {mleTimestampMs = ts, mleMetadataFile = path}
      maxEntries = case Map.lookup
        "write.metadata.previous-versions-max"
        (tmProperties tm) of
        Just t -> case TR.decimal t of
          Right (n, rest) | T.null rest -> n
          _ -> defaultMaxEntries
        Nothing -> defaultMaxEntries
      log' = V.snoc (tmMetadataLog tm) entry
      trimmed
        | V.length log' > maxEntries = V.drop (V.length log' - maxEntries) log'
        | otherwise = log'
  in tm {tmMetadataLog = trimmed}
  where
    defaultMaxEntries = 100
