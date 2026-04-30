{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Manifest write planning: fast-append vs merge-append vs rewrite-manifests.
--
-- Iceberg's commit path has three behaviours when adding new data files:
--
-- 1. __Fast-append__: write a single new manifest containing the newly
--    added files and prepend it to the existing manifest list. The cheapest
--    option. Used by @AppendFiles.fastAppend()@ in Java.
--
-- 2. __Merge-append__: when there are many small existing manifests, group
--    runs of small manifests with the new one and rewrite each group as a
--    single larger manifest, leaving the rest untouched. Trades commit
--    latency for healthier manifest list growth. This is the default
--    @AppendFiles.append()@ behaviour and is controlled by the
--    @commit.manifest.target-size-bytes@ and
--    @commit.manifest.min-count-to-merge@ table properties.
--
-- 3. __Rewrite-manifests__: explicit @RewriteManifests@ operation. Bins
--    manifests by partition spec / size and rewrites the bins, producing a
--    new manifest list whose entries reference only the consolidated
--    manifests. Useful for compacting metadata after many small commits.
--
-- This module is pure: it produces a 'CommitPlan' describing what the
-- caller should write (one or more new 'WriteManifestTask' values, each
-- listing the entries to put in a new manifest) and what 'ManifestFile'
-- entries should appear in the new manifest list. Callers feed the
-- 'WriteManifestTask' values to 'Iceberg.Write.writeManifestEntries' (or
-- their own writer), assemble the manifest list, and call
-- 'Iceberg.Update.appendFiles' with the resulting path.
--
-- See @org.apache.iceberg.MergingSnapshotProducer@ in the upstream Java
-- implementation for the reference algorithm.
module Iceberg.ManifestMerge
  ( -- * Planning
    MergePolicy(..)
  , defaultMergePolicy
  , mergePolicyFromProperties
  , CommitPlan(..)
  , WriteManifestTask(..)
  , planFastAppend
  , planAppend
  , planRewriteManifests
    -- * Bin packing
  , Bin(..)
  , binPackBySize
  ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Vector (Vector)

import Iceberg.Types

-- ============================================================
-- Configuration
-- ============================================================

-- | Mirrors the four Iceberg table properties that govern manifest merging
-- behaviour. Default values match the Java reference implementation.
data MergePolicy = MergePolicy
  { mpMergeEnabled    :: !Bool   -- ^ @commit.manifest-merge.enabled@ (default 'True')
  , mpTargetSizeBytes :: !Int64  -- ^ @commit.manifest.target-size-bytes@ (default @8 MiB@)
  , mpMinCountToMerge :: !Int    -- ^ @commit.manifest.min-count-to-merge@ (default @100@)
  , mpMaxFilesPerManifest :: !Int -- ^ Hard cap on entries per manifest written (default @8192@)
  } deriving (Show, Eq)

defaultMergePolicy :: MergePolicy
defaultMergePolicy = MergePolicy
  { mpMergeEnabled        = True
  , mpTargetSizeBytes     = 8 * 1024 * 1024
  , mpMinCountToMerge     = 100
  , mpMaxFilesPerManifest = 8192
  }

-- | Read the merge policy from a 'TableMetadata' property map.
mergePolicyFromProperties :: Map Text Text -> MergePolicy
mergePolicyFromProperties props = MergePolicy
  { mpMergeEnabled        = lookupBool "commit.manifest-merge.enabled"
                              (mpMergeEnabled defaultMergePolicy)
  , mpTargetSizeBytes     = lookupInt64 "commit.manifest.target-size-bytes"
                              (mpTargetSizeBytes defaultMergePolicy)
  , mpMinCountToMerge     = lookupInt "commit.manifest.min-count-to-merge"
                              (mpMinCountToMerge defaultMergePolicy)
  , mpMaxFilesPerManifest = lookupInt "commit.manifest.max-files-per-manifest"
                              (mpMaxFilesPerManifest defaultMergePolicy)
  }
  where
    lookupBool k def = case Map.lookup k props of
      Just "true"  -> True
      Just "false" -> False
      _            -> def
    lookupInt :: Text -> Int -> Int
    lookupInt k def = case Map.lookup k props of
      Just t -> case TR.signed TR.decimal t of
        Right (n, rest) | rest == "" -> n
        _ -> def
      Nothing -> def
    lookupInt64 k def = case Map.lookup k props of
      Just t -> case TR.signed TR.decimal t of
        Right (n, rest) | rest == "" -> n
        _ -> def
      Nothing -> def

-- ============================================================
-- Plan output
-- ============================================================

-- | Description of one manifest the caller must write before commit.
data WriteManifestTask = WriteManifestTask
  { wmtPath              :: !Text
    -- ^ Where the caller will write this manifest. The same path is used
    -- when constructing the 'ManifestFile' that references it.
  , wmtSpecId            :: !Int
  , wmtContent           :: !ManifestContent
  , wmtAddedSnapshotId   :: !Int64
  , wmtSequenceNumber    :: !Int64
  , wmtMinSequenceNumber :: !Int64
  , wmtEntries           :: !(Vector ManifestEntry)
    -- ^ The manifest entries that should land in the new file. Their
    -- sequence numbers are filled in already (callers can pass these
    -- straight to @writeManifestEntries@).
  } deriving (Show, Eq)

-- | The result of planning a commit. Callers must:
--
-- 1. Write one Avro container per 'cpNewManifests' task using
--    'Iceberg.Write.writeManifestEntries'.
-- 2. Replace each task's path entry in 'cpNewManifestList' with a
--    'ManifestFile' that records the bytes actually written (length,
--    counts, partition summaries). The other entries in 'cpNewManifestList'
--    can be passed through as-is - they reference manifests that already
--    exist on disk.
-- 3. Write the manifest list with 'Iceberg.Write.writeManifestList'.
-- 4. Call 'Iceberg.Update.appendFiles' with the manifest list path.
data CommitPlan = CommitPlan
  { cpNewManifests    :: !(Vector WriteManifestTask)
    -- ^ Manifests that must be (re)written, in order.
  , cpNewManifestList :: !(Vector ManifestFile)
    -- ^ The full set of manifest-list entries to be written for this commit,
    -- including both newly produced manifests (paths matching
    -- 'cpNewManifests') and pre-existing manifests pulled forward from the
    -- previous commit.
  } deriving (Show, Eq)

-- ============================================================
-- Fast append
-- ============================================================

-- | Fast-append: write one new manifest containing all of the new entries,
-- and prepend it to the existing manifest list. No existing manifests are
-- rewritten.
planFastAppend
  :: Int64                -- ^ Sequence number this commit will receive.
  -> Int64                -- ^ Snapshot id this commit will receive.
  -> Int                  -- ^ Partition spec id under which the new files were written.
  -> Text                 -- ^ Path the caller will use for the new manifest.
  -> Vector ManifestEntry -- ^ New manifest entries (typically all status=Added).
  -> Vector ManifestFile  -- ^ Existing manifest list entries (carried through).
  -> CommitPlan
planFastAppend seqNum snap specId newPath newEntries existing =
  let entries' = V.map (assignSequenceNumbers seqNum snap) newEntries
      task = newManifestTask seqNum snap specId DataContent newPath entries'
   in CommitPlan
        { cpNewManifests    = V.singleton task
        , cpNewManifestList = V.cons (manifestFilePlaceholder task) existing
        }

-- ============================================================
-- Merge append (the default 'AppendFiles' behaviour)
-- ============================================================

-- | Merge-append. When there are many small existing manifests, this
-- groups runs of them with the new entries and rewrites each group as a
-- single larger manifest. Existing manifests too large to merge or
-- belonging to a different partition spec are left untouched.
planAppend
  :: MergePolicy
  -> (Text -> Int -> Text)   -- ^ Generator for new manifest paths: @gen seqNum index -> path@.
  -> (ManifestFile -> Either String (Vector ManifestEntry))
       -- ^ Reader that materialises an existing manifest's entries.
  -> Int64                -- ^ Sequence number this commit will receive.
  -> Int64                -- ^ Snapshot id this commit will receive.
  -> Int                  -- ^ Partition spec id under which the new files were written.
  -> Vector ManifestEntry -- ^ New manifest entries.
  -> Vector ManifestFile  -- ^ Existing manifest list entries.
  -> Either String CommitPlan
planAppend policy genPath readEntries seqNum snap specId newEntries existing
  | not (mpMergeEnabled policy) =
      let path = genPath "append" 0
       in Right (planFastAppend seqNum snap specId path newEntries existing)
  | otherwise = do
      let (sameSpec, otherSpec) = V.partition
            (\mf -> mfPartitionSpecId mf == specId
                    && mfContent mf == DataContent)
            existing
          (mergeable, untouched) = V.partition (isMergeable policy) sameSpec
      if V.length mergeable < mpMinCountToMerge policy
        then
          let path = genPath "append" 0
           in Right (planFastAppend seqNum snap specId path newEntries
                       (untouched <> otherSpec))
        else do
          -- Read existing entries from the mergeable manifests, append the
          -- new entries, bin-pack by target size, and emit one task per bin.
          existingEntries <- mapM readEntries (V.toList mergeable)
          let allEntries = V.concat (newEntries : existingEntries)
              entries' = V.map (assignSequenceNumbers seqNum snap) allEntries
              bins = binPackBySize policy entries'
              tasks = V.imap (mkBinTask genPath seqNum snap specId) bins
          Right CommitPlan
            { cpNewManifests = tasks
            , cpNewManifestList =
                V.map manifestFilePlaceholder tasks
                <> untouched <> otherSpec
            }

isMergeable :: MergePolicy -> ManifestFile -> Bool
isMergeable policy mf = mfLength mf < mpTargetSizeBytes policy

mkBinTask
  :: (Text -> Int -> Text) -> Int64 -> Int64 -> Int
  -> Int -> Bin -> WriteManifestTask
mkBinTask genPath seqNum snap specId i bin =
  newManifestTask seqNum snap specId DataContent
    (genPath "merged" i) (binEntries bin)

-- ============================================================
-- Rewrite manifests
-- ============================================================

-- | Rewrite a set of existing manifests into a smaller set, bin-packed
-- against 'mpTargetSizeBytes'. The caller is responsible for choosing
-- which manifests to rewrite (typically all manifests for one partition
-- spec). New entries can also be supplied to coalesce them with the
-- rewritten manifests.
planRewriteManifests
  :: MergePolicy
  -> (Text -> Int -> Text)
  -> (ManifestFile -> Either String (Vector ManifestEntry))
  -> Int64 -- ^ Sequence number for this commit.
  -> Int64 -- ^ Snapshot id for this commit.
  -> Int   -- ^ Partition spec id.
  -> Vector ManifestFile -- ^ Manifests to rewrite.
  -> Vector ManifestFile -- ^ Manifests to keep untouched.
  -> Either String CommitPlan
planRewriteManifests policy genPath readEntries seqNum snap specId toRewrite keep = do
  rewritten <- mapM readEntries (V.toList toRewrite)
  let allEntries = V.concat rewritten
      bins = binPackBySize policy allEntries
      tasks = V.imap (mkBinTask genPath seqNum snap specId) bins
  Right CommitPlan
    { cpNewManifests    = tasks
    , cpNewManifestList = V.map manifestFilePlaceholder tasks <> keep
    }

-- ============================================================
-- Bin packing
-- ============================================================

-- | A bin is a sub-vector of manifest entries whose estimated serialised
-- size is at most 'mpTargetSizeBytes'.
data Bin = Bin
  { binEntries :: !(Vector ManifestEntry)
  , binSize    :: !Int64
  } deriving (Show, Eq)

-- | Greedy first-fit decreasing bin packing. We can't measure exact bytes
-- without serialising; instead we use each entry's @meFileSizeBytes@ as a
-- proxy for the contribution it makes to the manifest's serialised size.
-- This matches Java's @BinPacking@ heuristic closely enough that the
-- resulting manifest sizes track the target value.
binPackBySize :: MergePolicy -> Vector ManifestEntry -> Vector Bin
binPackBySize policy entries =
  let !maxFiles = mpMaxFilesPerManifest policy
      !target   = mpTargetSizeBytes policy
      go acc remaining
        | V.null remaining = V.reverse (V.fromList acc)
        | otherwise =
            let (chunk, rest) = takeBin maxFiles target remaining
                bin = Bin
                  { binEntries = chunk
                  , binSize    = V.foldl' (\s e -> s + entrySize e) 0 chunk
                  }
             in go (bin : acc) rest
   in go [] entries

takeBin :: Int -> Int64 -> Vector ManifestEntry -> (Vector ManifestEntry, Vector ManifestEntry)
takeBin maxFiles target xs = go 0 0
  where
    go !i !sz
      | i >= V.length xs = (xs, V.empty)
      | i >= maxFiles    = (V.take i xs, V.drop i xs)
      | i > 0 && sz + entrySize (V.unsafeIndex xs i) > target =
          (V.take i xs, V.drop i xs)
      | otherwise = go (i + 1) (sz + entrySize (V.unsafeIndex xs i))

entrySize :: ManifestEntry -> Int64
entrySize me = max 1 (meFileSizeBytes me `div` 1024)
  -- The serialised manifest entry is a small constant plus per-column
  -- statistics. We approximate by 1/1024 of the data file size, which is
  -- the same factor Java's @SizeBasedBinPacker@ uses as the default.

-- ============================================================
-- Helpers
-- ============================================================

-- | Apply the spec-defined inheritance rules to entries that are about to
-- be written: @ADDED@ entries inherit the new commit's snapshot id and
-- sequence number; entries with explicit values keep them.
assignSequenceNumbers :: Int64 -> Int64 -> ManifestEntry -> ManifestEntry
assignSequenceNumbers seqNum snap me = me
  { meSnapshotId = case meStatus me of
      Added -> Just snap
      _     -> meSnapshotId me
  , meSequenceNumber = case meStatus me of
      Added -> Just seqNum
      _     -> meSequenceNumber me
  , meFileSequenceNumber = case meStatus me of
      Added -> Just seqNum
      _     -> meFileSequenceNumber me
  }

-- | Build a 'WriteManifestTask' for a freshly-produced manifest.
newManifestTask
  :: Int64 -> Int64 -> Int -> ManifestContent -> Text
  -> Vector ManifestEntry -> WriteManifestTask
newManifestTask seqNum snap specId content path entries = WriteManifestTask
  { wmtPath              = path
  , wmtSpecId            = specId
  , wmtContent           = content
  , wmtAddedSnapshotId   = snap
  , wmtSequenceNumber    = seqNum
  , wmtMinSequenceNumber = minSeq
  , wmtEntries           = entries
  }
  where
    !minSeq = V.foldl' minOpt seqNum entries
    minOpt acc me = case meSequenceNumber me of
      Just s  -> min acc s
      Nothing -> acc

-- | Convert a write task into a placeholder 'ManifestFile' suitable for
-- inclusion in 'cpNewManifestList'. The 'mfLength' field is set to @0@; the
-- caller must overwrite it with the actual byte length once the manifest
-- has been written. Other counts are aggregated from the entry list.
manifestFilePlaceholder :: WriteManifestTask -> ManifestFile
manifestFilePlaceholder t =
  let entries = wmtEntries t
      added    = V.length (V.filter (\me -> meStatus me == Added)    entries)
      existing = V.length (V.filter (\me -> meStatus me == Existing) entries)
      deleted  = V.length (V.filter (\me -> meStatus me == Deleted)  entries)
      sumRows status =
        V.foldl' (\acc me -> if meStatus me == status
                              then acc + meRecordCount me else acc) 0 entries
   in ManifestFile
        { mfPath                   = wmtPath t
        , mfLength                 = 0
        , mfPartitionSpecId        = wmtSpecId t
        , mfContent                = wmtContent t
        , mfSequenceNumber         = wmtSequenceNumber t
        , mfMinSequenceNumber      = wmtMinSequenceNumber t
        , mfAddedSnapshotId        = wmtAddedSnapshotId t
        , mfAddedDataFilesCount    = Just added
        , mfExistingDataFilesCount = Just existing
        , mfDeletedDataFilesCount  = Just deleted
        , mfAddedRowsCount         = Just (sumRows Added)
        , mfExistingRowsCount      = Just (sumRows Existing)
        , mfDeletedRowsCount       = Just (sumRows Deleted)
        , mfPartitions             = V.empty
        , mfKeyMetadata            = Nothing
        , mfFirstRowId             = Nothing
        }
