{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Hudi timeline reader.
--
-- Hudi tables sit on top of Parquet files plus a /timeline/
-- of instant files under @.hoodie/@. Each file is named
-- @<instantTime>.<action>[.<state>]@ where:
--
--   * @action@ is @commit@, @deltacommit@, @clean@,
--     @compaction@, @rollback@, @savepoint@, @restore@, or
--     @replacecommit@.
--   * @state@ is @requested@, @inflight@, or @completed@.
--     A trailing state of @completed@ is /omitted/ on disk
--     (a bare @<instantTime>.<action>@ filename is the
--     completed instant).
--
-- The completed instants describe which Parquet files
-- (Copy-on-Write tables) or Avro/Parquet log files
-- (Merge-on-Read tables) form the table's current view.
--
-- This module exposes:
--
--   * 'parseInstantFileName' — file-name → typed instant.
--   * 'sortInstants' / 'completedInstants' — list helpers.
--   * 'parseCommitJson' — typed @HoodieCommitMetadata@ (the JSON
--     representation that completed @commit@ /
--     @deltacommit@ instants used in Hudi 0.x and earlier;
--     pending instants and post-1.0 completed instants are
--     Avro and need a separate Avro decoder, which is a
--     follow-up).
--   * 'TableState' — fold of completed commit metadata into
--     active file slices keyed by partition.
--
-- Out of scope (still): Avro instant payloads (Hudi 1.x+),
-- log-file block decoding (MoR readers), record-level merge
-- key resolution, the metadata table.
module Hudi.Timeline
  ( -- * File-name parsing
    Action (..)
  , State (..)
  , Instant (..)
  , parseInstantFileName
  , sortInstants
  , completedInstants
    -- * Commit metadata (JSON representation)
  , HoodieCommitMetadata (..)
  , HoodieWriteStat (..)
  , parseCommitJson
    -- * Replace-commit metadata
  , HoodieReplaceCommitMetadata (..)
  , parseReplaceCommitJson
    -- * Clean metadata
  , HoodieCleanMetadata (..)
  , HoodieCleanPartitionMetadata (..)
  , parseCleanJson
    -- * File-slice fold
  , FileSlice (..)
  , TableState (..)
  , emptyTableState
  , applyCommit
  , applyReplaceCommit
  , applyClean
  , tableStateFromCommits
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , (.:?)
  , decode
  , withObject
  )
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================
-- File-name parsing
-- ============================================================

data Action
  = Commit
  | DeltaCommit
  | Clean
  | Compaction
  | Rollback
  | Savepoint
  | Restore
  | ReplaceCommit
  | UnknownAction !Text
  deriving (Show, Eq)

data State
  = Requested
  | Inflight
  | Completed
  deriving (Show, Eq, Enum, Bounded)

-- | One entry in the @.hoodie/@ timeline.
data Instant = Instant
  { instantTime   :: !Text
    -- ^ Sortable timestamp string (typically
    -- @yyyyMMddHHmmssSSS@), used as the instant's id.
  , instantAction :: !Action
  , instantState  :: !State
  } deriving (Show, Eq)

-- | Parse a @.hoodie/@ instant file name into its components.
-- Accepts both the explicit @<time>.<action>.<state>@ form
-- (used by @requested@ / @inflight@) and the implicit
-- @<time>.<action>@ form (used by @completed@ instants in
-- recent Hudi versions).
--
-- @
-- parseInstantFileName \"20240106120000000.commit\"
--   == Just (Instant \"20240106120000000\" Commit Completed)
-- parseInstantFileName \"20240106120000000.commit.requested\"
--   == Just (Instant \"20240106120000000\" Commit Requested)
-- @
parseInstantFileName :: Text -> Maybe Instant
parseInstantFileName t = case T.splitOn "." t of
  [time, action] ->
    Just $ Instant time (parseAction action) Completed
  [time, action, state] -> do
    st <- parseState state
    Just $ Instant time (parseAction action) st
  _ -> Nothing

parseAction :: Text -> Action
parseAction = \case
  "commit"        -> Commit
  "deltacommit"   -> DeltaCommit
  "clean"         -> Clean
  "compaction"    -> Compaction
  "rollback"      -> Rollback
  "savepoint"     -> Savepoint
  "restore"       -> Restore
  "replacecommit" -> ReplaceCommit
  other           -> UnknownAction other

parseState :: Text -> Maybe State
parseState = \case
  "requested" -> Just Requested
  "inflight"  -> Just Inflight
  "completed" -> Just Completed
  _           -> Nothing

-- | Sort instants chronologically by 'instantTime'. Hudi names
-- instants with sortable @yyyyMMddHHmmssSSS@ strings, so a
-- lexicographic compare on the textual id is the same as the
-- temporal order.
sortInstants :: [Instant] -> [Instant]
sortInstants = Map.elems . Map.fromList . map (\i -> ((instantTime i, stateRank (instantState i)), i))
  where
    -- For a given timestamp we want @requested@ < @inflight@ < @completed@
    -- to match Hudi's own ordering. Map.fromList collapses duplicates,
    -- which is fine because each (time, state) is unique on disk.
    stateRank Requested = (0 :: Int)
    stateRank Inflight  = 1
    stateRank Completed = 2

-- | Filter to just the 'Completed' instants; usually paired
-- with 'sortInstants' before walking the commit metadata.
completedInstants :: [Instant] -> [Instant]
completedInstants = filter ((== Completed) . instantState)

-- ============================================================
-- Commit metadata (JSON; Hudi 0.x format)
-- ============================================================

-- | Hudi @HoodieCommitMetadata@ in its JSON form (Hudi 0.x
-- and Hudi 1.x backward-compat). Fields are kept in sync with
-- @hudi-common/src/main/avro/HoodieCommitMetadata.avsc@; only
-- the fields most readers actually consult are typed; the
-- rest are kept as raw 'Value's in 'hcmExtra' for forward
-- compat.
data HoodieCommitMetadata = HoodieCommitMetadata
  { hcmPartitionToWriteStats :: !(Map.Map Text [HoodieWriteStat])
  , hcmCompacted             :: !(Maybe Bool)
  , hcmExtraMetadata         :: !(HM.HashMap Text Value)
  , hcmOperationType         :: !(Maybe Text)
  , hcmTotalCreateTime       :: !(Maybe Int64)
  , hcmTotalUpsertTime       :: !(Maybe Int64)
  , hcmTotalScanTime         :: !(Maybe Int64)
  , hcmExtra                 :: !(HM.HashMap Text Value)
    -- ^ Forward-compat: any additional top-level fields.
  } deriving (Show, Eq)

-- | One @HoodieWriteStat@ row. We carry the fields modern
-- planners need (file id, path, partition, base/log file
-- pointers, record counts) and surface the rest through
-- 'hwsExtra' for forward compat. Optional fields default to
-- 'Nothing' when absent.
data HoodieWriteStat = HoodieWriteStat
  { hwsFileId           :: !(Maybe Text)
  , hwsPath             :: !(Maybe Text)
  , hwsPrevCommit       :: !(Maybe Text)
  , hwsPartitionPath    :: !(Maybe Text)
  , hwsNumWrites        :: !(Maybe Int64)
  , hwsNumDeletes       :: !(Maybe Int64)
  , hwsNumUpdateWrites  :: !(Maybe Int64)
  , hwsNumInserts       :: !(Maybe Int64)
  , hwsTotalWriteBytes  :: !(Maybe Int64)
  , hwsTotalWriteErrors :: !(Maybe Int64)
  , hwsFileSizeInBytes  :: !(Maybe Int64)
  , hwsBaseFile         :: !(Maybe Text)
  , hwsLogFiles         :: ![Text]
  , hwsTotalLogRecords  :: !(Maybe Int64)
  , hwsTotalLogFiles    :: !(Maybe Int64)
  , hwsTotalLogBlocks   :: !(Maybe Int64)
  , hwsExtra            :: !(HM.HashMap Text Value)
  } deriving (Show, Eq)

instance FromJSON HoodieWriteStat where
  parseJSON = withObject "HoodieWriteStat" $ \o -> do
    fileId    <- o .:? "fileId"
    path      <- o .:? "path"
    prev      <- o .:? "prevCommit"
    part      <- o .:? "partitionPath"
    nw        <- o .:? "numWrites"
    nd        <- o .:? "numDeletes"
    nu        <- o .:? "numUpdateWrites"
    ni        <- o .:? "numInserts"
    twb       <- o .:? "totalWriteBytes"
    twe       <- o .:? "totalWriteErrors"
    fsz       <- o .:? "fileSizeInBytes"
    base      <- o .:? "baseFile"
    logFiles  <- fromMaybe [] <$> o .:? "logFiles"
    tlr       <- o .:? "totalLogRecords"
    tlf       <- o .:? "totalLogFiles"
    tlb       <- o .:? "totalLogBlocks"
    pure HoodieWriteStat
      { hwsFileId            = fileId
      , hwsPath              = path
      , hwsPrevCommit        = prev
      , hwsPartitionPath     = part
      , hwsNumWrites         = nw
      , hwsNumDeletes        = nd
      , hwsNumUpdateWrites   = nu
      , hwsNumInserts        = ni
      , hwsTotalWriteBytes   = twb
      , hwsTotalWriteErrors  = twe
      , hwsFileSizeInBytes   = fsz
      , hwsBaseFile          = base
      , hwsLogFiles          = logFiles
      , hwsTotalLogRecords   = tlr
      , hwsTotalLogFiles     = tlf
      , hwsTotalLogBlocks    = tlb
      , hwsExtra             = HM.empty
      }

instance FromJSON HoodieCommitMetadata where
  parseJSON = withObject "HoodieCommitMetadata" $ \o -> do
    partition <- fromMaybe Map.empty <$> o .:? "partitionToWriteStats"
    compacted <- o .:? "compacted"
    extraMeta <- fromMaybe HM.empty <$> o .:? "extraMetadata"
    opType    <- o .:? "operationType"
    tct       <- o .:? "totalCreateTime"
    tut       <- o .:? "totalUpsertTime"
    tst       <- o .:? "totalScanTime"
    pure HoodieCommitMetadata
      { hcmPartitionToWriteStats = partition
      , hcmCompacted             = compacted
      , hcmExtraMetadata         = extraMeta
      , hcmOperationType         = opType
      , hcmTotalCreateTime       = tct
      , hcmTotalUpsertTime       = tut
      , hcmTotalScanTime         = tst
      , hcmExtra                 = HM.empty
      }

-- | Decode a JSON-encoded @HoodieCommitMetadata@ payload (e.g.
-- the contents of @.hoodie/<instant>.commit@). Returns 'Left'
-- on parse failure.
parseCommitJson :: BL.ByteString -> Either String HoodieCommitMetadata
parseCommitJson bs = case decode bs of
  Nothing -> Left "Hudi.Timeline.parseCommitJson: malformed JSON"
  Just v  -> parseEither parseJSON v

-- ============================================================
-- File-slice fold
-- ============================================================

-- | One file slice (a base file plus its log files) attached
-- to a @fileId@ inside a partition. Hudi readers stream these
-- to deliver a snapshot.
data FileSlice = FileSlice
  { fsFileId       :: !Text
  , fsBaseFile     :: !(Maybe Text)
    -- ^ Path of the most recent base (Parquet) file for this
    -- file id, if any.
  , fsLogFiles     :: ![Text]
    -- ^ Active log files (typically Avro / Parquet, MoR only).
    -- Newest first.
  , fsLatestCommit :: !Text
    -- ^ The completion timestamp of the most recent instant
    -- that touched this file id.
  } deriving (Show, Eq)

-- | Aggregate snapshot of the table built up from completed
-- commit metadata. 'tsPartitions' is keyed by partition path
-- (or the empty string for unpartitioned tables).
data TableState = TableState
  { tsPartitions  :: !(Map.Map Text (Map.Map Text FileSlice))
    -- ^ partition path → fileId → slice.
  , tsLatestInstant :: !(Maybe Text)
  } deriving (Show, Eq)

emptyTableState :: TableState
emptyTableState = TableState
  { tsPartitions    = Map.empty
  , tsLatestInstant = Nothing
  }

-- | Fold a single completed commit / deltacommit metadata
-- payload into the running table state. The instant timestamp
-- (the @<time>@ portion of the file name) must be passed in
-- because the JSON payload itself doesn't always carry it.
applyCommit :: Text -> HoodieCommitMetadata -> TableState -> TableState
applyCommit ts hcm !st0 =
  let st1 = Map.foldlWithKey' overPartition st0 (hcmPartitionToWriteStats hcm)
      overPartition !s part stats =
        let !partMap = fromMaybe Map.empty (Map.lookup part (tsPartitions s))
            !merged  = foldl (mergeStat ts) partMap stats
         in s { tsPartitions = Map.insert part merged (tsPartitions s) }
   in st1 { tsLatestInstant = Just ts }

mergeStat :: Text -> Map.Map Text FileSlice -> HoodieWriteStat -> Map.Map Text FileSlice
mergeStat ts !acc hws = case hwsFileId hws of
  Nothing  -> acc
  Just fid ->
    let prev = Map.lookup fid acc
        newBase = case hwsBaseFile hws of
          Just b  -> Just b
          Nothing -> case hwsPath hws of
            -- For Copy-on-Write commits Hudi puts the rewritten
            -- Parquet on @path@; treat that as the base file when
            -- there's no explicit @baseFile@ (matches what Hudi's
            -- own CoW reader does).
            Just p | hasParquetExt p -> Just p
            _                        -> Nothing
        prevLogs = maybe [] fsLogFiles prev
        combinedLogs = hwsLogFiles hws ++ prevLogs
        slice = FileSlice
          { fsFileId       = fid
          , fsBaseFile     = case newBase of
              Just _  -> newBase
              Nothing -> prev >>= fsBaseFile
          , fsLogFiles     = dedupKeepFirst combinedLogs
          , fsLatestCommit = ts
          }
     in Map.insert fid slice acc

hasParquetExt :: Text -> Bool
hasParquetExt p = ".parquet" `T.isSuffixOf` p

-- | Drop duplicates while preserving the order of first
-- appearance. We avoid going through @Set@ here since the
-- expected list lengths are tiny (handful of log files per
-- file id).
dedupKeepFirst :: Eq a => [a] -> [a]
dedupKeepFirst = go []
  where
    go _    []     = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise     = x : go (x:seen) xs

-- | Replay a chronologically-ordered list of completed
-- @commit@ / @deltacommit@ payloads into a 'TableState'.
-- Callers are responsible for sorting and parsing the JSON
-- payloads before invocation.
tableStateFromCommits :: [(Text, HoodieCommitMetadata)] -> TableState
tableStateFromCommits = foldl (\s (ts, hcm) -> applyCommit ts hcm s) emptyTableState

-- ============================================================
-- Replace-commit metadata (replacecommit instants)
-- ============================================================
--
-- @replacecommit@ instants are used by INSERT_OVERWRITE,
-- CLUSTERING, and DELETE_PARTITION operations. They carry
-- regular HoodieCommitMetadata content /plus/ a
-- @partitionToReplaceFileIds@ map saying which existing
-- @fileId@s the new ones supersede. Without consuming this
-- map a 'TableState' fold over a table that's been clustered
-- shows duplicated file slices (the old ones plus the
-- replacements).

-- | A @replacecommit@ instant's metadata. Wraps a regular
-- 'HoodieCommitMetadata' and adds the per-partition list of
-- file ids being replaced.
data HoodieReplaceCommitMetadata = HoodieReplaceCommitMetadata
  { hrcmCommit                   :: !HoodieCommitMetadata
  , hrcmPartitionToReplaceFileIds :: !(Map.Map Text [Text])
    -- ^ partition path → file ids whose existing slices this
    -- replacecommit invalidates.
  } deriving (Show, Eq)

instance FromJSON HoodieReplaceCommitMetadata where
  parseJSON v = do
    base <- parseJSON v
    case v of
      Object o -> do
        rep <- fromMaybe Map.empty <$> o .:? "partitionToReplaceFileIds"
        pure HoodieReplaceCommitMetadata
          { hrcmCommit                   = base
          , hrcmPartitionToReplaceFileIds = rep
          }
      _ -> pure HoodieReplaceCommitMetadata
        { hrcmCommit                   = base
        , hrcmPartitionToReplaceFileIds = Map.empty
        }

-- | Parse a @replacecommit@ instant's JSON payload.
parseReplaceCommitJson
  :: BL.ByteString
  -> Either String HoodieReplaceCommitMetadata
parseReplaceCommitJson bs = case decode bs of
  Nothing -> Left "Hudi.Timeline.parseReplaceCommitJson: malformed JSON"
  Just v  -> parseEither parseJSON v

-- | Apply a @replacecommit@ instant to the running state.
-- First drops every replaced @fileId@ from the named
-- partition's active map, then layers on the new write
-- stats via the regular 'applyCommit' path.
applyReplaceCommit
  :: Text
  -> HoodieReplaceCommitMetadata
  -> TableState
  -> TableState
applyReplaceCommit ts hrcm !st0 =
  let !purged   = Map.foldlWithKey' purgePartition st0
                    (hrcmPartitionToReplaceFileIds hrcm)
      !st       = applyCommit ts (hrcmCommit hrcm) purged
   in st
  where
    purgePartition !s part fids =
      let !pm    = fromMaybe Map.empty (Map.lookup part (tsPartitions s))
          !pm'   = foldl' (flip Map.delete) pm fids
       in s { tsPartitions = Map.insert part pm' (tsPartitions s) }

-- ============================================================
-- Clean metadata (clean instants)
-- ============================================================
--
-- @clean@ instants record housekeeping: physical files that
-- have been deleted because they were superseded by a later
-- commit and are now older than the configured retention.
-- Walking these prunes 'TableState' so the active slice map
-- doesn't carry references to files that are no longer on
-- disk.

-- | One row of a clean instant's @partitionMetadata@ entry.
-- The @successDeleteFiles@ list names the relative paths the
-- cleaner actually removed.
data HoodieCleanPartitionMetadata = HoodieCleanPartitionMetadata
  { hcpmPartitionPath       :: !Text
  , hcpmPolicy              :: !(Maybe Text)
  , hcpmDeletePathPatterns  :: ![Text]
  , hcpmSuccessDeleteFiles  :: ![Text]
  , hcpmFailedDeleteFiles   :: ![Text]
  , hcpmIsPartitionDeleted  :: !(Maybe Bool)
  } deriving (Show, Eq)

instance FromJSON HoodieCleanPartitionMetadata where
  parseJSON = withObject "HoodieCleanPartitionMetadata" $ \o -> do
    p <- o .:? "partitionPath"
    HoodieCleanPartitionMetadata
      <$> pure (fromMaybe "" p)
      <*> o .:? "policy"
      <*> (fromMaybe [] <$> o .:? "deletePathPatterns")
      <*> (fromMaybe [] <$> o .:? "successDeleteFiles")
      <*> (fromMaybe [] <$> o .:? "failedDeleteFiles")
      <*> o .:? "isPartitionDeleted"

-- | Top-level @clean@ instant payload.
data HoodieCleanMetadata = HoodieCleanMetadata
  { hcmStartCleanTime         :: !Text
  , hcmTimeTakenInMillis      :: !(Maybe Int64)
  , hcmTotalFilesDeleted      :: !(Maybe Int64)
  , hcmEarliestCommitToRetain :: !(Maybe Text)
  , hcmPartitionMetadata      :: !(Map.Map Text HoodieCleanPartitionMetadata)
  } deriving (Show, Eq)

instance FromJSON HoodieCleanMetadata where
  parseJSON = withObject "HoodieCleanMetadata" $ \o -> HoodieCleanMetadata
    <$> (fromMaybe "" <$> o .:? "startCleanTime")
    <*> o .:? "timeTakenInMillis"
    <*> o .:? "totalFilesDeleted"
    <*> o .:? "earliestCommitToRetain"
    <*> (fromMaybe Map.empty <$> o .:? "partitionMetadata")

-- | Parse a @clean@ instant's JSON payload.
parseCleanJson :: BL.ByteString -> Either String HoodieCleanMetadata
parseCleanJson bs = case decode bs of
  Nothing -> Left "Hudi.Timeline.parseCleanJson: malformed JSON"
  Just v  -> parseEither parseJSON v

-- | Apply a @clean@ instant to the running state by removing
-- any active file slice whose @baseFile@ shows up in a partition's
-- @successDeleteFiles@ list. The match is on the bare base-
-- file /name/ (not the full path) since clean payloads list
-- paths and our slices carry just the filename.
applyClean :: HoodieCleanMetadata -> TableState -> TableState
applyClean hcm !st0 =
  Map.foldlWithKey' clearPartition st0 (hcmPartitionMetadata hcm)
  where
    clearPartition !s _ pmd =
      let !deleted = hcpmSuccessDeleteFiles pmd
          !names   = map basename deleted
       in s { tsPartitions = Map.map (purgeBy names) (tsPartitions s) }

    -- Drop slices whose base-file name was deleted by the clean.
    purgeBy :: [Text] -> Map.Map Text FileSlice -> Map.Map Text FileSlice
    purgeBy names = Map.filter
      (\fs -> case fsBaseFile fs of
        Just b  -> basename b `notElem` names
        Nothing -> True)

    basename :: Text -> Text
    basename t = case T.breakOnEnd "/" t of
      ("", whole) -> whole
      (_,  rest)  -> rest

-- The 'foldl'' import is used inside 'applyReplaceCommit'.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !acc []     = acc
foldl' f !acc (x:xs) = foldl' f (f acc x) xs
