{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | High-level IO entry points for Apache Hudi tables.

This is the surface most callers want: hand it a path on disk
and it gives back a typed snapshot of the table at the latest
commit. The format-level decoders in "Hudi.Timeline" stay
pure-data so they can be reused by tests / fixtures.
-}
module Hudi.IO (
  -- * Discovery
  scanTimeline,
  readHoodieProperties,

  -- * High-level openers
  HudiTable (..),
  openHudiTable,
  openHudiTableAt,

  -- * Snapshot helpers
  activeFiles,
  activeBaseFilePaths,

  -- * Helpers
  tableSchemaFromCommits,

  -- * Re-exports
  module Hudi.Timeline,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hudi.Avro qualified as HAvro
import Hudi.Timeline
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))


-- ============================================================
-- Discovery
-- ============================================================

{- | Walk @<root>/.hoodie/@ and return every parseable instant
file together with its absolute path, sorted by
@(instantTime, state)@. Bogus filenames (@archived/@,
@metadata/@, @hoodie.properties@, etc.) are silently dropped.

The list preserves /every/ parseable entry — including the
requested / inflight states and any non-commit actions — so
callers can decide which subset they care about. To collapse
a single instant per timestamp use 'sortInstants' on the
'fst's instead.
-}
scanTimeline :: FilePath -> IO [(Instant, FilePath)]
scanTimeline tableRoot = do
  let hoodie = tableRoot </> ".hoodie"
  ok <- doesDirectoryExist hoodie
  if not ok
    then pure []
    else do
      entries <- listDirectory hoodie
      let parsed = mapMaybe (toEntry hoodie) entries
      pure $ sortBy (comparing keyOf) parsed
  where
    toEntry hoodie e = do
      i <- parseInstantFileName (T.pack e)
      Just (i, hoodie </> e)
    keyOf (i, _) = (instantTime i, stateRank (instantState i))
    stateRank Requested = 0 :: Int
    stateRank Inflight = 1
    stateRank Completed = 2


{- | Read @<root>/.hoodie/hoodie.properties@ into a flat
key→value map. The file is the source of truth for table
type, partition fields, the writer payload class, etc. We
treat it as a permissive Java-style properties file: lines
starting with @#@ or @!@ are comments, blank lines are
skipped, otherwise the first @=@ or @:@ separates the key
from the value. Returns 'Nothing' if the file is missing.
-}
readHoodieProperties :: FilePath -> IO (Maybe (Map.Map Text Text))
readHoodieProperties tableRoot = do
  let p = tableRoot </> ".hoodie" </> "hoodie.properties"
  ok <- doesFileExist p
  if not ok
    then pure Nothing
    else do
      bs <- BS.readFile p
      let !txt = TE.decodeUtf8 bs
          !pairs = mapMaybe parseLine (T.lines txt)
      pure (Just (Map.fromList pairs))
  where
    parseLine line =
      let stripped = T.strip line
      in if T.null stripped
           || T.head stripped `elem` ("#!" :: String)
           then Nothing
           else
             let (k, rest) = T.breakOn "=" stripped
             in if T.null rest
                  -- Try ':' as the separator (also valid per spec).
                  then case T.breakOn ":" stripped of
                    (_, "") -> Nothing
                    (k', v) -> Just (T.strip k', T.strip (T.drop 1 v))
                  else Just (T.strip k, T.strip (T.drop 1 rest))


-- ============================================================
-- Opener
-- ============================================================

-- | A Hudi table opened from disk.
data HudiTable = HudiTable
  { hutRoot :: !FilePath
  -- ^ Filesystem path to the table directory.
  , hutProperties :: !(Map.Map Text Text)
  {- ^ Decoded @hoodie.properties@; empty if the file was
  absent.
  -}
  , hutInstants :: ![Instant]
  {- ^ Every parseable instant on the timeline, sorted by
  @(instantTime, state)@.
  -}
  , hutCommits :: ![(Text, HoodieCommitMetadata)]
  {- ^ Every successfully-decoded completed commit /
  deltacommit instant, in chronological order.
  -}
  , hutParseFailures :: ![(Text, String)]
  {- ^ @(instantTime, error)@ pairs for instants whose JSON
  payload didn't decode (corrupt, Avro instead of JSON,
  etc.). Surfacing these is more useful than silently
  dropping them.
  -}
  , hutState :: !TableState
  -- ^ The folded snapshot at the latest commit.
  , hutSchemaJson :: !(Maybe Text)
  {- ^ Avro JSON schema string, lifted from the latest
  commit's @extraMetadata.schema@ field.
  -}
  }
  deriving (Show, Eq)


{- | Open a Hudi table from a directory and replay every
completed commit / deltacommit JSON into a 'TableState'.

/Out of scope:/ Avro instant payloads (Hudi 1.x+) — the
decoder is JSON-only, and any @<time>.commit@ that opens with
the Avro magic will land in 'hutParseFailures' so callers can
decide whether to treat that as fatal.
-}
openHudiTable :: FilePath -> IO (Either String HudiTable)
openHudiTable = openHudiTableImpl Nothing


{- | Open a Hudi table whose state matches the snapshot at
exactly @atInstant@. The @atInstant@ value is a Hudi
timestamp string (typically @yyyyMMddHHmmssSSS@); commits
with @instantTime > atInstant@ are skipped. Useful for
reproducing scans against a frozen point in the table
history.

Returns @Left@ if @atInstant@ is earlier than every
completed instant on the timeline (i.e. no replayable
state at that point).
-}
openHudiTableAt
  :: FilePath
  -> Text
  -- ^ instant time to open at
  -> IO (Either String HudiTable)
openHudiTableAt tableRoot ts = openHudiTableImpl (Just ts) tableRoot


{- | Internal worker shared by 'openHudiTable' and
'openHudiTableAt'. The @atInstant@ filter, when 'Just',
restricts the timeline to instants whose @instantTime <=
ts@.
-}
openHudiTableImpl :: Maybe Text -> FilePath -> IO (Either String HudiTable)
openHudiTableImpl mAt tableRoot = do
  let hoodie = tableRoot </> ".hoodie"
  ok <- doesDirectoryExist hoodie
  if not ok
    then pure (Left ("Hudi.IO: missing " ++ hoodie))
    else do
      props <- readHoodieProperties tableRoot
      timeline <- scanTimeline tableRoot
      let !instants = map fst timeline
          !candidates =
            filter
              ( \(i, _) ->
                  isCompletedReplayable i
                    && atOrBefore i
              )
              timeline
      decoded <- mapM readInstant candidates
      let !ok' = mapMaybe asOk decoded
          !fails = mapMaybe asFail decoded
          !commits = [(t, c) | (_, t, JsonCommit c) <- ok']
          !state =
            foldl
              (\s (i, t, payload) -> applyPayload i t payload s)
              emptyTableState
              [(i, t, p) | (i, t, p) <- ok']
      pure $
        Right
          HudiTable
            { hutRoot = tableRoot
            , hutProperties = case props of
                Just m -> m
                Nothing -> Map.empty
            , hutInstants = instants
            , hutCommits = commits
            , hutParseFailures = fails
            , hutState = state
            , hutSchemaJson = tableSchemaFromCommits commits
            }
  where
    -- Completed commit / deltacommit / replacecommit / clean
    -- instants are the four kinds we know how to fold. Other
    -- actions (compaction, rollback, savepoint, restore) are
    -- still surfaced via 'hutInstants' but we don't decode
    -- their payloads.
    isCompletedReplayable i =
      instantState i == Completed
        && instantAction i
          `elem` [Commit, DeltaCommit, ReplaceCommit, Clean]

    atOrBefore i = case mAt of
      Nothing -> True
      Just t -> instantTime i <= t

    asOk (_, _, _, Left _) = Nothing
    asOk (i, t, p, Right ()) = Just (i, t, p)
      where
        _ = i -- silence -Wunused-pattern-binds
    asFail (i, _, _, Left e) = Just (instantTime i, e)
    asFail _ = Nothing


{- | What the per-instant decoder produced. The four supported
shapes correspond to the four 'Action's
'isCompletedReplayable' admits.
-}
data InstantPayload
  = JsonCommit !HoodieCommitMetadata
  | ReplaceCmt !HoodieReplaceCommitMetadata
  | CleanCmt !HoodieCleanMetadata
  deriving (Show)


readInstant
  :: (Instant, FilePath)
  -> IO (Instant, Text, InstantPayload, Either String ())
readInstant (i, fp) = do
  bs <- BL.readFile fp
  let ts = instantTime i
  case instantAction i of
    ReplaceCommit -> case parseReplaceCommitJson bs of
      Right hrcm -> pure (i, ts, ReplaceCmt hrcm, Right ())
      Left e -> pure (i, ts, ReplaceCmt (defaultReplace ts), Left e)
    Clean -> case parseCleanJson bs of
      Right hcm -> pure (i, ts, CleanCmt hcm, Right ())
      Left e -> pure (i, ts, CleanCmt (defaultClean ts), Left e)
    _ -> case parseCommitJson bs of
      Right hcm -> pure (i, ts, JsonCommit hcm, Right ())
      Left jsonErr -> do
        case HAvro.decodeCommitAvro (BL.toStrict bs) of
          Right hcm ->
            pure (i, ts, JsonCommit hcm, Right ())
          Left avroErr ->
            pure
              ( i
              , ts
              , JsonCommit defaultCommit
              , Left ("json: " ++ jsonErr ++ "; avro: " ++ avroErr)
              )


defaultCommit :: HoodieCommitMetadata
defaultCommit =
  HoodieCommitMetadata
    { hcmPartitionToWriteStats = Map.empty
    , hcmCompacted = Nothing
    , hcmExtraMetadata = HM.empty
    , hcmOperationType = Nothing
    , hcmTotalCreateTime = Nothing
    , hcmTotalUpsertTime = Nothing
    , hcmTotalScanTime = Nothing
    , hcmExtra = HM.empty
    }


defaultReplace :: Text -> HoodieReplaceCommitMetadata
defaultReplace _ =
  HoodieReplaceCommitMetadata
    { hrcmCommit = defaultCommit
    , hrcmPartitionToReplaceFileIds = Map.empty
    }


defaultClean :: Text -> HoodieCleanMetadata
defaultClean _ =
  HoodieCleanMetadata
    { hcmStartCleanTime = ""
    , hcmTimeTakenInMillis = Nothing
    , hcmTotalFilesDeleted = Nothing
    , hcmEarliestCommitToRetain = Nothing
    , hcmPartitionMetadata = Map.empty
    }


-- | Fold one decoded payload onto the running state.
applyPayload
  :: Instant
  -> Text
  -> InstantPayload
  -> TableState
  -> TableState
applyPayload _ ts (JsonCommit hcm) = applyCommit ts hcm
applyPayload _ ts (ReplaceCmt hrcm) = applyReplaceCommit ts hrcm
applyPayload _ _ (CleanCmt hcl) = applyClean hcl


-- ============================================================
-- Snapshot helpers
-- ============================================================

{- | Every active 'FileSlice' across all partitions, in
(partition, fileId) order. Useful for downstream readers
that just want a flat list.
-}
activeFiles :: HudiTable -> [FileSlice]
activeFiles ht =
  [ s
  | (_, slices) <- Map.toAscList (tsPartitions (hutState ht))
  , (_, s) <- Map.toAscList slices
  ]


{- | Just the base-file paths of the active slices, in the
same (partition, fileId) order as 'activeFiles'. Skips
slices whose 'fsBaseFile' is 'Nothing' (which happens for
pure log-only file groups in MoR tables).
-}
activeBaseFilePaths :: HudiTable -> [Text]
activeBaseFilePaths ht =
  [ join part b
  | (part, slices) <- Map.toAscList (tsPartitions (hutState ht))
  , (_, slice) <- Map.toAscList slices
  , Just b <- [fsBaseFile slice]
  ]
  where
    join "" b = b
    join p b = p <> "/" <> b


-- ============================================================
-- Helpers
-- ============================================================

{- | Lift the Avro JSON schema string out of a commit list.
Hudi writers stash the table's writer-side schema in
@extraMetadata.schema@ on every commit — for read-time the
/latest/ commit's value wins. Returns 'Nothing' if no commit
has a @schema@ entry (e.g. the table was bootstrapped without
one).
-}
tableSchemaFromCommits :: [(Text, HoodieCommitMetadata)] -> Maybe Text
tableSchemaFromCommits commits =
  -- Walk newest-first; take the first @schema@ we see.
  listToMaybe $ mapMaybe pluck (reverse commits)
  where
    pluck (_, hcm) =
      case HM.lookup "schema" (hcmExtraMetadata hcm) of
        Just (Aeson.String s) -> Just s
        _ -> Nothing
