{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | High-level IO entry points for Delta Lake tables.
--
-- This is the surface most callers want: hand it a path on disk
-- and it gives back a typed snapshot of the table at the latest
-- version. The format-level decoders in "Delta.Log" stay
-- pure-data so they can be reused by tests / fixtures, the
-- protocol-rewriter, etc.
module Delta.IO
  ( -- * Discovery
    findLastCheckpoint
  , findCommits
  , listLogEntries
    -- * High-level opener
  , DeltaTable (..)
  , openDeltaTable
    -- * Re-exports
  , module Delta.Log
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName, takeBaseName, takeExtension)
import Text.Read (readMaybe)

import Delta.Log

-- ============================================================
-- Discovery
-- ============================================================

-- | Read the @_delta_log/_last_checkpoint@ pointer file, if
-- present. Returns 'Nothing' both when the file is absent (a
-- young table that hasn't been checkpointed) and when its JSON
-- is malformed (a partially-written checkpoint). Either way the
-- caller should fall back to a full log walk.
findLastCheckpoint :: FilePath -> IO (Maybe LastCheckpoint)
findLastCheckpoint tableRoot = do
  let p = tableRoot </> "_delta_log" </> "_last_checkpoint"
  ok <- doesFileExist p
  if not ok then pure Nothing
  else do
    res <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
    case res of
      Left _   -> pure Nothing
      Right bs -> pure (parseLastCheckpoint bs)

-- | List every numbered commit JSON file under
-- @<root>/_delta_log/@, in chronological order. Each tuple is
-- @(version, absolute path)@. Files whose name is not a 20-digit
-- numeric stem followed by @.json@ are skipped (the
-- @_last_checkpoint@ pointer, partial / temp files, sidecar
-- directories, …).
findCommits :: FilePath -> IO [(Word64, FilePath)]
findCommits tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok then pure []
  else do
    entries <- listDirectory logDir
    let candidates = mapMaybe (toCommit logDir) entries
    pure (sort candidates)
  where
    toCommit dir e = do
      stem <- stripJson e
      v    <- readMaybe stem :: Maybe Word64
      Just (v, dir </> e)
    stripJson e
      | takeExtension e == ".json"
      , length (takeBaseName e) == 20  -- the canonical 20-digit zero-pad
      = Just (takeBaseName e)
      | otherwise = Nothing

-- | List every entry in @<root>/_delta_log/@ along with its
-- classification: @"commit"@, @"checkpoint"@, or
-- @"unknown"@. Useful for diagnostics.
listLogEntries :: FilePath -> IO [(FilePath, String)]
listLogEntries tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok then pure []
  else do
    entries <- listDirectory logDir
    pure $ map (\e -> (e, classify e)) (sort entries)
  where
    classify e
      | takeFileName e == "_last_checkpoint" = "last_checkpoint"
      | takeExtension e == ".json"           = "commit"
      | ".checkpoint.parquet" `endsWith` e   = "checkpoint"
      | otherwise                            = "unknown"
    endsWith suffix s =
      length suffix <= length s &&
      drop (length s - length suffix) s == suffix

-- ============================================================
-- Opener
-- ============================================================

-- | A Delta Lake table opened from disk.
data DeltaTable = DeltaTable
  { dtRoot               :: !FilePath
    -- ^ Filesystem path to the table directory.
  , dtLastCheckpoint     :: !(Maybe LastCheckpoint)
    -- ^ The @_last_checkpoint@ pointer, if present.
  , dtCheckpointAvailable :: !(Maybe Word64)
    -- ^ Version of the most-recent checkpoint Parquet file
    -- that's actually on disk. May differ from
    -- 'lcVersion' (e.g. when the pointer file was deleted) and
    -- equal 'Nothing' even when 'dtLastCheckpoint' is 'Just'
    -- (the parquet file was vacuumed but the pointer wasn't
    -- updated).
  , dtCommits            :: ![(Word64, FilePath)]
    -- ^ Every numbered @NNNN.json@ commit file found under
    -- @_delta_log/@, in chronological order. The replay walks
    -- /every/ commit; checkpoint-aware fast-replay is a
    -- follow-up.
  , dtSnapshot           :: !TableSnapshot
    -- ^ Folded snapshot at the latest version.
  , dtVersion            :: !(Maybe Word64)
    -- ^ Highest commit version observed, or 'Nothing' for an
    -- empty log.
  } deriving (Show, Eq)

-- | Open a Delta table from a directory and replay every JSON
-- commit into a 'TableSnapshot'.
--
-- /Future work:/ when a @*.checkpoint.parquet@ file is present
-- the cheaper path is to seed the snapshot from the checkpoint
-- and only replay later commits. The discovery surface is in
-- place ('dtCheckpointAvailable', 'dtLastCheckpoint') but the
-- checkpoint Parquet decoder in "Delta.Checkpoint" can only
-- read it after the per-leaf path-aware Parquet reading
-- support is wired in. The decoder itself is structurally
-- correct against the checkpoint schema; it stalls on the
-- 'parquetFileArrowSchema' projection which flattens
-- duplicate struct-leaf names ('add.path' and 'remove.path'
-- both map to a top-level @path@ field). For now we walk
-- every JSON commit, which is O(N) commits but works on
-- every Delta table the test suite produces.
openDeltaTable :: FilePath -> IO (Either String DeltaTable)
openDeltaTable tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure (Left ("Delta.IO: missing " ++ logDir))
    else do
      lc           <- findLastCheckpoint tableRoot
      ckptOnDisk   <- findCheckpointParquet tableRoot
      commits      <- findCommits tableRoot
      acts         <- concat <$> mapM readActions commits
      let !snap = snapshotFromActions acts
          !ver  = case commits of
            []  -> Nothing
            xs  -> Just (fst (last xs))
      pure $ Right DeltaTable
        { dtRoot               = tableRoot
        , dtLastCheckpoint     = lc
        , dtCheckpointAvailable = ckptOnDisk
        , dtCommits            = commits
        , dtSnapshot           = snap
        , dtVersion            = ver
        }

readActions :: (Word64, FilePath) -> IO [DeltaAction]
readActions (_, fp) = do
  bs <- BL.readFile fp
  pure (parseLogFile bs)

-- | Find the highest-version checkpoint Parquet that exists on
-- disk (regardless of what @_last_checkpoint@ claims). Returns
-- 'Nothing' if no @*.checkpoint.parquet@ files are present.
findCheckpointParquet :: FilePath -> IO (Maybe Word64)
findCheckpointParquet tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok then pure Nothing
  else do
    entries <- listDirectory logDir
    let versions = mapMaybe parseCheckpointName entries
    pure $ case versions of
      [] -> Nothing
      vs -> Just (maximum vs)
  where
    parseCheckpointName e
      | length stem == 20
      , Just v <- readMaybe stem :: Maybe Word64
      , rest == ".checkpoint.parquet"
      = Just v
      | otherwise = Nothing
      where
        (stem, rest) = splitAt 20 e
