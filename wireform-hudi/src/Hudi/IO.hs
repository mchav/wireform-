{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | High-level IO entry points for Apache Hudi tables.
--
-- This is the surface most callers want: hand it a path on disk
-- and it gives back a typed snapshot of the table at the latest
-- commit. The format-level decoders in "Hudi.Timeline" stay
-- pure-data so they can be reused by tests / fixtures.
module Hudi.IO
  ( -- * Discovery
    scanTimeline
  , readHoodieProperties
    -- * High-level opener
  , HudiTable (..)
  , openHudiTable
    -- * Helpers
  , tableSchemaFromCommits
    -- * Re-exports
  , module Hudi.Timeline
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson

import qualified Hudi.Avro as HAvro
import Hudi.Timeline

-- ============================================================
-- Discovery
-- ============================================================

-- | Walk @<root>/.hoodie/@ and return every parseable instant
-- file together with its absolute path, sorted by
-- @(instantTime, state)@. Bogus filenames (@archived/@,
-- @metadata/@, @hoodie.properties@, etc.) are silently dropped.
--
-- The list preserves /every/ parseable entry — including the
-- requested / inflight states and any non-commit actions — so
-- callers can decide which subset they care about. To collapse
-- a single instant per timestamp use 'sortInstants' on the
-- 'fst's instead.
scanTimeline :: FilePath -> IO [(Instant, FilePath)]
scanTimeline tableRoot = do
  let hoodie = tableRoot </> ".hoodie"
  ok <- doesDirectoryExist hoodie
  if not ok then pure []
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
    stateRank Inflight  = 1
    stateRank Completed = 2

-- | Read @<root>/.hoodie/hoodie.properties@ into a flat
-- key→value map. The file is the source of truth for table
-- type, partition fields, the writer payload class, etc. We
-- treat it as a permissive Java-style properties file: lines
-- starting with @#@ or @!@ are comments, blank lines are
-- skipped, otherwise the first @=@ or @:@ separates the key
-- from the value. Returns 'Nothing' if the file is missing.
readHoodieProperties :: FilePath -> IO (Maybe (Map.Map Text Text))
readHoodieProperties tableRoot = do
  let p = tableRoot </> ".hoodie" </> "hoodie.properties"
  ok <- doesFileExist p
  if not ok then pure Nothing
  else do
    bs <- BS.readFile p
    let !txt   = TE.decodeUtf8 bs
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
  { hutRoot       :: !FilePath
    -- ^ Filesystem path to the table directory.
  , hutProperties :: !(Map.Map Text Text)
    -- ^ Decoded @hoodie.properties@; empty if the file was
    -- absent.
  , hutInstants   :: ![Instant]
    -- ^ Every parseable instant on the timeline, sorted by
    -- @(instantTime, state)@.
  , hutCommits    :: ![(Text, HoodieCommitMetadata)]
    -- ^ Every successfully-decoded completed commit /
    -- deltacommit instant, in chronological order.
  , hutParseFailures :: ![(Text, String)]
    -- ^ @(instantTime, error)@ pairs for instants whose JSON
    -- payload didn't decode (corrupt, Avro instead of JSON,
    -- etc.). Surfacing these is more useful than silently
    -- dropping them.
  , hutState      :: !TableState
    -- ^ The folded snapshot at the latest commit.
  , hutSchemaJson :: !(Maybe Text)
    -- ^ Avro JSON schema string, lifted from the latest
    -- commit's @extraMetadata.schema@ field.
  } deriving (Show, Eq)

-- | Open a Hudi table from a directory and replay every
-- completed commit / deltacommit JSON into a 'TableState'.
--
-- /Out of scope:/ Avro instant payloads (Hudi 1.x+) — the
-- decoder is JSON-only, and any @<time>.commit@ that opens with
-- the Avro magic will land in 'hutParseFailures' so callers can
-- decide whether to treat that as fatal.
openHudiTable :: FilePath -> IO (Either String HudiTable)
openHudiTable tableRoot = do
  let hoodie = tableRoot </> ".hoodie"
  ok <- doesDirectoryExist hoodie
  if not ok
    then pure (Left ("Hudi.IO: missing " ++ hoodie))
    else do
      props      <- readHoodieProperties tableRoot
      timeline   <- scanTimeline tableRoot
      let !instants = map fst timeline
          !candidates = filter isCompletedCommit timeline
      decoded    <- mapM readCommit candidates
      let !ok'   = mapMaybe asOk decoded
          !fails = mapMaybe asFail decoded
          !state = tableStateFromCommits ok'
      pure $ Right HudiTable
        { hutRoot          = tableRoot
        , hutProperties    = case props of
            Just m  -> m
            Nothing -> Map.empty
        , hutInstants      = instants
        , hutCommits       = ok'
        , hutParseFailures = fails
        , hutState         = state
        , hutSchemaJson    = tableSchemaFromCommits ok'
        }
  where
    isCompletedCommit (i, _) =
      instantState i == Completed
        && instantAction i `elem` [Commit, DeltaCommit]

    asOk   (i, Right hcm) = Just (instantTime i, hcm)
    asOk   _              = Nothing
    asFail (i, Left e)    = Just (instantTime i, e)
    asFail _              = Nothing

readCommit
  :: (Instant, FilePath)
  -> IO (Instant, Either String HoodieCommitMetadata)
readCommit (i, fp) = do
  bs <- BL.readFile fp
  -- Hudi 1.x writes commit instants as Avro container files; older
  -- versions write JSON. Try JSON first (cheaper, no schema lookup);
  -- if it fails, fall back to the Avro container reader. This matches
  -- the order Hudi-rs walks (it tolerates either shape).
  case parseCommitJson bs of
    Right hcm -> pure (i, Right hcm)
    Left  jsonErr -> do
      let !strict = BL.toStrict bs
      case HAvro.decodeCommitAvro strict of
        Right hcm -> pure (i, Right hcm)
        Left avroErr ->
          pure (i, Left ("json: " ++ jsonErr ++ "; avro: " ++ avroErr))

-- ============================================================
-- Helpers
-- ============================================================

-- | Lift the Avro JSON schema string out of a commit list.
-- Hudi writers stash the table's writer-side schema in
-- @extraMetadata.schema@ on every commit — for read-time the
-- /latest/ commit's value wins. Returns 'Nothing' if no commit
-- has a @schema@ entry (e.g. the table was bootstrapped without
-- one).
tableSchemaFromCommits :: [(Text, HoodieCommitMetadata)] -> Maybe Text
tableSchemaFromCommits commits =
  -- Walk newest-first; take the first @schema@ we see.
  listToMaybe $ mapMaybe pluck (reverse commits)
  where
    pluck (_, hcm) =
      case HM.lookup "schema" (hcmExtraMetadata hcm) of
        Just (Aeson.String s) -> Just s
        _                     -> Nothing
