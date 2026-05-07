{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-hudi interop probe.
--
-- Walks a Hudi table's @.hoodie/@ directory, parses every
-- completed @<time>.commit@ JSON via 'Hudi.Timeline.parseCommitJson',
-- folds the resulting commit metadata via
-- 'Hudi.Timeline.tableStateFromCommits', and writes a JSON summary
-- to @argv[2]@ (or stdout). The Python driver compares against
-- @hudi-rs@'s 'HudiTable.get_file_slices()' on the same directory.
--
-- Usage:
--   wireform-hudi-interop-probe <table-root> [<output.json>]
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Hudi.Timeline as H

main :: IO ()
main = do
  args <- getArgs
  (tableRoot, outputDest) <- case args of
    [i]    -> pure (i, Nothing)
    [i, o] -> pure (i, Just o)
    _      -> do
      putStrLn "usage: wireform-hudi-interop-probe <table-root> [<output.json>]"
      exitFailure

  let hoodie = tableRoot </> ".hoodie"
  ok <- doesDirectoryExist hoodie
  if not ok
    then do
      putStrLn $ "wireform-hudi-interop-probe: missing " ++ hoodie
      exitFailure
    else do
      entries <- listDirectory hoodie
      -- Pull out completed commit / deltacommit instants and pair
      -- each with its on-disk path; sort by instant time so the
      -- replay folds in chronological order.
      let candidates = mapMaybe (toInstant hoodie) entries
          instants   = sortByInstantTime candidates
      commits <- mapM readCommit instants
      let toOk (i, Right hcm) = Just (H.instantTime i, hcm)
          toOk _              = Nothing
          toFail (i, Left e)  = Just (H.instantTime i, e)
          toFail _            = Nothing
          okCommits  = mapMaybe toOk commits
          parseFails = mapMaybe toFail commits
          state      = H.tableStateFromCommits okCommits
          summary = Aeson.Object $ KM.fromList
            [ (Key.fromString "completed_commits",
                Aeson.Array $ V.fromList
                  [Aeson.String t | (t, _) <- okCommits])
            , (Key.fromString "parse_failures",
                Aeson.Array $ V.fromList
                  [Aeson.Object $ KM.fromList
                    [ (Key.fromString "instant", Aeson.String t)
                    , (Key.fromString "error",   Aeson.String (T.pack e))]
                   | (t, e) <- parseFails])
            , (Key.fromString "latest_instant", case H.tsLatestInstant state of
                Just t  -> Aeson.String t
                Nothing -> Aeson.Null)
            , (Key.fromString "active_file_slices", fileSlicesJSON state)
            , (Key.fromString "active_file_slice_count",
                Aeson.Number (fromIntegral (countSlices state)))
            ]
      case outputDest of
        Nothing -> BL.putStr (Aeson.encode summary)
        Just o  -> BL.writeFile o (Aeson.encode summary)

readCommit
  :: (H.Instant, FilePath)
  -> IO (H.Instant, Either String H.HoodieCommitMetadata)
readCommit (i, fp) = do
  bs <- BL.readFile fp
  pure (i, H.parseCommitJson bs)

-- | Decode an entry name into (Instant, fullPath), keeping only
-- completed @commit@ / @deltacommit@ instants.
toInstant :: FilePath -> FilePath -> Maybe (H.Instant, FilePath)
toInstant hoodie e = do
  i <- H.parseInstantFileName (T.pack e)
  if H.instantState i /= H.Completed then Nothing
  else if H.instantAction i `notElem` [H.Commit, H.DeltaCommit] then Nothing
  else Just (i, hoodie </> e)

-- | Sort a list of (Instant, path) pairs by the instant's
-- timestamp. We avoid an Ord instance on Instant itself because
-- the type doesn't ship one and we don't want to leak ordering
-- semantics from the probe.
sortByInstantTime
  :: [(H.Instant, FilePath)] -> [(H.Instant, FilePath)]
sortByInstantTime = Map.elems . Map.fromList . map keyed
  where
    keyed (i, p) = ((H.instantTime i, p), (i, p))

fileSlicesJSON :: H.TableState -> Aeson.Value
fileSlicesJSON state =
  Aeson.Array $ V.fromList $ do
    (part, slices) <- Map.toAscList (H.tsPartitions state)
    (fid, slice)   <- Map.toAscList slices
    pure $ Aeson.Object $ KM.fromList
      [ (Key.fromString "partition_path", Aeson.String part)
      , (Key.fromString "file_id",        Aeson.String fid)
      , (Key.fromString "base_file",      case H.fsBaseFile slice of
          Just b  -> Aeson.String b
          Nothing -> Aeson.Null)
      , (Key.fromString "log_files",
          Aeson.Array (V.fromList (map Aeson.String (H.fsLogFiles slice))))
      , (Key.fromString "latest_commit",  Aeson.String (H.fsLatestCommit slice))
      ]

countSlices :: H.TableState -> Int
countSlices = sum . map Map.size . Map.elems . H.tsPartitions
