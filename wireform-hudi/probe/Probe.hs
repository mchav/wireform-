{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-hudi interop probe.
--
-- Opens a Hudi table on disk via 'Hudi.IO.openHudiTable' and
-- writes a JSON summary. The Python driver compares against
-- @hudi-rs@'s 'HudiTable.get_file_slices()' on the same
-- directory.
--
-- Usage:
--   wireform-hudi-interop-probe <table-root> [<output.json>]
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Hudi.IO as HIO
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

  res <- HIO.openHudiTable tableRoot
  case res of
    Left err -> do
      putStrLn ("wireform-hudi-interop-probe: " ++ err)
      exitFailure
    Right ht -> do
      let state = HIO.hutState ht
          summary = Aeson.Object $ KM.fromList
            [ (Key.fromString "table_name",
                stringFromMap (HIO.hutProperties ht) "hoodie.table.name")
            , (Key.fromString "table_type",
                stringFromMap (HIO.hutProperties ht) "hoodie.table.type")
            , (Key.fromString "completed_commits",
                Aeson.Array $ V.fromList (map (Aeson.String . fst) (HIO.hutCommits ht)))
            , (Key.fromString "parse_failures",
                Aeson.Array $ V.fromList (map failJSON (HIO.hutParseFailures ht)))
            , (Key.fromString "latest_instant", case H.tsLatestInstant state of
                Just t  -> Aeson.String t
                Nothing -> Aeson.Null)
            , (Key.fromString "active_file_slices", fileSlicesJSON state)
            , (Key.fromString "active_file_slice_count",
                Aeson.Number (fromIntegral (countSlices state)))
            , (Key.fromString "schema_json", case HIO.hutSchemaJson ht of
                Just s  -> Aeson.String s
                Nothing -> Aeson.Null)
            ]
      case outputDest of
        Nothing -> BL.putStr (Aeson.encode summary)
        Just o  -> BL.writeFile o (Aeson.encode summary)

stringFromMap :: Map.Map Text Text -> Text -> Aeson.Value
stringFromMap m k = case Map.lookup k m of
  Just t  -> Aeson.String t
  Nothing -> Aeson.Null

failJSON :: (Text, String) -> Aeson.Value
failJSON (t, e) = Aeson.Object $ KM.fromList
  [ (Key.fromString "instant", Aeson.String t)
  , (Key.fromString "error",   Aeson.String (T.pack e))
  ]

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
