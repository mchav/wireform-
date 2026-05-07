{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-delta interop probe.
--
-- Walks a Delta table's @_delta_log/@ directory, parses every
-- @NNNN.json@ commit file with 'Delta.Log.parseLogFile', folds
-- the resulting actions into a 'Delta.Log.TableSnapshot', and
-- writes a JSON summary to @argv[2]@ (or stdout) so the Python
-- driver can compare against @deltalake@'s own view of the same
-- table.
--
-- Usage:
--   wireform-delta-interop-probe <table-root> [<output.json>]
--
-- Limitations matching the rest of the skeleton:
--   * Only NDJSON commit files are walked; checkpoint Parquet
--     files are ignored.
--   * Schema decode is best-effort; the raw @schemaString@ is
--     also exposed in case the typed decoder loses something.
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)

import qualified Delta.Log as D

main :: IO ()
main = do
  args <- getArgs
  (tableRoot, outputDest) <- case args of
    [i]    -> pure (i, Nothing)
    [i, o] -> pure (i, Just o)
    _      -> do
      putStrLn "usage: wireform-delta-interop-probe <table-root> [<output.json>]"
      exitFailure

  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then do
      putStrLn $ "wireform-delta-interop-probe: missing " ++ logDir
      exitFailure
    else do
      entries <- listDirectory logDir
      let commits = sort (map (logDir </>) (filter (\e -> takeExtension e == ".json") entries))
      actions <- concat <$> mapM readCommit commits
      let snap = D.snapshotFromActions actions
      let summary = Aeson.Object $ KM.fromList
            [ (Key.fromString "num_commits", Aeson.Number (fromIntegral (length commits)))
            , (Key.fromString "num_actions", Aeson.Number (fromIntegral (length actions)))
            , (Key.fromString "active_files", filesJSON snap)
            , (Key.fromString "active_file_count", Aeson.Number (fromIntegral (Map.size (D.tsFiles snap))))
            , (Key.fromString "protocol", protocolJSON snap)
            , (Key.fromString "metadata", metadataJSON snap)
            , (Key.fromString "txn_app_versions", Aeson.Object $
                KM.fromList (map txnEntry (Map.toAscList (D.tsAppIds snap))))
            , (Key.fromString "last_commit_operation", case D.tsLastCommit snap of
                Nothing -> Aeson.Null
                Just c  -> case D.ciOperation c of
                  Just op -> Aeson.String op
                  Nothing -> Aeson.Null)
            ]
      case outputDest of
        Nothing -> BL.putStr (Aeson.encode summary)
        Just o  -> BL.writeFile o (Aeson.encode summary)

readCommit :: FilePath -> IO [D.DeltaAction]
readCommit fp = do
  bs <- BL.readFile fp
  pure (D.parseLogFile bs)

txnEntry :: (Text, Word64) -> (Key.Key, Aeson.Value)
txnEntry (k, v) = (Key.fromText k, Aeson.Number (fromIntegral v))

filesJSON :: D.TableSnapshot -> Aeson.Value
filesJSON snap =
  Aeson.Array $ V.fromList $ map fileEntry $ Map.toAscList (D.tsFiles snap)
  where
    fileEntry (path, a) = Aeson.Object $ KM.fromList
      [ (Key.fromString "path",            Aeson.String path)
      , (Key.fromString "size",            Aeson.Number (fromIntegral (D.addSize a)))
      , (Key.fromString "modificationTime",Aeson.Number (fromIntegral (D.addModificationTime a)))
      , (Key.fromString "partition_values", Aeson.Object $
          KM.fromList (map pvEntry (Map.toAscList (D.addPartitionValues a))))
      ]
    pvEntry (k, Nothing) = (Key.fromText k, Aeson.Null)
    pvEntry (k, Just t)  = (Key.fromText k, Aeson.String t)

protocolJSON :: D.TableSnapshot -> Aeson.Value
protocolJSON snap = case D.tsProtocol snap of
  Nothing -> Aeson.Null
  Just p  -> Aeson.Object $ KM.fromList
    [ (Key.fromString "min_reader_version", Aeson.Number (fromIntegral (D.pMinReaderVersion p)))
    , (Key.fromString "min_writer_version", Aeson.Number (fromIntegral (D.pMinWriterVersion p)))
    , (Key.fromString "reader_features",    Aeson.Array (V.fromList (map Aeson.String (D.pReaderFeatures p))))
    , (Key.fromString "writer_features",    Aeson.Array (V.fromList (map Aeson.String (D.pWriterFeatures p))))
    ]

metadataJSON :: D.TableSnapshot -> Aeson.Value
metadataJSON snap = case D.tsMetaData snap of
  Nothing -> Aeson.Null
  Just md -> Aeson.Object $ KM.fromList
    [ (Key.fromString "id", Aeson.String (D.mdId md))
    , (Key.fromString "partition_columns",
        Aeson.Array (V.fromList (map Aeson.String (D.mdPartitionColumns md))))
    , (Key.fromString "schema_field_names", schemaFieldNames md)
    ]
  where
    schemaFieldNames mdv = case D.parseDeltaSchema (D.mdSchemaString mdv) of
      Right s -> Aeson.Array (V.fromList (map (Aeson.String . D.dfName) (D.dsFields s)))
      Left _  -> Aeson.Null
