{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-delta interop probe.
--
-- Opens a Delta table on disk via 'Delta.IO.openDeltaTable' and
-- writes a JSON summary of what wireform sees: protocol +
-- metadata + active file set + per-app txn versions + last
-- commit operation + last_checkpoint pointer + checkpoint
-- discovery.
--
-- Usage:
--   wireform-delta-interop-probe <table-root> [<output.json>]
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Delta.IO as DIO
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

  res <- DIO.openDeltaTable tableRoot
  case res of
    Left err -> do
      putStrLn ("wireform-delta-interop-probe: " ++ err)
      exitFailure
    Right dt -> do
      let snap = DIO.dtSnapshot dt
          summary = Aeson.Object $ KM.fromList
            [ (Key.fromString "version", maybe Aeson.Null numW64 (DIO.dtVersion dt))
            , (Key.fromString "num_commits", Aeson.Number (fromIntegral (length (DIO.dtCommits dt))))
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
            , (Key.fromString "last_checkpoint", lastCheckpointJSON (DIO.dtLastCheckpoint dt))
            , (Key.fromString "checkpoint_parquet_version",
                maybe Aeson.Null numW64 (DIO.dtCheckpointAvailable dt))
            ]
      case outputDest of
        Nothing -> BL.putStr (Aeson.encode summary)
        Just o  -> BL.writeFile o (Aeson.encode summary)

numW64 :: Word64 -> Aeson.Value
numW64 = Aeson.Number . fromIntegral

txnEntry :: (Text, Word64) -> (Key.Key, Aeson.Value)
txnEntry (k, v) = (Key.fromText k, numW64 v)

filesJSON :: D.TableSnapshot -> Aeson.Value
filesJSON snap =
  Aeson.Array $ V.fromList $ map fileEntry $ Map.toAscList (D.tsFiles snap)
  where
    fileEntry (path, a) = Aeson.Object $ KM.fromList
      [ (Key.fromString "path",            Aeson.String path)
      , (Key.fromString "size",            numW64 (D.addSize a))
      , (Key.fromString "modificationTime",numW64 (D.addModificationTime a))
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

lastCheckpointJSON :: Maybe D.LastCheckpoint -> Aeson.Value
lastCheckpointJSON Nothing   = Aeson.Null
lastCheckpointJSON (Just lc) = Aeson.Object $ KM.fromList
  [ (Key.fromString "version", numW64 (D.lcVersion lc))
  , (Key.fromString "size",    numW64 (D.lcSize lc))
  , (Key.fromString "parts",
      maybe Aeson.Null numW64 (D.lcParts lc))
  , (Key.fromString "size_in_bytes",
      maybe Aeson.Null numW64 (D.lcSizeInBytes lc))
  , (Key.fromString "num_of_add_files",
      maybe Aeson.Null numW64 (D.lcNumOfAddFiles lc))
  ]
