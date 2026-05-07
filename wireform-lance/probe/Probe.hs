{-# LANGUAGE OverloadedStrings #-}
-- | wireform-lance interop probe.
--
-- Two modes:
--
--   * @--file <path.lance>@ — single-file footer probe; reads
--     one @.lance@ file with 'Lance.IO.openLanceFile' and emits
--     the typed footer + offset tables.
--
--   * @--dataset <path.lance/>@ — dataset probe; reads
--     @<path>/_versions/*.manifest@ + the active manifest's
--     footer + the on-disk data file enumeration.
--
-- For backward-compat, a positional argument with a @.lance@
-- extension is treated as @--file@; otherwise as @--dataset@.
--
-- Usage:
--   wireform-lance-interop-probe --file <input.lance> [<output.json>]
--   wireform-lance-interop-probe --dataset <input.lance/> [<output.json>]
--   wireform-lance-interop-probe <input> [<output.json>]
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, takeFileName)

import qualified Lance.Format   as L
import qualified Lance.IO       as LIO
import qualified Lance.Manifest as LM
import qualified Lance.Pb.Lance.Table as Pb

main :: IO ()
main = do
  args <- getArgs
  (mode, input, outputDest) <- parseArgs args

  case mode of
    File    -> probeSingleFile input outputDest
    Dataset -> probeDataset    input outputDest

data Mode = File | Dataset

probeManifestBytes :: FilePath -> IO ()
probeManifestBytes fp = do
  bs <- BS.readFile fp
  case LM.decodeManifest bs of
    Left  e -> putStrLn ("ERROR: " ++ e)
    Right m -> do
      putStrLn $ "version=" ++ show (Pb.manifestVersion m)
      putStrLn $ "fragments=" ++ show (V.length (Pb.manifestFragments m))
      putStrLn $ "writer_version=" ++ show (Pb.manifestWriterVersion m)
      putStrLn $ "data_format=" ++ show (Pb.manifestDataFormat m)

parseArgs :: [String] -> IO (Mode, FilePath, Maybe FilePath)
parseArgs args0 = case args0 of
  ["--file", i]       -> pure (File, i, Nothing)
  ["--file", i, o]    -> pure (File, i, Just o)
  ["--dataset", i]    -> pure (Dataset, i, Nothing)
  ["--dataset", i, o] -> pure (Dataset, i, Just o)
  ["--manifest", i]   -> probeManifestBytes i >> exitFailure
  [i]                 -> autoDetect i Nothing
  [i, o]              -> autoDetect i (Just o)
  _                   -> do
    putStrLn "usage: wireform-lance-interop-probe [--file|--dataset|--manifest] <path> [<output.json>]"
    exitFailure
  where
    autoDetect i o = do
      isDir <- doesDirectoryExist i
      if isDir then pure (Dataset, i, o)
      else if takeExtension i == ".lance"
        then pure (File, i, o)
        else pure (File, i, o)  -- default to file mode

-- ============================================================
-- Single-file mode
-- ============================================================

probeSingleFile :: FilePath -> Maybe FilePath -> IO ()
probeSingleFile input outputDest = do
  bs <- BS.readFile input
  case L.readLanceFile bs of
    Left err -> do
      putStrLn ("wireform-lance-interop-probe: " ++ err)
      exitFailure
    Right lf -> do
      colTbl <- expect "column table" (L.parseColumnOffsetTable lf)
      gboTbl <- expect "GBO table"    (L.parseGlobalBufferOffsetTable lf)
      let summary = singleFileJSON (BS.length bs) lf colTbl gboTbl
      emit summary outputDest
  where
    expect _    (Right t)  = pure t
    expect what (Left err) = do
      putStrLn ("wireform-lance-interop-probe: " ++ what ++ ": " ++ err)
      exitFailure

singleFileJSON
  :: Int
  -> L.LanceFile
  -> V.Vector L.ColumnSlice
  -> V.Vector L.GlobalBufferSlice
  -> Aeson.Value
singleFileJSON sz lf colTbl gboTbl =
  let footer = L.lfFooter lf
   in Aeson.Object $ KM.fromList
        [ (Key.fromString "mode",      Aeson.String "file")
        , (Key.fromString "file_size", Aeson.Number (fromIntegral sz))
        , (Key.fromString "footer",    footerJSON footer)
        , (Key.fromString "columns",         Aeson.Array (V.map sliceJSON colTbl))
        , (Key.fromString "global_buffers",  Aeson.Array (V.map gbsJSON gboTbl))
        ]

-- ============================================================
-- Dataset mode
-- ============================================================

probeDataset :: FilePath -> Maybe FilePath -> IO ()
probeDataset input outputDest = do
  res <- LIO.openLanceDataset input
  case res of
    Left err -> do
      putStrLn ("wireform-lance-interop-probe: " ++ err)
      exitFailure
    Right ds -> do
      -- If there's a latest manifest, decode its protobuf body too
      -- so we can surface the typed Manifest fields (writer
      -- version, fragment list, data files, etc.).
      (manifest, manifestErr) <- case LIO.ldVersions ds of
        []         -> pure (Nothing, Nothing)
        ((_, p):_) -> do
          mr <- LM.readDatasetManifest p
          case mr of
            Right (_, m) -> pure (Just m, Nothing)
            Left  e      -> pure (Nothing, Just e)
      emit (datasetJSON ds manifest manifestErr) outputDest

datasetJSON :: LIO.LanceDataset -> Maybe Pb.Manifest -> Maybe String -> Aeson.Value
datasetJSON ds mManifest mErr = Aeson.Object $ KM.fromList
  [ (Key.fromString "mode",         Aeson.String "dataset")
  , (Key.fromString "root",         textPath (LIO.ldRoot ds))
  , (Key.fromString "latest_version", case LIO.ldLatestVersion ds of
      Just v  -> Aeson.Number (fromIntegral v)
      Nothing -> Aeson.Null)
  , (Key.fromString "versions",
      Aeson.Array (V.fromList (map versionEntry (LIO.ldVersions ds))))
  , (Key.fromString "latest_manifest_footer", case LIO.ldLatestManifestFooter ds of
      Just lmf -> manifestFooterJSON lmf
      Nothing  -> Aeson.Null)
  , (Key.fromString "data_file_names",
      Aeson.Array (V.fromList (map dataFileEntry (LIO.ldDataFiles ds))))
  , (Key.fromString "data_file_count",
      Aeson.Number (fromIntegral (length (LIO.ldDataFiles ds))))
  , (Key.fromString "manifest", maybe Aeson.Null manifestJSON mManifest)
  , (Key.fromString "manifest_decode_error", case mErr of
      Just e  -> Aeson.String (T.pack e)
      Nothing -> Aeson.Null)
  ]
  where
    versionEntry (v, p) = Aeson.Object $ KM.fromList
      [ (Key.fromString "version", Aeson.Number (fromIntegral v))
      , (Key.fromString "manifest_basename", textPath (takeFileName p))
      ]
    dataFileEntry p = textPath (takeFileName p)

manifestJSON :: Pb.Manifest -> Aeson.Value
manifestJSON m = Aeson.Object $ KM.fromList
  [ (Key.fromString "version",
      Aeson.Number (fromIntegral (Pb.manifestVersion m)))
  , (Key.fromString "tag", Aeson.String (Pb.manifestTag m))
  , (Key.fromString "transaction_file",
      Aeson.String (Pb.manifestTransactionFile m))
  , (Key.fromString "max_fragment_id", case Pb.manifestMaxFragmentId m of
      Just i  -> Aeson.Number (fromIntegral i)
      Nothing -> Aeson.Null)
  , (Key.fromString "next_row_id",
      Aeson.Number (fromIntegral (Pb.manifestNextRowId m)))
  , (Key.fromString "writer_version", case Pb.manifestWriterVersion m of
      Just w  -> writerVersionJSON w
      Nothing -> Aeson.Null)
  , (Key.fromString "data_format", case Pb.manifestDataFormat m of
      Just f  -> dataFormatJSON f
      Nothing -> Aeson.Null)
  , (Key.fromString "fragments",
      Aeson.Array (V.map fragmentJSON (Pb.manifestFragments m)))
  , (Key.fromString "fragment_count",
      Aeson.Number (fromIntegral (V.length (Pb.manifestFragments m))))
  ]

writerVersionJSON :: Pb.Manifest'WriterVersion -> Aeson.Value
writerVersionJSON wv = Aeson.Object $ KM.fromList
  [ (Key.fromString "library", Aeson.String (Pb.manifestWriterVersionLibrary wv))
  , (Key.fromString "version", Aeson.String (Pb.manifestWriterVersionVersion wv))
  , (Key.fromString "prerelease", case Pb.manifestWriterVersionPrerelease wv of
      Just t  -> Aeson.String t
      Nothing -> Aeson.Null)
  , (Key.fromString "build_metadata", case Pb.manifestWriterVersionBuildMetadata wv of
      Just t  -> Aeson.String t
      Nothing -> Aeson.Null)
  ]

dataFormatJSON :: Pb.Manifest'DataStorageFormat -> Aeson.Value
dataFormatJSON dsf = Aeson.Object $ KM.fromList
  [ (Key.fromString "file_format",
      Aeson.String (Pb.manifestDataStorageFormatFileFormat dsf))
  , (Key.fromString "version",
      Aeson.String (Pb.manifestDataStorageFormatVersion dsf))
  ]

fragmentJSON :: Pb.DataFragment -> Aeson.Value
fragmentJSON df = Aeson.Object $ KM.fromList
  [ (Key.fromString "id",
      Aeson.Number (fromIntegral (Pb.dataFragmentId df)))
  , (Key.fromString "physical_rows",
      Aeson.Number (fromIntegral (Pb.dataFragmentPhysicalRows df)))
  , (Key.fromString "files",
      Aeson.Array (V.map dataFileJSON (Pb.dataFragmentFiles df)))
  ]

dataFileJSON :: Pb.DataFile -> Aeson.Value
dataFileJSON f = Aeson.Object $ KM.fromList
  [ (Key.fromString "path",     Aeson.String (Pb.dataFilePath f))
  , (Key.fromString "file_major_version",
      Aeson.Number (fromIntegral (Pb.dataFileFileMajorVersion f)))
  , (Key.fromString "file_minor_version",
      Aeson.Number (fromIntegral (Pb.dataFileFileMinorVersion f)))
  , (Key.fromString "file_size_bytes",
      Aeson.Number (fromIntegral (Pb.dataFileFileSizeBytes f)))
  ]

-- ============================================================
-- Shared helpers
-- ============================================================

footerJSON :: L.LanceFooter -> Aeson.Value
footerJSON f = Aeson.Object $ KM.fromList
  [ (Key.fromString "column_meta_0_offset",
      Aeson.Number (fromIntegral (L.lfColumnMeta0Offset f)))
  , (Key.fromString "cmo_table_offset",
      Aeson.Number (fromIntegral (L.lfCMOTableOffset f)))
  , (Key.fromString "gbo_table_offset",
      Aeson.Number (fromIntegral (L.lfGBOTableOffset f)))
  , (Key.fromString "num_global_buffers",
      Aeson.Number (fromIntegral (L.lfNumGlobalBuffers f)))
  , (Key.fromString "num_columns",
      Aeson.Number (fromIntegral (L.lfNumColumns f)))
  , (Key.fromString "major_version",
      Aeson.Number (fromIntegral (L.lfMajorVersion f)))
  , (Key.fromString "minor_version",
      Aeson.Number (fromIntegral (L.lfMinorVersion f)))
  ]

manifestFooterJSON :: L.LanceManifestFooter -> Aeson.Value
manifestFooterJSON f = Aeson.Object $ KM.fromList
  [ (Key.fromString "manifest_position",
      Aeson.Number (fromIntegral (L.lmfManifestPosition f)))
  , (Key.fromString "major_version",
      Aeson.Number (fromIntegral (L.lmfMajorVersion f)))
  , (Key.fromString "minor_version",
      Aeson.Number (fromIntegral (L.lmfMinorVersion f)))
  ]

sliceJSON :: L.ColumnSlice -> Aeson.Value
sliceJSON cs = Aeson.Object $ KM.fromList
  [ (Key.fromString "position", Aeson.Number (fromIntegral (L.csPosition cs)))
  , (Key.fromString "size",     Aeson.Number (fromIntegral (L.csSize     cs)))
  ]

gbsJSON :: L.GlobalBufferSlice -> Aeson.Value
gbsJSON gb = Aeson.Object $ KM.fromList
  [ (Key.fromString "position", Aeson.Number (fromIntegral (L.gbsPosition gb)))
  , (Key.fromString "size",     Aeson.Number (fromIntegral (L.gbsSize     gb)))
  ]

textPath :: FilePath -> Aeson.Value
textPath = Aeson.String . T.pack

emit :: Aeson.Value -> Maybe FilePath -> IO ()
emit v Nothing  = BL.putStr (Aeson.encode v)
emit v (Just o) = BL.writeFile o (Aeson.encode v)
