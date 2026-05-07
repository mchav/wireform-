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

import qualified Lance.Format as L
import qualified Lance.IO     as LIO

main :: IO ()
main = do
  args <- getArgs
  (mode, input, outputDest) <- parseArgs args

  case mode of
    File    -> probeSingleFile input outputDest
    Dataset -> probeDataset    input outputDest

data Mode = File | Dataset

parseArgs :: [String] -> IO (Mode, FilePath, Maybe FilePath)
parseArgs args0 = case args0 of
  ["--file", i]       -> pure (File, i, Nothing)
  ["--file", i, o]    -> pure (File, i, Just o)
  ["--dataset", i]    -> pure (Dataset, i, Nothing)
  ["--dataset", i, o] -> pure (Dataset, i, Just o)
  [i]                 -> autoDetect i Nothing
  [i, o]              -> autoDetect i (Just o)
  _                   -> do
    putStrLn "usage: wireform-lance-interop-probe [--file|--dataset] <path> [<output.json>]"
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
    Right ds -> emit (datasetJSON ds) outputDest

datasetJSON :: LIO.LanceDataset -> Aeson.Value
datasetJSON ds = Aeson.Object $ KM.fromList
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
  ]
  where
    versionEntry (v, p) = Aeson.Object $ KM.fromList
      [ (Key.fromString "version", Aeson.Number (fromIntegral v))
      , (Key.fromString "manifest_basename", textPath (takeFileName p))
      ]
    dataFileEntry p = textPath (takeFileName p)

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
