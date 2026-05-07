{-# LANGUAGE OverloadedStrings #-}
-- | wireform-lance interop probe.
--
-- Reads a Lance file with 'Lance.Format' and emits a JSON
-- summary to stdout (or @argv[2]@), so the Python interop driver
-- can compare it against pylance's view of the same file.
--
-- Usage:
--   wireform-lance-interop-probe <lance-file> [<output-json>]
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Lance.Format as L

main :: IO ()
main = do
  args <- getArgs
  (input, outputDest) <- case args of
    [i]    -> pure (i, Nothing)
    [i, o] -> pure (i, Just o)
    _      -> do
      putStrLn "usage: wireform-lance-interop-probe <input.lance> [<output.json>]"
      exitFailure

  bs <- BS.readFile input
  case L.readLanceFile bs of
    Left err -> do
      putStrLn ("wireform-lance-interop-probe: " ++ err)
      exitFailure
    Right lf -> do
      let footer = L.lfFooter lf
      colTbl <- case L.parseColumnOffsetTable lf of
        Right t  -> pure t
        Left err -> do
          putStrLn ("wireform-lance-interop-probe: column table: " ++ err)
          exitFailure
      gboTbl <- case L.parseGlobalBufferOffsetTable lf of
        Right t  -> pure t
        Left err -> do
          putStrLn ("wireform-lance-interop-probe: GBO table: " ++ err)
          exitFailure
      let summary = Aeson.Object $ KM.fromList
            [ (Key.fromString "file_size",     Aeson.Number (fromIntegral (BS.length bs)))
            , (Key.fromString "footer", Aeson.Object $ KM.fromList
                [ (Key.fromString "column_meta_0_offset", Aeson.Number (fromIntegral (L.lfColumnMeta0Offset footer)))
                , (Key.fromString "cmo_table_offset",     Aeson.Number (fromIntegral (L.lfCMOTableOffset footer)))
                , (Key.fromString "gbo_table_offset",     Aeson.Number (fromIntegral (L.lfGBOTableOffset footer)))
                , (Key.fromString "num_global_buffers",   Aeson.Number (fromIntegral (L.lfNumGlobalBuffers footer)))
                , (Key.fromString "num_columns",          Aeson.Number (fromIntegral (L.lfNumColumns footer)))
                , (Key.fromString "major_version",        Aeson.Number (fromIntegral (L.lfMajorVersion footer)))
                , (Key.fromString "minor_version",        Aeson.Number (fromIntegral (L.lfMinorVersion footer)))
                ])
            , (Key.fromString "columns", Aeson.Array $ V.map sliceJSON colTbl)
            , (Key.fromString "global_buffers", Aeson.Array $ V.map gbsJSON gboTbl)
            ]
      let payload = Aeson.encode summary
      case outputDest of
        Nothing -> BL.putStr payload
        Just o  -> BL.writeFile o payload
  where
    sliceJSON cs = Aeson.Object $ KM.fromList
      [ (Key.fromString "position", Aeson.Number (fromIntegral (L.csPosition cs)))
      , (Key.fromString "size",     Aeson.Number (fromIntegral (L.csSize     cs)))
      ]
    gbsJSON gb = Aeson.Object $ KM.fromList
      [ (Key.fromString "position", Aeson.Number (fromIntegral (L.gbsPosition gb)))
      , (Key.fromString "size",     Aeson.Number (fromIntegral (L.gbsSize     gb)))
      ]
