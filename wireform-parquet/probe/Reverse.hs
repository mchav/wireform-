{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Reverse-direction Parquet interop probe.

Given a directory of Parquet files (typically produced by
pyarrow / duckdb / polars in the companion shell driver),
try to decode each one with wireform-parquet's high-level
facade and print one line per file: 'OK <name>' or
'FAIL <name>: <error>'. Exit code is the number of failures
(capped at 99) so CI can flag regressions.
-}
module Main (main) where

import Arrow.Types qualified as AT
import Control.Exception (SomeException, catch)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Parquet.Arrow qualified as PArrow
import Parquet.HighLevel qualified as PHL
import Parquet.Read qualified as PR
import Parquet.Types qualified as P
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeExtension, (</>))


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> do
      files <- filter ((== ".parquet") . takeExtension) <$> listDirectory dir
      results <- mapM (probe dir) files
      let !failures = [() | (_, Left _) <- results]
      mapM_ printResult results
      let !nFail = length failures
      putStrLn $ replicate 50 '-'
      putStrLn $
        show (length results)
          ++ " files, "
          ++ show (length results - nFail)
          ++ " ok, "
          ++ show nFail
          ++ " failed"
      if nFail == 0
        then pure ()
        else exitWith (ExitFailure (min 99 nFail))
    _ -> do
      putStrLn "usage: wireform-parquet-reverse-probe <dir>"
      exitFailure


printResult :: (FilePath, Either String String) -> IO ()
printResult (n, Left e) = putStrLn $ "  FAIL " ++ n ++ ": " ++ e
printResult (n, Right s) = putStrLn $ "  OK   " ++ n ++ " " ++ s


probe :: FilePath -> FilePath -> IO (FilePath, Either String String)
probe dir name = do
  let !path = dir </> name
  bs <- BS.readFile path
  res <-
    (decodeOnce bs) `catch` \e ->
      pure (Left (show (e :: SomeException)))
  pure (name, res)


decodeOnce :: BS.ByteString -> IO (Either String String)
decodeOnce bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left e -> pure (Left ("decode footer: " ++ e))
  Right pf -> do
    let !sch = PArrow.parquetFileArrowSchema pf
        !nRG = PArrow.numRowGroups pf
        !nCols = V.length (AT.arrowFields sch)
        results =
          [ ( rg
            , c
            , PArrow.readParquetColumn
                pf
                rg
                c
                (V.unsafeIndex (AT.arrowFields sch) c)
            )
          | rg <- [0 .. nRG - 1]
          , c <- [0 .. nCols - 1]
          ]
        firstFail =
          [ "rg "
              ++ show rg
              ++ " col "
              ++ show c
              ++ ": "
              ++ show e
              ++ describeChunk pf rg c
          | (rg, c, Left e) <- results
          ]
    pure $ case firstFail of
      [] ->
        Right
          ("(" ++ show nRG ++ " rg x " ++ show nCols ++ " col)")
      (e : _) -> Left e


{- | Return a short " [dpo=N data=M sz=S]" suffix to help
diagnose chunk-slice issues without needing a separate
dump tool.
-}
describeChunk
  :: PR.ParquetFile
  -> Int
  -> Int
  -> String
describeChunk pf rg c =
  case (P.fmRowGroups (PR.pfFooter pf)) V.!? rg of
    Just rgrec -> case P.rgColumns rgrec V.!? c of
      Just chunk -> case P.ccMetadata chunk of
        Just cm ->
          " [dpo="
            ++ show (P.cmDictionaryPageOffset cm)
            ++ " data="
            ++ show (P.cmDataPageOffset cm)
            ++ " sz="
            ++ show (P.cmTotalCompressedSize cm)
            ++ "]"
        _ -> ""
      _ -> ""
    _ -> ""
