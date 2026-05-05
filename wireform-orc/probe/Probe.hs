{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-orc -> pyarrow / duckdb interop probe.
--
-- Writes a fixed catalogue of one-file-per-shape ORC files
-- under @argv[1]@. The companion 'scripts/orc_interop.py'
-- driver reads each one with pyarrow.orc and duckdb and
-- asserts the contents match what we wrote.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT
import qualified ORC
import qualified ORC.Arrow as OArrow

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> do
      writeAllProbes outDir
      putStrLn $ "wrote orc probe outputs to " ++ outDir
    _ -> do
      putStrLn "usage: wireform-orc-interop-probe <output-dir>"
      exitFailure

writeAllProbes :: FilePath -> IO ()
writeAllProbes outDir = do
  writeOne outDir "int64_required.orc" int64Sch int64Batches
  writeOne outDir "double_required.orc" doubleSch doubleBatches
  writeOne outDir "string_required.orc" stringSch stringBatches
  writeOne outDir "mixed_required.orc" mixedSch mixedBatches
  writeOne outDir "bool_required.orc" boolSch boolBatches

writeOne :: FilePath -> FilePath -> AT.Schema -> [V.Vector AC.ColumnArray] -> IO ()
writeOne outDir fname sch batches =
  case OArrow.arrowToORC sch batches of
    Left err -> do
      putStrLn $ "FAIL " ++ fname ++ " (arrowToORC): " ++ err
      exitFailure
    Right (types, stripesWithRows) ->
      case ORC.encodeORC ORC.defaultWriteOptions types stripesWithRows of
        Right bs -> BS.writeFile (outDir </> fname) bs
        Left err -> do
          putStrLn $ "FAIL " ++ fname ++ " (encodeORC): " ++ err
          exitFailure

-- ============================================================
-- Schemas + payloads
-- ============================================================

int64Sch :: AT.Schema
int64Sch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AInt 64 True)))

int64Batches :: [V.Vector AC.ColumnArray]
int64Batches =
  [ V.singleton (AC.ColInt64 (VP.fromList [10, 20, 30, 40, 50 :: Int64])) ]

doubleSch :: AT.Schema
doubleSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AFloatingPoint AT.DoublePrecision)))

doubleBatches :: [V.Vector AC.ColumnArray]
doubleBatches =
  [ V.singleton (AC.ColDouble (VP.fromList [1.5, -2.5, 3.14159 :: Double])) ]

stringSch :: AT.Schema
stringSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "name" False AT.AUtf8))

stringBatches :: [V.Vector AC.ColumnArray]
stringBatches =
  [ V.singleton (AC.ColUtf8 (V.fromList ["alpha", "beta", "gamma"])) ]

boolSch :: AT.Schema
boolSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "b" False AT.ABool))

boolBatches :: [V.Vector AC.ColumnArray]
boolBatches =
  [ V.singleton (AC.ColBool (V.fromList [True, False, True, True, False])) ]

mixedSch :: AT.Schema
mixedSch = AT.defaultSchema (V.fromList
  [ AT.defaultLeafField "id"   False (AT.AInt 64 True)
  , AT.defaultLeafField "name" False AT.AUtf8
  , AT.defaultLeafField "score" False (AT.AFloatingPoint AT.DoublePrecision)
  ])

mixedBatches :: [V.Vector AC.ColumnArray]
mixedBatches =
  [ V.fromList
      [ AC.ColInt64 (VP.fromList [10, 20, 30 :: Int64])
      , AC.ColUtf8 (V.fromList ["alice", "bob", "carol"])
      , AC.ColDouble (VP.fromList [1.5, 2.5, 3.5 :: Double])
      ]
  ]
