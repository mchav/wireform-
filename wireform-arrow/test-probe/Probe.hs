{-# LANGUAGE OverloadedStrings #-}
-- | Writes candidate Arrow IPC streams to disk for external testing
-- with pyarrow / arrow-cpp / arrow-rs, and exercises the reader by
-- consuming any pyarrow-produced sample handed in via @--read PATH@.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Arrow.Column (ColumnArray (..))
import Arrow.Types
import Arrow.FlatBufferIPC
  ( buildRecordBatchBytes
  , materializeRecordBatchFB
  , readArrowFileFB
  , readArrowStreamFB
  , writeArrowFileFB
  , writeArrowStreamFB
  , writeArrowStreamFBFromColumns
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--read" : path : _)      -> readMode readArrowStreamFB path
    ("--read-file" : path : _) -> readMode readArrowFileFB   path
    _                          -> writeMode (case args of { (d:_) -> d; [] -> "/tmp" })

readMode :: (BS.ByteString -> Either String (Schema, [(RecordBatchDef, BS.ByteString)])) -> FilePath -> IO ()
readMode reader path = do
  bs <- BS.readFile path
  case reader bs of
    Left e -> do
      putStrLn ("read error: " ++ e)
      exitFailure
    Right (sch, batches) -> do
      putStrLn ("schema: " ++ show sch)
      putStrLn ("batches: " ++ show (length batches))
      mapM_ (\(rb, body) -> do
                putStrLn $
                    "  rb len=" ++ show (rbLength rb)
                    ++ " nodes=" ++ show (V.length (rbNodes rb))
                    ++ " bufs="  ++ show (V.length (rbBuffers rb))
                    ++ " body="  ++ show (BS.length body) ++ " bytes"
                case materializeRecordBatchFB sch rb body of
                  Left e   -> putStrLn $ "    materialize: ERR " ++ e
                  Right cs -> putStrLn $ "    materialized: " ++ show (V.toList cs))
            batches

writeMode :: FilePath -> IO ()
writeMode outDir = do
  let schemaInt = Schema
        { arrowFields = V.singleton (Field "a" False (AInt 32 True) V.empty)
        , arrowEndianness = Little
        }
      col :: ColumnArray
      col = ColInt32 (VP.fromList ([1, 2, 3, 4, 5] :: [Int32]))
      batch = V.singleton col
  BS.writeFile (outDir <> "/ours_schema_only.arrows")
    (writeArrowStreamFB schemaInt [])
  BS.writeFile (outDir <> "/ours_int32_batch.arrows")
    (writeArrowStreamFBFromColumns schemaInt (V.singleton batch))

  -- A multi-type batch exercises alignment between buffers and
  -- variable-length encoding.
  let schemaMix = Schema
        { arrowFields = V.fromList
            [ Field "i"  False (AInt 64 True)      V.empty
            , Field "s"  False AUtf8               V.empty
            , Field "b"  True  ABool               V.empty
            ]
        , arrowEndianness = Little
        }
      mixCols = V.fromList
        [ ColInt64 (VP.fromList ([10, 20, 30] :: [Int64]))
        , ColUtf8  (V.fromList ["hello", "world", "!"])
        , ColBoolMaybe (V.fromList [Just True, Nothing, Just False])
        ]
  BS.writeFile (outDir <> "/ours_mixed_batch.arrows")
    (writeArrowStreamFBFromColumns schemaMix (V.singleton mixCols))

  -- Arrow file format (with proper Footer + Block index): same
  -- payload as the int32 stream above, exercised through
  -- writeArrowFileFB.
  let intBatchPair = buildRecordBatchBytes schemaInt batch
  BS.writeFile (outDir <> "/ours_int32_batch.arrow")
    (writeArrowFileFB schemaInt [intBatchPair])

  -- Post-V5 column types: Utf8View (with one inlined value and
  -- one out-of-line value to exercise the variadic buffer).
  let schemaView = Schema
        { arrowFields = V.singleton (Field "v" True AUtf8View V.empty)
        , arrowEndianness = Little
        }
      viewCols = V.singleton (ColUtf8ViewMaybe (V.fromList
        [ Just "short"
        , Nothing
        , Just "this string is definitely longer than twelve bytes"
        ]))
  BS.writeFile (outDir <> "/ours_utf8view.arrows")
    (writeArrowStreamFBFromColumns schemaView (V.singleton viewCols))

  -- RunEndEncoded(int32, int64) column.
  let schemaREE = Schema
        { arrowFields = V.singleton $
            Field "ree" True ARunEndEncoded $ V.fromList
              [ Field "run_ends" False (AInt 32 True) V.empty
              , Field "values"   True  (AInt 64 True) V.empty
              ]
        , arrowEndianness = Little
        }
      reeCols = V.singleton $ ColRunEndEncoded
                  (ColInt32 (VP.fromList ([3, 5, 8] :: [Int32])))
                  (ColInt64Maybe (V.fromList [Just 100, Nothing, Just 300]))
  BS.writeFile (outDir <> "/ours_ree.arrows")
    (writeArrowStreamFBFromColumns schemaREE (V.singleton reeCols))

  -- ListView<int32> column.
  let schemaListView = Schema
        { arrowFields = V.singleton $
            Field "lv" False AListView
              (V.singleton (Field "item" False (AInt 32 True) V.empty))
        , arrowEndianness = Little
        }
      lvCols = V.singleton (ColListView
                 (VP.fromList ([0, 2, 5] :: [Int32]))
                 (VP.fromList ([2, 3, 1] :: [Int32]))
                 (ColInt32 (VP.fromList ([10,20,30,40,50,60] :: [Int32]))))
  BS.writeFile (outDir <> "/ours_listview.arrows")
    (writeArrowStreamFBFromColumns schemaListView (V.singleton lvCols))

  putStrLn ("wrote probe outputs to " ++ outDir)
