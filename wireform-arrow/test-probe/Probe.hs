{-# LANGUAGE OverloadedStrings #-}
-- | Writes candidate Arrow IPC streams to disk for external testing
-- with pyarrow / arrow-cpp / arrow-rs, and exercises the reader by
-- consuming any pyarrow-produced sample handed in via @--read PATH@.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Int as Int
import Data.Int (Int32, Int64)
import Control.Monad (forM)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Arrow.Column (ColumnArray (..), materializeRecordBatch, resolveDictionaryColumn)
import Arrow.Types
import Arrow.FlatBufferIPC
  ( DictBatch (..)
  , buildRecordBatchBytes
  , denormaliseBuffers
  , materializeRecordBatchFB
  , readArrowFileFB
  , readArrowStreamFB
  , readArrowStreamFBWithDicts
  , writeArrowFileFB
  , writeArrowStreamFB
  , writeArrowStreamFBFromColumns
  , writeArrowStreamFBWithDicts
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--read" : path : _)      -> readMode readArrowStreamFB path
    ("--read-file" : path : _) -> readMode readArrowFileFB   path
    ("--read-dicts" : path : _) -> readWithDicts path
    _                          -> writeMode (case args of { (d:_) -> d; [] -> "/tmp" })

readWithDicts :: FilePath -> IO ()
readWithDicts path = do
  bs <- BS.readFile path
  case readArrowStreamFBWithDicts bs of
    Left e -> do
      putStrLn ("read error: " ++ e)
      exitFailure
    Right (sch, dicts, batches) -> do
      putStrLn ("schema: " ++ show sch)
      putStrLn ("dicts: " ++ show (length dicts))
      -- Materialise each dict batch's data column and stash it
      -- keyed by id.
      dictMap <- forM' dicts $ \db -> do
        let dRb  = denormaliseBuffers (dictSchemaFor sch (dbId db)) (dbData db)
            dCols = case materializeRecordBatch (dictSchemaFor sch (dbId db)) dRb (dbBody db) of
              Right cs | not (V.null cs) -> Just (V.head cs)
              _                          -> Nothing
        pure (dbId db, dCols)
      putStrLn ("batches: " ++ show (length batches))
      mapM_ (\(rb, body) -> do
                case materializeRecordBatchFB sch rb body of
                  Left e -> putStrLn $ "    materialize: ERR " ++ e
                  Right cs ->
                    let !resolved = V.map (resolveDictionaryColumn (lookupDict dictMap)) cs
                    in  putStrLn $ "    materialized: " ++ show (V.toList resolved))
            batches
  where
    lookupDict dm did = case lookup did dm of
      Just (Just c) -> Just c
      _             -> Nothing
    forM' xs f = traverse f xs

-- | Build a synthetic single-field schema describing the values
-- column inside a 'DictBatch' with dictionary id @did@. Looks up
-- the matching field in the input schema and strips the
-- @fieldDictionary@ encoding so the materializer treats it as a
-- regular value column.
dictSchemaFor :: Schema -> Int.Int64 -> Schema
dictSchemaFor sch did =
  let !match = case V.find (\f ->
                  case fieldDictionary f of
                    Just de -> deId de == did
                    Nothing -> False) (arrowFields sch) of
                 Just f  -> f { fieldDictionary = Nothing, fieldChildren = V.empty }
                 Nothing -> Field "v" True AUtf8 V.empty Nothing
  in  sch { arrowFields = V.singleton match }

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

-- | Smart constructor: a Field with no dictionary encoding.
pField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
pField nm nullable ty children =
  Field nm nullable ty children Nothing

writeMode :: FilePath -> IO ()
writeMode outDir = do
  let schemaInt = Schema
        { arrowFields = V.singleton (pField "a" False (AInt 32 True) V.empty)
        , arrowEndianness = Little
        }
      col :: ColumnArray
      col = ColInt32 (VP.fromList ([1, 2, 3, 4, 5] :: [Int32]))
      batch = V.singleton col
  BS.writeFile (outDir <> "/ours_schema_only.arrows")
    (writeArrowStreamFB schemaInt [])
  BS.writeFile (outDir <> "/ours_int32_batch.arrows")
    (writeArrowStreamFBFromColumns schemaInt (V.singleton batch))

  let schemaMix = Schema
        { arrowFields = V.fromList
            [ pField "i"  False (AInt 64 True)      V.empty
            , pField "s"  False AUtf8               V.empty
            , pField "b"  True  ABool               V.empty
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

  let intBatchPair = buildRecordBatchBytes schemaInt batch
  BS.writeFile (outDir <> "/ours_int32_batch.arrow")
    (writeArrowFileFB schemaInt [intBatchPair])

  let schemaView = Schema
        { arrowFields = V.singleton (pField "v" True AUtf8View V.empty)
        , arrowEndianness = Little
        }
      viewCols = V.singleton (ColUtf8ViewMaybe (V.fromList
        [ Just "short"
        , Nothing
        , Just "this string is definitely longer than twelve bytes"
        ]))
  BS.writeFile (outDir <> "/ours_utf8view.arrows")
    (writeArrowStreamFBFromColumns schemaView (V.singleton viewCols))

  let schemaREE = Schema
        { arrowFields = V.singleton $
            pField "ree" True ARunEndEncoded $ V.fromList
              [ pField "run_ends" False (AInt 32 True) V.empty
              , pField "values"   True  (AInt 64 True) V.empty
              ]
        , arrowEndianness = Little
        }
      reeCols = V.singleton $ ColRunEndEncoded
                  (ColInt32 (VP.fromList ([3, 5, 8] :: [Int32])))
                  (ColInt64Maybe (V.fromList [Just 100, Nothing, Just 300]))
  BS.writeFile (outDir <> "/ours_ree.arrows")
    (writeArrowStreamFBFromColumns schemaREE (V.singleton reeCols))

  -- Dictionary-encoded utf8 column. We emit a dict batch (id=0)
  -- defining 3 string values, then a record batch holding indices.
  let schemaDict = Schema
        { arrowFields = V.singleton $
            Field "d" True AUtf8 V.empty
                  (Just (DictionaryEncoding 0 (AInt 32 True) False))
        , arrowEndianness = Little
        }
      dictValues = ColUtf8 (V.fromList ["a","b","c"])
      dictValuesSchema = Schema
        { arrowFields = V.singleton (pField "d" False AUtf8 V.empty)
        , arrowEndianness = Little
        }
      (dictRb, dictBody) = buildRecordBatchBytes dictValuesSchema (V.singleton dictValues)
      dictBatch = DictBatch
        { dbId      = 0
        , dbIsDelta = False
        , dbData    = dictRb
        , dbBody    = dictBody
        }
      dictIndices = ColInt32Maybe (V.fromList
        [Just 0, Just 1, Just 0, Just 2, Just 1])
      -- The record batch carries the indices using the original
      -- dict-encoded schema.
      indicesSchema = Schema
        { arrowFields = V.singleton (pField "d" True (AInt 32 True) V.empty)
        , arrowEndianness = Little
        }
      (indicesRb, indicesBody) =
        buildRecordBatchBytes indicesSchema (V.singleton dictIndices)
  BS.writeFile (outDir <> "/ours_dict.arrows")
    (writeArrowStreamFBWithDicts schemaDict [dictBatch]
       [(indicesRb, indicesBody)])

  let schemaListView = Schema
        { arrowFields = V.singleton $
            pField "lv" False AListView
              (V.singleton (pField "item" False (AInt 32 True) V.empty))
        , arrowEndianness = Little
        }
      lvCols = V.singleton (ColListView
                 (VP.fromList ([0, 2, 5] :: [Int32]))
                 (VP.fromList ([2, 3, 1] :: [Int32]))
                 (ColInt32 (VP.fromList ([10,20,30,40,50,60] :: [Int32]))))
  BS.writeFile (outDir <> "/ours_listview.arrows")
    (writeArrowStreamFBFromColumns schemaListView (V.singleton lvCols))

  putStrLn ("wrote probe outputs to " ++ outDir)
