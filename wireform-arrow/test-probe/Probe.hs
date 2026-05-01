{-# LANGUAGE OverloadedStrings #-}
-- | Writes candidate Arrow IPC streams to disk for external testing
-- with pyarrow / arrow-cpp / arrow-rs, and exercises the reader by
-- consuming any pyarrow-produced sample handed in via @--read PATH@.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Arrow.Column (ColumnArray (..))
import Arrow.Types
import Arrow.Stream
  ( decodeArrowStream
  , encodeArrowFile
  , encodeArrowStream
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--read" : path : _) -> readMode path
    _                     -> writeMode (case args of { (d:_) -> d; [] -> "/tmp" })

-- | Read mode: consume any spec-compliant Arrow IPC stream and
-- print one batch per line as a 'show'd ColumnArray vector. Used
-- to verify pyarrow-produced bytes round-trip through our
-- high-level reader.
readMode :: FilePath -> IO ()
readMode path = do
  bs <- BS.readFile path
  case decodeArrowStream bs of
    Left e -> do
      putStrLn ("read error: " ++ e)
      exitFailure
    Right (sch, batches) -> do
      putStrLn ("schema: " ++ show sch)
      putStrLn ("batches: " ++ show (length batches))
      mapM_ (\cols ->
               putStrLn $ "  " ++ show (V.toList cols)) batches

-- | Smart constructor: a Field with no dictionary encoding.
pField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
pField nm nullable ty children =
  Field nm nullable ty children Nothing

-- | Field with a dictionary encoding.
dField :: Text -> Bool -> ArrowType -> Int64 -> Field
dField nm nullable ty did =
  Field nm nullable ty V.empty
    (Just (DictionaryEncoding did (AInt 32 True) False))

writeMode :: FilePath -> IO ()
writeMode outDir = do
  -- A representative gallery covering every column type the
  -- high-level API supports. The same input shape — Schema +
  -- [V.Vector ColumnArray] — works for primitive, variable-length,
  -- nullable, post-V5, and dictionary-encoded columns.
  let writeSample name sch batches = do
        BS.writeFile (outDir <> "/ours_" <> name <> ".arrows")
          (encodeArrowStream sch batches)

  -- 1) Pure primitives.
  writeSample "int32"
    Schema
      { arrowFields = V.singleton (pField "a" False (AInt 32 True) V.empty)
      , arrowEndianness = Little
      }
    [V.singleton (ColInt32 (VP.fromList ([1,2,3,4,5] :: [Int32])))]

  -- 2) Mixed primitives + variable-length + nullable.
  writeSample "mixed"
    Schema
      { arrowFields = V.fromList
          [ pField "i" False (AInt 64 True) V.empty
          , pField "s" False AUtf8           V.empty
          , pField "b" True  ABool           V.empty
          ]
      , arrowEndianness = Little
      }
    [V.fromList
       [ ColInt64 (VP.fromList ([10,20,30] :: [Int64]))
       , ColUtf8  (V.fromList ["hello","world","!"])
       , ColBoolMaybe (V.fromList [Just True, Nothing, Just False])
       ]]

  -- 3) Post-V5: Utf8View (inline + null + out-of-line).
  writeSample "utf8view"
    Schema
      { arrowFields = V.singleton (pField "v" True AUtf8View V.empty)
      , arrowEndianness = Little
      }
    [V.singleton (ColUtf8ViewMaybe (V.fromList
       [ Just "short"
       , Nothing
       , Just "this string is definitely longer than twelve bytes"
       ]))]

  -- 4) Post-V5: ListView<int32>.
  writeSample "listview"
    Schema
      { arrowFields = V.singleton $
          pField "lv" False AListView
            (V.singleton (pField "item" False (AInt 32 True) V.empty))
      , arrowEndianness = Little
      }
    [V.singleton (ColListView
        (VP.fromList ([0,2,5] :: [Int32]))
        (VP.fromList ([2,3,1] :: [Int32]))
        (ColInt32 (VP.fromList ([10,20,30,40,50,60] :: [Int32]))))]

  -- 5) Post-V5: RunEndEncoded.
  writeSample "ree"
    Schema
      { arrowFields = V.singleton $
          pField "ree" True ARunEndEncoded $ V.fromList
            [ pField "run_ends" False (AInt 32 True) V.empty
            , pField "values"   True  (AInt 64 True) V.empty
            ]
      , arrowEndianness = Little
      }
    [V.singleton (ColRunEndEncoded
        (ColInt32 (VP.fromList ([3,5,8] :: [Int32])))
        (ColInt64Maybe (V.fromList [Just 100, Nothing, Just 300])))]

  -- 6) Dictionary-encoded utf8 — handled automatically by the
  --    high-level API: no manual DictBatch construction.
  writeSample "dict"
    Schema
      { arrowFields = V.singleton (dField "d" True AUtf8 0)
      , arrowEndianness = Little
      }
    [V.singleton (ColDictionary 0
        (VP.fromList ([0,1,0,2,1] :: [Int32]))
        (ColUtf8 (V.fromList ["a","b","c"])))]

  -- File format with the same data as the int32 stream.
  let intSchema = Schema
        { arrowFields = V.singleton (pField "a" False (AInt 32 True) V.empty)
        , arrowEndianness = Little
        }
      intBatch = V.singleton (ColInt32 (VP.fromList ([1,2,3,4,5] :: [Int32])))
  BS.writeFile (outDir <> "/ours_int32_batch.arrow")
    (encodeArrowFile intSchema [intBatch])

  -- File format with a dictionary-encoded column. Exercises the
  -- footer's @dictionaries: [Block]@ slot.
  let dictSchema = Schema
        { arrowFields = V.singleton (dField "d" True AUtf8 0)
        , arrowEndianness = Little
        }
      dictBatch = V.singleton (ColDictionary 0
        (VP.fromList ([0,1,0,2,1] :: [Int32]))
        (ColUtf8 (V.fromList ["a","b","c"])))
  BS.writeFile (outDir <> "/ours_dict.arrow")
    (encodeArrowFile dictSchema [dictBatch])

  putStrLn ("wrote probe outputs to " ++ outDir)
