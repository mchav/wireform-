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
  ( readArrowStreamFB
  , writeArrowStreamFB
  , writeArrowStreamFBFromColumns
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--read" : path : _) -> readMode path
    _                     -> writeMode (case args of { (d:_) -> d; [] -> "/tmp" })

readMode :: FilePath -> IO ()
readMode path = do
  bs <- BS.readFile path
  case readArrowStreamFB bs of
    Left e -> do
      putStrLn ("read error: " ++ e)
      exitFailure
    Right (sch, batches) -> do
      putStrLn ("schema: " ++ show sch)
      putStrLn ("batches: " ++ show (length batches))
      mapM_ (\(rb, body) -> putStrLn $
                "  rb len=" ++ show (rbLength rb)
                ++ " nodes=" ++ show (V.length (rbNodes rb))
                ++ " bufs="  ++ show (V.length (rbBuffers rb))
                ++ " body="  ++ show (BS.length body) ++ " bytes")
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

  putStrLn ("wrote probe outputs to " ++ outDir)
