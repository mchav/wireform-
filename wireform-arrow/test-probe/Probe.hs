{-# LANGUAGE OverloadedStrings #-}
-- | Writes a few candidate Arrow IPC streams to disk so we can test
-- with an external reader (pyarrow). This is a probe exe, not a
-- test suite — pyarrow isn't available on every dev machine.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import System.Environment (getArgs)

import Arrow.Types
import Arrow.FlatBufferIPC (writeArrowStreamFB)

main :: IO ()
main = do
  args <- getArgs
  let outDir = case args of { (d:_) -> d; [] -> "/tmp" }
  let schema1 = Schema
        { arrowFields = V.fromList
            [ Field "a" False (AInt 32 True) V.empty
            , Field "b" True  AUtf8          V.empty
            ]
        , arrowEndianness = Little
        }
  BS.writeFile (outDir <> "/ours_schema_only.arrows")
    (writeArrowStreamFB schema1 [])
  putStrLn ("wrote probe outputs to " ++ outDir)
