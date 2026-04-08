-- | Example: create a Bond struct, encode with Compact Binary, and decode.
--
-- Run with: cabal run example-bond
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Bond.Value as B
import qualified Bond.Encode as BE
import qualified Bond.Decode as BD

main :: IO ()
main = do
  let person = B.Struct V.empty $ V.fromList
        [ (1, B.BT_STRING, B.String "Frank")
        , (2, B.BT_INT32,  B.Int32 45)
        , (3, B.BT_BOOL,   B.Bool True)
        , (4, B.BT_DOUBLE, B.Double 1.75)
        , (5, B.BT_LIST,   B.List B.BT_INT32 $ V.fromList
                              [B.Int32 10, B.Int32 20, B.Int32 30])
        ]

  let bytes = BE.encode person
  putStrLn $ "Compact Binary encoded: " ++ show (BS.length bytes) ++ " bytes"

  case BD.decode B.BT_STRUCT bytes of
    Right val -> putStrLn $ "Decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err
