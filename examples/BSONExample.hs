-- | Example: create a BSON document with nested values, encode, and decode.
--
-- Run with: cabal run example-bson
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified BSON.Value as B
import qualified BSON.Encode as BE
import qualified BSON.Decode as BD

main :: IO ()
main = do
  let address = B.Document $ V.fromList
        [ ("street", B.String "123 Main St")
        , ("city",   B.String "Springfield")
        , ("zip",    B.Int32 62701)
        ]
  let person = B.Document $ V.fromList
        [ ("name",    B.String "Alice")
        , ("age",     B.Int32 30)
        , ("active",  B.Bool True)
        , ("score",   B.Double 98.5)
        , ("address", address)
        , ("tags",    B.Array $ V.fromList [B.String "admin", B.String "user"])
        ]

  let bytes = BE.encode person
  putStrLn $ "Encoded: " ++ show (BS.length bytes) ++ " bytes"

  case BD.decode bytes of
    Right val -> putStrLn $ "Decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err
