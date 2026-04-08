-- | Example: create a Python-style dict, encode as Pickle protocol 2, and decode.
--
-- Run with: cabal run example-pickle
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Pickle.Value as P
import qualified Pickle.Encode as PE
import qualified Pickle.Decode as PD

main :: IO ()
main = do
  let pyDict = P.Dict $ V.fromList
        [ (P.String "name",   P.String "Grace")
        , (P.String "age",    P.Int 29)
        , (P.String "scores", P.List $ V.fromList [P.Int 100, P.Int 95, P.Int 88])
        , (P.String "active", P.Bool True)
        ]

  let bytes = PE.encode pyDict
  putStrLn $ "Pickle encoded: " ++ show (BS.length bytes) ++ " bytes"

  case PD.decode bytes of
    Right val -> putStrLn $ "Decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err

  let tup = P.Tuple $ V.fromList [P.Int 1, P.String "two", P.None]
  let tupBytes = PE.encode tup
  putStrLn $ "Tuple encoded: " ++ show (BS.length tupBytes) ++ " bytes"
  case PD.decode tupBytes of
    Right val -> putStrLn $ "Tuple decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err
