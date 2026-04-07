-- | Example: encode a person record to MessagePack and decode it back.
--
-- Run with: cabal run example-msgpack
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified MsgPack.Value as MP
import qualified MsgPack.Encode as MPE
import qualified MsgPack.Decode as MPD

main :: IO ()
main = do
  let person = MP.Map $ V.fromList
        [ (MP.String "name", MP.String "Alice")
        , (MP.String "age", MP.Int 30)
        , (MP.String "active", MP.Bool True)
        ]
  let bytes = MPE.encode person
  putStrLn $ "Encoded: " ++ show (BS.length bytes) ++ " bytes"
  case MPD.decode bytes of
    Right val -> putStrLn $ "Decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err

  let arr = MP.Array $ V.fromList [MP.Int 1, MP.Int 2, MP.Int 3]
  let arrBytes = MPE.encode arr
  putStrLn $ "Array encoded: " ++ show (BS.length arrBytes) ++ " bytes"
  case MPD.decode arrBytes of
    Right val -> putStrLn $ "Array decoded: " ++ show val
    Left err  -> putStrLn $ "Array error: " ++ err
