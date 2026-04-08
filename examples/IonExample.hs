-- | Example: create Ion values with annotations, encode to binary, and decode.
--
-- Run with: cabal run example-ion
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Ion.Value as I
import qualified Ion.Encode as IE
import qualified Ion.Decode as ID

main :: IO ()
main = do
  let val = I.Struct $ V.fromList
        [ ("name",   I.String "Bob")
        , ("age",    I.Int 25)
        , ("scores", I.List $ V.fromList [I.Int 95, I.Int 88, I.Int 72])
        ]

  let bytes = IE.encode val
  putStrLn $ "Struct encoded: " ++ show (BS.length bytes) ++ " bytes"
  case ID.decode bytes of
    Right decoded -> putStrLn $ "Struct decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err

  let annotated = I.Annotation "dollars" (I.Float 19.99)
  let annBytes = IE.encode annotated
  putStrLn $ "Annotated encoded: " ++ show (BS.length annBytes) ++ " bytes"
  case ID.decode annBytes of
    Right decoded -> putStrLn $ "Annotated decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err
