-- | Example: encode a record to CBOR and decode it back.
--
-- Run with: cabal run example-cbor
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified CBOR.Value as C
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD

main :: IO ()
main = do
  let person = C.Map $ V.fromList
        [ (C.TextString "name", C.TextString "Bob")
        , (C.TextString "age", C.UInt 25)
        , (C.TextString "active", C.Bool True)
        ]
  let bytes = CE.encode person
  putStrLn $ "Encoded: " ++ show (BS.length bytes) ++ " bytes"
  case CD.decode bytes of
    Right val -> putStrLn $ "Decoded: " ++ show val
    Left err  -> putStrLn $ "Error: " ++ err

  let tagged = C.Tag 1 (C.UInt 1234567890)
  let tagBytes = CE.encode tagged
  putStrLn $ "Tagged encoded: " ++ show (BS.length tagBytes) ++ " bytes"
  case CD.decode tagBytes of
    Right val -> putStrLn $ "Tagged decoded: " ++ show val
    Left err  -> putStrLn $ "Tagged error: " ++ err
