-- | Example: encode and decode Thrift values using binary and compact protocols.
--
-- Run with: cabal run example-thrift
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Thrift.Value as T
import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Decode (decodeBinary, decodeCompact)

main :: IO ()
main = do
  let person = T.Struct $ V.fromList
        [ (1, T.String "Diana")
        , (2, T.I32 28)
        , (3, T.Bool True)
        ]

  -- Binary protocol
  let binBytes = encodeBinary person
  putStrLn $ "Binary encoded: " ++ show (BS.length binBytes) ++ " bytes"
  case decodeBinary binBytes of
    Right val -> putStrLn $ "Binary decoded: " ++ show val
    Left err  -> putStrLn $ "Binary error: " ++ err

  -- Compact protocol
  let compactBytes = encodeCompact person
  putStrLn $ "Compact encoded: " ++ show (BS.length compactBytes) ++ " bytes"
  case decodeCompact compactBytes of
    Right val -> putStrLn $ "Compact decoded: " ++ show val
    Left err  -> putStrLn $ "Compact error: " ++ err
