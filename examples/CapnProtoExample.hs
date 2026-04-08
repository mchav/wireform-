-- | This format uses schema-driven codegen. For real usage:
-- wireform-gen capnp -i schema.capnp -o src/Gen/
-- Then use the generated types directly.
--
-- Example: create a Cap'n Proto struct with data and pointer fields,
-- encode, and decode.
--
-- Run with: cabal run example-capnproto
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified CapnProto.Value as CP
import qualified CapnProto.Encode as CPE
import qualified CapnProto.Decode as CPD

main :: IO ()
main = do
  let val = CP.Struct
        (V.fromList [CP.Int64 42, CP.Float64 3.14])
        (V.fromList [CP.Text "hello capnp"])

  let bytes = CPE.encode val
  putStrLn $ "Struct encoded: " ++ show (BS.length bytes) ++ " bytes"
  case CPD.decode bytes of
    Right decoded -> putStrLn $ "Struct decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err

  let listVal = CP.List $ V.fromList [CP.UInt32 1, CP.UInt32 2, CP.UInt32 3]
  let listBytes = CPE.encode listVal
  putStrLn $ "List encoded: " ++ show (BS.length listBytes) ++ " bytes"
  case CPD.decode listBytes of
    Right decoded -> putStrLn $ "List decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err
