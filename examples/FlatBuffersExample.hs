-- | This format uses schema-driven codegen. For real usage:
-- wireform-gen flatbuffers -i schema.fbs -o src/Gen/
-- Then use the generated types directly.
--
-- Example: create a FlatBuffers table with mixed fields, encode, and decode.
--
-- Run with: cabal run example-flatbuffers
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified FlatBuffers.Value as FB
import qualified FlatBuffers.Encode as FBE
import qualified FlatBuffers.Decode as FBD

main :: IO ()
main = do
  let table = FB.VTable $ V.fromList
        [ Just (FB.VString "wireform")
        , Just (FB.VInt32 42)
        , Just (FB.VDouble 2.718)
        , Just (FB.VBool True)
        , Nothing
        ]

  let bytes = FBE.encode table
  putStrLn $ "Table encoded: " ++ show (BS.length bytes) ++ " bytes"
  case FBD.decode bytes of
    Right decoded -> putStrLn $ "Table decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err

  -- A spec-compliant flatbuffer always has a table root, so we
  -- wrap a vector in a single-slot table to encode it.
  let vec = FB.VTable $ V.singleton (Just (FB.VVector
              (V.fromList [FB.VInt32 10, FB.VInt32 20, FB.VInt32 30])))
  let vecBytes = FBE.encode vec
  putStrLn $ "Vector encoded: " ++ show (BS.length vecBytes) ++ " bytes"
  case FBD.decode vecBytes of
    Right decoded -> putStrLn $ "Vector decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Approach 1: Schema-driven API (as shown above)

-- Approach 2: TH from .fbs IDL
--   [fbs|
--     table Person {
--       name:string;
--       age:int;
--     }
--   |]

-- Approach 3: CLI codegen
--   wireform-gen flatbuffers -i schema.fbs -o src/Gen/
