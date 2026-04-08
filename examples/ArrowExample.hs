-- | This format uses schema-driven codegen. For real usage:
-- wireform-gen arrow -i schema.fbs -o src/Gen/
-- Then use the generated types directly.
--
-- Example: create an Arrow Schema, encode as IPC message, and decode.
--
-- Run with: cabal run example-arrow
module Main where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Arrow.Types as A
import qualified Arrow.IPC as AIPC

main :: IO ()
main = do
  let schema = A.Schema
        { A.arrowFields = V.fromList
            [ A.Field "id" False (A.AInt 64 True) V.empty
            , A.Field "name" True A.AUtf8 V.empty
            , A.Field "score" False (A.AFloatingPoint A.DoublePrecision) V.empty
            , A.Field "active" True A.ABool V.empty
            ]
        , A.arrowEndianness = A.Little
        }

  let msg = A.SchemaMessage schema
  let bytes = AIPC.encodeIPCMessage msg
  putStrLn $ "Arrow IPC encoded: " ++ show (BS.length bytes) ++ " bytes"

  case AIPC.decodeIPCMessage bytes of
    Right (A.SchemaMessage s) ->
      putStrLn $ "Decoded schema: " ++ show (V.length (A.arrowFields s)) ++ " fields"
    Right other -> putStrLn $ "Decoded: " ++ show other
    Left err    -> putStrLn $ "Error: " ++ err
