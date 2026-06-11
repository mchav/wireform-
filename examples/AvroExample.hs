{- | Avro is schema-driven: you define a schema and encode/decode values
against it. For real usage, use wireform-gen to generate Haskell types
from .avsc schema files. This example shows the schema-driven API directly.

Run with: cabal run example-avro
-}
module Main where

import Avro.Decode (decodeAvro)
import Avro.Encode (encodeAvro)
import Avro.Schema
import Avro.Value qualified as A
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V


main :: IO ()
main = do
  let schema =
        AvroRecord
          { avroRecordName = "Person"
          , avroRecordNamespace = Nothing
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordFields =
              V.fromList
                [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
                ]
          , avroRecordProps = Map.empty
          }

  let val = A.Record (V.fromList [A.String "Charlie", A.Int 42])
  let bytes = encodeAvro schema val
  putStrLn $ "Encoded: " ++ show (BS.length bytes) ++ " bytes"

  case decodeAvro schema bytes of
    Right decoded -> do
      putStrLn $ "Decoded: " ++ show decoded
      putStrLn $ "Roundtrip: " ++ show (decoded == val)
    Left err -> putStrLn $ "Error: " ++ err

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Approach 1: Schema-driven API (as shown above)

-- Approach 2: TH codegen from .avsc file
--   $(deriveAvroFromJSON [avsc|{"type":"record","name":"Person",...}|])

-- Approach 3: CLI codegen
--   wireform-gen avro -i person.avsc -o src/Gen/
