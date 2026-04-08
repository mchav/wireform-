-- | Avro is schema-driven: you define a schema and encode/decode values
-- against it. For real usage, use wireform-gen to generate Haskell types
-- from .avsc schema files. This example shows the schema-driven API directly.
--
-- Run with: cabal run example-avro
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Avro.Value as A
import Avro.Encode (encodeAvro)
import Avro.Decode (decodeAvro)
import Avro.Schema

main :: IO ()
main = do
  let schema = AvroRecord
        { avroRecordName = "Person"
        , avroRecordNamespace = Nothing
        , avroRecordDoc = Nothing
        , avroRecordAliases = V.empty
        , avroRecordFields = V.fromList
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
