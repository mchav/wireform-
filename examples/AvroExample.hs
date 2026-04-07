-- | Example: encode and decode Avro values with a schema.
--
-- Run with: cabal run example-avro
module Main where

import qualified Data.ByteString as BS
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
            [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing
            , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing
            ]
        }

  let val = A.Record (V.fromList [A.String "Charlie", A.Int 42])
  let bytes = encodeAvro schema val
  putStrLn $ "Encoded: " ++ show (BS.length bytes) ++ " bytes"

  case decodeAvro schema bytes of
    Right decoded -> putStrLn $ "Decoded: " ++ show decoded
    Left err      -> putStrLn $ "Error: " ++ err

  let intSchema = AvroPrimitive AvroLong
      intVal = A.Long 1234567890
      intBytes = encodeAvro intSchema intVal
  putStrLn $ "Long encoded: " ++ show (BS.length intBytes) ++ " bytes"
  case decodeAvro intSchema intBytes of
    Right decoded -> putStrLn $ "Long decoded: " ++ show decoded
    Left err      -> putStrLn $ "Long error: " ++ err
