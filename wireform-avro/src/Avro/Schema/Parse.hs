{- | Avro schema parsing from JSON.

Parses Avro schema definitions from JSON-encoded 'ByteString' data
(i.e. the contents of an @.avsc@ file). Delegates to 'Avro.JSON' for
the actual JSON-to-schema conversion.

@
import Avro.Schema.Parse (parseAvroSchema)
schema <- BS.readFile \"user.avsc\"
case parseAvroSchema schema of
  Right ty -> print ty
  Left err -> putStrLn err
@
-}
module Avro.Schema.Parse (
  parseAvroSchema,
  parseAvroSchemaFile,
) where

import Avro.JSON (avroSchemaFromJSON)
import Avro.Schema (AvroType)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS


{- | Parse an Avro schema from a JSON-encoded 'ByteString' (e.g. the contents
of a @.avsc@ file).
-}
parseAvroSchema :: ByteString -> Either String AvroType
parseAvroSchema bs =
  case Aeson.decodeStrict bs of
    Nothing -> Left "Avro.Schema.Parse: invalid JSON"
    Just v -> avroSchemaFromJSON v


-- | Parse an Avro schema from a @.avsc@ file on disk.
parseAvroSchemaFile :: FilePath -> IO (Either String AvroType)
parseAvroSchemaFile fp = parseAvroSchema <$> BS.readFile fp
