-- | Example: create Iceberg TableMetadata, serialize to JSON, and parse back.
--
-- Run with: cabal run example-iceberg
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Iceberg.Types as Ice
import qualified Iceberg.JSON as IJ
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  let schema = Ice.Schema
        { Ice.schemaId = 0
        , Ice.schemaFields = V.fromList
            [ Ice.StructField 1 "id" True Ice.TLong Nothing
            , Ice.StructField 2 "name" True Ice.TString Nothing
            , Ice.StructField 3 "ts" False Ice.TTimestamp Nothing
            ]
        }

  let metadata = Ice.TableMetadata
        { Ice.tmFormatVersion      = 2
        , Ice.tmTableUuid          = "550e8400-e29b-41d4-a716-446655440000"
        , Ice.tmLocation           = "s3://bucket/warehouse/db/table"
        , Ice.tmLastSequenceNumber = 0
        , Ice.tmLastUpdatedMs      = 1700000000000
        , Ice.tmLastColumnId       = 3
        , Ice.tmCurrentSchemaId    = 0
        , Ice.tmSchemas            = V.singleton schema
        , Ice.tmCurrentSnapshotId  = Nothing
        , Ice.tmSnapshots          = V.empty
        , Ice.tmPartitionSpecs     = V.singleton (Ice.PartitionSpec 0 V.empty)
        , Ice.tmDefaultSpecId      = 0
        , Ice.tmSortOrders         = V.singleton (Ice.SortOrder 0 V.empty)
        , Ice.tmDefaultSortOrderId = 0
        , Ice.tmProperties         = Map.singleton "owner" "analytics"
        , Ice.tmSnapshotLog        = V.empty
        }

  let json = IJ.metadataToJSON metadata
  let jsonBytes = Aeson.encode json
  putStrLn $ "Serialized: " ++ show (BL.length jsonBytes) ++ " bytes"

  case IJ.metadataFromJSON json of
    Right tm' -> putStrLn $ "Parsed back: format-version=" ++ show (Ice.tmFormatVersion tm')
                          ++ ", schemas=" ++ show (V.length (Ice.tmSchemas tm'))
    Left err  -> putStrLn $ "Error: " ++ err
