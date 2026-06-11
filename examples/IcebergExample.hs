{- | This format uses schema-driven codegen. For real usage:
wireform-gen iceberg -i spec.json -o src/Gen/
Then use the generated types directly.

Example: create Iceberg TableMetadata, serialize to JSON, and parse back.

Run with: cabal run example-iceberg
-}
module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Iceberg.JSON qualified as IJ
import Iceberg.Types qualified as Ice


main :: IO ()
main = do
  let schema =
        Ice.Schema
          { Ice.schemaId = 0
          , Ice.schemaFields =
              V.fromList
                [ mkField 1 "id" True Ice.TLong
                , mkField 2 "name" True Ice.TString
                , mkField 3 "ts" False Ice.TTimestamp
                ]
          , Ice.schemaIdentifierFieldIds = V.empty
          }

  let metadata =
        Ice.TableMetadata
          { Ice.tmFormatVersion = 2
          , Ice.tmTableUuid = "550e8400-e29b-41d4-a716-446655440000"
          , Ice.tmLocation = "s3://bucket/warehouse/db/table"
          , Ice.tmLastSequenceNumber = 0
          , Ice.tmLastUpdatedMs = 1700000000000
          , Ice.tmLastColumnId = 3
          , Ice.tmCurrentSchemaId = 0
          , Ice.tmSchemas = V.singleton schema
          , Ice.tmCurrentSnapshotId = Nothing
          , Ice.tmSnapshots = V.empty
          , Ice.tmPartitionSpecs = V.singleton (Ice.PartitionSpec 0 V.empty)
          , Ice.tmDefaultSpecId = 0
          , Ice.tmLastPartitionId = 0
          , Ice.tmSortOrders = V.singleton (Ice.SortOrder 0 V.empty)
          , Ice.tmDefaultSortOrderId = 0
          , Ice.tmProperties = Map.singleton "owner" "analytics"
          , Ice.tmSnapshotLog = V.empty
          , Ice.tmMetadataLog = V.empty
          , Ice.tmSnapshotRefs = Map.empty
          , Ice.tmStatistics = V.empty
          , Ice.tmPartitionStatistics = V.empty
          , Ice.tmNextRowId = Nothing
          , Ice.tmEncryptionKeys = Map.empty
          }

  let json = IJ.metadataToJSON metadata
  let jsonBytes = Aeson.encode json
  putStrLn $ "Serialized: " ++ show (BL.length jsonBytes) ++ " bytes"

  case IJ.metadataFromJSON json of
    Right tm' ->
      putStrLn $
        "Parsed back: format-version="
          ++ show (Ice.tmFormatVersion tm')
          ++ ", schemas="
          ++ show (V.length (Ice.tmSchemas tm'))
    Left err -> putStrLn $ "Error: " ++ err
  where
    mkField fid name req ty =
      Ice.StructField
        { Ice.sfId = fid
        , Ice.sfName = name
        , Ice.sfRequired = req
        , Ice.sfType = ty
        , Ice.sfDoc = Nothing
        , Ice.sfInitialDefault = Nothing
        , Ice.sfWriteDefault = Nothing
        }
