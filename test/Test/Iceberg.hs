module Test.Iceberg (icebergTests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Types
import Iceberg.JSON (metadataToJSON, metadataFromJSON)
import Iceberg.Manifest (manifestEntrySchema, manifestFileSchema)
import Avro.Schema (AvroType(..))

icebergTests :: TestTree
icebergTests = testGroup "Iceberg"
  [ jsonRoundtripTests
  , schemaTypeTests
  , partitionSpecTests
  , manifestSchemaTests
  ]

-- ============================================================
-- JSON roundtrip
-- ============================================================

jsonRoundtripTests :: TestTree
jsonRoundtripTests = testGroup "JSON roundtrip"
  [ testCase "Minimal table metadata roundtrip" $ do
      let tm = minimalMetadata
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with snapshots" $ do
      let snap = Snapshot
            { snapId = 100
            , snapParentId = Nothing
            , snapSequenceNumber = 1
            , snapTimestampMs = 1672531200000
            , snapManifestList = "s3://bucket/manifest-list.avro"
            , snapSummary = Map.fromList [("operation", "append")]
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 100
            , tmSnapshots = V.singleton snap
            , tmSnapshotLog = V.singleton (SnapshotLogEntry 1672531200000 100)
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with snapshot parent" $ do
      let snap = Snapshot
            { snapId = 200
            , snapParentId = Just 100
            , snapSequenceNumber = 2
            , snapTimestampMs = 1672531300000
            , snapManifestList = "s3://bucket/manifest-list-2.avro"
            , snapSummary = Map.fromList [("operation", "overwrite")]
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 200
            , tmSnapshots = V.singleton snap
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with properties" $ do
      let tm = minimalMetadata
            { tmProperties = Map.fromList
                [ ("write.format.default", "parquet")
                , ("commit.retry.num-retries", "4")
                ]
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- Schema with various types
-- ============================================================

schemaTypeTests :: TestTree
schemaTypeTests = testGroup "Schema types"
  [ testCase "Primitive types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "bool_col" True TBoolean Nothing
            , StructField 2 "int_col" True TInt Nothing
            , StructField 3 "long_col" True TLong Nothing
            , StructField 4 "float_col" True TFloat Nothing
            , StructField 5 "double_col" True TDouble Nothing
            , StructField 6 "string_col" True TString Nothing
            , StructField 7 "binary_col" True TBinary Nothing
            , StructField 8 "uuid_col" False TUuid (Just "A UUID column")
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Date/time types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "date_col" True TDate Nothing
            , StructField 2 "time_col" True TTime Nothing
            , StructField 3 "ts_col" True TTimestamp Nothing
            , StructField 4 "tstz_col" True TTimestampTz Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Fixed and Decimal types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "fixed_col" True (TFixed 16) Nothing
            , StructField 2 "dec_col" True (TDecimal 10 2) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Nested struct type roundtrip" $ do
      let innerFields = V.fromList
            [ StructField 10 "x" True TInt Nothing
            , StructField 11 "y" True TInt Nothing
            ]
          fields = V.fromList
            [ StructField 1 "point" True (TStruct innerFields) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "List type roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "tags" True (TList 100 TString) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Map type roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "attrs" True (TMap 100 TString 101 TLong) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- PartitionSpec with transforms
-- ============================================================

partitionSpecTests :: TestTree
partitionSpecTests = testGroup "PartitionSpec"
  [ testCase "Identity transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "id_part" Identity))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Bucket transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "bucket_part" (Bucket 16)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Truncate transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 2 1001 "trunc_part" (Truncate 10)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Time-based transforms" $ do
      let ps = PartitionSpec 0 (V.fromList
            [ PartitionField 1 1000 "year_part" Year
            , PartitionField 2 1001 "month_part" Month
            , PartitionField 3 1002 "day_part" Day
            , PartitionField 4 1003 "hour_part" Hour
            ])
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Void transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "void_part" Void))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Sort orders roundtrip" $ do
      let so = SortOrder 1 (V.fromList
            [ SortField 1 Identity Asc NullsFirst
            , SortField 2 (Bucket 8) Desc NullsLast
            ])
          tm = minimalMetadata { tmSortOrders = V.singleton so }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- Manifest schema construction
-- ============================================================

manifestSchemaTests :: TestTree
manifestSchemaTests = testGroup "Manifest schemas"
  [ testCase "manifestEntrySchema is an AvroRecord" $
      case manifestEntrySchema of
        AvroRecord{avroRecordName = name} -> name @?= "manifest_entry"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestFileSchema is an AvroRecord" $
      case manifestFileSchema of
        AvroRecord{avroRecordName = name} -> name @?= "manifest_file"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestEntrySchema has correct namespace" $
      case manifestEntrySchema of
        AvroRecord{avroRecordNamespace = ns} -> ns @?= Just "org.apache.iceberg"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestFileSchema has expected fields" $
      case manifestFileSchema of
        AvroRecord{avroRecordFields = fields} ->
          V.length fields > 0 @?= True
        _ -> assertFailure "expected AvroRecord"
  ]

-- ============================================================
-- Helpers
-- ============================================================

minimalMetadata :: TableMetadata
minimalMetadata = TableMetadata
  { tmFormatVersion      = 2
  , tmTableUuid          = "550e8400-e29b-41d4-a716-446655440000"
  , tmLocation           = "s3://bucket/table"
  , tmLastSequenceNumber = 0
  , tmLastUpdatedMs      = 1672531200000
  , tmLastColumnId       = 0
  , tmCurrentSchemaId    = 0
  , tmSchemas            = V.singleton (Schema 0 V.empty)
  , tmCurrentSnapshotId  = Nothing
  , tmSnapshots          = V.empty
  , tmPartitionSpecs     = V.singleton (PartitionSpec 0 V.empty)
  , tmDefaultSpecId      = 0
  , tmSortOrders         = V.singleton (SortOrder 0 V.empty)
  , tmDefaultSortOrderId = 0
  , tmProperties         = Map.empty
  , tmSnapshotLog        = V.empty
  }
