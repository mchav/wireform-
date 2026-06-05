{-# LANGUAGE RecordWildCards #-}
module Test.Iceberg (icebergTests) where

import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Syd

import Avro.Container (writeContainer)
import Avro.Schema (AvroType (..))
import qualified Avro.Value as AV
import Iceberg.JSON (metadataFromJSON, metadataToJSON)
import Iceberg.Manifest (manifestEntrySchema, manifestFileSchema)
import Iceberg.Read
  ( applyPositionDeletes
  , dataManifestPaths
  , deleteManifestPaths
  , positionDeletesFromColumns
  , readManifestEntries
  , readManifestList
  , ScanPlan(..)
  , planScan
  )
import Iceberg.SchemaEvolution
  (currentSchema, findFieldById, projectSchema, schemaById)
import Iceberg.Snapshot
  ( applicableDeletes
  , currentPartitionSpec
  , currentSnapshot
  , filterBySequenceNumber
  , snapshotById
  , snapshotParentChain
  )
import Iceberg.Types

-- | Smart constructor matching the legacy positional StructField shape.
mkSF :: Int -> Text -> Bool -> IcebergType -> Maybe Text -> StructField
mkSF i n r t d = StructField
  { sfId = i, sfName = n, sfRequired = r, sfType = t, sfDoc = d
  , sfInitialDefault = Nothing, sfWriteDefault = Nothing
  }

mkSchema :: Int -> V.Vector StructField -> Schema
mkSchema sid fs = Schema { schemaId = sid, schemaFields = fs, schemaIdentifierFieldIds = V.empty }

mkSnap
  :: Int64 -> Maybe Int64 -> Int64 -> Int64 -> Text -> Map.Map Text Text -> Snapshot
mkSnap sid pid sn ts ml summ = Snapshot
  { snapId = sid, snapParentId = pid, snapSequenceNumber = sn
  , snapTimestampMs = ts, snapManifestList = ml, snapSummary = summ
  , snapSchemaId = Nothing, snapFirstRowId = Nothing, snapKeyId = Nothing
  }

icebergTests :: Spec
icebergTests = describe "Iceberg" $ sequence_
  [ jsonRoundtripTests
  , schemaTypeTests
  , partitionSpecTests
  , manifestSchemaTests
  , manifestAvroTests
  , snapshotTests
  , schemaEvolutionTests
  , deleteFileTests
  , positionDeleteTests
  , manifestPathFilterTests
  , sequenceNumberTests
  , scanPlanDeleteTests
  , snapshotRefTests
  ]

-- ============================================================
-- JSON roundtrip
-- ============================================================

jsonRoundtripTests :: Spec
jsonRoundtripTests = describe "JSON roundtrip" $ sequence_
  [ it "Minimal table metadata roundtrip" $ do
      let tm = minimalMetadata
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Table metadata with snapshots" $ do
      let snap = (mkSnap 100 Nothing 1 1672531200000
                         "s3://bucket/manifest-list.avro"
                         (Map.fromList [("operation", "append")]))
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 100
            , tmSnapshots = V.singleton snap
            , tmSnapshotLog = V.singleton (SnapshotLogEntry 1672531200000 100)
            }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Table metadata with snapshot parent" $ do
      let snap = (mkSnap 200 (Just 100) 2 1672531300000
                         "s3://bucket/manifest-list-2.avro"
                         (Map.fromList [("operation", "overwrite")]))
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 200
            , tmSnapshots = V.singleton snap
            }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Table metadata with properties" $ do
      let tm = minimalMetadata
            { tmProperties = Map.fromList
                [ ("write.format.default", "parquet")
                , ("commit.retry.num-retries", "4")
                ]
            }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm
  ]

-- ============================================================
-- Schema with various types
-- ============================================================

schemaTypeTests :: Spec
schemaTypeTests = describe "Schema types" $ sequence_
  [ it "Primitive types roundtrip" $ do
      let fields = V.fromList
            [ mkSF 1 "bool_col" True TBoolean Nothing
            , mkSF 2 "int_col" True TInt Nothing
            , mkSF 3 "long_col" True TLong Nothing
            , mkSF 4 "float_col" True TFloat Nothing
            , mkSF 5 "double_col" True TDouble Nothing
            , mkSF 6 "string_col" True TString Nothing
            , mkSF 7 "binary_col" True TBinary Nothing
            , mkSF 8 "uuid_col" False TUuid (Just "A UUID column")
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Date/time types roundtrip" $ do
      let fields = V.fromList
            [ mkSF 1 "date_col" True TDate Nothing
            , mkSF 2 "time_col" True TTime Nothing
            , mkSF 3 "ts_col" True TTimestamp Nothing
            , mkSF 4 "tstz_col" True TTimestampTz Nothing
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Fixed and Decimal types roundtrip" $ do
      let fields = V.fromList
            [ mkSF 1 "fixed_col" True (TFixed 16) Nothing
            , mkSF 2 "dec_col" True (TDecimal 10 2) Nothing
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Nested struct type roundtrip" $ do
      let innerFields = V.fromList
            [ mkSF 10 "x" True TInt Nothing
            , mkSF 11 "y" True TInt Nothing
            ]
          fields = V.fromList
            [ mkSF 1 "point" True (TStruct innerFields) Nothing
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "List type roundtrip" $ do
      let fields = V.fromList
            [ mkSF 1 "tags" True (TList 100 TString) Nothing
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Map type roundtrip" $ do
      let fields = V.fromList
            [ mkSF 1 "attrs" True (TMap 100 TString 101 TLong) Nothing
            ]
          schema = mkSchema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm
  ]

-- ============================================================
-- PartitionSpec with transforms
-- ============================================================

partitionSpecTests :: Spec
partitionSpecTests = describe "PartitionSpec" $ sequence_
  [ it "Identity transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField (V.singleton 1) 1000 "id_part" Identity))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Bucket transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField (V.singleton 1) 1000 "bucket_part" (Bucket 16)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Truncate transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField (V.singleton 2) 1001 "trunc_part" (Truncate 10)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Time-based transforms" $ do
      let ps = PartitionSpec 0 (V.fromList
            [ PartitionField (V.singleton 1) 1000 "year_part" Year
            , PartitionField (V.singleton 2) 1001 "month_part" Month
            , PartitionField (V.singleton 3) 1002 "day_part" Day
            , PartitionField (V.singleton 4) 1003 "hour_part" Hour
            ])
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Void transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField (V.singleton 1) 1000 "void_part" Void))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "Sort orders roundtrip" $ do
      let so = SortOrder 1 (V.fromList
            [ SortField 1 Identity Asc NullsFirst
            , SortField 2 (Bucket 8) Desc NullsLast
            ])
          tm = minimalMetadata { tmSortOrders = V.singleton so }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm
  ]

-- ============================================================
-- Manifest schema construction
-- ============================================================

manifestSchemaTests :: Spec
manifestSchemaTests = describe "Manifest schemas" $ sequence_
  [ it "manifestEntrySchema is an AvroRecord" $
      case manifestEntrySchema of
        AvroRecord{avroRecordName = name} -> name `shouldBe` "manifest_entry"
        _ -> expectationFailure "expected AvroRecord"

  , it "manifestFileSchema is an AvroRecord" $
      case manifestFileSchema of
        AvroRecord{avroRecordName = name} -> name `shouldBe` "manifest_file"
        _ -> expectationFailure "expected AvroRecord"

  , it "manifestEntrySchema has correct namespace" $
      case manifestEntrySchema of
        AvroRecord{avroRecordNamespace = ns} -> ns `shouldBe` Just "org.apache.iceberg"
        _ -> expectationFailure "expected AvroRecord"

  , it "manifestFileSchema has expected fields" $
      case manifestFileSchema of
        AvroRecord{avroRecordFields = fields} ->
          V.length fields > 0 `shouldBe` True
        _ -> expectationFailure "expected AvroRecord"
  ]

-- ============================================================
-- Avro manifest / manifest-list containers
-- ============================================================

manifestAvroTests :: Spec
manifestAvroTests = describe "Manifest Avro" $ sequence_
  [ it "roundtrip manifest entry container" $ do
      let u0 = AV.Union 0 AV.Null
          dataFile =
            AV.Record $
              V.fromList
                [ AV.String "s3://b/f.parquet"
                , AV.String "parquet"
                , AV.Record V.empty
                , AV.Long 42
                , AV.Long 2048
                , AV.Long 67108864
                , u0
                , u0
                , u0
                , u0
                , u0
                ]
          entry =
            AV.Record $
              V.fromList
                [ AV.Int 1
                , AV.Union 1 (AV.Long 99)
                , u0
                , AV.Union 1 (AV.Long 7)
                , dataFile
                ]
          bs = writeContainer manifestEntrySchema (V.singleton entry)
      case readManifestEntries bs of
        Left e -> expectationFailure e
        Right (_, vec) -> do
          V.length vec `shouldBe` 1
          let me = V.unsafeIndex vec 0
          meStatus me `shouldBe` Added
          meSnapshotId me `shouldBe` Just 99
          meSequenceNumber me `shouldBe` Nothing
          meFileSequenceNumber me `shouldBe` Just 7
          meFilePath me `shouldBe` "s3://b/f.parquet"
          meFileFormat me `shouldBe` ParquetFormat
          meRecordCount me `shouldBe` 42
          meFileSizeBytes me `shouldBe` 2048
          V.length (mePartition me) `shouldBe` 0

  , it "roundtrip manifest list container" $ do
      let u0 = AV.Union 0 AV.Null
          mf =
            AV.Record $
              V.fromList
                [ AV.String "s3://bucket/m1.avro"
                , AV.Long 500
                , AV.Int 0
                , AV.Int 0
                , AV.Long 10
                , AV.Long 5
                , AV.Long 99
                , u0
                , u0
                , u0
                , u0
                , u0
                , u0
                ]
          bs = writeContainer manifestFileSchema (V.singleton mf)
      case readManifestList bs of
        Left e -> expectationFailure e
        Right (_, vec) -> do
          V.length vec `shouldBe` 1
          let m = V.unsafeIndex vec 0
          mfPath m `shouldBe` "s3://bucket/m1.avro"
          mfLength m `shouldBe` 500
          mfPartitionSpecId m `shouldBe` 0
          mfContent m `shouldBe` DataContent
          mfSequenceNumber m `shouldBe` 10
          mfMinSequenceNumber m `shouldBe` 5
          mfAddedSnapshotId m `shouldBe` 99
          mfAddedDataFilesCount m `shouldBe` Nothing
          mfExistingDataFilesCount m `shouldBe` Nothing
          mfDeletedDataFilesCount m `shouldBe` Nothing
          mfAddedRowsCount m `shouldBe` Nothing
          mfExistingRowsCount m `shouldBe` Nothing
          mfDeletedRowsCount m `shouldBe` Nothing
  ]

-- ============================================================
-- Snapshot operations
-- ============================================================

snapshotTests :: Spec
snapshotTests = describe "Snapshot operations" $ sequence_
  [ it "currentSnapshot returns matching snapshot" $ do
      let snap = Snapshot
            { snapId = 42
            , snapParentId = Nothing
            , snapSequenceNumber = 1
            , snapTimestampMs = 1000
            , snapManifestList = "s3://bucket/ml.avro"
            , snapSummary = Map.singleton "operation" "append"
            , snapSchemaId = Nothing
            , snapFirstRowId = Nothing
            , snapKeyId = Nothing
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 42
            , tmSnapshots = V.singleton snap
            }
      currentSnapshot tm `shouldBe` Just snap

  , it "currentSnapshot returns Nothing when no current ID" $ do
      let tm = minimalMetadata { tmCurrentSnapshotId = Nothing }
      currentSnapshot tm `shouldBe` Nothing

  , it "currentSnapshot returns Nothing for missing ID" $ do
      let snap = mkSnap 1 Nothing 1 1000 "ml.avro" Map.empty
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 999
            , tmSnapshots = V.singleton snap
            }
      currentSnapshot tm `shouldBe` Nothing

  , it "snapshotById finds the right snapshot" $ do
      let s1 = mkSnap 10 Nothing 1 1000 "ml1.avro" Map.empty
          s2 = mkSnap 20 (Just 10) 2 2000 "ml2.avro" Map.empty
          tm = minimalMetadata { tmSnapshots = V.fromList [s1, s2] }
      snapshotById tm 20 `shouldBe` Just s2
      snapshotById tm 10 `shouldBe` Just s1
      snapshotById tm 99 `shouldBe` Nothing

  , it "snapshotParentChain with 3 snapshots" $ do
      let s1 = mkSnap 1 Nothing   1 1000 "ml1.avro" Map.empty
          s2 = mkSnap 2 (Just 1)  2 2000 "ml2.avro" Map.empty
          s3 = mkSnap 3 (Just 2)  3 3000 "ml3.avro" Map.empty
          tm = minimalMetadata
            { tmSnapshots = V.fromList [s1, s2, s3] }
          chain = snapshotParentChain tm s3
      case chain of
        [p1, p2] -> do
          snapId p1 `shouldBe` 2
          snapId p2 `shouldBe` 1
        _ -> expectationFailure ("expected 2-element chain, got " ++ show (length chain))

  , it "snapshotParentChain stops at root" $ do
      let s1 = mkSnap 1 Nothing 1 1000 "ml1.avro" Map.empty
          tm = minimalMetadata { tmSnapshots = V.singleton s1 }
      snapshotParentChain tm s1 `shouldBe` []

  , it "currentPartitionSpec returns default spec" $ do
      let ps = PartitionSpec 5 (V.singleton (PartitionField (V.singleton 1) 1000 "p" Identity))
          tm = minimalMetadata
            { tmPartitionSpecs = V.fromList
                [ PartitionSpec 0 V.empty
                , ps
                ]
            , tmDefaultSpecId = 5
            }
      currentPartitionSpec tm `shouldBe` Just ps

  , it "currentPartitionSpec returns Nothing for missing ID" $ do
      let tm = minimalMetadata { tmDefaultSpecId = 99 }
      currentPartitionSpec tm `shouldBe` Nothing
  ]

-- ============================================================
-- Schema evolution
-- ============================================================

schemaEvolutionTests :: Spec
schemaEvolutionTests = describe "Schema evolution" $ sequence_
  [ it "schemaById lookup" $ do
      let s0 = mkSchema 0 V.empty
          s1 = mkSchema 1 (V.singleton (mkSF 1 "x" True TInt Nothing))
          tm = minimalMetadata { tmSchemas = V.fromList [s0, s1] }
      schemaById tm 1 `shouldBe` Just s1
      schemaById tm 0 `shouldBe` Just s0
      schemaById tm 99 `shouldBe` Nothing

  , it "currentSchema matches tmCurrentSchemaId" $ do
      let s0 = mkSchema 0 V.empty
          s1 = mkSchema 1 (V.singleton (mkSF 1 "x" True TLong Nothing))
          tm = minimalMetadata
            { tmSchemas = V.fromList [s0, s1]
            , tmCurrentSchemaId = 1
            }
      fmap schemaId (currentSchema tm) `shouldBe` Just 1

  , it "findFieldById at top level" $ do
      let fields = V.fromList
            [ mkSF 1 "a" True TInt Nothing
            , mkSF 2 "b" True TString Nothing
            ]
          schema = mkSchema 0 fields
      fmap sfName (findFieldById schema 2) `shouldBe` Just "b"
      findFieldById schema 99 `shouldBe` Nothing

  , it "findFieldById in nested struct" $ do
      let inner = V.fromList
            [ mkSF 10 "x" True TInt Nothing
            , mkSF 11 "y" True TInt Nothing
            ]
          fields = V.fromList
            [ mkSF 1 "id" True TLong Nothing
            , mkSF 2 "point" True (TStruct inner) Nothing
            ]
          schema = mkSchema 0 fields
      fmap sfName (findFieldById schema 11) `shouldBe` Just "y"
      fmap sfName (findFieldById schema 10) `shouldBe` Just "x"
      fmap sfName (findFieldById schema 1) `shouldBe` Just "id"

  , it "findFieldById in list element struct" $ do
      let elemFields = V.fromList
            [ mkSF 20 "name" True TString Nothing
            , mkSF 21 "val" True TDouble Nothing
            ]
          fields = V.singleton
            (mkSF 1 "items" True (TList 100 (TStruct elemFields)) Nothing)
          schema = mkSchema 0 fields
      fmap sfName (findFieldById schema 20) `shouldBe` Just "name"
      fmap sfName (findFieldById schema 21) `shouldBe` Just "val"

  , it "findFieldById in map value struct" $ do
      let valFields = V.singleton (mkSF 30 "score" True TFloat Nothing)
          fields = V.singleton
            (mkSF 1 "scores" True
              (TMap 100 TString 101 (TStruct valFields)) Nothing)
          schema = mkSchema 0 fields
      fmap sfName (findFieldById schema 30) `shouldBe` Just "score"

  , it "projectSchema keeping subset of fields" $ do
      let fields = V.fromList
            [ mkSF 1 "a" True TInt Nothing
            , mkSF 2 "b" True TString Nothing
            , mkSF 3 "c" True TLong Nothing
            , mkSF 4 "d" False TDouble Nothing
            ]
          schema = mkSchema 0 fields
      case projectSchema schema [2, 4] of
        Left e -> expectationFailure e
        Right projected -> do
          V.length (schemaFields projected) `shouldBe` 2
          sfName (V.unsafeIndex (schemaFields projected) 0) `shouldBe` "b"
          sfName (V.unsafeIndex (schemaFields projected) 1) `shouldBe` "d"

  , it "projectSchema with empty field list" $ do
      let schema = mkSchema 0 (V.singleton (mkSF 1 "a" True TInt Nothing))
      case projectSchema schema [] of
        Left _  -> expectationFailure "empty field list should succeed"
        Right s -> V.length (schemaFields s) `shouldBe` 0

  , it "projectSchema with no matching IDs fails" $ do
      let schema = mkSchema 0 (V.singleton (mkSF 1 "a" True TInt Nothing))
      case projectSchema schema [99, 100] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left for no matching IDs"

  , it "JSON roundtrip -> currentSnapshot -> verify" $ do
      let snap1 = mkSnap 10 Nothing  1 1000 "s3://b/ml1.avro"
                    (Map.singleton "operation" "append")
          snap2 = mkSnap 20 (Just 10) 2 2000 "s3://b/ml2.avro"
                    (Map.singleton "operation" "overwrite")
          s0 = mkSchema 0 (V.singleton (mkSF 1 "id" True TLong Nothing))
          s1 = mkSchema 1 (V.fromList
                 [ mkSF 1 "id" True TLong Nothing
                 , mkSF 2 "name" True TString Nothing
                 ])
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 20
            , tmSnapshots = V.fromList [snap1, snap2]
            , tmCurrentSchemaId = 1
            , tmSchemas = V.fromList [s0, s1]
            , tmSnapshotLog = V.fromList
                [ SnapshotLogEntry 1000 10
                , SnapshotLogEntry 2000 20
                ]
            }
          json = metadataToJSON tm
      case metadataFromJSON json of
        Left e -> expectationFailure e
        Right tm' -> do
          tm' `shouldBe` tm
          let mSnap = currentSnapshot tm'
          fmap snapId mSnap `shouldBe` Just 20
          fmap snapParentId mSnap `shouldBe` Just (Just 10)
          let mSchema = currentSchema tm'
          fmap schemaId mSchema `shouldBe` Just 1
          fmap (V.length . schemaFields) mSchema `shouldBe` Just 2
  ]

-- ============================================================
-- Delete file types
-- ============================================================

deleteFileTests :: Spec
deleteFileTests = describe "Delete file types" $ sequence_
  [ it "DeleteFile construction" $ do
      let df = DeleteFile
            { dfFilePath = "s3://b/del.parquet"
            , dfFileFormat = ParquetFormat
            , dfContent = PositionDeletes
            , dfRecordCount = 100
            , dfFileSizeInBytes = 4096
            , dfEqualityFieldIds = V.empty
            , dfPartition = Map.empty
            , dfSequenceNumber = Just 5
            }
      dfFilePath df `shouldBe` "s3://b/del.parquet"
      dfContent df `shouldBe` PositionDeletes
      dfRecordCount df `shouldBe` 100

  , it "EqualityDeletes variant" $ do
      let df = DeleteFile
            { dfFilePath = "s3://b/eq-del.parquet"
            , dfFileFormat = ParquetFormat
            , dfContent = EqualityDeletes
            , dfRecordCount = 50
            , dfFileSizeInBytes = 2048
            , dfEqualityFieldIds = V.fromList [1, 2, 3 :: Int32]
            , dfPartition = Map.singleton "region" (AV.String "us-east")
            , dfSequenceNumber = Nothing
            }
      dfContent df `shouldBe` EqualityDeletes
      V.length (dfEqualityFieldIds df) `shouldBe` 3
  ]

-- ============================================================
-- Position deletes
-- ============================================================

positionDeleteTests :: Spec
positionDeleteTests = describe "Position deletes" $ sequence_
  [ it "positionDeletesFromColumns zips correctly" $ do
      let paths = V.fromList ["f1.parquet", "f1.parquet", "f2.parquet"]
          positions = V.fromList [0, 5, 3]
          pds = positionDeletesFromColumns paths positions
      V.length pds `shouldBe` 3
      pdFilePath (V.unsafeIndex pds 0) `shouldBe` "f1.parquet"
      pdPosition (V.unsafeIndex pds 0) `shouldBe` 0
      pdFilePath (V.unsafeIndex pds 2) `shouldBe` "f2.parquet"
      pdPosition (V.unsafeIndex pds 2) `shouldBe` 3

  , it "applyPositionDeletes removes correct rows" $ do
      let deletes = V.fromList
            [ PositionDelete "data.parquet" 1
            , PositionDelete "data.parquet" 3
            , PositionDelete "other.parquet" 0
            ]
          rows = V.fromList ["a", "b", "c", "d", "e" :: String]
          result = applyPositionDeletes deletes "data.parquet" rows
      result `shouldBe` V.fromList ["a", "c", "e"]

  , it "applyPositionDeletes with no matching deletes" $ do
      let deletes = V.fromList [PositionDelete "other.parquet" 0]
          rows = V.fromList [10, 20, 30 :: Int]
      applyPositionDeletes deletes "data.parquet" rows `shouldBe` rows

  , it "applyPositionDeletes with empty deletes" $ do
      let rows = V.fromList [1, 2, 3 :: Int]
      applyPositionDeletes V.empty "data.parquet" rows `shouldBe` rows
  ]

-- ============================================================
-- Manifest path filtering
-- ============================================================

manifestPathFilterTests :: Spec
manifestPathFilterTests = describe "Manifest path filtering" $ sequence_
  [ it "deleteManifestPaths filters to DeletesContent" $ do
      let mfs = V.fromList
            [ mkManifestFile "m1.avro" DataContent 10
            , mkManifestFile "m2.avro" DeletesContent 10
            , mkManifestFile "m3.avro" DataContent 10
            , mkManifestFile "m4.avro" DeletesContent 10
            ]
      deleteManifestPaths mfs `shouldBe` V.fromList ["m2.avro", "m4.avro"]

  , it "dataManifestPaths filters to DataContent" $ do
      let mfs = V.fromList
            [ mkManifestFile "m1.avro" DataContent 10
            , mkManifestFile "m2.avro" DeletesContent 10
            ]
      dataManifestPaths mfs `shouldBe` V.fromList ["m1.avro"]

  , it "both filters on empty input" $ do
      deleteManifestPaths V.empty `shouldBe` V.empty
      dataManifestPaths V.empty `shouldBe` V.empty
  ]

-- ============================================================
-- Sequence number filtering
-- ============================================================

sequenceNumberTests :: Spec
sequenceNumberTests = describe "Sequence number filtering" $ sequence_
  [ it "filterBySequenceNumber keeps entries <= threshold" $ do
      let entries = V.fromList
            [ mkEntry (Just 3) "f1.parquet"
            , mkEntry (Just 5) "f2.parquet"
            , mkEntry (Just 7) "f3.parquet"
            , mkEntry Nothing  "f4.parquet"
            ]
          result = filterBySequenceNumber 5 entries
      V.length result `shouldBe` 3
      V.map meFilePath result `shouldBe` V.fromList ["f1.parquet", "f2.parquet", "f4.parquet"]

  , it "applicableDeletes filters delete manifests by sequence number" $ do
      let snap = mkSnap 1 Nothing 5 1000 "ml.avro" Map.empty
          mfs = V.fromList
            [ mkManifestFile "d1.avro" DeletesContent 3
            , mkManifestFile "d2.avro" DeletesContent 7
            , mkManifestFile "d3.avro" DataContent 3
            , mkManifestFile "d4.avro" DeletesContent 5
            ]
          result = applicableDeletes snap mfs
      V.length result `shouldBe` 2
      V.map mfPath result `shouldBe` V.fromList ["d1.avro", "d4.avro"]
  ]

-- ============================================================
-- ScanPlan with delete file paths
-- ============================================================

scanPlanDeleteTests :: Spec
scanPlanDeleteTests = describe "ScanPlan with deletes" $ sequence_
  [ it "planScan sets empty delete file paths" $ do
      let snap = mkSnap 1 Nothing 1 1000 "s3://b/ml.avro" (Map.singleton "operation" "append")
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 1
            , tmSnapshots = V.singleton snap
            , tmSchemas = V.singleton (mkSchema 0 V.empty)
            }
          u0 = AV.Union 0 AV.Null
          dataFile = AV.Record $ V.fromList
            [ AV.String "s3://b/data.parquet", AV.String "parquet"
            , AV.Record V.empty, AV.Long 10, AV.Long 1024
            , AV.Long 67108864, u0, u0, u0, u0, u0
            ]
          entry = AV.Record $ V.fromList [AV.Int 1, u0, u0, u0, dataFile]
          manifestBs = writeContainer manifestEntrySchema (V.singleton entry)
          mfRec = AV.Record $ V.fromList
            [ AV.String "manifest.avro", AV.Long 500, AV.Int 0, AV.Int 0
            , AV.Long 1, AV.Long 1, AV.Long 1, u0, u0, u0, u0, u0, u0
            ]
          mlBs = writeContainer manifestFileSchema (V.singleton mfRec)
          readManifest _ = Right manifestBs
      case planScan tm mlBs readManifest of
        Left e -> expectationFailure e
        Right sp -> do
          spDeleteFilePaths sp `shouldBe` V.empty
          V.length (spDataFilePaths sp) `shouldBe` 1
  ]

-- ============================================================
-- SnapshotRef JSON roundtrip
-- ============================================================

snapshotRefTests :: Spec
snapshotRefTests = describe "SnapshotRef" $ sequence_
  [ it "JSON roundtrip with snapshot refs" $ do
      let ref = SnapshotRef
            { srSnapshotId = 100
            , srType = "branch"
            , srMaxRefAgeMs = Just 86400000
            , srMaxSnapshotAgeMs = Nothing
            , srMinSnapshotsToKeep = Just 5
            }
          tm = minimalMetadata
            { tmSnapshotRefs = Map.singleton "main" ref }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "JSON roundtrip with empty snapshot refs" $ do
      let tm = minimalMetadata
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm

  , it "JSON roundtrip with format version 2 and all v2 fields" $ do
      let ref = SnapshotRef
            { srSnapshotId = 200
            , srType = "tag"
            , srMaxRefAgeMs = Nothing
            , srMaxSnapshotAgeMs = Just 172800000
            , srMinSnapshotsToKeep = Nothing
            }
          snap = mkSnap 200 Nothing 10 2000 "s3://b/ml.avro" Map.empty
          tm = minimalMetadata
            { tmFormatVersion = 2
            , tmLastSequenceNumber = 10
            , tmCurrentSnapshotId = Just 200
            , tmSnapshots = V.singleton snap
            , tmSnapshotRefs = Map.fromList
                [ ("main", ref { srSnapshotId = 200, srType = "branch" })
                , ("v1.0", ref)
                ]
            }
          json = metadataToJSON tm
      metadataFromJSON json `shouldBe` Right tm
  ]

-- ============================================================
-- Helpers
-- ============================================================

mkManifestFile :: Text -> ManifestContent -> Int64 -> ManifestFile
mkManifestFile path content seqNum = ManifestFile
  { mfPath = path
  , mfLength = 1000
  , mfPartitionSpecId = 0
  , mfContent = content
  , mfSequenceNumber = seqNum
  , mfMinSequenceNumber = 0
  , mfAddedSnapshotId = 1
  , mfAddedDataFilesCount = Nothing
  , mfExistingDataFilesCount = Nothing
  , mfDeletedDataFilesCount = Nothing
  , mfAddedRowsCount = Nothing
  , mfExistingRowsCount = Nothing
  , mfDeletedRowsCount = Nothing
  , mfPartitions = V.empty
  , mfKeyMetadata = Nothing
  , mfFirstRowId = Nothing
  }

mkEntry :: Maybe Int64 -> Text -> ManifestEntry
mkEntry seqNo path = ManifestEntry
  { meStatus = Added
  , meSnapshotId = Just 1
  , meSequenceNumber = seqNo
  , meFileSequenceNumber = Nothing
  , meFilePath = path
  , meFileFormat = ParquetFormat
  , mePartition = V.empty
  , meRecordCount = 100
  , meFileSizeBytes = 4096
  , meDataFile = Nothing
  }

minimalMetadata :: TableMetadata
minimalMetadata = TableMetadata
  { tmFormatVersion       = 2
  , tmTableUuid           = "550e8400-e29b-41d4-a716-446655440000"
  , tmLocation            = "s3://bucket/table"
  , tmLastSequenceNumber  = 0
  , tmLastUpdatedMs       = 1672531200000
  , tmLastColumnId        = 0
  , tmCurrentSchemaId     = 0
  , tmSchemas             = V.singleton (mkSchema 0 V.empty)
  , tmCurrentSnapshotId   = Nothing
  , tmSnapshots           = V.empty
  , tmPartitionSpecs      = V.singleton (PartitionSpec 0 V.empty)
  , tmDefaultSpecId       = 0
  , tmLastPartitionId     = 0
  , tmSortOrders          = V.singleton (SortOrder 0 V.empty)
  , tmDefaultSortOrderId  = 0
  , tmProperties          = Map.empty
  , tmSnapshotLog         = V.empty
  , tmMetadataLog         = V.empty
  , tmSnapshotRefs        = Map.empty
  , tmStatistics          = V.empty
  , tmPartitionStatistics = V.empty
  , tmNextRowId           = Nothing
  , tmEncryptionKeys      = Map.empty
  }
