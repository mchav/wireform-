{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the incremental-scan helpers in 'Iceberg.Read'.
module Test.Iceberg.Incremental (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Syd

import qualified Avro.Value as AV
import qualified Avro.Container as Avro
import qualified Iceberg.Manifest as IM
import qualified Iceberg.Read as IR
import qualified Iceberg.Types as I
import qualified Iceberg.Update as IU
import qualified Iceberg.Write as IW

minimal :: I.Schema -> I.TableMetadata
minimal sch = I.TableMetadata
  { I.tmFormatVersion = 2, I.tmTableUuid = "u", I.tmLocation = "s3://b"
  , I.tmLastSequenceNumber = 0, I.tmLastUpdatedMs = 0, I.tmLastColumnId = 0
  , I.tmCurrentSchemaId = 0, I.tmSchemas = V.singleton sch
  , I.tmCurrentSnapshotId = Nothing, I.tmSnapshots = V.empty
  , I.tmPartitionSpecs = V.singleton (I.PartitionSpec 0 V.empty)
  , I.tmDefaultSpecId = 0, I.tmLastPartitionId = 0
  , I.tmSortOrders = V.singleton (I.SortOrder 0 V.empty)
  , I.tmDefaultSortOrderId = 0, I.tmProperties = Map.empty
  , I.tmSnapshotLog = V.empty, I.tmMetadataLog = V.empty
  , I.tmSnapshotRefs = Map.empty, I.tmStatistics = V.empty
  , I.tmPartitionStatistics = V.empty, I.tmNextRowId = Nothing
  , I.tmEncryptionKeys = Map.empty
  }

manifestFor :: Text -> I.ManifestStatus -> Int64 -> ByteString
manifestFor path status snapId =
  IW.writeManifestEntries (V.singleton (I.ManifestEntry
    { I.meStatus = status, I.meSnapshotId = Just snapId
    , I.meSequenceNumber = Nothing, I.meFileSequenceNumber = Nothing
    , I.meFilePath = path, I.meFileFormat = I.ParquetFormat
    , I.mePartition = V.empty, I.meRecordCount = 100
    , I.meFileSizeBytes = 1024, I.meDataFile = Nothing
    }))

manifestListFor :: Text -> Int64 -> I.ManifestContent -> Int64 -> ByteString
manifestListFor manifestPath len content seqNum =
  IW.writeManifestList (V.singleton (I.ManifestFile
    { I.mfPath = manifestPath, I.mfLength = len, I.mfPartitionSpecId = 0
    , I.mfContent = content, I.mfSequenceNumber = seqNum
    , I.mfMinSequenceNumber = seqNum, I.mfAddedSnapshotId = 0
    , I.mfAddedDataFilesCount = Just 1, I.mfExistingDataFilesCount = Just 0
    , I.mfDeletedDataFilesCount = Just 0, I.mfAddedRowsCount = Just 100
    , I.mfExistingRowsCount = Nothing, I.mfDeletedRowsCount = Nothing
    , I.mfPartitions = V.empty, I.mfKeyMetadata = Nothing, I.mfFirstRowId = Nothing
    }))

tests :: Spec
tests = describe "Iceberg.Read incremental scans" $ sequence_
  [ it "planIncrementalAppend across two snapshots" $ do
      -- Snapshot 1: adds a.parquet via manifest m1 / mlist ml1.
      -- Snapshot 2: adds b.parquet via manifest m2 / mlist ml2.
      -- Iceberg.Update.appendFiles allocates snapshot ids 1 and 2 in order.
      let schema = I.Schema 0 V.empty V.empty
          mEntryA = manifestFor "a.parquet" I.Added 1
          mEntryB = manifestFor "b.parquet" I.Added 2
          mlA = manifestListFor "m1.avro" (fromIntegral (BS.length mEntryA)) I.DataContent 1
          mlB = manifestListFor "m2.avro" (fromIntegral (BS.length mEntryB)) I.DataContent 2
          t0  = minimal schema
          t1  = IU.appendFiles t0 IU.AppendFiles
            { IU.apfNewManifestList = "ml1.avro"
            , IU.apfTimestampMs = 1000, IU.apfSummary = Map.empty
            , IU.apfStats = Nothing, IU.apfSchemaId = Just 0 }
          t2  = IU.appendFiles t1 IU.AppendFiles
            { IU.apfNewManifestList = "ml2.avro"
            , IU.apfTimestampMs = 2000, IU.apfSummary = Map.empty
            , IU.apfStats = Nothing, IU.apfSchemaId = Just 0 }

          readMl path
            | path == "ml1.avro" = Right mlA
            | path == "ml2.avro" = Right mlB
            | otherwise          = Left $ "no such ml: " ++ show path
          readM path
            | path == "m1.avro"  = Right mEntryA
            | path == "m2.avro"  = Right mEntryB
            | otherwise          = Left $ "no such manifest: " ++ show path

          sid1 = I.snapId (V.unsafeIndex (I.tmSnapshots t2) 0)
          Just sid2 = I.tmCurrentSnapshotId t2

      case IR.planIncrementalAppend t2 (Just sid1) sid2 readMl readM of
        Right plan -> do
          V.length (IR.ispAddedFiles plan) `shouldBe` 1
          let me = IR.fstDataFile (V.unsafeIndex (IR.ispAddedFiles plan) 0)
          I.meFilePath me `shouldBe` "b.parquet"
        Left e -> expectationFailure e

  , it "planIncrementalChangelog produces insert + delete tasks" $ do
      let schema = I.Schema 0 V.empty V.empty
          mAdd = manifestFor "a.parquet" I.Added   1
          mDel = manifestFor "a.parquet" I.Deleted 2
          mlA = manifestListFor "m1.avro" (fromIntegral (BS.length mAdd)) I.DataContent 1
          mlB = manifestListFor "m2.avro" (fromIntegral (BS.length mDel)) I.DataContent 2
          t0 = minimal schema
          t1 = IU.appendFiles t0 IU.AppendFiles
            { IU.apfNewManifestList = "ml1.avro", IU.apfTimestampMs = 1000
            , IU.apfSummary = Map.empty, IU.apfStats = Nothing
            , IU.apfSchemaId = Just 0 }
          t2 = IU.appendFiles t1 IU.AppendFiles
            { IU.apfNewManifestList = "ml2.avro", IU.apfTimestampMs = 2000
            , IU.apfSummary = Map.empty, IU.apfStats = Nothing
            , IU.apfSchemaId = Just 0 }

          readMl path
            | path == "ml1.avro" = Right mlA
            | path == "ml2.avro" = Right mlB
            | otherwise          = Left "no"
          readM path
            | path == "m1.avro"  = Right mAdd
            | path == "m2.avro"  = Right mDel
            | otherwise          = Left "no"

          Just sid2 = I.tmCurrentSnapshotId t2

      case IR.planIncrementalChangelog t2 Nothing sid2 readMl readM of
        Right tasks -> do
          let ops = map IR.ctOperation (V.toList tasks)
          (IR.OpInsert `elem` ops) `shouldBe` True
          (IR.OpDelete `elem` ops) `shouldBe` True
        Left e -> expectationFailure e
  ]

-- Reference Avro re-exports so the import is non-redundant in case the
-- Iceberg.Manifest etc. tree gets pruned later.
_unused :: ()
_unused = (\_ -> ())
  ( Avro.readContainer
  , AV.Null
  , IM.manifestEntrySchema
  )
