module Test.Iceberg.SequenceInheritance (tests) where

import Data.Int (Int64)
import qualified Data.Vector as V
import Test.Syd

import Iceberg.Read (inheritSequenceNumbers)
import Iceberg.Types

mkMf :: Int64 -> ManifestFile
mkMf seqNum = ManifestFile
  { mfPath = "m.avro", mfLength = 1, mfPartitionSpecId = 0, mfContent = DataContent
  , mfSequenceNumber = seqNum, mfMinSequenceNumber = seqNum, mfAddedSnapshotId = 1
  , mfAddedDataFilesCount = Nothing, mfExistingDataFilesCount = Nothing
  , mfDeletedDataFilesCount = Nothing, mfAddedRowsCount = Nothing
  , mfExistingRowsCount = Nothing, mfDeletedRowsCount = Nothing
  , mfPartitions = V.empty, mfKeyMetadata = Nothing, mfFirstRowId = Nothing
  }

mkEntry :: ManifestStatus -> Maybe Int64 -> Maybe Int64 -> ManifestEntry
mkEntry status sq fsq = ManifestEntry
  { meStatus = status, meSnapshotId = Just 1
  , meSequenceNumber = sq
  , meFileSequenceNumber = fsq
  , meFilePath = "f.parquet", meFileFormat = ParquetFormat
  , mePartition = V.empty, meRecordCount = 0, meFileSizeBytes = 0
  , meDataFile = Nothing
  }

tests :: Spec
tests = describe "Iceberg.Read inheritSequenceNumbers" $ sequence_
  [ it "ADDED entry inherits the parent manifest's sequence number" $ do
      let mf = mkMf 7
          me = mkEntry Added Nothing Nothing
          me' = inheritSequenceNumbers mf me
      meSequenceNumber me' `shouldBe` Just 7
      meFileSequenceNumber me' `shouldBe` Just 7

  , it "EXISTING entry retains its own sequence number" $ do
      let mf = mkMf 7
          me = mkEntry Existing (Just 3) (Just 3)
          me' = inheritSequenceNumbers mf me
      meSequenceNumber me' `shouldBe` Just 3
      meFileSequenceNumber me' `shouldBe` Just 3

  , it "DELETED entry without seq stays nullable" $ do
      let mf = mkMf 7
          me = mkEntry Deleted Nothing Nothing
          me' = inheritSequenceNumbers mf me
      meSequenceNumber me' `shouldBe` Nothing
      meFileSequenceNumber me' `shouldBe` Nothing

  , it "Existing minimal manifest example does not crash" $ do
      let mf = mkMf 0
      mfPath mf `shouldBe` "m.avro"
  ]
