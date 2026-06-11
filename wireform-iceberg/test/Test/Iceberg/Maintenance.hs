{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.Maintenance (tests) where

import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V
import Iceberg.Maintenance qualified as M
import Iceberg.Types qualified as I
import Iceberg.Update qualified as IU
import Test.Syd


minimal :: I.TableMetadata
minimal =
  I.TableMetadata
    { I.tmFormatVersion = 2
    , I.tmTableUuid = "u"
    , I.tmLocation = "s3://b"
    , I.tmLastSequenceNumber = 0
    , I.tmLastUpdatedMs = 0
    , I.tmLastColumnId = 0
    , I.tmCurrentSchemaId = 0
    , I.tmSchemas = V.singleton (I.Schema 0 V.empty V.empty)
    , I.tmCurrentSnapshotId = Nothing
    , I.tmSnapshots = V.empty
    , I.tmPartitionSpecs = V.singleton (I.PartitionSpec 0 V.empty)
    , I.tmDefaultSpecId = 0
    , I.tmLastPartitionId = 0
    , I.tmSortOrders = V.singleton (I.SortOrder 0 V.empty)
    , I.tmDefaultSortOrderId = 0
    , I.tmProperties = Map.empty
    , I.tmSnapshotLog = V.empty
    , I.tmMetadataLog = V.empty
    , I.tmSnapshotRefs = Map.empty
    , I.tmStatistics = V.empty
    , I.tmPartitionStatistics = V.empty
    , I.tmNextRowId = Nothing
    , I.tmEncryptionKeys = Map.empty
    }


threeAppends :: I.TableMetadata
threeAppends = appendAt 3000 (appendAt 2000 (appendAt 1000 minimal))
  where
    appendAt ts tm =
      IU.appendFiles
        tm
        IU.AppendFiles
          { IU.apfNewManifestList = "ml.avro"
          , IU.apfTimestampMs = ts
          , IU.apfSummary = Map.empty
          , IU.apfStats = Nothing
          , IU.apfSchemaId = Just 0
          }


tests :: Spec
tests =
  describe "Iceberg.Maintenance" $
    sequence_
      [ it "expireSnapshots drops snapshots older than the cutoff" $ do
          let now = 4000 :: Int64
              policy =
                M.ExpiryPolicy
                  { M.epMaxAgeMs = Just 1500 -- keep snapshots >= now - 1500 = 2500
                  , M.epMinSnapshots = 0
                  , M.epRetainSnapshots = Set.empty
                  }
              result = M.expireSnapshots now policy threeAppends
              tm' = M.exNewMetadata result
          length (M.exExpiredSnapshots result) `shouldBe` 2 -- snapshots at 1000 and 2000
          V.length (I.tmSnapshots tm') `shouldBe` 1 -- only the 3000 snapshot survives
          I.tmCurrentSnapshotId tm' /= Nothing `shouldBe` True
      , it "expireSnapshots keeps minimum count even when all are old" $ do
          let now = 1_000_000 :: Int64
              policy =
                M.defaultExpiryPolicy
                  { M.epMaxAgeMs = Just 1
                  , M.epMinSnapshots = 2
                  }
              result = M.expireSnapshots now policy threeAppends
          V.length (I.tmSnapshots (M.exNewMetadata result)) `shouldBe` 2
      , it "expireSnapshots respects retainSnapshots" $ do
          let oldestId = I.snapId (V.unsafeIndex (I.tmSnapshots threeAppends) 0)
              now = 1_000_000 :: Int64
              policy =
                M.defaultExpiryPolicy
                  { M.epMaxAgeMs = Just 1
                  , M.epMinSnapshots = 0
                  , M.epRetainSnapshots = Set.singleton oldestId
                  }
              result = M.expireSnapshots now policy threeAppends
              ids = map I.snapId (V.toList (I.tmSnapshots (M.exNewMetadata result)))
          (oldestId `elem` ids) `shouldBe` True
      , it "referencedFilePaths includes manifest list, metadata log, statistics" $ do
          let tm =
                threeAppends
                  { I.tmMetadataLog = V.singleton (I.MetadataLogEntry 1000 "m-old.json")
                  , I.tmStatistics = V.singleton (I.StatisticsFile 1 "s.puffin" 1024 256 Nothing V.empty)
                  }
              paths = M.referencedFilePaths tm
          Set.member "ml.avro" paths `shouldBe` True
          Set.member "m-old.json" paths `shouldBe` True
          Set.member "s.puffin" paths `shouldBe` True
      , it "orphanFileCandidates returns set difference" $ do
          let tm = threeAppends
              referenced = M.referencedFilePaths tm
              discovered =
                referenced
                  `Set.union` Set.fromList ["junk1.avro", "junk2.parquet"]
              orphans = M.orphanFileCandidates tm discovered
          orphans `shouldBe` Set.fromList ["junk1.avro", "junk2.parquet"]
      ]
