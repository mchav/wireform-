{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.SnapshotHistory (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Syd

import Iceberg.Snapshot
import Iceberg.Types
import Iceberg.Update (rollbackToSnapshot, AppendFiles (..), appendFiles)

minimal :: TableMetadata
minimal = TableMetadata
  { tmFormatVersion       = 2
  , tmTableUuid           = "u"
  , tmLocation            = "s3://b"
  , tmLastSequenceNumber  = 0
  , tmLastUpdatedMs       = 0
  , tmLastColumnId        = 0
  , tmCurrentSchemaId     = 0
  , tmSchemas             = V.singleton (Schema 0 V.empty V.empty)
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

threeAppends :: TableMetadata
threeAppends =
  let s1 = appendFiles minimal (AppendFiles "ml1.avro" 100 Map.empty Nothing (Just 0))
      s2 = appendFiles s1      (AppendFiles "ml2.avro" 200 Map.empty Nothing (Just 0))
      s3 = appendFiles s2      (AppendFiles "ml3.avro" 300 Map.empty Nothing (Just 0))
   in s3

tests :: Spec
tests = describe "Iceberg.Snapshot history + Update.rollback" $ sequence_
  [ it "currentAncestors returns 3 snapshots after 3 appends" $ do
      length (currentAncestors threeAppends) `shouldBe` 3

  , it "snapshotsBetween fromOldest toNewest returns 2 entries" $ do
      let snaps = tmSnapshots threeAppends
          firstId = snapId (V.unsafeIndex snaps 0)
          lastId  = snapId (V.unsafeIndex snaps 2)
      case snapshotsBetween threeAppends firstId lastId of
        Just xs -> length xs `shouldBe` 2
        Nothing -> expectationFailure "expected snapshots"

  , it "snapshotAsOfTime picks the latest snapshot at or before target" $ do
      case snapshotAsOfTime threeAppends 250 of
        Just s  -> snapTimestampMs s `shouldBe` 200
        Nothing -> expectationFailure "expected match"

  , it "isAncestor relates first and third snapshots" $ do
      let snaps = tmSnapshots threeAppends
          firstId = snapId (V.unsafeIndex snaps 0)
          lastId  = snapId (V.unsafeIndex snaps 2)
      isAncestor threeAppends firstId lastId `shouldBe` True
      isAncestor threeAppends lastId firstId `shouldBe` False

  , it "rollbackToSnapshot accepts an ancestor" $ do
      let snaps = tmSnapshots threeAppends
          firstId = snapId (V.unsafeIndex snaps 0)
      case rollbackToSnapshot firstId threeAppends of
        Right tm' -> tmCurrentSnapshotId tm' `shouldBe` Just firstId
        Left e    -> expectationFailure e

  , it "rollbackToSnapshot rejects a non-ancestor" $ do
      case rollbackToSnapshot 99999 threeAppends of
        Right _ -> expectationFailure "expected rollback failure"
        Left _  -> pure ()
  ]
