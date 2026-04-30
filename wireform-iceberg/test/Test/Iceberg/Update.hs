module Test.Iceberg.Update (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Types
import Iceberg.Update

minimal :: TableMetadata
minimal = TableMetadata
  { tmFormatVersion       = 2
  , tmTableUuid           = "uuid"
  , tmLocation            = "s3://b/t"
  , tmLastSequenceNumber  = 0
  , tmLastUpdatedMs       = 1700000000000
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

tests :: TestTree
tests = testGroup "Iceberg.Update"
  [ testCase "appendFiles increments sequence number, records snapshot, advances main" $ do
      let after = appendFiles minimal AppendFiles
            { apfNewManifestList = "s3://b/ml1.avro"
            , apfTimestampMs     = 1700000001000
            , apfSummary         = Map.empty
            , apfSchemaId        = Just 0
            }
      tmLastSequenceNumber after @?= 1
      V.length (tmSnapshots after) @?= 1
      tmCurrentSnapshotId after @?= Just (snapId (V.unsafeIndex (tmSnapshots after) 0))
      Map.lookup "main" (tmSnapshotRefs after) /= Nothing @?= True

  , testCase "createTag adds a tag pointing at a snapshot" $ do
      let s1 = appendFiles minimal AppendFiles
                 { apfNewManifestList = "s3://b/ml.avro"
                 , apfTimestampMs = 1
                 , apfSummary = Map.empty
                 , apfSchemaId = Just 0
                 }
          Just sid = tmCurrentSnapshotId s1
          tagged = createTag "v1.0" sid (Just 86400000) s1
      case Map.lookup "v1.0" (tmSnapshotRefs tagged) of
        Just r -> do
          srSnapshotId r @?= sid
          srType r @?= "tag"
        Nothing -> assertFailure "tag not created"

  , testCase "removeRef does not remove main" $ do
      let s1 = appendFiles minimal AppendFiles
                 { apfNewManifestList = "s3://b/ml.avro"
                 , apfTimestampMs = 1
                 , apfSummary = Map.empty
                 , apfSchemaId = Just 0
                 }
      Map.lookup "main" (tmSnapshotRefs (removeRef "main" s1)) /= Nothing @?= True

  , testCase "fastForwardBranch moves branch only inside its history" $ do
      let s1 = appendFiles minimal AppendFiles
                 { apfNewManifestList = "ml1.avro"
                 , apfTimestampMs = 1
                 , apfSummary = Map.empty
                 , apfSchemaId = Just 0
                 }
          s2 = appendFiles s1 AppendFiles
                 { apfNewManifestList = "ml2.avro"
                 , apfTimestampMs = 2
                 , apfSummary = Map.empty
                 , apfSchemaId = Just 0
                 }
          Just sid2 = tmCurrentSnapshotId s2
          Just sid1 = fmap snapId (V.find (\s -> snapParentId s == Nothing) (tmSnapshots s2))
          tagged = createTag "lagging" sid1 Nothing s2
          fwd    = fastForwardBranch "lagging" sid2 tagged
      case Map.lookup "lagging" (tmSnapshotRefs fwd) of
        Just r -> srSnapshotId r @?= sid2
        Nothing -> assertFailure "lagging ref disappeared"
  ]
