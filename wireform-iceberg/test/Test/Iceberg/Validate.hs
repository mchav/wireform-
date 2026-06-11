{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.Validate (tests) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Iceberg.Types
import Iceberg.Validate
import Test.Syd


baseTable :: TableMetadata
baseTable =
  TableMetadata
    { tmFormatVersion = 2
    , tmTableUuid = "u"
    , tmLocation = "s3://b"
    , tmLastSequenceNumber = 0
    , tmLastUpdatedMs = 0
    , tmLastColumnId = 1
    , tmCurrentSchemaId = 0
    , tmSchemas = V.singleton (Schema 0 V.empty V.empty)
    , tmCurrentSnapshotId = Nothing
    , tmSnapshots = V.empty
    , tmPartitionSpecs = V.singleton (PartitionSpec 0 V.empty)
    , tmDefaultSpecId = 0
    , tmLastPartitionId = 0
    , tmSortOrders = V.singleton (SortOrder 0 V.empty)
    , tmDefaultSortOrderId = 0
    , tmProperties = Map.empty
    , tmSnapshotLog = V.empty
    , tmMetadataLog = V.empty
    , tmSnapshotRefs = Map.empty
    , tmStatistics = V.empty
    , tmPartitionStatistics = V.empty
    , tmNextRowId = Nothing
    , tmEncryptionKeys = Map.empty
    }


tests :: Spec
tests =
  describe "Iceberg.Validate" $
    sequence_
      [ it "valid v2 table passes validation" $
          validateMetadata baseTable `shouldBe` ValidationOk
      , it "current-schema-id missing fails" $ do
          let t = baseTable {tmCurrentSchemaId = 99}
          case validateMetadata t of
            ValidationOk -> expectationFailure "expected errors"
            ValidationErrors _ -> pure ()
      , it "default-spec-id missing fails" $ do
          let t = baseTable {tmDefaultSpecId = 99}
          case validateMetadata t of
            ValidationOk -> expectationFailure "expected errors"
            ValidationErrors _ -> pure ()
      , it "snapshot ref pointing at unknown id fails" $ do
          let t =
                baseTable
                  { tmSnapshotRefs =
                      Map.singleton
                        "main"
                        (SnapshotRef 999 "branch" Nothing Nothing Nothing)
                  }
          case validateMetadata t of
            ValidationOk -> expectationFailure "expected errors"
            ValidationErrors _ -> pure ()
      , it "identifier-field-id on a float column fails" $ do
          let s =
                Schema
                  0
                  (V.singleton (StructField 1 "x" True TFloat Nothing Nothing Nothing))
                  (V.singleton 1)
              t =
                baseTable
                  { tmSchemas = V.singleton s
                  , tmCurrentSchemaId = 0
                  }
          case validateMetadata t of
            ValidationOk -> expectationFailure "expected errors"
            ValidationErrors _ -> pure ()
      ]
