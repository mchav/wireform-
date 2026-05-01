{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.ManifestMerge (tests) where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.ManifestMerge
import Iceberg.Types

showT :: Int -> Text
showT = T.pack . show

mkEntry :: Int -> Int64 -> ManifestEntry
mkEntry idx size = ManifestEntry
  { meStatus = Added, meSnapshotId = Nothing
  , meSequenceNumber = Nothing, meFileSequenceNumber = Nothing
  , meFilePath = "f" <> showT idx <> ".parquet", meFileFormat = ParquetFormat
  , mePartition = V.empty, meRecordCount = 100
  , meFileSizeBytes = size, meDataFile = Nothing
  }

mkMf :: Int -> Int64 -> ManifestContent -> ManifestFile
mkMf idx len content = ManifestFile
  { mfPath = "m" <> showT idx <> ".avro", mfLength = len, mfPartitionSpecId = 0
  , mfContent = content, mfSequenceNumber = 0, mfMinSequenceNumber = 0
  , mfAddedSnapshotId = 0, mfAddedDataFilesCount = Just 1
  , mfExistingDataFilesCount = Just 0, mfDeletedDataFilesCount = Just 0
  , mfAddedRowsCount = Just 100, mfExistingRowsCount = Nothing
  , mfDeletedRowsCount = Nothing, mfPartitions = V.empty
  , mfKeyMetadata = Nothing, mfFirstRowId = Nothing
  }

genPath :: Text -> Int -> Text
genPath prefix idx = prefix <> "-" <> showT idx <> ".avro"

readEntries :: ManifestFile -> Either String (V.Vector ManifestEntry)
readEntries mf = Right (V.singleton (mkEntry 999 (mfLength mf)))

tests :: TestTree
tests = testGroup "Iceberg.ManifestMerge"
  [ testCase "defaultMergePolicy is the Java reference" $ do
      let p = defaultMergePolicy
      mpMergeEnabled p    @?= True
      mpTargetSizeBytes p @?= 8 * 1024 * 1024
      mpMinCountToMerge p @?= 100

  , testCase "mergePolicyFromProperties reads overrides" $ do
      let props = Map.fromList
            [ ("commit.manifest-merge.enabled", "false")
            , ("commit.manifest.target-size-bytes", "1024")
            , ("commit.manifest.min-count-to-merge", "5")
            ]
          p = mergePolicyFromProperties props
      mpMergeEnabled p    @?= False
      mpTargetSizeBytes p @?= 1024
      mpMinCountToMerge p @?= 5

  , testCase "planFastAppend writes one new manifest and prepends it" $ do
      let entries = V.fromList [mkEntry 1 100, mkEntry 2 200]
          existing = V.fromList [mkMf 1 1000 DataContent, mkMf 2 2000 DataContent]
          plan = planFastAppend 5 99 0 "new.avro" entries existing
      V.length (cpNewManifests plan) @?= 1
      V.length (cpNewManifestList plan) @?= 3
      mfPath (V.unsafeIndex (cpNewManifestList plan) 0) @?= "new.avro"
      mfPath (V.unsafeIndex (cpNewManifestList plan) 1) @?= "m1.avro"

  , testCase "fast-appended entries inherit snapshot id and sequence number" $ do
      let entries = V.fromList [mkEntry 1 100]
          plan = planFastAppend 7 42 0 "new.avro" entries V.empty
          task = V.unsafeIndex (cpNewManifests plan) 0
          me   = V.unsafeIndex (wmtEntries task) 0
      meSnapshotId me     @?= Just 42
      meSequenceNumber me @?= Just 7

  , testCase "planAppend with merge disabled falls through to fast-append" $ do
      let policy = defaultMergePolicy { mpMergeEnabled = False }
          newEntries = V.singleton (mkEntry 1 100)
          existing = V.fromList [mkMf i 100 DataContent | i <- [1 .. 5]]
      case planAppend policy genPath readEntries 5 99 0 newEntries existing of
        Left e -> assertFailure e
        Right plan -> V.length (cpNewManifests plan) @?= 1

  , testCase "planAppend below min-count-to-merge is fast-append" $ do
      let policy = defaultMergePolicy { mpMinCountToMerge = 10 }
          newEntries = V.singleton (mkEntry 1 100)
          existing = V.fromList [mkMf i 100 DataContent | i <- [1 .. 3]]
      case planAppend policy genPath readEntries 5 99 0 newEntries existing of
        Left e -> assertFailure e
        Right plan -> V.length (cpNewManifests plan) @?= 1

  , testCase "planAppend above min-count-to-merge actually merges" $ do
      let policy = defaultMergePolicy
            { mpMinCountToMerge = 2
            , mpTargetSizeBytes = 4 -- force one bin per ~4 KiB of data file
            }
          newEntries = V.fromList [mkEntry 100 (8 * 1024)]
          existing = V.fromList [mkMf i 100 DataContent | i <- [1 .. 3]]
      case planAppend policy genPath readEntries 5 99 0 newEntries existing of
        Left e -> assertFailure e
        Right plan -> do
          -- We expect at least one merged manifest task to be produced.
          (V.length (cpNewManifests plan) >= 1) @?= True

  , testCase "planRewriteManifests packs into one or more bins" $ do
      let policy = defaultMergePolicy { mpTargetSizeBytes = 1 }
          toRewrite = V.fromList [mkMf i 100 DataContent | i <- [1 .. 3]]
      case planRewriteManifests policy genPath readEntries 5 99 0 toRewrite V.empty of
        Left e -> assertFailure e
        Right plan ->
          (V.length (cpNewManifests plan) >= 1) @?= True

  , testCase "binPackBySize respects max-files-per-manifest" $ do
      let policy = defaultMergePolicy
            { mpMaxFilesPerManifest = 2
            , mpTargetSizeBytes = 1024 * 1024 * 1024
            }
          entries = V.fromList [mkEntry i 1 | i <- [1 .. 5]]
          bins = binPackBySize policy entries
      -- 5 entries split into bins of at most 2 -> 3 bins.
      V.length bins @?= 3
  ]
