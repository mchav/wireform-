{-# LANGUAGE OverloadedStrings #-}
-- | Round-trip tests for "Iceberg.Catalog.Hadoop". Uses an in-memory
-- 'FileSystem' built on 'IORef' so the tests don't touch real disk and
-- so we can deterministically observe every read / write.
module Test.Iceberg.CatalogHadoop (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.IORef
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Catalog.Hadoop
import Iceberg.Types

-- ============================================================
-- In-memory file system
-- ============================================================

memFS :: IORef (Map.Map FilePath ByteString) -> FileSystem
memFS ref = FileSystem
  { fsReadFile = \p -> Map.lookup p <$> readIORef ref
  , fsWriteFile = \p b -> modifyIORef' ref (Map.insert p b)
  , fsAtomicReplace = \p b -> modifyIORef' ref (Map.insert p b)
  , fsListDirectory = \p -> do
      m <- readIORef ref
      pure [ k | k <- Map.keys m, take (length p + 1) k == (p ++ "/") ]
  , fsCreateDirectory = \_ -> pure ()
  }

-- ============================================================
-- Sample table metadata
-- ============================================================

emptySchema :: Schema
emptySchema = Schema { schemaId = 0
                     , schemaFields = V.empty
                     , schemaIdentifierFieldIds = V.empty
                     }

mkMeta :: Int -> TableMetadata
mkMeta v = TableMetadata
  { tmFormatVersion = 2
  , tmTableUuid = "00000000-0000-0000-0000-000000000000"
  , tmLocation = "test://t"
  , tmLastSequenceNumber = fromIntegral v
  , tmLastUpdatedMs = 0
  , tmLastColumnId = 0
  , tmSchemas = V.singleton emptySchema
  , tmCurrentSchemaId = 0
  , tmPartitionSpecs = V.empty
  , tmDefaultSpecId = 0
  , tmLastPartitionId = 999
  , tmSortOrders = V.empty
  , tmDefaultSortOrderId = 0
  , tmProperties = Map.empty
  , tmCurrentSnapshotId = Nothing
  , tmSnapshots = V.empty
  , tmSnapshotLog = V.empty
  , tmMetadataLog = V.empty
  , tmSnapshotRefs = Map.empty
  , tmStatistics = V.empty
  , tmPartitionStatistics = V.empty
  , tmEncryptionKeys = Map.empty
  , tmNextRowId = Nothing
  }

-- ============================================================
-- Tests
-- ============================================================

tests :: TestTree
tests = testGroup "Iceberg.Catalog.Hadoop"
  [ testCase "currentVersion is Nothing on empty catalog" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
      r <- currentVersion cat (V.singleton "ns") "tbl"
      r @?= Nothing

  , testCase "first commit creates v1" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
      r <- commitMetadata cat (V.singleton "ns") "tbl" Nothing (mkMeta 1)
      r @?= Right 1
      v <- currentVersion cat (V.singleton "ns") "tbl"
      v @?= Just 1

  , testCase "second commit increments to v2" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
      _ <- commitMetadata cat (V.singleton "ns") "tbl" Nothing (mkMeta 1)
      r <- commitMetadata cat (V.singleton "ns") "tbl" (Just 1) (mkMeta 2)
      r @?= Right 2

  , testCase "stale assertion is rejected" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
      _ <- commitMetadata cat (V.singleton "ns") "tbl" Nothing (mkMeta 1)
      r <- commitMetadata cat (V.singleton "ns") "tbl" Nothing (mkMeta 1)
      case r of
        Left _ -> pure ()
        Right _ -> assertFailure "expected version-conflict error"

  , testCase "currentMetadata reflects last commit" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
      _ <- commitMetadata cat (V.singleton "ns") "tbl" Nothing (mkMeta 1)
      _ <- commitMetadata cat (V.singleton "ns") "tbl" (Just 1) (mkMeta 2)
      r <- currentMetadata cat (V.singleton "ns") "tbl"
      case r of
        Right (Just (v, _tm)) -> v @?= 2
        Right Nothing -> assertFailure "expected metadata"
        Left e        -> assertFailure e

  , testCase "metadataPath formats v<N>.metadata.json" $ do
      ref <- newIORef Map.empty
      let cat = mkHadoopCatalog "/wh" (memFS ref)
          p = metadataPath cat (V.fromList ["a", "b"]) "tbl" 7
      p @?= "/wh/a/b/tbl/metadata/v7.metadata.json"
  ]
