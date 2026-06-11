{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Iceberg.Catalog.Glue" using an in-memory 'GlueBackend'
so the suite doesn't need an AWS account.
-}
module Test.Iceberg.CatalogGlue (tests) where

import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Iceberg.Catalog.Glue
import Test.Syd


-- ============================================================
-- In-memory Glue backend
-- ============================================================
--
-- Stores tables in a Map keyed by (database, name). The
-- 'gbUpdateTable' contract honours the IfMatchVersionId field by
-- comparing it against the row's current metadata_location, just
-- like AWS Glue's UpdateTable does.

type Mem = Map (Text, Text) GlueTable


memBackend :: IORef Mem -> GlueBackend
memBackend ref =
  GlueBackend
    { gbGetTable = \db name -> do
        m <- readIORef ref
        pure (Right (Map.lookup (db, name) m))
    , gbCreateTable = \db t -> do
        let key = (db, gtName t)
        m <- readIORef ref
        case Map.lookup key m of
          Just _ -> pure (Left "AlreadyExistsException")
          Nothing -> do
            writeIORef ref (Map.insert key t m)
            pure (Right ())
    , gbUpdateTable = \db t mIfMatch -> do
        let key = (db, gtName t)
        m <- readIORef ref
        case Map.lookup key m of
          Nothing -> pure (Left "EntityNotFoundException")
          Just existing ->
            let storedLoc =
                  Map.lookup
                    "metadata_location"
                    (gtParameters existing)
            in case mIfMatch of
                 Just expected
                   | storedLoc /= Just expected ->
                       pure (Left "ConcurrentModificationException")
                 _ -> do
                   writeIORef ref (Map.insert key t m)
                   pure (Right ())
    , gbDeleteTable = \db name -> do
        let key = (db, name)
        m <- readIORef ref
        case Map.lookup key m of
          Nothing -> pure (Left "EntityNotFoundException")
          Just _ -> do
            writeIORef ref (Map.delete key m)
            pure (Right ())
    , gbListTables = \db -> do
        m <- readIORef ref
        pure
          ( Right
              ( V.fromList
                  ( Map.foldrWithKey
                      (\(d, n) _ acc -> if d == db then n : acc else acc)
                      []
                      m
                  )
              )
          )
    }


mkCat :: IO (GlueCatalog, IORef Mem)
mkCat = do
  ref <- newIORef Map.empty
  pure (mkGlueCatalog "iceberg_db" (memBackend ref), ref)


-- ============================================================
-- Tests
-- ============================================================

tests :: Spec
tests =
  describe "Iceberg.Catalog.Glue" $
    sequence_
      [ it "createTable + currentMetadataLocation" $ do
          (cat, _) <- mkCat
          r <- createTable cat "orders" "s3://b/m/v0.json" "s3://b/orders/"
          r `shouldBe` Right ()
          loc <- currentMetadataLocation cat "orders"
          loc `shouldBe` Just "s3://b/m/v0.json"
      , it "createTable rejects duplicates" $ do
          (cat, _) <- mkCat
          _ <- createTable cat "orders" "s3://b/m/v0.json" "s3://b/orders/"
          r <- createTable cat "orders" "s3://b/m/v0.json" "s3://b/orders/"
          case r of
            Left (GlueTableAlreadyExists _) -> pure ()
            other ->
              expectationFailure
                ( "expected GlueTableAlreadyExists, got "
                    ++ show other
                )
      , it "commitTable: happy path" $ do
          (cat, _) <- mkCat
          _ <- createTable cat "orders" "s3://b/m/v0.json" "s3://b/orders/"
          r <-
            commitTable
              cat
              "orders"
              (Just "s3://b/m/v0.json")
              "s3://b/m/v1.json"
          r `shouldBe` Right ()
          loc <- currentMetadataLocation cat "orders"
          loc `shouldBe` Just "s3://b/m/v1.json"
      , it "commitTable: rejects stale CAS" $ do
          (cat, _) <- mkCat
          _ <- createTable cat "orders" "s3://b/m/v0.json" "s3://b/orders/"
          _ <- commitTable cat "orders" (Just "s3://b/m/v0.json") "s3://b/m/v1.json"
          r <- commitTable cat "orders" (Just "s3://b/m/v0.json") "s3://b/m/v2.json"
          case r of
            Left (GlueCommitConflict _) -> pure ()
            other ->
              expectationFailure
                ( "expected GlueCommitConflict, got "
                    ++ show other
                )
      , it "commitTable: GlueNoSuchTable for missing table" $ do
          (cat, _) <- mkCat
          r <- commitTable cat "ghost" Nothing "s3://b/m/v1.json"
          case r of
            Left (GlueNoSuchTable _) -> pure ()
            other ->
              expectationFailure
                ( "expected GlueNoSuchTable, got "
                    ++ show other
                )
      , it "icebergParameters / parseIcebergParameters round-trip" $ do
          let params = icebergParameters "s3://b/m/v3.json" (Just "s3://b/m/v2.json")
          Map.lookup "table_type" params `shouldBe` Just "ICEBERG"
          Map.lookup "metadata_location" params `shouldBe` Just "s3://b/m/v3.json"
          parseIcebergParameters params
            `shouldBe` Just ("s3://b/m/v3.json", Just "s3://b/m/v2.json")
      , it "parseIcebergParameters rejects non-ICEBERG tables" $ do
          let params = Map.fromList [("table_type", "HIVE_TABLE")]
          parseIcebergParameters params `shouldBe` Nothing
      , it "currentMetadataLocation skips non-ICEBERG tables" $ do
          ref <- newIORef Map.empty
          let cat = mkGlueCatalog "db" (memBackend ref)
              hiveTable =
                GlueTable
                  "hive_t"
                  "db"
                  "s3://b/h/"
                  "EXTERNAL_TABLE"
                  V.empty
                  (Map.fromList [("table_type", "HIVE_TABLE")])
          writeIORef ref (Map.singleton ("db", "hive_t") hiveTable)
          loc <- currentMetadataLocation cat "hive_t"
          loc `shouldBe` Nothing
      , it "listTables returns names" $ do
          (cat, _) <- mkCat
          _ <- createTable cat "a" "s3://b/m/a.json" "s3://b/a/"
          _ <- createTable cat "b" "s3://b/m/b.json" "s3://b/b/"
          r <- listTables cat
          case r of
            Right names -> do
              length (V.toList names) `shouldBe` 2
              ("a" `elem` V.toList names) `shouldBe` True
              ("b" `elem` V.toList names) `shouldBe` True
            Left e -> expectationFailure (show e)
      , it "dropTable removes the row" $ do
          (cat, _) <- mkCat
          _ <- createTable cat "tmp" "s3://b/m/tmp.json" "s3://b/tmp/"
          _ <- dropTable cat "tmp"
          loc <- currentMetadataLocation cat "tmp"
          loc `shouldBe` Nothing
      ]
