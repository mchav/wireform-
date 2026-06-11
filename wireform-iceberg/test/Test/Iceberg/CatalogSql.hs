{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Iceberg.Catalog.Sql" using a small reference
in-memory 'SqlBackend' that pattern-matches on the four query
shapes the catalog ever issues. This keeps the test suite from
needing a real database driver while exercising every code path.
-}
module Test.Iceberg.CatalogSql (tests) where

import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Iceberg.Catalog.Sql
import Test.Syd


-- ============================================================
-- In-memory SQL "backend"
-- ============================================================
--
-- Two maps backing the two tables. Keys mirror the primary keys the
-- standard schema declares so a CAS UPDATE has the same semantics
-- here as on a real RDBMS.

data MemState = MemState
  { msTables :: !(Map (Text, Text, Text) (Maybe Text, Maybe Text))
  -- ^ (catalog, namespace, table) -> (metadata_location, prev)
  , msProps :: !(Map (Text, Text, Text) Text)
  -- ^ (catalog, namespace, key) -> value
  }
  deriving (Show)


emptyMem :: MemState
emptyMem = MemState Map.empty Map.empty


memBackend :: IORef MemState -> SqlBackend
memBackend ref =
  SqlBackend
    { sbQuery = doQuery
    , sbExecute = doExecute
    , sbExecuteCas = doExecute
    }
  where
    doQuery q args = do
      st <- readIORef ref
      pure (queryAgainst st q args)
    doExecute q args = do
      st <- readIORef ref
      let (st', n) = executeAgainst st q args
      writeIORef ref st'
      pure n


-- The query / execute dispatch is just `case` on the catalog's
-- canonical SQL strings (whitespace-normalised). Real drivers parse
-- and bind; we don't need to.

normalise :: Text -> Text
normalise = T.unwords . T.words


queryAgainst :: MemState -> Text -> [SqlValue] -> Vector (Vector SqlValue)
queryAgainst st q args = case (normalise q, args) of
  ( "SELECT metadata_location FROM iceberg_tables \
    \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
    , [SqlText cat, SqlText ns, SqlText tbl]
    ) ->
      case Map.lookup (cat, ns, tbl) (msTables st) of
        Just (Just loc, _) -> V.singleton (V.singleton (SqlText loc))
        Just (Nothing, _) -> V.singleton (V.singleton SqlNull)
        Nothing -> V.empty
  ( "SELECT table_name FROM iceberg_tables \
    \WHERE catalog_name = ? AND table_namespace = ? \
    \ORDER BY table_name"
    , [SqlText cat, SqlText ns]
    ) ->
      let names =
            Map.foldrWithKey
              ( \(c, n, tbl) _ acc ->
                  if c == cat && n == ns then tbl : acc else acc
              )
              []
              (msTables st)
      in V.fromList (map (V.singleton . SqlText) names)
  ( "SELECT DISTINCT table_namespace FROM iceberg_tables WHERE catalog_name = ? \
    \UNION SELECT DISTINCT namespace FROM iceberg_namespace_properties WHERE catalog_name = ?"
    , [SqlText cat, _]
    ) ->
      let collectNs cur (c, n, _) _ = if c == cat then Map.insert n () cur else cur
          fromTables = Map.foldlWithKey' collectNs Map.empty (msTables st)
          fromProps = Map.foldlWithKey' collectNs fromTables (msProps st)
      in V.fromList (map (V.singleton . SqlText) (Map.keys fromProps))
  ( "SELECT property_key, property_value FROM iceberg_namespace_properties \
    \WHERE catalog_name = ? AND namespace = ?"
    , [SqlText cat, SqlText ns]
    ) ->
      let pairs =
            Map.foldrWithKey
              ( \(c, n, k) v acc ->
                  if c == cat && n == ns
                    then V.fromList [SqlText k, SqlText v] : acc
                    else acc
              )
              []
              (msProps st)
      in V.fromList pairs
  _ -> error ("memBackend: unrecognised query: " ++ T.unpack (normalise q))


executeAgainst :: MemState -> Text -> [SqlValue] -> (MemState, Int)
executeAgainst st q args = case (normalise q, args) of
  ( "INSERT INTO iceberg_tables \
    \(catalog_name, table_namespace, table_name, metadata_location, previous_metadata_location) \
    \VALUES (?, ?, ?, ?, ?)"
    , [SqlText cat, SqlText ns, SqlText tbl, mLoc, mPrev]
    ) ->
      let loc = sqlMaybe mLoc
          prev = sqlMaybe mPrev
      in (st {msTables = Map.insert (cat, ns, tbl) (loc, prev) (msTables st)}, 1)
  ( "UPDATE iceberg_tables SET previous_metadata_location = metadata_location, \
    \metadata_location = ? \
    \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ? \
    \AND metadata_location = ?"
    , [SqlText newLoc, SqlText cat, SqlText ns, SqlText tbl, SqlText assertedPrev]
    ) ->
      case Map.lookup (cat, ns, tbl) (msTables st) of
        Just (Just curLoc, _)
          | curLoc == assertedPrev ->
              ( st {msTables = Map.insert (cat, ns, tbl) (Just newLoc, Just curLoc) (msTables st)}
              , 1
              )
        _ -> (st, 0)
  ( "UPDATE iceberg_tables SET previous_metadata_location = metadata_location, \
    \metadata_location = ? \
    \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ? \
    \AND metadata_location IS NULL"
    , [SqlText newLoc, SqlText cat, SqlText ns, SqlText tbl]
    ) ->
      case Map.lookup (cat, ns, tbl) (msTables st) of
        Just (Nothing, _) ->
          ( st {msTables = Map.insert (cat, ns, tbl) (Just newLoc, Nothing) (msTables st)}
          , 1
          )
        _ -> (st, 0)
  ( "DELETE FROM iceberg_tables \
    \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
    , [SqlText cat, SqlText ns, SqlText tbl]
    ) ->
      case Map.lookup (cat, ns, tbl) (msTables st) of
        Just _ -> (st {msTables = Map.delete (cat, ns, tbl) (msTables st)}, 1)
        Nothing -> (st, 0)
  ( "UPDATE iceberg_tables SET table_namespace = ?, table_name = ? \
    \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
    , [SqlText newNs, SqlText newTbl, SqlText cat, SqlText oldNs, SqlText oldTbl]
    ) ->
      case Map.lookup (cat, oldNs, oldTbl) (msTables st) of
        Just row ->
          ( st
              { msTables =
                  Map.insert
                    (cat, newNs, newTbl)
                    row
                    (Map.delete (cat, oldNs, oldTbl) (msTables st))
              }
          , 1
          )
        Nothing -> (st, 0)
  ( "INSERT INTO iceberg_namespace_properties \
    \(catalog_name, namespace, property_key, property_value) \
    \VALUES (?, ?, ?, ?)"
    , [SqlText cat, SqlText ns, SqlText k, SqlText v]
    ) ->
      (st {msProps = Map.insert (cat, ns, k) v (msProps st)}, 1)
  ( "UPDATE iceberg_namespace_properties SET property_value = ? \
    \WHERE catalog_name = ? AND namespace = ? AND property_key = ?"
    , [SqlText v, SqlText cat, SqlText ns, SqlText k]
    ) ->
      case Map.lookup (cat, ns, k) (msProps st) of
        Just _ -> (st {msProps = Map.insert (cat, ns, k) v (msProps st)}, 1)
        Nothing -> (st, 0)
  ( "DELETE FROM iceberg_namespace_properties \
    \WHERE catalog_name = ? AND namespace = ? AND property_key = ?"
    , [SqlText cat, SqlText ns, SqlText k]
    ) ->
      case Map.lookup (cat, ns, k) (msProps st) of
        Just _ -> (st {msProps = Map.delete (cat, ns, k) (msProps st)}, 1)
        Nothing -> (st, 0)
  ( "DELETE FROM iceberg_namespace_properties \
    \WHERE catalog_name = ? AND namespace = ?"
    , [SqlText cat, SqlText ns]
    ) ->
      let keep =
            Map.filterWithKey
              (\(c, n, _) _ -> not (c == cat && n == ns))
              (msProps st)
      in (st {msProps = keep}, 1)
  _ -> error ("memBackend: unrecognised execute: " ++ T.unpack (normalise q))


sqlMaybe :: SqlValue -> Maybe Text
sqlMaybe (SqlText t) = Just t
sqlMaybe SqlNull = Nothing


-- ============================================================
-- Tests
-- ============================================================

ns1 :: Vector Text
ns1 = V.singleton "sales"


mkCat :: IO (SqlCatalog, IORef MemState)
mkCat = do
  ref <- newIORef emptyMem
  pure (mkSqlCatalog "main" (memBackend ref), ref)


tests :: Spec
tests =
  describe "Iceberg.Catalog.Sql" $
    sequence_
      [ it "createTable + currentMetadataLocation" $ do
          (cat, _) <- mkCat
          r <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          r `shouldBe` Right ()
          loc <- currentMetadataLocation cat ns1 "orders"
          loc `shouldBe` Just "s3://b/m/v0.json"
      , it "createTable rejects duplicates" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          r <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          case r of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected TableAlreadyExists"
      , it "commitTable advances metadata location with valid CAS" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          r <-
            commitTable
              cat
              ns1
              "orders"
              (Just "s3://b/m/v0.json")
              "s3://b/m/v1.json"
          r `shouldBe` Right ()
          loc <- currentMetadataLocation cat ns1 "orders"
          loc `shouldBe` Just "s3://b/m/v1.json"
      , it "commitTable rejects stale CAS" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          _ <- commitTable cat ns1 "orders" (Just "s3://b/m/v0.json") "s3://b/m/v1.json"
          r <- commitTable cat ns1 "orders" (Just "s3://b/m/v0.json") "s3://b/m/v2.json"
          case r of
            Left (CommitConflict _ _) -> pure ()
            _ -> expectationFailure "expected CommitConflict"
      , it "commitTable from staged-create (NULL previous)" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" Nothing
          r <- commitTable cat ns1 "orders" Nothing "s3://b/m/v1.json"
          r `shouldBe` Right ()
      , it "renameTable" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "s3://b/m/v0.json")
          r <- renameTable cat ns1 "orders" ns1 "orders_v2"
          r `shouldBe` Right ()
          old <- currentMetadataLocation cat ns1 "orders"
          old `shouldBe` Nothing
          new <- currentMetadataLocation cat ns1 "orders_v2"
          new `shouldBe` Just "s3://b/m/v0.json"
      , it "listTables in namespace" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "a" (Just "x")
          _ <- createTable cat ns1 "b" (Just "y")
          _ <- createTable cat (V.singleton "other") "c" (Just "z")
          ts <- listTables cat ns1
          V.toList ts `shouldBe` ["a", "b"]
      , it "dropTable removes the row" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "x")
          _ <- dropTable cat ns1 "orders"
          loc <- currentMetadataLocation cat ns1 "orders"
          loc `shouldBe` Nothing
      , it "namespace property updates round-trip" $ do
          (cat, _) <- mkCat
          _ <- createNamespace cat ns1 (Map.fromList [("owner", "team-a")])
          props0 <- loadNamespaceProperties cat ns1
          Map.lookup "owner" props0 `shouldBe` Just "team-a"
          updateNamespaceProperties
            cat
            ns1
            (V.singleton "owner")
            (Map.fromList [("retention", "30d"), ("owner", "team-b")])
          props1 <- loadNamespaceProperties cat ns1
          Map.lookup "owner" props1 `shouldBe` Just "team-b"
          Map.lookup "retention" props1 `shouldBe` Just "30d"
      , it "dropNamespace refuses non-empty" $ do
          (cat, _) <- mkCat
          _ <- createNamespace cat ns1 Map.empty
          _ <- createTable cat ns1 "orders" (Just "x")
          r <- dropNamespace cat ns1
          case r of
            Left (NamespaceNotEmpty _) -> pure ()
            _ -> expectationFailure "expected NamespaceNotEmpty"
      , it "listNamespaces unions tables + properties" $ do
          (cat, _) <- mkCat
          _ <- createTable cat ns1 "orders" (Just "x")
          _ <-
            createNamespace
              cat
              (V.singleton "marketing")
              (Map.singleton "k" "v")
          nss <- listNamespaces cat
          let names =
                map
                  (T.intercalate (T.singleton '\x1F') . V.toList)
                  (V.toList nss)
          ("sales" `elem` names) `shouldBe` True
          ("marketing" `elem` names) `shouldBe` True
      ]
