{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Iceberg "SQL" catalog (a.k.a. the JDBC catalog).

The Java SDK's name "JDBC catalog" is misleading: there's no actual
JDBC API contract on the wire. The catalog state is just two SQL
tables (@iceberg_tables@ and @iceberg_namespace_properties@) and
the protocol is row-level CAS on the metadata-location column. Any
relational store that supports the four required ops (select, two
conditional updates, insert) plus a transactional namespace-table
semantics is enough.

This module exposes:

* 'SqlBackend' — backend-agnostic interface; users supply the
  actual SQL execution. The standard schema constants
  ('createIcebergTablesDdl' etc.) are exported so callers can run
  them at startup, and 'Iceberg.Catalog.Sql.IORef' (planned) will
  ship a reference in-memory implementation that the test suite
  uses as a stand-in.
* 'SqlCatalog' / 'mkSqlCatalog' — handle.
* 'currentMetadataLocation' / 'commitTable' / 'createTable' /
  'dropTable' / 'listTables' / 'renameTable' on the table side;
  'listNamespaces' / 'createNamespace' / 'dropNamespace' /
  'loadNamespace' / 'updateNamespaceProperties' on the namespace
  side.

Optimistic concurrency: 'commitTable' fails atomically when the
@previousMetadataLocation@ the caller supplied no longer matches
the row's current @metadata_location@. Callers are expected to
refresh and retry the same way the REST and Hadoop catalogs do.
-}
module Iceberg.Catalog.Sql (
  -- * Backend interface
  SqlBackend (..),
  SqlValue (..),

  -- * Standard DDL
  createIcebergTablesDdl,
  createIcebergNamespacePropertiesDdl,

  -- * Catalog handle
  SqlCatalog (..),
  mkSqlCatalog,

  -- * Table operations
  currentMetadataLocation,
  commitTable,
  createTable,
  dropTable,
  listTables,
  renameTable,

  -- * Namespace operations
  listNamespaces,
  createNamespace,
  dropNamespace,
  loadNamespaceProperties,
  updateNamespaceProperties,

  -- * Errors
  SqlCatalogError (..),
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V


-- ============================================================
-- Backend
-- ============================================================

{- | Plain values that the catalog ever needs to bind into SQL
statements: text columns and the occasional NULL (modeled as
@SqlNull@). The catalog deliberately doesn't take SQL types it
doesn't use, so backends can implement the contract with a thin
adapter rather than a full SQL value model.
-}
data SqlValue
  = SqlText !Text
  | SqlNull
  deriving (Show, Eq)


{- | Backend-agnostic SQL execution. Each method takes a bound query
(positional @?@ placeholders, identical to the JDBC convention)
plus its argument vector.

Implementations are responsible for:

* Mapping 'SqlText' to a @VARCHAR@ bind (truncating to 255 / 1000 to
  match the standard schema is a /backend/ choice; the catalog
  itself never produces over-length text).
* Mapping 'SqlNull' to a NULL bind.
* Returning all rows for @sbQuery@ as a sequence of column-aligned
  'SqlValue' rows; @SqlNull@ for any column the row had as NULL.
* Returning the affected-row count for @sbExecute@ /and/
  @sbExecuteCas@. The CAS variant is identical to @sbExecute@
  except that it MUST be a single transaction so that the
  affected-row count of @0@ unambiguously means "the row's
  @WHERE@ clause didn't match" (not "the connection died after
  updating it").

The catalog only ever issues one statement per call; no
multi-statement batching is required.
-}
data SqlBackend = SqlBackend
  { sbQuery :: !(Text -> [SqlValue] -> IO (Vector (Vector SqlValue)))
  -- ^ @SELECT@-style query.
  , sbExecute :: !(Text -> [SqlValue] -> IO Int)
  -- ^ @INSERT@/@UPDATE@/@DELETE@. Returns affected-row count.
  , sbExecuteCas :: !(Text -> [SqlValue] -> IO Int)
  {- ^ Same as 'sbExecute' but used by the CAS commit path so
  backends can opt to wrap it in a transaction.
  -}
  }


-- ============================================================
-- Standard schema DDL
-- ============================================================

{- | DDL for the @iceberg_tables@ table the Java catalog uses. Run
once at catalog provisioning time.
-}
createIcebergTablesDdl :: Text
createIcebergTablesDdl =
  T.unlines
    [ "CREATE TABLE iceberg_tables ("
    , "  catalog_name               VARCHAR(255) NOT NULL,"
    , "  table_namespace            VARCHAR(255) NOT NULL,"
    , "  table_name                 VARCHAR(255) NOT NULL,"
    , "  metadata_location          VARCHAR(1000),"
    , "  previous_metadata_location VARCHAR(1000),"
    , "  PRIMARY KEY (catalog_name, table_namespace, table_name)"
    , ")"
    ]


{- | DDL for the @iceberg_namespace_properties@ table. Run once at
catalog provisioning time.
-}
createIcebergNamespacePropertiesDdl :: Text
createIcebergNamespacePropertiesDdl =
  T.unlines
    [ "CREATE TABLE iceberg_namespace_properties ("
    , "  catalog_name   VARCHAR(255) NOT NULL,"
    , "  namespace      VARCHAR(255) NOT NULL,"
    , "  property_key   VARCHAR(255) NOT NULL,"
    , "  property_value VARCHAR(1000),"
    , "  PRIMARY KEY (catalog_name, namespace, property_key)"
    , ")"
    ]


-- ============================================================
-- Catalog handle
-- ============================================================

{- | A SQL catalog handle. Carries the catalog name (so multiple
logical catalogs can share one database) and a 'SqlBackend'.
-}
data SqlCatalog = SqlCatalog
  { scCatalogName :: !Text
  , scBackend :: !SqlBackend
  }


mkSqlCatalog :: Text -> SqlBackend -> SqlCatalog
mkSqlCatalog = SqlCatalog


-- ============================================================
-- Errors
-- ============================================================

data SqlCatalogError
  = TableAlreadyExists !Text !Text
  | NoSuchTable !Text !Text
  | {- | The row's current
    metadata_location no longer
    matches the asserted previous.
    -}
    CommitConflict !Text !Text
  | NamespaceAlreadyExists !Text
  | NoSuchNamespace !Text
  | NamespaceNotEmpty !Text
  deriving (Show, Eq)


-- ============================================================
-- Table operations
-- ============================================================

{- | Look up the current metadata location for a table. Returns
'Nothing' if the row doesn't exist (i.e. the table hasn't been
created yet) or the row exists but its metadata_location is NULL
(i.e. it's been registered without a snapshot).
-}
currentMetadataLocation
  :: SqlCatalog -> Vector Text -> Text -> IO (Maybe Text)
currentMetadataLocation cat ns name = do
  rows <-
    sbQuery
      (scBackend cat)
      "SELECT metadata_location FROM iceberg_tables \
      \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
      [ SqlText (scCatalogName cat)
      , SqlText (encodeNamespace ns)
      , SqlText name
      ]
  pure $ case V.toList rows of
    (row : _) | V.length row >= 1 -> case V.unsafeIndex row 0 of
      SqlText t -> Just t
      SqlNull -> Nothing
    _ -> Nothing


{- | Create a row for a new table. Returns @Left TableAlreadyExists@
if the row already exists.

Tables created without an initial metadata-file pointer (i.e. the
@stage-create@ flow) pass 'Nothing' for the @metadataLocation@; the
subsequent 'commitTable' assertion that 'previous == Nothing' is
what advances them to a real snapshot.
-}
createTable
  :: SqlCatalog
  -> Vector Text
  -> Text
  -> Maybe Text
  -> IO (Either SqlCatalogError ())
createTable cat ns name metadataLocation = do
  existing <- currentMetadataLocation cat ns name
  case existing of
    Just _ -> pure (Left (TableAlreadyExists (encodeNamespace ns) name))
    Nothing -> do
      _ <-
        sbExecute
          (scBackend cat)
          "INSERT INTO iceberg_tables \
          \(catalog_name, table_namespace, table_name, metadata_location, previous_metadata_location) \
          \VALUES (?, ?, ?, ?, ?)"
          [ SqlText (scCatalogName cat)
          , SqlText (encodeNamespace ns)
          , SqlText name
          , maybe SqlNull SqlText metadataLocation
          , SqlNull
          ]
      pure (Right ())


{- | CAS commit: advance the table's @metadata_location@ atomically
only when its current value still matches @assertedPrevious@.
-}
commitTable
  :: SqlCatalog
  -> Vector Text
  -- ^ namespace
  -> Text
  -- ^ table name
  -> Maybe Text
  -- ^ asserted previous metadata location
  -> Text
  -- ^ new metadata location
  -> IO (Either SqlCatalogError ())
commitTable cat ns name assertedPrev newLoc = do
  affected <- case assertedPrev of
    Just prev ->
      sbExecuteCas
        (scBackend cat)
        "UPDATE iceberg_tables \
        \SET previous_metadata_location = metadata_location, \
        \    metadata_location = ? \
        \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ? \
        \  AND metadata_location = ?"
        [ SqlText newLoc
        , SqlText (scCatalogName cat)
        , SqlText (encodeNamespace ns)
        , SqlText name
        , SqlText prev
        ]
    Nothing ->
      sbExecuteCas
        (scBackend cat)
        "UPDATE iceberg_tables \
        \SET previous_metadata_location = metadata_location, \
        \    metadata_location = ? \
        \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ? \
        \  AND metadata_location IS NULL"
        [ SqlText newLoc
        , SqlText (scCatalogName cat)
        , SqlText (encodeNamespace ns)
        , SqlText name
        ]
  if affected == 1
    then pure (Right ())
    else pure (Left (CommitConflict (encodeNamespace ns) name))


dropTable :: SqlCatalog -> Vector Text -> Text -> IO (Either SqlCatalogError ())
dropTable cat ns name = do
  affected <-
    sbExecute
      (scBackend cat)
      "DELETE FROM iceberg_tables \
      \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
      [ SqlText (scCatalogName cat)
      , SqlText (encodeNamespace ns)
      , SqlText name
      ]
  if affected == 1
    then pure (Right ())
    else pure (Left (NoSuchTable (encodeNamespace ns) name))


-- | List tables in a namespace.
listTables :: SqlCatalog -> Vector Text -> IO (Vector Text)
listTables cat ns = do
  rows <-
    sbQuery
      (scBackend cat)
      "SELECT table_name FROM iceberg_tables \
      \WHERE catalog_name = ? AND table_namespace = ? \
      \ORDER BY table_name"
      [ SqlText (scCatalogName cat)
      , SqlText (encodeNamespace ns)
      ]
  pure $ V.mapMaybe firstColAsText rows
  where
    firstColAsText row
      | V.length row >= 1 = case V.unsafeIndex row 0 of
          SqlText t -> Just t
          SqlNull -> Nothing
      | otherwise = Nothing


renameTable
  :: SqlCatalog
  -> Vector Text
  -> Text
  -- ^ source ns + table
  -> Vector Text
  -> Text
  -- ^ target ns + table
  -> IO (Either SqlCatalogError ())
renameTable cat srcNs srcName dstNs dstName = do
  -- Fail early if the destination already exists, otherwise UPDATE
  -- can pollute the row identity.
  exists <- currentMetadataLocation cat dstNs dstName
  case exists of
    Just _ -> pure (Left (TableAlreadyExists (encodeNamespace dstNs) dstName))
    Nothing -> do
      affected <-
        sbExecute
          (scBackend cat)
          "UPDATE iceberg_tables \
          \SET table_namespace = ?, table_name = ? \
          \WHERE catalog_name = ? AND table_namespace = ? AND table_name = ?"
          [ SqlText (encodeNamespace dstNs)
          , SqlText dstName
          , SqlText (scCatalogName cat)
          , SqlText (encodeNamespace srcNs)
          , SqlText srcName
          ]
      if affected == 1
        then pure (Right ())
        else pure (Left (NoSuchTable (encodeNamespace srcNs) srcName))


-- ============================================================
-- Namespace operations
-- ============================================================

{- | List every namespace that has either a table or a property
recorded against it. This matches Java's @JdbcCatalog.listNamespaces@:
a namespace exists iff it appears in either of the two tables.
-}
listNamespaces :: SqlCatalog -> IO (Vector (Vector Text))
listNamespaces cat = do
  rows <-
    sbQuery
      (scBackend cat)
      "SELECT DISTINCT table_namespace FROM iceberg_tables WHERE catalog_name = ? \
      \UNION SELECT DISTINCT namespace FROM iceberg_namespace_properties WHERE catalog_name = ?"
      [SqlText (scCatalogName cat), SqlText (scCatalogName cat)]
  pure $ V.mapMaybe firstAsNamespace rows
  where
    firstAsNamespace row
      | V.length row >= 1 = case V.unsafeIndex row 0 of
          SqlText t -> Just (decodeNamespace t)
          SqlNull -> Nothing
      | otherwise = Nothing


createNamespace
  :: SqlCatalog
  -> Vector Text
  -> Map Text Text
  -> IO (Either SqlCatalogError ())
createNamespace cat ns props = do
  -- Atomicity is best-effort here; for a real backend this would
  -- live inside a transaction. The Java implementation issues N+1
  -- inserts and relies on the unique key to detect collisions; we
  -- mirror that.
  if Map.null props
    then do
      _ <-
        sbExecute
          (scBackend cat)
          "INSERT INTO iceberg_namespace_properties \
          \(catalog_name, namespace, property_key, property_value) \
          \VALUES (?, ?, ?, ?)"
          [ SqlText (scCatalogName cat)
          , SqlText (encodeNamespace ns)
          , SqlText "exists"
          , SqlText "true"
          ]
      pure (Right ())
    else do
      mapM_
        ( \(k, v) ->
            sbExecute
              (scBackend cat)
              "INSERT INTO iceberg_namespace_properties \
              \(catalog_name, namespace, property_key, property_value) \
              \VALUES (?, ?, ?, ?)"
              [ SqlText (scCatalogName cat)
              , SqlText (encodeNamespace ns)
              , SqlText k
              , SqlText v
              ]
        )
        (Map.toList props)
      pure (Right ())


dropNamespace :: SqlCatalog -> Vector Text -> IO (Either SqlCatalogError ())
dropNamespace cat ns = do
  -- Refuse to drop a namespace that still contains tables.
  tbls <- listTables cat ns
  if not (V.null tbls)
    then pure (Left (NamespaceNotEmpty (encodeNamespace ns)))
    else do
      _ <-
        sbExecute
          (scBackend cat)
          "DELETE FROM iceberg_namespace_properties \
          \WHERE catalog_name = ? AND namespace = ?"
          [SqlText (scCatalogName cat), SqlText (encodeNamespace ns)]
      pure (Right ())


loadNamespaceProperties
  :: SqlCatalog -> Vector Text -> IO (Map Text Text)
loadNamespaceProperties cat ns = do
  rows <-
    sbQuery
      (scBackend cat)
      "SELECT property_key, property_value FROM iceberg_namespace_properties \
      \WHERE catalog_name = ? AND namespace = ?"
      [SqlText (scCatalogName cat), SqlText (encodeNamespace ns)]
  pure (V.foldl' addRow Map.empty rows)
  where
    addRow !acc row
      | V.length row >= 2 =
          case (V.unsafeIndex row 0, V.unsafeIndex row 1) of
            (SqlText k, SqlText v) -> Map.insert k v acc
            _ -> acc
      | otherwise = acc


updateNamespaceProperties
  :: SqlCatalog
  -> Vector Text
  -- ^ namespace
  -> Vector Text
  -- ^ properties to remove
  -> Map Text Text
  -- ^ properties to insert / overwrite
  -> IO ()
updateNamespaceProperties cat ns removals updates = do
  mapM_
    ( \k ->
        sbExecute
          (scBackend cat)
          "DELETE FROM iceberg_namespace_properties \
          \WHERE catalog_name = ? AND namespace = ? AND property_key = ?"
          [ SqlText (scCatalogName cat)
          , SqlText (encodeNamespace ns)
          , SqlText k
          ]
    )
    (V.toList removals)
  mapM_
    ( \(k, v) -> do
        -- Equivalent to @MERGE@: try update, fall back to insert.
        n <-
          sbExecute
            (scBackend cat)
            "UPDATE iceberg_namespace_properties \
            \SET property_value = ? \
            \WHERE catalog_name = ? AND namespace = ? AND property_key = ?"
            [ SqlText v
            , SqlText (scCatalogName cat)
            , SqlText (encodeNamespace ns)
            , SqlText k
            ]
        if n == 1
          then pure ()
          else do
            _ <-
              sbExecute
                (scBackend cat)
                "INSERT INTO iceberg_namespace_properties \
                \(catalog_name, namespace, property_key, property_value) \
                \VALUES (?, ?, ?, ?)"
                [ SqlText (scCatalogName cat)
                , SqlText (encodeNamespace ns)
                , SqlText k
                , SqlText v
                ]
            pure ()
    )
    (Map.toList updates)


-- ============================================================
-- Helpers
-- ============================================================

{- | Iceberg's namespace components are joined with the unit separator
(@\\u001F@), matching the URL convention the REST catalog uses.
This keeps the catalog table-format identical to what
iceberg-python and the Java SDK produce, so a row written by one
catalog instance is readable by another.
-}
encodeNamespace :: Vector Text -> Text
encodeNamespace = T.intercalate (T.singleton '\x1F') . V.toList


decodeNamespace :: Text -> Vector Text
decodeNamespace = V.fromList . T.splitOn (T.singleton '\x1F')
