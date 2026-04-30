{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Iceberg AWS Glue catalog dialect.
--
-- Glue stores each Iceberg table as a Glue 'Table' record whose
-- @Parameters@ map carries the Iceberg-specific properties:
--
-- @
-- table_type           = "ICEBERG"
-- metadata_location    = "s3://...vN.metadata.json"
-- previous_metadata_location (optional)
-- @
--
-- The commit protocol is a conditional 'UpdateTable': when the
-- caller asserts the previous metadata location, the request
-- succeeds only if Glue's stored value still matches. (Glue
-- supports this via its 'IfMatchVersion' field on UpdateTable; we
-- expose it as a CAS the same way the SQL catalog does.)
--
-- Wire transport is intentionally factored out: this module
-- depends on a 'GlueBackend' record that the user wires to
-- @amazonka-glue@, @aws-sdk-go@-via-FFI, or any hand-rolled
-- SigV4 transport. We deliberately don't dictate which AWS SDK
-- to bring in.
module Iceberg.Catalog.Glue
  ( -- * Backend interface
    GlueBackend (..)
  , GlueTable (..)
  , GlueColumn (..)
    -- * Catalog handle
  , GlueCatalog (..)
  , mkGlueCatalog
    -- * Table operations
  , currentMetadataLocation
  , createTable
  , commitTable
  , dropTable
  , listTables
    -- * Iceberg <-> Glue parameter map
  , icebergParameters
  , parseIcebergParameters
    -- * Errors
  , GlueCatalogError (..)
    -- * Convenience: minimal Glue table description for a metadata location
  , minimalGlueTable
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)

-- ============================================================
-- Backend interface
-- ============================================================

-- | Minimal subset of the AWS Glue 'Table' record. The catalog
-- writes / reads the @Parameters@ map (which carries the Iceberg
-- @metadata_location@ + @table_type@) plus the
-- @StorageDescriptor.Location@ field. Other Glue table fields are
-- exposed for completeness but the catalog itself only needs the
-- @parameters@ + @location@ to do its job.
data GlueTable = GlueTable
  { gtName          :: !Text
  , gtDatabaseName  :: !Text
  , gtLocation      :: !Text
  , gtTableType     :: !Text          -- always "EXTERNAL_TABLE" for Iceberg
  , gtColumns       :: !(Vector GlueColumn)
  , gtParameters    :: !(Map Text Text)
  } deriving (Show, Eq)

-- | One column in 'GlueTable'. Iceberg readers don't actually
-- consult these (the schema is in the metadata.json on S3), but
-- Glue requires at least one column for the table to be queryable
-- by Athena / Redshift. Callers usually emit a placeholder.
data GlueColumn = GlueColumn
  { gcName :: !Text
  , gcType :: !Text                  -- Glue type string (e.g. "string")
  } deriving (Show, Eq)

-- | Backend-agnostic Glue HTTP API contract. Each method returns
-- 'Either' so callers can surface AWS errors without exceptions.
--
-- The catalog only ever needs five operations; that's all that's
-- here. SigV4, retries, throttling, etc. are the implementer's
-- responsibility.
data GlueBackend = GlueBackend
  { gbGetTable      :: Text -> Text -> IO (Either Text (Maybe GlueTable))
    -- ^ @database, name -> Maybe Table@. @Nothing@ when EntityNotFoundException.
  , gbCreateTable   :: Text -> GlueTable -> IO (Either Text ())
    -- ^ @database, table@; fails if the table already exists.
  , gbUpdateTable   :: Text -> GlueTable -> Maybe Text -> IO (Either Text ())
    -- ^ @database, table, IfMatchVersionId@. The @IfMatchVersionId@
    --   carries the previous metadata_location for CAS. Glue's
    --   actual UpdateTable accepts a 'VersionId' field; the
    --   contract here is "if Just, the operation must fail when
    --   Glue's current metadata_location doesn't match that".
  , gbDeleteTable   :: Text -> Text -> IO (Either Text ())
  , gbListTables    :: Text -> IO (Either Text (Vector Text))
    -- ^ @database -> table names@. Filters server-side to
    --   table_type == ICEBERG when the backend supports it.
  }

-- ============================================================
-- Catalog handle
-- ============================================================

data GlueCatalog = GlueCatalog
  { gcDatabase :: !Text
  , gcBackend  :: !GlueBackend
  }

mkGlueCatalog :: Text -> GlueBackend -> GlueCatalog
mkGlueCatalog = GlueCatalog

-- ============================================================
-- Errors
-- ============================================================

data GlueCatalogError
  = GlueTableAlreadyExists !Text
  | GlueNoSuchTable !Text
  | GlueCommitConflict !Text
  | GlueBackendError !Text
  | GlueNotIcebergTable !Text
  deriving (Show, Eq)

-- ============================================================
-- Table operations
-- ============================================================

-- | Look up the current metadata location for an Iceberg table.
-- Returns 'Nothing' when the table doesn't exist or doesn't have
-- the @table_type@ Iceberg marker.
currentMetadataLocation :: GlueCatalog -> Text -> IO (Maybe Text)
currentMetadataLocation cat name = do
  res <- gbGetTable (gcBackend cat) (gcDatabase cat) name
  case res of
    Right (Just t)
      | Map.lookup "table_type" (gtParameters t) == Just "ICEBERG" ->
          pure (Map.lookup "metadata_location" (gtParameters t))
    _ -> pure Nothing

-- | Create a new Iceberg table in Glue. Composes 'minimalGlueTable'
-- for the Glue side and rejects pre-existing rows.
createTable
  :: GlueCatalog
  -> Text                       -- ^ table name
  -> Text                       -- ^ initial metadata_location
  -> Text                       -- ^ S3 location for the table directory
  -> IO (Either GlueCatalogError ())
createTable cat name metadataLoc tableLoc = do
  existing <- gbGetTable (gcBackend cat) (gcDatabase cat) name
  case existing of
    Left e -> pure (Left (GlueBackendError e))
    Right (Just _) -> pure (Left (GlueTableAlreadyExists name))
    Right Nothing -> do
      let !t = minimalGlueTable name (gcDatabase cat) tableLoc
                 (Map.fromList
                    [ ("table_type",        "ICEBERG")
                    , ("metadata_location", metadataLoc)
                    ])
      r <- gbCreateTable (gcBackend cat) (gcDatabase cat) t
      case r of
        Right ()  -> pure (Right ())
        Left  err -> pure (Left (GlueBackendError err))

-- | CAS commit on the metadata location.
commitTable
  :: GlueCatalog
  -> Text                       -- ^ table name
  -> Maybe Text                 -- ^ asserted previous metadata location
  -> Text                       -- ^ new metadata location
  -> IO (Either GlueCatalogError ())
commitTable cat name assertedPrev newLoc = do
  res <- gbGetTable (gcBackend cat) (gcDatabase cat) name
  case res of
    Left e -> pure (Left (GlueBackendError e))
    Right Nothing -> pure (Left (GlueNoSuchTable name))
    Right (Just t)
      | Map.lookup "table_type" (gtParameters t) /= Just "ICEBERG" ->
          pure (Left (GlueNotIcebergTable name))
      | Map.lookup "metadata_location" (gtParameters t) /= assertedPrev ->
          pure (Left (GlueCommitConflict name))
      | otherwise -> do
          let !params' = Map.insert "metadata_location" newLoc
                       . Map.insert "previous_metadata_location"
                           (case assertedPrev of
                              Just p  -> p
                              Nothing -> "")
                       $ gtParameters t
              !t' = t { gtParameters = params' }
          r <- gbUpdateTable (gcBackend cat) (gcDatabase cat) t' assertedPrev
          case r of
            Right ()  -> pure (Right ())
            Left  err -> pure (Left (GlueCommitConflict (name <> ": " <> err)))

dropTable :: GlueCatalog -> Text -> IO (Either GlueCatalogError ())
dropTable cat name = do
  r <- gbDeleteTable (gcBackend cat) (gcDatabase cat) name
  case r of
    Right () -> pure (Right ())
    Left e   -> pure (Left (GlueBackendError e))

listTables :: GlueCatalog -> IO (Either GlueCatalogError (Vector Text))
listTables cat = do
  r <- gbListTables (gcBackend cat) (gcDatabase cat)
  pure (either (Left . GlueBackendError) Right r)

-- ============================================================
-- Parameter helpers
-- ============================================================

-- | Build the Iceberg-specific @Parameters@ entries that go on a
-- Glue 'GlueTable'. Use this when constructing Glue tables from
-- scratch outside of 'createTable'.
icebergParameters :: Text -> Maybe Text -> Map Text Text
icebergParameters metadataLoc mPrev =
  let base = Map.fromList
        [ ("table_type",        "ICEBERG")
        , ("metadata_location", metadataLoc)
        ]
   in case mPrev of
        Just p  -> Map.insert "previous_metadata_location" p base
        Nothing -> base

-- | Inverse of 'icebergParameters': pull the Iceberg-specific
-- entries out of a Glue 'Parameters' map and return them as
-- @(metadata_location, previous_metadata_location)@. Returns
-- 'Nothing' when @table_type@ isn't @ICEBERG@.
parseIcebergParameters :: Map Text Text -> Maybe (Text, Maybe Text)
parseIcebergParameters params = do
  ty <- Map.lookup "table_type" params
  if ty /= "ICEBERG"
    then Nothing
    else do
      ml <- Map.lookup "metadata_location" params
      let pl = Map.lookup "previous_metadata_location" params
      pure (ml, pl)

-- | A minimal 'GlueTable' suitable for Iceberg storage. Glue
-- requires a non-empty column list to make the table queryable
-- by Athena, so we emit a single placeholder column; downstream
-- engines that read Iceberg load the real schema from the metadata
-- file at @metadata_location@.
minimalGlueTable :: Text -> Text -> Text -> Map Text Text -> GlueTable
minimalGlueTable name db location params = GlueTable
  { gtName         = name
  , gtDatabaseName = db
  , gtLocation     = location
  , gtTableType    = "EXTERNAL_TABLE"
  , gtColumns      = V.singleton (GlueColumn "iceberg" "string")
    -- The placeholder column is what iceberg-python emits when it
    -- creates a Glue table; engines ignore it and read the real
    -- schema from the metadata file on S3.
  , gtParameters   = params
  }

