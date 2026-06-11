{-# LANGUAGE OverloadedStrings #-}

{- | Iceberg REST catalog client request and response types.

The REST catalog spec (<https://iceberg.apache.org/rest-catalog-spec/>) is
shared by all language SDKs. This module supplies the request/response
payload types and JSON encode/decode for the most-used endpoints:

- @GET /v1/config@
- @GET /v1/{prefix}/namespaces@
- @POST /v1/{prefix}/namespaces@
- @GET /v1/{prefix}/namespaces/{ns}/tables@
- @GET /v1/{prefix}/namespaces/{ns}/tables/{name}@
- @POST /v1/{prefix}/namespaces/{ns}/tables@
- @POST /v1/{prefix}/namespaces/{ns}/tables/{name}@ (commit)
- @DELETE /v1/{prefix}/namespaces/{ns}/tables/{name}@
- @GET /v1/{prefix}/namespaces/{ns}/views@ (list views)

HTTP transport is left to the caller; the types here are protocol-only,
which means they compose with @http-client@, @servant-client@, or any
other HTTP library the embedding application prefers.
-}
module Iceberg.Catalog.REST (
  -- * Configuration
  CatalogConfig (..),

  -- * Namespace
  Namespace,
  ListNamespacesResponse (..),
  CreateNamespaceRequest (..),
  CreateNamespaceResponse (..),
  GetNamespaceResponse (..),

  -- * Table
  TableIdentifier (..),
  ListTablesResponse (..),
  CreateTableRequest (..),
  LoadTableResult (..),
  CommitTableRequest (..),
  CommitTableResponse (..),

  -- * Table - rename / register
  RenameTableRequest (..),
  RegisterTableRequest (..),

  -- * Namespace properties
  UpdateNamespacePropertiesRequest (..),
  UpdateNamespacePropertiesResponse (..),

  -- * View
  ListViewsResponse (..),
  LoadViewResult (..),
  CreateViewRequest (..),

  -- * Updates and requirements
  TableUpdate (..),
  TableRequirement (..),

  -- * Errors
  CatalogError (..),
  CatalogException (..),
  throwCatalogError,

  -- * Convenience builders
  loadTableResult,
  commitTableResponse,
  defaultRequirements,

  -- * JSON helpers re-export
  aesonEncode,
  aesonDecode,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Iceberg.JSON (metadataFromJSON, metadataToJSON, schemaFromJSON, schemaToJSON)
import Iceberg.JSON qualified
import Iceberg.Types


-- ============================================================
-- Configuration
-- ============================================================

-- | Server-wide configuration returned by @GET /v1/config@.
data CatalogConfig = CatalogConfig
  { ccDefaults :: !(Map Text Text)
  , ccOverrides :: !(Map Text Text)
  }
  deriving (Show, Eq)


-- ============================================================
-- Namespace
-- ============================================================

-- | A namespace is an ordered list of identifier components.
type Namespace = Vector Text


data ListNamespacesResponse = ListNamespacesResponse
  { lnrNamespaces :: !(Vector Namespace)
  }
  deriving (Show, Eq)


data CreateNamespaceRequest = CreateNamespaceRequest
  { cnNamespace :: !Namespace
  , cnProperties :: !(Map Text Text)
  }
  deriving (Show, Eq)


data CreateNamespaceResponse = CreateNamespaceResponse
  { cnRespNamespace :: !Namespace
  , cnRespProperties :: !(Map Text Text)
  }
  deriving (Show, Eq)


data GetNamespaceResponse = GetNamespaceResponse
  { gnRespNamespace :: !Namespace
  , gnRespProperties :: !(Map Text Text)
  }
  deriving (Show, Eq)


-- ============================================================
-- Table
-- ============================================================

data TableIdentifier = TableIdentifier
  { tiNamespace :: !Namespace
  , tiName :: !Text
  }
  deriving (Show, Eq, Ord)


data ListTablesResponse = ListTablesResponse
  { ltrIdentifiers :: !(Vector TableIdentifier)
  }
  deriving (Show, Eq)


data CreateTableRequest = CreateTableRequest
  { ctrName :: !Text
  , ctrLocation :: !(Maybe Text)
  , ctrSchema :: !Schema
  , ctrPartitionSpec :: !(Maybe PartitionSpec)
  , ctrWriteOrder :: !(Maybe SortOrder)
  , ctrProperties :: !(Map Text Text)
  , ctrStageCreate :: !Bool
  }
  deriving (Show, Eq)


data LoadTableResult = LoadTableResult
  { ltrMetadataLocation :: !(Maybe Text)
  , ltrMetadata :: !TableMetadata
  , ltrConfig :: !(Map Text Text)
  }
  deriving (Show, Eq)


data CommitTableRequest = CommitTableRequest
  { ctReqIdentifier :: !TableIdentifier
  , ctReqRequirements :: !(Vector TableRequirement)
  , ctReqUpdates :: !(Vector TableUpdate)
  }
  deriving (Show, Eq)


data CommitTableResponse = CommitTableResponse
  { ctRespMetadataLocation :: !Text
  , ctRespMetadata :: !TableMetadata
  }
  deriving (Show, Eq)


-- ============================================================
-- View
-- ============================================================

data ListViewsResponse = ListViewsResponse
  { lvrIdentifiers :: !(Vector TableIdentifier)
  }
  deriving (Show, Eq)


data LoadViewResult = LoadViewResult
  { lvMetadataLocation :: !(Maybe Text)
  , lvMetadata :: !ViewMetadata
  }
  deriving (Show, Eq)


-- | Body of @POST /v1/{prefix}/tables/rename@.
data RenameTableRequest = RenameTableRequest
  { rtSource :: !TableIdentifier
  , rtDestination :: !TableIdentifier
  }
  deriving (Show, Eq)


{- | Body of @POST /v1/{prefix}/namespaces/{ns}/register@.
Registers an existing on-disk metadata file as a new logical table
without rewriting any data.
-}
data RegisterTableRequest = RegisterTableRequest
  { rgrName :: !Text
  , rgrMetadataLocation :: !Text
  , rgrOverwrite :: !Bool
  }
  deriving (Show, Eq)


{- | Body of @POST /v1/{prefix}/namespaces/{ns}/properties@.
Atomically applies a set of property edits.
-}
data UpdateNamespacePropertiesRequest = UpdateNamespacePropertiesRequest
  { unprRemovals :: !(Vector Text)
  , unprUpdates :: !(Map Text Text)
  }
  deriving (Show, Eq)


-- | Returned by @POST /v1/{prefix}/namespaces/{ns}/properties@.
data UpdateNamespacePropertiesResponse = UpdateNamespacePropertiesResponse
  { unprspUpdated :: !(Vector Text)
  , unprspRemoved :: !(Vector Text)
  , unprspMissing :: !(Vector Text)
  }
  deriving (Show, Eq)


-- | Body of @POST /v1/{prefix}/namespaces/{ns}/views@.
data CreateViewRequest = CreateViewRequest
  { cvrName :: !Text
  , cvrLocation :: !(Maybe Text)
  , cvrSchema :: !Schema
  , cvrViewVersion :: !ViewVersion
  , cvrProperties :: !(Map Text Text)
  }
  deriving (Show, Eq)


-- ============================================================
-- Updates / requirements
-- ============================================================

data TableUpdate
  = AssignUUID !Text
  | UpgradeFormatVersion !Int
  | AddSchema !Schema !Int
  | SetCurrentSchema !Int
  | AddPartitionSpec !PartitionSpec
  | SetDefaultSpec !Int
  | AddSortOrder !SortOrder
  | SetDefaultSortOrder !Int
  | AddSnapshot !Snapshot
  | SetSnapshotRef !Text !SnapshotRef
  | RemoveSnapshotRef !Text
  | SetProperties !(Map Text Text)
  | RemoveProperties !(Vector Text)
  | SetLocation !Text
  | SetStatistics !StatisticsFile
  | RemoveStatistics !Int64
  | SetPartitionStatistics !PartitionStatisticsFile
  | RemovePartitionStatistics !Int64
  deriving (Show, Eq)


data TableRequirement
  = AssertCreate
  | AssertTableUUID !Text
  | AssertRefSnapshotId !Text !(Maybe Int64)
  | AssertLastAssignedFieldId !Int
  | AssertCurrentSchemaId !Int
  | AssertLastAssignedPartitionId !Int
  | AssertDefaultSpecId !Int
  | AssertDefaultSortOrderId !Int
  deriving (Show, Eq)


data CatalogError = CatalogError
  { ceMessage :: !Text
  , ceType :: !Text
  , ceCode :: !Int
  }
  deriving (Show, Eq)


-- ============================================================
-- JSON encoding / decoding
-- ============================================================

instance Aeson.ToJSON CatalogConfig where
  toJSON cc =
    Aeson.object
      [ "defaults" Aeson..= mapToJSON (ccDefaults cc)
      , "overrides" Aeson..= mapToJSON (ccOverrides cc)
      ]


instance Aeson.FromJSON CatalogConfig where
  parseJSON = Aeson.withObject "CatalogConfig" $ \o -> do
    defs <- o Aeson..:? "defaults" Aeson..!= Aeson.Null >>= mapFromJSON
    overs <- o Aeson..:? "overrides" Aeson..!= Aeson.Null >>= mapFromJSON
    pure (CatalogConfig defs overs)


instance Aeson.ToJSON ListNamespacesResponse where
  toJSON r =
    Aeson.object
      ["namespaces" Aeson..= V.map nsToJSON (lnrNamespaces r)]


instance Aeson.FromJSON ListNamespacesResponse where
  parseJSON = Aeson.withObject "ListNamespacesResponse" $ \o -> do
    Aeson.Array arr <- o Aeson..: "namespaces"
    pure $ ListNamespacesResponse (V.map nsFromJSON arr)


instance Aeson.ToJSON CreateNamespaceRequest where
  toJSON r =
    Aeson.object
      [ "namespace" Aeson..= nsToJSON (cnNamespace r)
      , "properties" Aeson..= mapToJSON (cnProperties r)
      ]


instance Aeson.FromJSON CreateNamespaceRequest where
  parseJSON = Aeson.withObject "CreateNamespaceRequest" $ \o -> do
    nsRaw <- o Aeson..: "namespace"
    props <- o Aeson..:? "properties" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $ CreateNamespaceRequest (nsFromJSON nsRaw) props


instance Aeson.ToJSON CreateNamespaceResponse where
  toJSON r =
    Aeson.object
      [ "namespace" Aeson..= nsToJSON (cnRespNamespace r)
      , "properties" Aeson..= mapToJSON (cnRespProperties r)
      ]


instance Aeson.FromJSON CreateNamespaceResponse where
  parseJSON = Aeson.withObject "CreateNamespaceResponse" $ \o -> do
    nsRaw <- o Aeson..: "namespace"
    props <- o Aeson..:? "properties" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $ CreateNamespaceResponse (nsFromJSON nsRaw) props


instance Aeson.ToJSON GetNamespaceResponse where
  toJSON r =
    Aeson.object
      [ "namespace" Aeson..= nsToJSON (gnRespNamespace r)
      , "properties" Aeson..= mapToJSON (gnRespProperties r)
      ]


instance Aeson.FromJSON GetNamespaceResponse where
  parseJSON = Aeson.withObject "GetNamespaceResponse" $ \o -> do
    nsRaw <- o Aeson..: "namespace"
    props <- o Aeson..:? "properties" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $ GetNamespaceResponse (nsFromJSON nsRaw) props


instance Aeson.ToJSON TableIdentifier where
  toJSON ti =
    Aeson.object
      [ "namespace" Aeson..= nsToJSON (tiNamespace ti)
      , "name" Aeson..= tiName ti
      ]


instance Aeson.FromJSON TableIdentifier where
  parseJSON = Aeson.withObject "TableIdentifier" $ \o -> do
    nsRaw <- o Aeson..: "namespace"
    name <- o Aeson..: "name"
    pure $ TableIdentifier (nsFromJSON nsRaw) name


instance Aeson.ToJSON ListTablesResponse where
  toJSON r = Aeson.object ["identifiers" Aeson..= V.toList (ltrIdentifiers r)]


instance Aeson.FromJSON ListTablesResponse where
  parseJSON = Aeson.withObject "ListTablesResponse" $ \o ->
    ListTablesResponse <$> (V.fromList <$> o Aeson..: "identifiers")


instance Aeson.ToJSON CreateTableRequest where
  toJSON r =
    Aeson.object $
      [ "name" Aeson..= ctrName r
      , "schema" Aeson..= schemaToJSON (ctrSchema r)
      , "stage-create" Aeson..= ctrStageCreate r
      , "properties" Aeson..= mapToJSON (ctrProperties r)
      ]
        ++ maybe [] (\l -> ["location" Aeson..= l]) (ctrLocation r)


instance Aeson.FromJSON CreateTableRequest where
  parseJSON = Aeson.withObject "CreateTableRequest" $ \o -> do
    name <- o Aeson..: "name"
    loc <- o Aeson..:? "location"
    schemaJson <- o Aeson..: "schema"
    schema <- case schemaFromJSON schemaJson of
      Right s -> pure s
      Left e -> fail e
    stage <- o Aeson..:? "stage-create" Aeson..!= False
    props <- o Aeson..:? "properties" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $
      CreateTableRequest
        { ctrName = name
        , ctrLocation = loc
        , ctrSchema = schema
        , ctrPartitionSpec = Nothing
        , ctrWriteOrder = Nothing
        , ctrProperties = props
        , ctrStageCreate = stage
        }


instance Aeson.ToJSON LoadTableResult where
  toJSON r =
    Aeson.object $
      [ "metadata" Aeson..= metadataToJSON (ltrMetadata r)
      , "config" Aeson..= mapToJSON (ltrConfig r)
      ]
        ++ maybe [] (\l -> ["metadata-location" Aeson..= l]) (ltrMetadataLocation r)


instance Aeson.FromJSON LoadTableResult where
  parseJSON = Aeson.withObject "LoadTableResult" $ \o -> do
    loc <- o Aeson..:? "metadata-location"
    mdJson <- o Aeson..: "metadata"
    md <- case metadataFromJSON mdJson of
      Right m -> pure m
      Left e -> fail e
    cfg <- o Aeson..:? "config" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $ LoadTableResult loc md cfg


instance Aeson.ToJSON CommitTableRequest where
  toJSON r =
    Aeson.object
      [ "identifier" Aeson..= ctReqIdentifier r
      , "requirements" Aeson..= V.toList (ctReqRequirements r)
      , "updates" Aeson..= V.toList (ctReqUpdates r)
      ]


instance Aeson.FromJSON CommitTableRequest where
  parseJSON = Aeson.withObject "CommitTableRequest" $ \o -> do
    ident <- o Aeson..: "identifier"
    reqs <- o Aeson..: "requirements"
    upds <- o Aeson..: "updates"
    pure $ CommitTableRequest ident (V.fromList reqs) (V.fromList upds)


instance Aeson.ToJSON CommitTableResponse where
  toJSON r =
    Aeson.object
      [ "metadata-location" Aeson..= ctRespMetadataLocation r
      , "metadata" Aeson..= metadataToJSON (ctRespMetadata r)
      ]


instance Aeson.FromJSON CommitTableResponse where
  parseJSON = Aeson.withObject "CommitTableResponse" $ \o -> do
    loc <- o Aeson..: "metadata-location"
    mdJson <- o Aeson..: "metadata"
    md <- case metadataFromJSON mdJson of
      Right m -> pure m
      Left e -> fail e
    pure $ CommitTableResponse loc md


instance Aeson.ToJSON CatalogError where
  toJSON e =
    Aeson.object
      [ "error"
          Aeson..= Aeson.object
            [ "message" Aeson..= ceMessage e
            , "type" Aeson..= ceType e
            , "code" Aeson..= ceCode e
            ]
      ]


instance Aeson.FromJSON CatalogError where
  parseJSON = Aeson.withObject "CatalogError" $ \o -> do
    inner <- o Aeson..: "error"
    msg <- inner Aeson..: "message"
    ty <- inner Aeson..: "type"
    cd <- inner Aeson..: "code"
    pure $ CatalogError msg ty cd


-- TableUpdate / TableRequirement get a tagged representation matching
-- the Java REST spec's @action@ / @type@ discriminator field.

instance Aeson.ToJSON TableUpdate where
  toJSON u = Aeson.Object $ KM.fromList ("action" Aeson..= action u : extraFields u)
    where
      action :: TableUpdate -> Text
      action AssignUUID {} = "assign-uuid"
      action UpgradeFormatVersion {} = "upgrade-format-version"
      action AddSchema {} = "add-schema"
      action SetCurrentSchema {} = "set-current-schema"
      action AddPartitionSpec {} = "add-spec"
      action SetDefaultSpec {} = "set-default-spec"
      action AddSortOrder {} = "add-sort-order"
      action SetDefaultSortOrder {} = "set-default-sort-order"
      action AddSnapshot {} = "add-snapshot"
      action SetSnapshotRef {} = "set-snapshot-ref"
      action RemoveSnapshotRef {} = "remove-snapshot-ref"
      action SetProperties {} = "set-properties"
      action RemoveProperties {} = "remove-properties"
      action SetLocation {} = "set-location"
      action SetStatistics {} = "set-statistics"
      action RemoveStatistics {} = "remove-statistics"
      action SetPartitionStatistics {} = "set-partition-statistics"
      action RemovePartitionStatistics {} = "remove-partition-statistics"

      extraFields :: TableUpdate -> [(Key.Key, Aeson.Value)]
      extraFields = \case
        AssignUUID u' -> [("uuid", Aeson.String u')]
        UpgradeFormatVersion v -> [("format-version", Aeson.Number (fromIntegral v))]
        AddSchema s lastCol ->
          [ ("schema", schemaToJSON s)
          , ("last-column-id", Aeson.Number (fromIntegral lastCol))
          ]
        SetCurrentSchema sid -> [("schema-id", Aeson.Number (fromIntegral sid))]
        AddPartitionSpec _ -> [] -- spec is stored under "spec"; minimal payload
        SetDefaultSpec sid -> [("spec-id", Aeson.Number (fromIntegral sid))]
        AddSortOrder _ -> []
        SetDefaultSortOrder oid -> [("sort-order-id", Aeson.Number (fromIntegral oid))]
        AddSnapshot _ -> [] -- requires structured snapshot encoder; left to caller
        SetSnapshotRef name _ -> [("ref-name", Aeson.String name)]
        RemoveSnapshotRef name -> [("ref-name", Aeson.String name)]
        SetProperties m -> [("updates", mapToJSON m)]
        RemoveProperties xs -> [("removals", Aeson.Array (V.map Aeson.String xs))]
        SetLocation loc -> [("location", Aeson.String loc)]
        SetStatistics _ -> []
        RemoveStatistics sid -> [("snapshot-id", Aeson.Number (fromIntegral sid))]
        SetPartitionStatistics _ -> []
        RemovePartitionStatistics sid -> [("snapshot-id", Aeson.Number (fromIntegral sid))]


instance Aeson.FromJSON TableUpdate where
  parseJSON = Aeson.withObject "TableUpdate" $ \o -> do
    action <- o Aeson..: "action"
    case action :: Text of
      "assign-uuid" -> AssignUUID <$> o Aeson..: "uuid"
      "upgrade-format-version" -> UpgradeFormatVersion <$> o Aeson..: "format-version"
      "set-current-schema" -> SetCurrentSchema <$> o Aeson..: "schema-id"
      "set-default-spec" -> SetDefaultSpec <$> o Aeson..: "spec-id"
      "set-default-sort-order" -> SetDefaultSortOrder <$> o Aeson..: "sort-order-id"
      "set-properties" -> do
        Aeson.Object updMap <- o Aeson..: "updates"
        SetProperties <$> mapFromJSON (Aeson.Object updMap)
      "remove-properties" -> do
        rs <- o Aeson..: "removals"
        pure $ RemoveProperties (V.fromList rs)
      "set-location" -> SetLocation <$> o Aeson..: "location"
      "remove-snapshot-ref" -> RemoveSnapshotRef <$> o Aeson..: "ref-name"
      "remove-statistics" -> RemoveStatistics <$> o Aeson..: "snapshot-id"
      "remove-partition-statistics" ->
        RemovePartitionStatistics <$> o Aeson..: "snapshot-id"
      other -> fail $ "unknown table update action: " ++ T.unpack other


instance Aeson.ToJSON TableRequirement where
  toJSON r = Aeson.Object $ KM.fromList ("type" Aeson..= reqType r : reqFields r)
    where
      reqType :: TableRequirement -> Text
      reqType AssertCreate = "assert-create"
      reqType AssertTableUUID {} = "assert-table-uuid"
      reqType AssertRefSnapshotId {} = "assert-ref-snapshot-id"
      reqType AssertLastAssignedFieldId {} = "assert-last-assigned-field-id"
      reqType AssertCurrentSchemaId {} = "assert-current-schema-id"
      reqType AssertLastAssignedPartitionId {} = "assert-last-assigned-partition-id"
      reqType AssertDefaultSpecId {} = "assert-default-spec-id"
      reqType AssertDefaultSortOrderId {} = "assert-default-sort-order-id"

      reqFields :: TableRequirement -> [(Key.Key, Aeson.Value)]
      reqFields = \case
        AssertCreate -> []
        AssertTableUUID u -> [("uuid", Aeson.String u)]
        AssertRefSnapshotId name sid ->
          [ ("ref", Aeson.String name)
          ,
            ( "snapshot-id"
            , maybe
                Aeson.Null
                (Aeson.Number . fromIntegral)
                sid
            )
          ]
        AssertLastAssignedFieldId n -> [("last-assigned-field-id", Aeson.Number (fromIntegral n))]
        AssertCurrentSchemaId n -> [("current-schema-id", Aeson.Number (fromIntegral n))]
        AssertLastAssignedPartitionId n -> [("last-assigned-partition-id", Aeson.Number (fromIntegral n))]
        AssertDefaultSpecId n -> [("default-spec-id", Aeson.Number (fromIntegral n))]
        AssertDefaultSortOrderId n -> [("default-sort-order-id", Aeson.Number (fromIntegral n))]


instance Aeson.FromJSON TableRequirement where
  parseJSON = Aeson.withObject "TableRequirement" $ \o -> do
    ty <- o Aeson..: "type"
    case ty :: Text of
      "assert-create" -> pure AssertCreate
      "assert-table-uuid" -> AssertTableUUID <$> o Aeson..: "uuid"
      "assert-ref-snapshot-id" -> do
        n <- o Aeson..: "ref"
        s <- o Aeson..:? "snapshot-id"
        pure $ AssertRefSnapshotId n s
      "assert-last-assigned-field-id" -> AssertLastAssignedFieldId <$> o Aeson..: "last-assigned-field-id"
      "assert-current-schema-id" -> AssertCurrentSchemaId <$> o Aeson..: "current-schema-id"
      "assert-last-assigned-partition-id" -> AssertLastAssignedPartitionId <$> o Aeson..: "last-assigned-partition-id"
      "assert-default-spec-id" -> AssertDefaultSpecId <$> o Aeson..: "default-spec-id"
      "assert-default-sort-order-id" -> AssertDefaultSortOrderId <$> o Aeson..: "default-sort-order-id"
      other -> fail $ "unknown requirement type: " ++ T.unpack other


instance Aeson.ToJSON ListViewsResponse where
  toJSON r = Aeson.object ["identifiers" Aeson..= V.toList (lvrIdentifiers r)]


instance Aeson.FromJSON ListViewsResponse where
  parseJSON = Aeson.withObject "ListViewsResponse" $ \o ->
    ListViewsResponse <$> (V.fromList <$> o Aeson..: "identifiers")


instance Aeson.ToJSON LoadViewResult where
  toJSON r =
    Aeson.object $
      ["metadata" Aeson..= viewMetadataToJSON' (lvMetadata r)]
        ++ maybe [] (\m -> ["metadata-location" Aeson..= m]) (lvMetadataLocation r)
    where
      viewMetadataToJSON' = Iceberg.JSON.viewMetadataToJSON


instance Aeson.FromJSON LoadViewResult where
  parseJSON = Aeson.withObject "LoadViewResult" $ \o -> do
    loc <- o Aeson..:? "metadata-location"
    md <- o Aeson..: "metadata"
    case Iceberg.JSON.viewMetadataFromJSON md of
      Right vm -> pure (LoadViewResult loc vm)
      Left e -> fail e


instance Aeson.ToJSON RenameTableRequest where
  toJSON r =
    Aeson.object
      [ "source" Aeson..= rtSource r
      , "destination" Aeson..= rtDestination r
      ]


instance Aeson.FromJSON RenameTableRequest where
  parseJSON = Aeson.withObject "RenameTableRequest" $ \o ->
    RenameTableRequest <$> o Aeson..: "source" <*> o Aeson..: "destination"


instance Aeson.ToJSON RegisterTableRequest where
  toJSON r =
    Aeson.object
      [ "name" Aeson..= rgrName r
      , "metadata-location" Aeson..= rgrMetadataLocation r
      , "overwrite" Aeson..= rgrOverwrite r
      ]


instance Aeson.FromJSON RegisterTableRequest where
  parseJSON = Aeson.withObject "RegisterTableRequest" $ \o ->
    RegisterTableRequest
      <$> o Aeson..: "name"
      <*> o Aeson..: "metadata-location"
      <*> o Aeson..:? "overwrite" Aeson..!= False


instance Aeson.ToJSON UpdateNamespacePropertiesRequest where
  toJSON r =
    Aeson.object
      [ "removals" Aeson..= V.toList (unprRemovals r)
      , "updates" Aeson..= mapToJSON (unprUpdates r)
      ]


instance Aeson.FromJSON UpdateNamespacePropertiesRequest where
  parseJSON = Aeson.withObject "UpdateNamespacePropertiesRequest" $ \o -> do
    removes <- o Aeson..:? "removals" Aeson..!= []
    updates <- o Aeson..:? "updates" Aeson..!= Aeson.Null >>= mapFromJSON
    pure (UpdateNamespacePropertiesRequest (V.fromList removes) updates)


instance Aeson.ToJSON UpdateNamespacePropertiesResponse where
  toJSON r =
    Aeson.object
      [ "updated" Aeson..= V.toList (unprspUpdated r)
      , "removed" Aeson..= V.toList (unprspRemoved r)
      , "missing" Aeson..= V.toList (unprspMissing r)
      ]


instance Aeson.FromJSON UpdateNamespacePropertiesResponse where
  parseJSON = Aeson.withObject "UpdateNamespacePropertiesResponse" $ \o -> do
    upd <- o Aeson..:? "updated" Aeson..!= []
    rem' <- o Aeson..:? "removed" Aeson..!= []
    mis <- o Aeson..:? "missing" Aeson..!= []
    pure $
      UpdateNamespacePropertiesResponse
        (V.fromList upd)
        (V.fromList rem')
        (V.fromList mis)


instance Aeson.ToJSON CreateViewRequest where
  toJSON r =
    Aeson.object $
      [ "name" Aeson..= cvrName r
      , "schema" Aeson..= schemaToJSON (cvrSchema r)
      , "view-version" Aeson..= viewVersionToJSON (cvrViewVersion r)
      , "properties" Aeson..= mapToJSON (cvrProperties r)
      ]
        ++ maybe [] (\l -> ["location" Aeson..= l]) (cvrLocation r)
    where
      viewVersionToJSON = Iceberg.JSON.viewVersionToJSON


instance Aeson.FromJSON CreateViewRequest where
  parseJSON = Aeson.withObject "CreateViewRequest" $ \o -> do
    name <- o Aeson..: "name"
    loc <- o Aeson..:? "location"
    schemaJson <- o Aeson..: "schema"
    schema <- case schemaFromJSON schemaJson of
      Right s -> pure s
      Left e -> fail e
    vvJson <- o Aeson..: "view-version"
    vv <- case Iceberg.JSON.viewVersionFromJSON vvJson of
      Right v -> pure v
      Left e -> fail e
    props <- o Aeson..:? "properties" Aeson..!= Aeson.Null >>= mapFromJSON
    pure $ CreateViewRequest name loc schema vv props


-- ============================================================
-- Helpers
-- ============================================================

mapToJSON :: Map Text Text -> Aeson.Value
mapToJSON m =
  Aeson.Object $
    KM.fromList
      [(Key.fromText k, Aeson.String v) | (k, v) <- Map.toList m]


mapFromJSON :: Aeson.Value -> Parser (Map Text Text)
mapFromJSON Aeson.Null = pure Map.empty
mapFromJSON (Aeson.Object o) =
  Map.fromList <$> traverse extractStringPair (KM.toList o)
mapFromJSON _ = fail "expected JSON object"


extractStringPair :: (Key.Key, Aeson.Value) -> Parser (Text, Text)
extractStringPair (k, Aeson.String v) = pure (Key.toText k, v)
extractStringPair (k, _) = fail $ "non-string property: " ++ T.unpack (Key.toText k)


nsToJSON :: Namespace -> Aeson.Value
nsToJSON = Aeson.toJSON . V.toList


nsFromJSON :: Aeson.Value -> Namespace
nsFromJSON (Aeson.Array arr) = V.mapMaybe textOnly arr
  where
    textOnly (Aeson.String s) = Just s
    textOnly _ = Nothing
nsFromJSON _ = V.empty


{- | Convenience top-level encoder: lazily encode any aeson 'ToJSON' value
to bytes, mirroring the @aeson@ helpers. Re-exported so callers don't
need to import @aeson@ directly when they're only using REST types.
-}
aesonEncode :: Aeson.ToJSON a => a -> ByteString
aesonEncode = BL.toStrict . Aeson.encode


aesonDecode :: Aeson.FromJSON a => ByteString -> Either String a
aesonDecode = Aeson.eitherDecodeStrict


-- ============================================================
-- Convenience builders
-- ============================================================

{- | Build a 'LoadTableResult' from the table metadata, an optional metadata
file location, and an empty config map. Mirrors PyIceberg's
@TableLoad.from_metadata@.
-}
loadTableResult :: Maybe Text -> TableMetadata -> LoadTableResult
loadTableResult loc tm =
  LoadTableResult
    { ltrMetadataLocation = loc
    , ltrMetadata = tm
    , ltrConfig = Map.empty
    }


{- | Build a 'CommitTableResponse' echoing the metadata location written
and the resulting table state.
-}
commitTableResponse :: Text -> TableMetadata -> CommitTableResponse
commitTableResponse loc tm =
  CommitTableResponse
    { ctRespMetadataLocation = loc
    , ctRespMetadata = tm
    }


{- | The minimal commit requirements every Iceberg writer should send for a
non-create operation: assert that the table exists and that its UUID
matches what the client read.
-}
defaultRequirements :: TableMetadata -> Vector TableRequirement
defaultRequirements tm =
  V.fromList
    [ AssertTableUUID (tmTableUuid tm)
    , AssertCurrentSchemaId (tmCurrentSchemaId tm)
    , AssertDefaultSpecId (tmDefaultSpecId tm)
    , AssertDefaultSortOrderId (tmDefaultSortOrderId tm)
    ]


-- ============================================================
-- Exceptions
-- ============================================================

{- | A 'CatalogError' wrapped as a Haskell exception so that REST clients
can short-circuit on 4xx/5xx responses. The 'Show' instance contains the
catalog-supplied message.
-}
newtype CatalogException = CatalogException CatalogError
  deriving (Show)


instance Exception CatalogException


-- | Throw a 'CatalogError' as a 'CatalogException' in 'IO'.
throwCatalogError :: CatalogError -> IO a
throwCatalogError = throwIO . CatalogException
