{-# LANGUAGE BangPatterns #-}
-- | JSON serialization for Apache Iceberg table metadata.
--
-- Per the Iceberg specification, table metadata is stored as JSON. This
-- module provides 'metadataToJSON' / 'metadataFromJSON' which round-trip
-- the full 'TableMetadata' tree, including v2 fields (snapshot refs,
-- sequence numbers, identifier field ids, statistics file refs, partition
-- statistics, metadata-log) and v3 extensions (default values, encryption
-- keys, row lineage, nanosecond timestamps, geospatial types, multi-arg
-- transforms, name mapping). View metadata is encoded by 'viewMetadataToJSON'.
module Iceberg.JSON
  ( -- * Table metadata
    metadataToJSON
  , metadataFromJSON
    -- * View metadata
  , viewMetadataToJSON
  , viewMetadataFromJSON
  , viewVersionToJSON
  , viewVersionFromJSON
    -- * Schema fragments
  , schemaToJSON
  , schemaFromJSON
  , icebergTypeToJSON
  , icebergTypeFromJSON
  , transformToJSON
  , transformFromJSON
    -- * Partition spec
  , partitionSpecToJSON
  , partitionSpecFromJSON
  , partitionFieldToJSON
  , partitionFieldFromJSON
    -- * Name mapping
  , nameMappingToJSON
  , nameMappingFromJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import qualified Avro.Value as AV

import Iceberg.Types

-- ============================================================
-- Table metadata
-- ============================================================

-- | Encode 'TableMetadata' to its JSON representation.
metadataToJSON :: TableMetadata -> Aeson.Value
metadataToJSON tm = Aeson.Object $ KM.fromList $
  [ ("format-version",       Aeson.Number (fromIntegral (tmFormatVersion tm)))
  , ("table-uuid",           Aeson.String (tmTableUuid tm))
  , ("location",             Aeson.String (tmLocation tm))
  , ("last-sequence-number", Aeson.Number (fromIntegral (tmLastSequenceNumber tm)))
  , ("last-updated-ms",      Aeson.Number (fromIntegral (tmLastUpdatedMs tm)))
  , ("last-column-id",       Aeson.Number (fromIntegral (tmLastColumnId tm)))
  , ("current-schema-id",    Aeson.Number (fromIntegral (tmCurrentSchemaId tm)))
  , ("schemas",              Aeson.Array (V.map schemaToJSON (tmSchemas tm)))
  , ("current-snapshot-id",  maybe Aeson.Null (Aeson.Number . fromIntegral) (tmCurrentSnapshotId tm))
  , ("snapshots",            Aeson.Array (V.map snapshotToJSON (tmSnapshots tm)))
  , ("partition-specs",      Aeson.Array (V.map partitionSpecToJSON (tmPartitionSpecs tm)))
  , ("default-spec-id",      Aeson.Number (fromIntegral (tmDefaultSpecId tm)))
  , ("last-partition-id",    Aeson.Number (fromIntegral (tmLastPartitionId tm)))
  , ("sort-orders",          Aeson.Array (V.map sortOrderToJSON (tmSortOrders tm)))
  , ("default-sort-order-id", Aeson.Number (fromIntegral (tmDefaultSortOrderId tm)))
  , ("properties",           mapToJSON (tmProperties tm))
  , ("snapshot-log",         Aeson.Array (V.map snapshotLogEntryToJSON (tmSnapshotLog tm)))
  , ("metadata-log",         Aeson.Array (V.map metadataLogEntryToJSON (tmMetadataLog tm)))
  , ("snapshot-refs",        snapshotRefsToJSON (tmSnapshotRefs tm))
  ]
  ++ optArrayField "statistics"           statisticsFileToJSON (tmStatistics tm)
  ++ optArrayField "partition-statistics" partitionStatisticsFileToJSON (tmPartitionStatistics tm)
  ++ maybe [] (\rid -> [("next-row-id", Aeson.Number (fromIntegral rid))]) (tmNextRowId tm)
  ++ (if Map.null (tmEncryptionKeys tm) then [] else [("encryption-keys", mapToJSON (tmEncryptionKeys tm))])

-- | Decode 'TableMetadata' from its JSON representation.
metadataFromJSON :: Aeson.Value -> Either String TableMetadata
metadataFromJSON (Aeson.Object obj) = do
  fmtVer   <- reqInt "format-version" obj
  uuid     <- reqStr "table-uuid" obj
  loc      <- reqStr "location" obj
  lastSeq  <- optInt64 "last-sequence-number" obj
  lastUpd  <- reqInt64 "last-updated-ms" obj
  lastCol  <- reqInt "last-column-id" obj
  curSch   <- reqInt "current-schema-id" obj
  schemas  <- reqArray "schemas" obj >>= V.mapM schemaFromJSON
  curSnap  <- optInt64 "current-snapshot-id" obj
  snaps    <- reqArray "snapshots" obj >>= V.mapM snapshotFromJSON
  pspecs   <- reqArray "partition-specs" obj >>= V.mapM partitionSpecFromJSON
  defSpec  <- reqInt "default-spec-id" obj
  lastPart <- optIntDef "last-partition-id" obj 0
  sords    <- reqArray "sort-orders" obj >>= V.mapM sortOrderFromJSON
  defSort  <- reqInt "default-sort-order-id" obj
  props    <- mapFromJSON "properties" obj
  slog     <- reqArray "snapshot-log" obj >>= V.mapM snapshotLogEntryFromJSON
  mlog     <- optArrayField' "metadata-log" obj metadataLogEntryFromJSON
  refs     <- snapshotRefsFromJSON obj
  stats    <- optArrayField' "statistics" obj statisticsFileFromJSON
  pstats   <- optArrayField' "partition-statistics" obj partitionStatisticsFileFromJSON
  nextRow  <- optInt64 "next-row-id" obj
  encKeys  <- case KM.lookup "encryption-keys" obj of
                Just _  -> mapFromJSON "encryption-keys" obj
                Nothing -> Right Map.empty
  Right TableMetadata
    { tmFormatVersion       = fmtVer
    , tmTableUuid           = uuid
    , tmLocation            = loc
    , tmLastSequenceNumber  = maybe 0 id lastSeq
    , tmLastUpdatedMs       = lastUpd
    , tmLastColumnId        = lastCol
    , tmCurrentSchemaId     = curSch
    , tmSchemas             = schemas
    , tmCurrentSnapshotId   = curSnap
    , tmSnapshots           = snaps
    , tmPartitionSpecs      = pspecs
    , tmDefaultSpecId       = defSpec
    , tmLastPartitionId     = lastPart
    , tmSortOrders          = sords
    , tmDefaultSortOrderId  = defSort
    , tmProperties          = props
    , tmSnapshotLog         = slog
    , tmMetadataLog         = mlog
    , tmSnapshotRefs        = refs
    , tmStatistics          = stats
    , tmPartitionStatistics = pstats
    , tmNextRowId           = nextRow
    , tmEncryptionKeys      = encKeys
    }
metadataFromJSON _ = Left "table metadata must be a JSON object"

-- ============================================================
-- Schema
-- ============================================================

schemaToJSON :: Schema -> Aeson.Value
schemaToJSON s = Aeson.Object $ KM.fromList $
  [ ("schema-id", Aeson.Number (fromIntegral (schemaId s)))
  , ("type", Aeson.String "struct")
  , ("fields", Aeson.Array (V.map structFieldToJSON (schemaFields s)))
  ]
  ++ (if V.null (schemaIdentifierFieldIds s)
        then []
        else [("identifier-field-ids", Aeson.Array (V.map (Aeson.Number . fromIntegral) (schemaIdentifierFieldIds s)))])

schemaFromJSON :: Aeson.Value -> Either String Schema
schemaFromJSON (Aeson.Object obj) = do
  sid    <- reqInt "schema-id" obj
  fields <- reqArray "fields" obj >>= V.mapM structFieldFromJSON
  ids    <- case KM.lookup "identifier-field-ids" obj of
              Just (Aeson.Array arr) -> V.mapM (numericInt "identifier-field-ids") arr
              Just Aeson.Null        -> Right V.empty
              Nothing                -> Right V.empty
              _                      -> Left "identifier-field-ids must be an array"
  Right Schema { schemaId = sid, schemaFields = fields, schemaIdentifierFieldIds = ids }
schemaFromJSON _ = Left "schema must be a JSON object"

-- ============================================================
-- StructField
-- ============================================================

structFieldToJSON :: StructField -> Aeson.Value
structFieldToJSON sf = Aeson.Object $ KM.fromList $
  [ ("id",       Aeson.Number (fromIntegral (sfId sf)))
  , ("name",     Aeson.String (sfName sf))
  , ("required", Aeson.Bool (sfRequired sf))
  , ("type",     icebergTypeToJSON (sfType sf))
  ]
  ++ maybe [] (\d -> [("doc", Aeson.String d)]) (sfDoc sf)
  ++ maybe [] (\v -> [("initial-default", defaultValueToJSON v)]) (sfInitialDefault sf)
  ++ maybe [] (\v -> [("write-default",   defaultValueToJSON v)]) (sfWriteDefault sf)

structFieldFromJSON :: Aeson.Value -> Either String StructField
structFieldFromJSON (Aeson.Object obj) = do
  fid  <- reqInt "id" obj
  name <- reqStr "name" obj
  req  <- reqBool "required" obj
  ty   <- case KM.lookup "type" obj of
            Just v  -> icebergTypeFromJSON v
            Nothing -> Left "field missing 'type'"
  let doc = case KM.lookup "doc" obj of
              Just (Aeson.String d) -> Just d
              _                     -> Nothing
  let initialDef = fmap defaultValueFromJSON (KM.lookup "initial-default" obj)
      writeDef   = fmap defaultValueFromJSON (KM.lookup "write-default" obj)
  Right StructField
    { sfId             = fid
    , sfName           = name
    , sfRequired       = req
    , sfType           = ty
    , sfDoc            = doc
    , sfInitialDefault = initialDef
    , sfWriteDefault   = writeDef
    }
structFieldFromJSON _ = Left "struct field must be a JSON object"

-- | Iceberg's JSON encoding stores defaults as raw JSON values. We mirror
-- them onto 'AV.Value' (the same carrier used for partition values) using
-- a small lossless mapping that round-trips primitives, arrays, and objects.
defaultValueToJSON :: DefaultValue -> Aeson.Value
defaultValueToJSON DefaultNull       = Aeson.Null
defaultValueToJSON (DefaultJSON av)  = avroToAeson av

defaultValueFromJSON :: Aeson.Value -> DefaultValue
defaultValueFromJSON Aeson.Null = DefaultNull
defaultValueFromJSON v          = DefaultJSON (aesonToAvro v)

avroToAeson :: AV.Value -> Aeson.Value
avroToAeson = \case
  AV.Null     -> Aeson.Null
  AV.Bool b   -> Aeson.Bool b
  AV.Int n    -> Aeson.Number (fromIntegral n)
  AV.Long n   -> Aeson.Number (fromIntegral n)
  AV.Float f  -> Aeson.Number (realToFrac f)
  AV.Double d -> Aeson.Number (realToFrac d)
  AV.Bytes bs -> Aeson.String (T.pack (show bs))
  AV.Fixed bs -> Aeson.String (T.pack (show bs))
  AV.String t -> Aeson.String t
  AV.Enum n   -> Aeson.Number (fromIntegral n)
  AV.Array xs -> Aeson.Array (V.map avroToAeson xs)
  AV.Map  xs  -> Aeson.Object (KM.fromList [(Key.fromText k, avroToAeson v) | (k, v) <- V.toList xs])
  AV.Record xs -> Aeson.Array (V.map avroToAeson xs)
  AV.Union _ inner -> avroToAeson inner

aesonToAvro :: Aeson.Value -> AV.Value
aesonToAvro = \case
  Aeson.Null     -> AV.Null
  Aeson.Bool b   -> AV.Bool b
  Aeson.Number n -> case toBoundedInteger n :: Maybe Int64 of
                      Just i  -> AV.Long i
                      Nothing -> AV.Double (realToFrac n)
  Aeson.String t -> AV.String t
  Aeson.Array xs -> AV.Array (V.map aesonToAvro xs)
  Aeson.Object o -> AV.Map (V.fromList [(Key.toText k, aesonToAvro v) | (k, v) <- KM.toList o])

-- ============================================================
-- IcebergType
-- ============================================================

icebergTypeToJSON :: IcebergType -> Aeson.Value
icebergTypeToJSON TBoolean        = Aeson.String "boolean"
icebergTypeToJSON TInt            = Aeson.String "int"
icebergTypeToJSON TLong           = Aeson.String "long"
icebergTypeToJSON TFloat          = Aeson.String "float"
icebergTypeToJSON TDouble         = Aeson.String "double"
icebergTypeToJSON TDate           = Aeson.String "date"
icebergTypeToJSON TTime           = Aeson.String "time"
icebergTypeToJSON TTimestamp      = Aeson.String "timestamp"
icebergTypeToJSON TTimestampTz    = Aeson.String "timestamptz"
icebergTypeToJSON TTimestampNs    = Aeson.String "timestamp_ns"
icebergTypeToJSON TTimestampTzNs  = Aeson.String "timestamptz_ns"
icebergTypeToJSON TString         = Aeson.String "string"
icebergTypeToJSON TUuid           = Aeson.String "uuid"
icebergTypeToJSON (TFixed n)      = Aeson.String (T.pack ("fixed[" ++ show n ++ "]"))
icebergTypeToJSON TBinary         = Aeson.String "binary"
icebergTypeToJSON (TDecimal p s)  = Aeson.String (T.pack ("decimal(" ++ show p ++ ", " ++ show s ++ ")"))
icebergTypeToJSON TUnknown        = Aeson.String "unknown"
icebergTypeToJSON TVariant        = Aeson.String "variant"
icebergTypeToJSON (TGeometry crs)
  | T.null crs = Aeson.String "geometry"
  | otherwise  = Aeson.String (T.concat ["geometry(", crs, ")"])
icebergTypeToJSON (TGeography crs algo) = Aeson.String $
  T.concat ["geography(", crs, ", ", algo, ")"]
icebergTypeToJSON (TStruct fields) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "struct")
  , ("fields", Aeson.Array (V.map structFieldToJSON fields))
  ]
icebergTypeToJSON (TList elemId elemTy) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "list")
  , ("element-id", Aeson.Number (fromIntegral elemId))
  , ("element", icebergTypeToJSON elemTy)
  , ("element-required", Aeson.Bool False)
  ]
icebergTypeToJSON (TMap keyId keyTy valId valTy) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "map")
  , ("key-id", Aeson.Number (fromIntegral keyId))
  , ("key", icebergTypeToJSON keyTy)
  , ("value-id", Aeson.Number (fromIntegral valId))
  , ("value", icebergTypeToJSON valTy)
  , ("value-required", Aeson.Bool False)
  ]

icebergTypeFromJSON :: Aeson.Value -> Either String IcebergType
icebergTypeFromJSON (Aeson.String s) = case s of
  "boolean"        -> Right TBoolean
  "int"            -> Right TInt
  "long"           -> Right TLong
  "float"          -> Right TFloat
  "double"         -> Right TDouble
  "date"           -> Right TDate
  "time"           -> Right TTime
  "timestamp"      -> Right TTimestamp
  "timestamptz"    -> Right TTimestampTz
  "timestamp_ns"   -> Right TTimestampNs
  "timestamptz_ns" -> Right TTimestampTzNs
  "string"         -> Right TString
  "uuid"           -> Right TUuid
  "binary"         -> Right TBinary
  "unknown"        -> Right TUnknown
  "variant"        -> Right TVariant
  "geometry"       -> Right (TGeometry T.empty)
  other
    | "fixed[" `T.isPrefixOf` other -> parseFixed other
    | "decimal(" `T.isPrefixOf` other -> parseDecimal other
    | "geometry(" `T.isPrefixOf` other -> parseGeometry other
    | "geography(" `T.isPrefixOf` other -> parseGeography other
    | otherwise -> Left $ "unknown iceberg type: " ++ T.unpack other
icebergTypeFromJSON (Aeson.Object obj) = do
  typStr <- reqStr "type" obj
  case typStr of
    "struct" -> do
      fields <- reqArray "fields" obj >>= V.mapM structFieldFromJSON
      Right (TStruct fields)
    "list" -> do
      elemId <- reqInt "element-id" obj
      elemTy <- case KM.lookup "element" obj of
                  Just v  -> icebergTypeFromJSON v
                  Nothing -> Left "list missing 'element'"
      Right (TList elemId elemTy)
    "map" -> do
      keyId <- reqInt "key-id" obj
      keyTy <- case KM.lookup "key" obj of
                 Just v  -> icebergTypeFromJSON v
                 Nothing -> Left "map missing 'key'"
      valId <- reqInt "value-id" obj
      valTy <- case KM.lookup "value" obj of
                 Just v  -> icebergTypeFromJSON v
                 Nothing -> Left "map missing 'value'"
      Right (TMap keyId keyTy valId valTy)
    other -> Left $ "unknown complex iceberg type: " ++ T.unpack other
icebergTypeFromJSON _ = Left "iceberg type must be a string or object"

parseFixed :: Text -> Either String IcebergType
parseFixed t =
  let inner = T.dropEnd 1 (T.drop 6 t)
  in case TR.decimal inner of
       Right (n, rest) | T.null rest -> Right (TFixed n)
       _ -> Left $ "invalid fixed type: " ++ T.unpack t

parseDecimal :: Text -> Either String IcebergType
parseDecimal t =
  let inner = T.dropEnd 1 (T.drop 8 t)
      parts = T.splitOn "," inner
  in case parts of
       [pTxt, sTxt] -> do
         (p, _) <- decAt "decimal precision" (T.strip pTxt)
         (s, _) <- decAt "decimal scale"     (T.strip sTxt)
         Right (TDecimal p s)
       _ -> Left $ "invalid decimal type: " ++ T.unpack t
  where
    decAt :: String -> Text -> Either String (Int, Text)
    decAt label x = case TR.signed TR.decimal x of
      Right r -> Right r
      Left e  -> Left $ label ++ ": " ++ e

parseGeometry :: Text -> Either String IcebergType
parseGeometry t =
  let inner = T.dropEnd 1 (T.drop (T.length "geometry(") t)
  in Right (TGeometry inner)

parseGeography :: Text -> Either String IcebergType
parseGeography t =
  let inner = T.dropEnd 1 (T.drop (T.length "geography(") t)
      parts = map T.strip (T.splitOn "," inner)
  in case parts of
       [crs, algo] -> Right (TGeography crs algo)
       [crs]       -> Right (TGeography crs "spherical")
       _           -> Left $ "invalid geography type: " ++ T.unpack t

-- ============================================================
-- Snapshot
-- ============================================================

snapshotToJSON :: Snapshot -> Aeson.Value
snapshotToJSON s = Aeson.Object $ KM.fromList $
  [ ("snapshot-id",      Aeson.Number (fromIntegral (snapId s)))
  , ("sequence-number",  Aeson.Number (fromIntegral (snapSequenceNumber s)))
  , ("timestamp-ms",     Aeson.Number (fromIntegral (snapTimestampMs s)))
  , ("manifest-list",    Aeson.String (snapManifestList s))
  , ("summary",          mapToJSON (snapSummary s))
  ]
  ++ maybe [] (\pid -> [("parent-snapshot-id", Aeson.Number (fromIntegral pid))]) (snapParentId s)
  ++ maybe [] (\sid -> [("schema-id", Aeson.Number (fromIntegral sid))]) (snapSchemaId s)
  ++ maybe [] (\rid -> [("first-row-id", Aeson.Number (fromIntegral rid))]) (snapFirstRowId s)
  ++ maybe [] (\k   -> [("key-id", Aeson.String k)]) (snapKeyId s)

snapshotFromJSON :: Aeson.Value -> Either String Snapshot
snapshotFromJSON (Aeson.Object obj) = do
  sid    <- reqInt64 "snapshot-id" obj
  seqn   <- optInt64 "sequence-number" obj
  ts     <- reqInt64 "timestamp-ms" obj
  ml     <- reqStr "manifest-list" obj
  summ   <- mapFromJSON "summary" obj
  pid    <- optInt64 "parent-snapshot-id" obj
  schId  <- optInt "schema-id" obj
  firstR <- optInt64 "first-row-id" obj
  let kid = case KM.lookup "key-id" obj of
              Just (Aeson.String k) -> Just k
              _                     -> Nothing
  Right Snapshot
    { snapId = sid
    , snapParentId = pid
    , snapSequenceNumber = maybe 0 id seqn
    , snapTimestampMs = ts
    , snapManifestList = ml
    , snapSummary = summ
    , snapSchemaId = schId
    , snapFirstRowId = firstR
    , snapKeyId = kid
    }
snapshotFromJSON _ = Left "snapshot must be a JSON object"

-- ============================================================
-- PartitionSpec
-- ============================================================

partitionSpecToJSON :: PartitionSpec -> Aeson.Value
partitionSpecToJSON ps = Aeson.Object $ KM.fromList
  [ ("spec-id", Aeson.Number (fromIntegral (psSpecId ps)))
  , ("fields",  Aeson.Array (V.map partitionFieldToJSON (psFields ps)))
  ]

partitionSpecFromJSON :: Aeson.Value -> Either String PartitionSpec
partitionSpecFromJSON (Aeson.Object obj) = do
  sid <- reqInt "spec-id" obj
  fs  <- reqArray "fields" obj >>= V.mapM partitionFieldFromJSON
  Right PartitionSpec { psSpecId = sid, psFields = fs }
partitionSpecFromJSON _ = Left "partition spec must be a JSON object"

partitionFieldToJSON :: PartitionField -> Aeson.Value
partitionFieldToJSON pf = Aeson.Object $ KM.fromList $
  -- Spec compatibility: emit @source-id@ for the V1/V2 single-source
  -- case (the universal case before V3) and @source-ids@ when the
  -- transform is a V3 multi-source bucket/truncate. Java / Python /
  -- Rust readers older than V3 only accept @source-id@.
  case V.toList (pfSourceIds pf) of
    [s] -> [ ("source-id",  Aeson.Number (fromIntegral s)) ]
    ss  -> [ ("source-ids",
              Aeson.Array
                (V.fromList (map (Aeson.Number . fromIntegral) ss))) ]
  ++
  [ ("field-id",   Aeson.Number (fromIntegral (pfFieldId pf)))
  , ("name",       Aeson.String (pfName pf))
  , ("transform",  transformToJSON (pfTransform pf))
  ]

partitionFieldFromJSON :: Aeson.Value -> Either String PartitionField
partitionFieldFromJSON (Aeson.Object obj) = do
  sids <- case KM.lookup "source-ids" obj of
            Just (Aeson.Array xs) ->
              V.mapM (\x -> case x of
                              Aeson.Number n -> Right (truncate (toRational n))
                              _ -> Left "partition field 'source-ids' entry not a number") xs
            Just _  -> Left "partition field 'source-ids' must be an array"
            Nothing -> do
              sid <- reqInt "source-id" obj
              Right (V.singleton sid)
  fid  <- reqInt "field-id" obj
  name <- reqStr "name" obj
  tr   <- case KM.lookup "transform" obj of
            Just v  -> transformFromJSON v
            Nothing -> Left "partition field missing 'transform'"
  Right PartitionField { pfSourceIds = sids, pfFieldId = fid, pfName = name, pfTransform = tr }
partitionFieldFromJSON _ = Left "partition field must be a JSON object"

-- ============================================================
-- Transform
-- ============================================================

transformToJSON :: Transform -> Aeson.Value
transformToJSON Identity              = Aeson.String "identity"
transformToJSON (Bucket n)            = Aeson.String (T.pack ("bucket[" ++ show n ++ "]"))
transformToJSON (Truncate n)          = Aeson.String (T.pack ("truncate[" ++ show n ++ "]"))
transformToJSON Year                  = Aeson.String "year"
transformToJSON Month                 = Aeson.String "month"
transformToJSON Day                   = Aeson.String "day"
transformToJSON Hour                  = Aeson.String "hour"
transformToJSON Void                  = Aeson.String "void"
transformToJSON (UnknownTransform t)  = Aeson.String t

transformFromJSON :: Aeson.Value -> Either String Transform
transformFromJSON (Aeson.String s) = case s of
  "identity" -> Right Identity
  "year"     -> Right Year
  "month"    -> Right Month
  "day"      -> Right Day
  "hour"     -> Right Hour
  "void"     -> Right Void
  other
    | "bucket[" `T.isPrefixOf` other ->
        decodeParam "bucket" 7 other Bucket
    | "truncate[" `T.isPrefixOf` other ->
        decodeParam "truncate" 9 other Truncate
    | otherwise -> Right (UnknownTransform other)
transformFromJSON _ = Left "transform must be a string"

decodeParam :: String -> Int -> Text -> (Int -> Transform) -> Either String Transform
decodeParam label drop' other ctor =
  case TR.decimal (T.dropEnd 1 (T.drop drop' other)) of
    Right (n, rest) | T.null rest -> Right (ctor n)
    _ -> Left $ "invalid " ++ label ++ " transform: " ++ T.unpack other

-- ============================================================
-- SortOrder
-- ============================================================

sortOrderToJSON :: SortOrder -> Aeson.Value
sortOrderToJSON so = Aeson.Object $ KM.fromList
  [ ("order-id", Aeson.Number (fromIntegral (soOrderId so)))
  , ("fields",   Aeson.Array (V.map sortFieldToJSON (soFields so)))
  ]

sortOrderFromJSON :: Aeson.Value -> Either String SortOrder
sortOrderFromJSON (Aeson.Object obj) = do
  oid <- reqInt "order-id" obj
  fs  <- reqArray "fields" obj >>= V.mapM sortFieldFromJSON
  Right SortOrder { soOrderId = oid, soFields = fs }
sortOrderFromJSON _ = Left "sort order must be a JSON object"

sortFieldToJSON :: SortField -> Aeson.Value
sortFieldToJSON sf = Aeson.Object $ KM.fromList
  [ ("source-id",  Aeson.Number (fromIntegral (sortSourceId sf)))
  , ("transform",  transformToJSON (sortTransform sf))
  , ("direction",  directionToJSON (sortDirection sf))
  , ("null-order", nullOrderToJSON (sortNullOrder sf))
  ]

sortFieldFromJSON :: Aeson.Value -> Either String SortField
sortFieldFromJSON (Aeson.Object obj) = do
  sid <- reqInt "source-id" obj
  tr  <- case KM.lookup "transform" obj of
           Just v  -> transformFromJSON v
           Nothing -> Left "sort field missing 'transform'"
  dir <- case KM.lookup "direction" obj of
           Just v  -> directionFromJSON v
           Nothing -> Left "sort field missing 'direction'"
  no  <- case KM.lookup "null-order" obj of
           Just v  -> nullOrderFromJSON v
           Nothing -> Left "sort field missing 'null-order'"
  Right SortField { sortSourceId = sid, sortTransform = tr, sortDirection = dir, sortNullOrder = no }
sortFieldFromJSON _ = Left "sort field must be a JSON object"

directionToJSON :: SortDirection -> Aeson.Value
directionToJSON Asc  = Aeson.String "asc"
directionToJSON Desc = Aeson.String "desc"

directionFromJSON :: Aeson.Value -> Either String SortDirection
directionFromJSON (Aeson.String "asc")  = Right Asc
directionFromJSON (Aeson.String "desc") = Right Desc
directionFromJSON _ = Left "direction must be 'asc' or 'desc'"

nullOrderToJSON :: NullOrder -> Aeson.Value
nullOrderToJSON NullsFirst = Aeson.String "nulls-first"
nullOrderToJSON NullsLast  = Aeson.String "nulls-last"

nullOrderFromJSON :: Aeson.Value -> Either String NullOrder
nullOrderFromJSON (Aeson.String "nulls-first") = Right NullsFirst
nullOrderFromJSON (Aeson.String "nulls-last")  = Right NullsLast
nullOrderFromJSON _ = Left "null-order must be 'nulls-first' or 'nulls-last'"

-- ============================================================
-- SnapshotLogEntry / MetadataLogEntry
-- ============================================================

snapshotLogEntryToJSON :: SnapshotLogEntry -> Aeson.Value
snapshotLogEntryToJSON sle = Aeson.Object $ KM.fromList
  [ ("timestamp-ms", Aeson.Number (fromIntegral (sleTimestampMs sle)))
  , ("snapshot-id",  Aeson.Number (fromIntegral (sleSnapshotId sle)))
  ]

snapshotLogEntryFromJSON :: Aeson.Value -> Either String SnapshotLogEntry
snapshotLogEntryFromJSON (Aeson.Object obj) = do
  ts  <- reqInt64 "timestamp-ms" obj
  sid <- reqInt64 "snapshot-id" obj
  Right SnapshotLogEntry { sleTimestampMs = ts, sleSnapshotId = sid }
snapshotLogEntryFromJSON _ = Left "snapshot log entry must be a JSON object"

metadataLogEntryToJSON :: MetadataLogEntry -> Aeson.Value
metadataLogEntryToJSON e = Aeson.Object $ KM.fromList
  [ ("timestamp-ms",  Aeson.Number (fromIntegral (mleTimestampMs e)))
  , ("metadata-file", Aeson.String (mleMetadataFile e))
  ]

metadataLogEntryFromJSON :: Aeson.Value -> Either String MetadataLogEntry
metadataLogEntryFromJSON (Aeson.Object obj) = do
  ts <- reqInt64 "timestamp-ms" obj
  fp <- reqStr "metadata-file" obj
  Right MetadataLogEntry { mleTimestampMs = ts, mleMetadataFile = fp }
metadataLogEntryFromJSON _ = Left "metadata log entry must be a JSON object"

-- ============================================================
-- Statistics files
-- ============================================================

statisticsFileToJSON :: StatisticsFile -> Aeson.Value
statisticsFileToJSON sf = Aeson.Object $ KM.fromList $
  [ ("snapshot-id",   Aeson.Number (fromIntegral (sfsSnapshotId sf)))
  , ("statistics-path", Aeson.String (sfsStatPath sf))
  , ("file-size-in-bytes", Aeson.Number (fromIntegral (sfsFileSize sf)))
  , ("file-footer-size-in-bytes", Aeson.Number (fromIntegral (sfsFooterSize sf)))
  , ("blob-metadata", Aeson.Array (V.map blobMetadataToJSON (sfsBlobMetadata sf)))
  ]
  ++ maybe [] (\k -> [("key-metadata", Aeson.String k)]) (sfsKeyMetadata sf)

statisticsFileFromJSON :: Aeson.Value -> Either String StatisticsFile
statisticsFileFromJSON (Aeson.Object obj) = do
  sid    <- reqInt64 "snapshot-id" obj
  path   <- reqStr "statistics-path" obj
  sz     <- reqInt64 "file-size-in-bytes" obj
  ftrSz  <- reqInt64 "file-footer-size-in-bytes" obj
  blobs  <- reqArray "blob-metadata" obj >>= V.mapM blobMetadataFromJSON
  let km = case KM.lookup "key-metadata" obj of
             Just (Aeson.String k) -> Just k
             _                     -> Nothing
  Right StatisticsFile
    { sfsSnapshotId = sid
    , sfsStatPath = path
    , sfsFileSize = sz
    , sfsFooterSize = ftrSz
    , sfsBlobMetadata = blobs
    , sfsKeyMetadata = km
    }
statisticsFileFromJSON _ = Left "statistics file must be a JSON object"

blobMetadataToJSON :: BlobMetadata -> Aeson.Value
blobMetadataToJSON b = Aeson.Object $ KM.fromList $
  [ ("type",            Aeson.String (bmType b))
  , ("snapshot-id",     Aeson.Number (fromIntegral (bmSnapshotId b)))
  , ("sequence-number", Aeson.Number (fromIntegral (bmSequenceNumber b)))
  , ("fields",          Aeson.Array (V.map (Aeson.Number . fromIntegral) (bmFields b)))
  ]
  ++ (if Map.null (bmProperties b) then [] else [("properties", mapToJSON (bmProperties b))])

blobMetadataFromJSON :: Aeson.Value -> Either String BlobMetadata
blobMetadataFromJSON (Aeson.Object obj) = do
  ty   <- reqStr "type" obj
  sid  <- reqInt64 "snapshot-id" obj
  seqn <- reqInt64 "sequence-number" obj
  fs   <- reqArray "fields" obj >>= V.mapM (numericInt "fields")
  props <- case KM.lookup "properties" obj of
             Just _  -> mapFromJSON "properties" obj
             Nothing -> Right Map.empty
  Right BlobMetadata
    { bmType = ty
    , bmSnapshotId = sid
    , bmSequenceNumber = seqn
    , bmFields = fs
    , bmProperties = props
    }
blobMetadataFromJSON _ = Left "blob metadata must be a JSON object"

partitionStatisticsFileToJSON :: PartitionStatisticsFile -> Aeson.Value
partitionStatisticsFileToJSON p = Aeson.Object $ KM.fromList
  [ ("snapshot-id",        Aeson.Number (fromIntegral (psfSnapshotId p)))
  , ("statistics-path",    Aeson.String (psfPath p))
  , ("file-size-in-bytes", Aeson.Number (fromIntegral (psfFileSize p)))
  ]

partitionStatisticsFileFromJSON :: Aeson.Value -> Either String PartitionStatisticsFile
partitionStatisticsFileFromJSON (Aeson.Object obj) = do
  sid <- reqInt64 "snapshot-id" obj
  pth <- reqStr "statistics-path" obj
  sz  <- reqInt64 "file-size-in-bytes" obj
  Right PartitionStatisticsFile { psfSnapshotId = sid, psfPath = pth, psfFileSize = sz }
partitionStatisticsFileFromJSON _ = Left "partition statistics file must be a JSON object"

-- ============================================================
-- SnapshotRef
-- ============================================================

snapshotRefToJSON :: SnapshotRef -> Aeson.Value
snapshotRefToJSON sr = Aeson.Object $ KM.fromList $
  [ ("snapshot-id", Aeson.Number (fromIntegral (srSnapshotId sr)))
  , ("type",        Aeson.String (srType sr))
  ] ++ maybe [] (\v -> [("max-ref-age-ms", Aeson.Number (fromIntegral v))]) (srMaxRefAgeMs sr)
    ++ maybe [] (\v -> [("max-snapshot-age-ms", Aeson.Number (fromIntegral v))]) (srMaxSnapshotAgeMs sr)
    ++ maybe [] (\v -> [("min-snapshots-to-keep", Aeson.Number (fromIntegral v))]) (srMinSnapshotsToKeep sr)

snapshotRefFromJSON :: Aeson.Value -> Either String SnapshotRef
snapshotRefFromJSON (Aeson.Object obj) = do
  sid     <- reqInt64 "snapshot-id" obj
  typ     <- reqStr "type" obj
  maxRef  <- optInt64 "max-ref-age-ms" obj
  maxSnap <- optInt64 "max-snapshot-age-ms" obj
  minKeep <- optInt32 "min-snapshots-to-keep" obj
  Right SnapshotRef
    { srSnapshotId = sid
    , srType = typ
    , srMaxRefAgeMs = maxRef
    , srMaxSnapshotAgeMs = maxSnap
    , srMinSnapshotsToKeep = minKeep
    }
snapshotRefFromJSON _ = Left "snapshot ref must be a JSON object"

snapshotRefsToJSON :: Map.Map Text SnapshotRef -> Aeson.Value
snapshotRefsToJSON m = Aeson.Object $ KM.fromList $
  map (\(k, v) -> (Key.fromText k, snapshotRefToJSON v)) (Map.toList m)

snapshotRefsFromJSON :: KM.KeyMap Aeson.Value -> Either String (Map.Map Text SnapshotRef)
snapshotRefsFromJSON obj = case KM.lookup "snapshot-refs" obj of
  Just (Aeson.Object m) ->
    Map.fromList <$> mapM (\(k, v) -> do
      ref <- snapshotRefFromJSON v
      Right (Key.toText k, ref)
    ) (KM.toList m)
  Just Aeson.Null -> Right Map.empty
  Nothing         -> Right Map.empty
  _               -> Left "snapshot-refs must be an object"

-- ============================================================
-- Name mapping
-- ============================================================

-- | Iceberg name mappings are a JSON array of mapping objects.
nameMappingToJSON :: NameMapping -> Aeson.Value
nameMappingToJSON (NameMapping fs) = Aeson.Array (V.map mappedFieldToJSON fs)

nameMappingFromJSON :: Aeson.Value -> Either String NameMapping
nameMappingFromJSON (Aeson.Array arr) =
  NameMapping <$> V.mapM mappedFieldFromJSON arr
nameMappingFromJSON _ = Left "name mapping must be a JSON array"

mappedFieldToJSON :: MappedField -> Aeson.Value
mappedFieldToJSON mf = Aeson.Object $ KM.fromList $
  [ ("names", Aeson.Array (V.map Aeson.String (mfName mf)))
  ]
  ++ maybe [] (\fid -> [("field-id", Aeson.Number (fromIntegral fid))]) (mfFieldId mf)
  ++ (if V.null (unNameMapping (mfFields mf))
        then []
        else [("fields", nameMappingToJSON (mfFields mf))])

mappedFieldFromJSON :: Aeson.Value -> Either String MappedField
mappedFieldFromJSON (Aeson.Object obj) = do
  ns <- case KM.lookup "names" obj of
          Just (Aeson.Array arr) -> V.mapM textOnly arr
          Just Aeson.Null        -> Right V.empty
          Nothing                -> Right V.empty
          _                      -> Left "names must be an array of strings"
  fid <- optInt "field-id" obj
  inner <- case KM.lookup "fields" obj of
             Just v  -> nameMappingFromJSON v
             Nothing -> Right (NameMapping V.empty)
  Right MappedField { mfName = ns, mfFieldId = fid, mfFields = inner }
mappedFieldFromJSON _ = Left "name mapping field must be a JSON object"
  where
textOnly :: Aeson.Value -> Either String Text
textOnly (Aeson.String s) = Right s
textOnly _ = Left "names must be strings"

-- ============================================================
-- View metadata
-- ============================================================

viewMetadataToJSON :: ViewMetadata -> Aeson.Value
viewMetadataToJSON vm = Aeson.Object $ KM.fromList
  [ ("view-uuid",          Aeson.String (vmViewUuid vm))
  , ("format-version",     Aeson.Number (fromIntegral (vmFormatVersion vm)))
  , ("location",           Aeson.String (vmLocation vm))
  , ("schemas",            Aeson.Array (V.map schemaToJSON (vmSchemas vm)))
  , ("current-version-id", Aeson.Number (fromIntegral (vmCurrentVersionId vm)))
  , ("versions",           Aeson.Array (V.map viewVersionToJSON (vmVersions vm)))
  , ("version-log",        Aeson.Array (V.map viewHistoryToJSON (vmVersionLog vm)))
  , ("properties",         mapToJSON (vmProperties vm))
  ]

viewMetadataFromJSON :: Aeson.Value -> Either String ViewMetadata
viewMetadataFromJSON (Aeson.Object obj) = do
  uuid <- reqStr "view-uuid" obj
  fmt  <- reqInt "format-version" obj
  loc  <- reqStr "location" obj
  ss   <- reqArray "schemas" obj >>= V.mapM schemaFromJSON
  cur  <- reqInt "current-version-id" obj
  vs   <- reqArray "versions" obj >>= V.mapM viewVersionFromJSON
  vlog <- reqArray "version-log" obj >>= V.mapM viewHistoryFromJSON
  ps   <- mapFromJSON "properties" obj
  Right ViewMetadata
    { vmViewUuid = uuid
    , vmFormatVersion = fmt
    , vmLocation = loc
    , vmSchemas = ss
    , vmCurrentVersionId = cur
    , vmVersions = vs
    , vmVersionLog = vlog
    , vmProperties = ps
    }
viewMetadataFromJSON _ = Left "view metadata must be a JSON object"

viewVersionToJSON :: ViewVersion -> Aeson.Value
viewVersionToJSON vv = Aeson.Object $ KM.fromList $
  [ ("version-id",      Aeson.Number (fromIntegral (vvVersionId vv)))
  , ("timestamp-ms",    Aeson.Number (fromIntegral (vvTimestampMs vv)))
  , ("schema-id",       Aeson.Number (fromIntegral (vvSchemaId vv)))
  , ("summary",         mapToJSON (vvSummary vv))
  , ("representations", Aeson.Array (V.map viewRepresentationToJSON (vvRepresentations vv)))
  ]
  ++ maybe [] (\c -> [("default-catalog", Aeson.String c)]) (vvDefaultCatalog vv)
  ++ (if V.null (vvDefaultNamespace vv) then [] else
        [("default-namespace", Aeson.Array (V.map Aeson.String (vvDefaultNamespace vv)))])

viewVersionFromJSON :: Aeson.Value -> Either String ViewVersion
viewVersionFromJSON (Aeson.Object obj) = do
  vid <- reqInt "version-id" obj
  ts  <- reqInt64 "timestamp-ms" obj
  sid <- reqInt "schema-id" obj
  sm  <- mapFromJSON "summary" obj
  rs  <- reqArray "representations" obj >>= V.mapM viewRepresentationFromJSON
  let defCat = case KM.lookup "default-catalog" obj of
                 Just (Aeson.String c) -> Just c
                 _                     -> Nothing
  defNs <- case KM.lookup "default-namespace" obj of
             Just (Aeson.Array arr) -> V.mapM textOnly2 arr
             Just Aeson.Null        -> Right V.empty
             Nothing                -> Right V.empty
             _                      -> Left "default-namespace must be a string array"
  Right ViewVersion
    { vvVersionId = vid
    , vvTimestampMs = ts
    , vvSchemaId = sid
    , vvSummary = sm
    , vvRepresentations = rs
    , vvDefaultCatalog = defCat
    , vvDefaultNamespace = defNs
    }
viewVersionFromJSON _ = Left "view version must be a JSON object"

textOnly2 :: Aeson.Value -> Either String Text
textOnly2 (Aeson.String s) = Right s
textOnly2 _ = Left "expected string"

viewRepresentationToJSON :: ViewRepresentation -> Aeson.Value
viewRepresentationToJSON (SqlViewRepresentation sql dialect) = Aeson.Object $ KM.fromList
  [ ("type",    Aeson.String "sql")
  , ("sql",     Aeson.String sql)
  , ("dialect", Aeson.String dialect)
  ]
viewRepresentationToJSON (UnknownViewRepresentation t fields) = Aeson.Object $ KM.fromList $
  ("type", Aeson.String t)
  : map (\(k, v) -> (Key.fromText k, Aeson.String v)) (Map.toList fields)

viewRepresentationFromJSON :: Aeson.Value -> Either String ViewRepresentation
viewRepresentationFromJSON (Aeson.Object obj) = do
  ty <- reqStr "type" obj
  case ty of
    "sql" -> do
      sql <- reqStr "sql" obj
      dl  <- reqStr "dialect" obj
      Right (SqlViewRepresentation sql dl)
    other -> do
      let pairs = [(Key.toText k, t) | (k, Aeson.String t) <- KM.toList obj, Key.toText k /= "type"]
      Right (UnknownViewRepresentation other (Map.fromList pairs))
viewRepresentationFromJSON _ = Left "view representation must be a JSON object"

viewHistoryToJSON :: ViewHistoryEntry -> Aeson.Value
viewHistoryToJSON e = Aeson.Object $ KM.fromList
  [ ("timestamp-ms", Aeson.Number (fromIntegral (vheTimestampMs e)))
  , ("version-id",   Aeson.Number (fromIntegral (vheVersionId e)))
  ]

viewHistoryFromJSON :: Aeson.Value -> Either String ViewHistoryEntry
viewHistoryFromJSON (Aeson.Object obj) = do
  ts <- reqInt64 "timestamp-ms" obj
  vi <- reqInt "version-id" obj
  Right ViewHistoryEntry { vheTimestampMs = ts, vheVersionId = vi }
viewHistoryFromJSON _ = Left "view history entry must be a JSON object"

-- ============================================================
-- Helpers
-- ============================================================

mapToJSON :: Map.Map Text Text -> Aeson.Value
mapToJSON m = Aeson.Object $ KM.fromList
  [(Key.fromText k, Aeson.String v) | (k, v) <- Map.toList m]

mapFromJSON :: Text -> KM.KeyMap Aeson.Value -> Either String (Map.Map Text Text)
mapFromJSON key obj = case KM.lookup (Key.fromText key) obj of
  Just (Aeson.Object m) ->
    Map.fromList <$> mapM (\(k, v) -> case v of
      Aeson.String s -> Right (Key.toText k, s)
      _ -> Left $ "map values must be strings in " ++ T.unpack key
      ) (KM.toList m)
  Just Aeson.Null -> Right Map.empty
  Nothing -> Right Map.empty
  _ -> Left $ T.unpack key ++ " must be an object"

reqStr :: Text -> KM.KeyMap Aeson.Value -> Either String Text
reqStr k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _ -> Left $ "missing or non-string field: " ++ T.unpack k

reqBool :: Text -> KM.KeyMap Aeson.Value -> Either String Bool
reqBool k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Bool b) -> Right b
  _ -> Left $ "missing or non-boolean field: " ++ T.unpack k

reqInt :: Text -> KM.KeyMap Aeson.Value -> Either String Int
reqInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right i
    Nothing -> Left $ "field out of Int range: " ++ T.unpack k
  _ -> Left $ "missing or non-numeric field: " ++ T.unpack k

reqInt64 :: Text -> KM.KeyMap Aeson.Value -> Either String Int64
reqInt64 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right i
    Nothing -> Left $ "field out of Int64 range: " ++ T.unpack k
  _ -> Left $ "missing or non-numeric field: " ++ T.unpack k

optInt :: Text -> KM.KeyMap Aeson.Value -> Either String (Maybe Int)
optInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left $ "field out of Int range: " ++ T.unpack k
  Just Aeson.Null -> Right Nothing
  Nothing         -> Right Nothing
  _ -> Left $ "non-numeric field: " ++ T.unpack k

optIntDef :: Text -> KM.KeyMap Aeson.Value -> Int -> Either String Int
optIntDef k obj def = do
  m <- optInt k obj
  Right (maybe def id m)

optInt64 :: Text -> KM.KeyMap Aeson.Value -> Either String (Maybe Int64)
optInt64 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left $ "field out of Int64 range: " ++ T.unpack k
  Just Aeson.Null -> Right Nothing
  Nothing         -> Right Nothing
  _ -> Left $ "non-numeric field: " ++ T.unpack k

optInt32 :: Text -> KM.KeyMap Aeson.Value -> Either String (Maybe Int32)
optInt32 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left $ "field out of Int32 range: " ++ T.unpack k
  Just Aeson.Null -> Right Nothing
  Nothing         -> Right Nothing
  _ -> Left $ "non-numeric field: " ++ T.unpack k

reqArray :: Text -> KM.KeyMap Aeson.Value -> Either String (V.Vector Aeson.Value)
reqArray k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Array arr) -> Right arr
  _ -> Left $ "missing or non-array field: " ++ T.unpack k

optArrayField :: Text -> (a -> Aeson.Value) -> V.Vector a -> [(Key.Key, Aeson.Value)]
optArrayField k toJ xs
  | V.null xs = []
  | otherwise = [(Key.fromText k, Aeson.Array (V.map toJ xs))]

optArrayField' :: Text -> KM.KeyMap Aeson.Value -> (Aeson.Value -> Either String a) -> Either String (V.Vector a)
optArrayField' k obj parse = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Array arr) -> V.mapM parse arr
  Just Aeson.Null        -> Right V.empty
  Nothing                -> Right V.empty
  _                      -> Left $ T.unpack k ++ " must be an array"

numericInt :: Text -> Aeson.Value -> Either String Int
numericInt label (Aeson.Number n) = case toBoundedInteger n :: Maybe Int of
  Just i  -> Right i
  Nothing -> Left $ T.unpack label ++ ": value out of Int range"
numericInt label _ = Left $ T.unpack label ++ ": expected number"

-- We don't reference Scientific itself other than via toBoundedInteger but
-- keep an explicit type ascription here so that 'Aeson.Number' behaviour does
-- not silently drift if 'Scientific' is dropped from aeson in future.
_unusedScientific :: Scientific -> Scientific
_unusedScientific = id
