{-# LANGUAGE BangPatterns #-}
-- | Read Iceberg manifest files and manifest lists from Avro object containers.
--
-- Manifests and manifest lists are standard Avro OCF files whose writer schema
-- matches the Iceberg spec. Use 'readManifestEntries' / 'readManifestList'
-- to obtain typed 'ManifestEntry' and 'ManifestFile' values.
--
-- 'planScan' coordinates snapshot lookup, manifest reading, and data file
-- collection into a single 'ScanPlan'.
module Iceberg.Read
  ( readManifestEntries
  , readDeleteManifestEntries
  , readManifestList
  , manifestFilePaths
  , manifestEntryPaths
  , manifestEntryParquetPaths
  , deleteManifestPaths
  , dataManifestPaths
  , positionDeletesFromColumns
  , applyPositionDeletes
  , ScanPlan(..)
  , FileScanTask(..)
  , planScan
  , planScanWithDeletes
  , planScanWithFilter
  , planScanAtSnapshot
  , planScanAsOfTime
  , fileMetricsFromEntry
    -- * Sequence-number inheritance
  , inheritSequenceNumbers
  , readManifestEntriesWithInheritance
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Avro.Container (readContainer)
import Avro.Schema (AvroField (..), AvroSchema (..), AvroType (..))
import qualified Avro.Value as AV

import qualified Iceberg.Expression as Expr
import qualified Iceberg.SchemaEvolution
import Iceberg.SchemaEvolution (currentSchema)
import qualified Iceberg.Snapshot
import Iceberg.Snapshot (applicableDeletes, currentSnapshot)
import Iceberg.Types
  ( DataFile (..)
  , FieldSummary (..)
  , FileFormat (..)
  , ManifestContent (..)
  , ManifestEntry (..)
  , ManifestFile (..)
  , ManifestStatus (..)
  , PositionDelete (..)
  , Schema
  , Snapshot (..)
  , TableMetadata
  )

-- | Paths from a manifest list Avro file (each @manifest_file.manifest_path@).
manifestFilePaths :: V.Vector ManifestFile -> V.Vector Text
manifestFilePaths = V.map mfPath

-- | Data file paths from a manifest Avro file (one per @data_file@).
manifestEntryPaths :: V.Vector ManifestEntry -> V.Vector Text
manifestEntryPaths = V.map meFilePath

-- | Paths of manifest entries that reference Parquet data files.
manifestEntryParquetPaths :: V.Vector ManifestEntry -> V.Vector Text
manifestEntryParquetPaths =
  V.map meFilePath . V.filter (\e -> meFileFormat e == ParquetFormat)

-- | Paths of manifests whose content is 'DeletesContent'.
deleteManifestPaths :: V.Vector ManifestFile -> V.Vector Text
deleteManifestPaths = V.map mfPath . V.filter (\mf -> mfContent mf == DeletesContent)

-- | Paths of manifests whose content is 'DataContent'.
dataManifestPaths :: V.Vector ManifestFile -> V.Vector Text
dataManifestPaths = V.map mfPath . V.filter (\mf -> mfContent mf == DataContent)

-- | Read an Iceberg manifest Avro file (sequence of @manifest_entry@ records).
readManifestEntries :: ByteString -> Either String (AvroType, V.Vector ManifestEntry)
readManifestEntries bs = do
  (writerTy, vals) <- readContainer bs
  entries <- V.mapM (manifestEntryFromAvro writerTy) vals
  Right (writerTy, entries)

-- | Read an Iceberg delete manifest Avro file. Same container format as data
-- manifests; entries reference delete files rather than data files.
readDeleteManifestEntries :: ByteString -> Either String (AvroType, V.Vector ManifestEntry)
readDeleteManifestEntries = readManifestEntries

-- | Read a manifest file and inherit sequence numbers from the parent
-- manifest list entry (per spec: \"new data and metadata file entries are
-- written with @null@ in place of a sequence number, which is replaced
-- with the manifest's sequence number at read time\"). The @file_sequence_number@
-- is inherited unconditionally for entries with status @ADDED@ that do not
-- carry one.
readManifestEntriesWithInheritance
  :: ManifestFile  -- ^ Owning manifest list entry, providing the inherited seq.
  -> ByteString
  -> Either String (AvroType, V.Vector ManifestEntry)
readManifestEntriesWithInheritance owner bs = do
  (writerTy, entries) <- readManifestEntries bs
  Right (writerTy, V.map (inheritSequenceNumbers owner) entries)

-- | Apply the sequence-number inheritance rule to a single manifest entry.
inheritSequenceNumbers :: ManifestFile -> ManifestEntry -> ManifestEntry
inheritSequenceNumbers owner me =
  let inheritedSeq = mfSequenceNumber owner
      seqNo' = case meSequenceNumber me of
        Nothing | meStatus me == Added -> Just inheritedSeq
        Just _  -> meSequenceNumber me
        Nothing -> meSequenceNumber me
      fileSeqNo' = case meFileSequenceNumber me of
        Nothing | meStatus me == Added -> Just inheritedSeq
        other   -> other
   in me { meSequenceNumber = seqNo', meFileSequenceNumber = fileSeqNo' }

-- | Read an Iceberg manifest-list Avro file (sequence of @manifest_file@ records).
readManifestList :: ByteString -> Either String (AvroType, V.Vector ManifestFile)
readManifestList bs = do
  (writerTy, vals) <- readContainer bs
  files <- V.mapM (manifestFileFromAvro writerTy) vals
  Right (writerTy, files)

-- | Decode one manifest entry using the container writer schema.
manifestEntryFromAvro :: AvroType -> AV.Value -> Either String ManifestEntry
manifestEntryFromAvro ty val = do
  fields <- recordFields ty
  vals <- asRecord val
  whenMismatch (V.length fields) (V.length vals)
  status <- lookupField fields vals "status" >>= asInt32 >>= manifestStatusFromInt
  snapId <- lookupField fields vals "snapshot_id" >>= optionalLong
  seqNo <- lookupField fields vals "sequence_number" >>= optionalLong
  fileSeqNo <- lookupFieldOptional fields vals "file_sequence_number" optionalLong
  dfVal <- lookupField fields vals "data_file"
  dfTy <- fieldTypeByName fields "data_file"
  df <- dataFileFromAvro dfTy dfVal
  Right
    ManifestEntry
      { meStatus = status
      , meSnapshotId = snapId
      , meSequenceNumber = seqNo
      , meFileSequenceNumber = fileSeqNo
      , meFilePath = dataFileFilePath df
      , meFileFormat = dataFileFileFormat df
      , mePartition = dataFilePartition df
      , meRecordCount = dataFileRecordCount df
      , meFileSizeBytes = dataFileFileSize df
      , meDataFile = Just df
      }

manifestFileFromAvro :: AvroType -> AV.Value -> Either String ManifestFile
manifestFileFromAvro ty val = do
  fields <- recordFields ty
  vals <- asRecord val
  whenMismatch (V.length fields) (V.length vals)
  path <- lookupField fields vals "manifest_path" >>= asText
  len <- lookupField fields vals "manifest_length" >>= asInt64
  specId <- lookupField fields vals "partition_spec_id" >>= asInt32
  -- @content@, @sequence_number@, and @min_sequence_number@ are required in
  -- v2 but the writer schema treats them as optional (with default null) so
  -- that v1 readers can ignore them. Decode either form.
  contentRaw <- lookupField fields vals "content"
  content <- case contentRaw of
    AV.Int n             -> manifestContentFromInt n
    AV.Union _ (AV.Int n) -> manifestContentFromInt n
    AV.Union _ AV.Null   -> Right DataContent
    _                    -> Left "Iceberg.Read: expected int for manifest content"
  seqRaw <- lookupField fields vals "sequence_number"
  seqN <- case seqRaw of
    AV.Long n             -> Right n
    AV.Union _ (AV.Long n) -> Right n
    AV.Union _ AV.Null    -> Right 0
    _                     -> Left "Iceberg.Read: expected long for sequence_number"
  minSeqRaw <- lookupField fields vals "min_sequence_number"
  minSeq <- case minSeqRaw of
    AV.Long n             -> Right n
    AV.Union _ (AV.Long n) -> Right n
    AV.Union _ AV.Null    -> Right 0
    _                     -> Left "Iceberg.Read: expected long for min_sequence_number"
  addSnap <- lookupField fields vals "added_snapshot_id" >>= asInt64
  addFiles <- lookupField fields vals "added_data_files_count" >>= optionalInt
  exFiles <- lookupField fields vals "existing_data_files_count" >>= optionalInt
  delFiles <- lookupField fields vals "deleted_data_files_count" >>= optionalInt
  addRows <- lookupField fields vals "added_rows_count" >>= optionalInt64
  exRows <- lookupField fields vals "existing_rows_count" >>= optionalInt64
  delRows <- lookupField fields vals "deleted_rows_count" >>= optionalInt64
  parts <- lookupFieldOptional fields vals "partitions" optionalFieldSummaryArray
  keyMd <- lookupFieldOptional fields vals "key_metadata" optionalBytes
  firstRow <- lookupFieldOptional fields vals "first_row_id" optionalLong
  Right
    ManifestFile
      { mfPath = path
      , mfLength = len
      , mfPartitionSpecId = fromIntegral specId
      , mfContent = content
      , mfSequenceNumber = seqN
      , mfMinSequenceNumber = minSeq
      , mfAddedSnapshotId = addSnap
      , mfAddedDataFilesCount = addFiles
      , mfExistingDataFilesCount = exFiles
      , mfDeletedDataFilesCount = delFiles
      , mfAddedRowsCount = addRows
      , mfExistingRowsCount = exRows
      , mfDeletedRowsCount = delRows
      , mfPartitions = maybe V.empty id parts
      , mfKeyMetadata = keyMd
      , mfFirstRowId = firstRow
      }

-- * data_file

dataFileFromAvro :: AvroType -> AV.Value -> Either String DataFile
dataFileFromAvro ty val = do
  fields <- recordFields ty
  vals <- asRecord val
  whenMismatch (V.length fields) (V.length vals)
  contentInt <- lookupFieldOptional fields vals "content" optionalInt
  let content = case contentInt of
        Just 0 -> DataContent
        Just _ -> DeletesContent
        Nothing -> DataContent
  path <- lookupField fields vals "file_path" >>= asText
  fmtStr <- lookupField fields vals "file_format" >>= asText
  fmt <- parseFileFormat fmtStr
  partTy <- fieldTypeByName fields "partition"
  partVal <- lookupField fields vals "partition"
  part <- partitionVector partTy partVal
  recCount <- lookupField fields vals "record_count" >>= asInt64
  fileSz <- lookupField fields vals "file_size_in_bytes" >>= asInt64
  colSizes  <- lookupFieldOptional fields vals "column_sizes" optionalIntInt64Map
  valCounts <- lookupFieldOptional fields vals "value_counts" optionalIntInt64Map
  nullCounts <- lookupFieldOptional fields vals "null_value_counts" optionalIntInt64Map
  nanCounts  <- lookupFieldOptional fields vals "nan_value_counts" optionalIntInt64Map
  lower <- lookupFieldOptional fields vals "lower_bounds" optionalIntBytesMap
  upper <- lookupFieldOptional fields vals "upper_bounds" optionalIntBytesMap
  keyMd <- lookupFieldOptional fields vals "key_metadata" optionalBytes
  splitOff <- lookupFieldOptional fields vals "split_offsets" optionalLongArray
  eqIds <- lookupFieldOptional fields vals "equality_ids" optionalIntArray
  sortId <- lookupFieldOptional fields vals "sort_order_id" optionalInt
  firstRow <- lookupFieldOptional fields vals "first_row_id" optionalLong
  refData <- lookupFieldOptional fields vals "referenced_data_file" optionalText
  cOff <- lookupFieldOptional fields vals "content_offset" optionalLong
  cSz  <- lookupFieldOptional fields vals "content_size_in_bytes" optionalLong
  Right DataFile
    { dataFileContent = content
    , dataFileFilePath = path
    , dataFileFileFormat = fmt
    , dataFilePartition = part
    , dataFileRecordCount = recCount
    , dataFileFileSize = fileSz
    , dataFileColumnSizes = maybe Map.empty id colSizes
    , dataFileValueCounts = maybe Map.empty id valCounts
    , dataFileNullValueCounts = maybe Map.empty id nullCounts
    , dataFileNanValueCounts = maybe Map.empty id nanCounts
    , dataFileLowerBounds = maybe Map.empty id lower
    , dataFileUpperBounds = maybe Map.empty id upper
    , dataFileKeyMetadata = keyMd
    , dataFileSplitOffsets = maybe V.empty id splitOff
    , dataFileEqualityIds = maybe V.empty id eqIds
    , dataFileSortOrderId = sortId
    , dataFileFirstRowId = firstRow
    , dataFileReferencedDataFile = refData
    , dataFileContentOffset = cOff
    , dataFileContentSize = cSz
    }

partitionVector :: AvroType -> AV.Value -> Either String (V.Vector (Maybe AV.Value))
partitionVector partTy partVal = do
  pFields <- recordFields partTy
  pVals <- asRecord partVal
  whenMismatch (V.length pFields) (V.length pVals)
  V.zipWithM partitionSlot pFields pVals

partitionSlot :: AvroField -> AV.Value -> Either String (Maybe AV.Value)
partitionSlot fld v = case avroFieldType fld of
  AvroUnion {avroUnionBranches = br}
    | V.length br == 2
    , isNullType (V.unsafeIndex br 0) ->
        case v of
          AV.Union 0 AV.Null -> Right Nothing
          AV.Union _ inner -> Right (Just inner)
          _ -> Left "Iceberg.Read: expected union for optional partition field"
  _ -> Right (Just v)

isNullType :: AvroType -> Bool
isNullType (AvroPrimitive AvroNull) = True
isNullType _ = False

-- * Field lookup by name

recordFields :: AvroType -> Either String (V.Vector AvroField)
recordFields (AvroRecord {avroRecordFields = fs}) = Right fs
recordFields (AvroLogical {avroLogicalBase = b}) = recordFields b
recordFields _ = Left "Iceberg.Read: expected Avro record type"

fieldTypeByName :: V.Vector AvroField -> Text -> Either String AvroType
fieldTypeByName fields name =
  case V.find (\f -> avroFieldName f == name) fields of
    Nothing -> Left $ "Iceberg.Read: missing field in schema: " ++ T.unpack name
    Just f -> Right (avroFieldType f)

lookupField :: V.Vector AvroField -> V.Vector AV.Value -> Text -> Either String AV.Value
lookupField fields vals name =
  case V.findIndex (\f -> avroFieldName f == name) fields of
    Nothing -> Left $ "Iceberg.Read: missing field: " ++ T.unpack name
    Just i -> Right (V.unsafeIndex vals i)

-- | Like 'lookupField' but returns 'Nothing' when the field is absent from
-- the writer schema (for backward compatibility with older manifest versions).
lookupFieldOptional
  :: V.Vector AvroField -> V.Vector AV.Value -> Text
  -> (AV.Value -> Either String (Maybe a))
  -> Either String (Maybe a)
lookupFieldOptional fields vals name decode =
  case V.findIndex (\f -> avroFieldName f == name) fields of
    Nothing -> Right Nothing
    Just i  -> decode (V.unsafeIndex vals i)

whenMismatch :: Int -> Int -> Either String ()
whenMismatch n m
  | n == m = Right ()
  | otherwise =
      Left $
        "Iceberg.Read: record field count mismatch (schema "
          ++ show n
          ++ " vs "
          ++ show m
          ++ " values)"

asRecord :: AV.Value -> Either String (V.Vector AV.Value)
asRecord (AV.Record vs) = Right vs
asRecord _ = Left "Iceberg.Read: expected Avro record"

asInt32 :: AV.Value -> Either String Int32
asInt32 (AV.Int n) = Right n
asInt32 _ = Left "Iceberg.Read: expected Avro int"

asInt64 :: AV.Value -> Either String Int64
asInt64 (AV.Long n) = Right n
asInt64 _ = Left "Iceberg.Read: expected Avro long"

asText :: AV.Value -> Either String Text
asText (AV.String t) = Right t
asText _ = Left "Iceberg.Read: expected Avro string"

optionalInt :: AV.Value -> Either String (Maybe Int)
optionalInt v = optionalWith v $ \v' -> case v' of
  AV.Int n -> Right (fromIntegral n)
  _ -> Left "Iceberg.Read: expected int in optional"

optionalInt64 :: AV.Value -> Either String (Maybe Int64)
optionalInt64 v = optionalWith v $ \v' -> case v' of
  AV.Long n -> Right n
  _ -> Left "Iceberg.Read: expected long in optional"

optionalLong :: AV.Value -> Either String (Maybe Int64)
optionalLong = optionalInt64

optionalBytes :: AV.Value -> Either String (Maybe ByteString)
optionalBytes v = optionalWith v $ \v' -> case v' of
  AV.Bytes bs -> Right bs
  AV.Fixed bs -> Right bs
  _           -> Left "Iceberg.Read: expected bytes in optional"

optionalText :: AV.Value -> Either String (Maybe Text)
optionalText v = optionalWith v $ \v' -> case v' of
  AV.String s -> Right s
  _           -> Left "Iceberg.Read: expected string in optional"

-- | Iceberg manifest maps from int -> X are encoded as Avro arrays of
-- @key_value@ records (the standard "logical map" trick).
optionalIntInt64Map :: AV.Value -> Either String (Maybe (Map.Map Int Int64))
optionalIntInt64Map v = optionalWith v $ \v' -> do
  arr <- avroArray v'
  Map.fromList <$> mapM intInt64KV (V.toList arr)
  where
    intInt64KV (AV.Record vs) | V.length vs == 2 = do
      k <- asInt32 (V.unsafeIndex vs 0)
      n <- asInt64 (V.unsafeIndex vs 1)
      Right (fromIntegral k, n)
    intInt64KV _ = Left "Iceberg.Read: expected key_value record"

optionalIntBytesMap :: AV.Value -> Either String (Maybe (Map.Map Int ByteString))
optionalIntBytesMap v = optionalWith v $ \v' -> do
  arr <- avroArray v'
  Map.fromList <$> mapM intBytesKV (V.toList arr)
  where
    intBytesKV (AV.Record vs) | V.length vs == 2 = do
      k <- asInt32 (V.unsafeIndex vs 0)
      bs <- asBytesLike (V.unsafeIndex vs 1)
      Right (fromIntegral k, bs)
    intBytesKV _ = Left "Iceberg.Read: expected key_value record"

optionalLongArray :: AV.Value -> Either String (Maybe (V.Vector Int64))
optionalLongArray v = optionalWith v $ \v' -> do
  arr <- avroArray v'
  V.mapM asInt64 arr

optionalIntArray :: AV.Value -> Either String (Maybe (V.Vector Int))
optionalIntArray v = optionalWith v $ \v' -> do
  arr <- avroArray v'
  V.mapM (\x -> fromIntegral <$> asInt32 x) arr

optionalFieldSummaryArray :: AV.Value -> Either String (Maybe (V.Vector FieldSummary))
optionalFieldSummaryArray v = optionalWith v $ \v' -> do
  arr <- avroArray v'
  V.mapM fieldSummaryFromAvro arr

fieldSummaryFromAvro :: AV.Value -> Either String FieldSummary
fieldSummaryFromAvro (AV.Record vs)
  | V.length vs >= 1 = do
      cn <- asBool (V.unsafeIndex vs 0)
      let nan = case lookupAt vs 1 of
                  Just x  -> case unionInner x of
                    Just (AV.Bool b) -> Just b
                    _                -> Nothing
                  Nothing -> Nothing
          lo  = optionalBytesPure (lookupAt vs 2)
          hi  = optionalBytesPure (lookupAt vs 3)
      Right FieldSummary
        { fsContainsNull = cn
        , fsContainsNan = nan
        , fsLowerBound = lo
        , fsUpperBound = hi
        }
fieldSummaryFromAvro _ = Left "Iceberg.Read: expected field_summary record"

asBool :: AV.Value -> Either String Bool
asBool (AV.Bool b) = Right b
asBool _ = Left "Iceberg.Read: expected Avro bool"

asBytesLike :: AV.Value -> Either String ByteString
asBytesLike (AV.Bytes b) = Right b
asBytesLike (AV.Fixed b) = Right b
asBytesLike _ = Left "Iceberg.Read: expected Avro bytes/fixed"

avroArray :: AV.Value -> Either String (V.Vector AV.Value)
avroArray (AV.Array xs) = Right xs
avroArray _ = Left "Iceberg.Read: expected Avro array"

lookupAt :: V.Vector a -> Int -> Maybe a
lookupAt vs i
  | i < V.length vs = Just (V.unsafeIndex vs i)
  | otherwise       = Nothing

unionInner :: AV.Value -> Maybe AV.Value
unionInner (AV.Union 0 AV.Null) = Nothing
unionInner (AV.Union _ inner)   = Just inner
unionInner v                    = Just v

optionalBytesPure :: Maybe AV.Value -> Maybe ByteString
optionalBytesPure mv = case mv >>= unionInner of
  Just (AV.Bytes b) -> Just b
  Just (AV.Fixed b) -> Just b
  _                 -> Nothing

optionalWith :: AV.Value -> (AV.Value -> Either String a) -> Either String (Maybe a)
optionalWith (AV.Union 0 AV.Null) _ = Right Nothing
optionalWith (AV.Union _ inner) f = Just <$> f inner
optionalWith v _ =
  Left $
    "Iceberg.Read: expected [null, T] union for optional field, got "
      ++ show (constrName v)

constrName :: AV.Value -> String
constrName = \case
  AV.Null {} -> "Null"
  AV.Bool {} -> "Bool"
  AV.Int {} -> "Int"
  AV.Long {} -> "Long"
  AV.Float {} -> "Float"
  AV.Double {} -> "Double"
  AV.Bytes {} -> "Bytes"
  AV.String {} -> "String"
  AV.Record {} -> "Record"
  AV.Enum {} -> "Enum"
  AV.Array {} -> "Array"
  AV.Map {} -> "Map"
  AV.Union {} -> "Union"
  AV.Fixed {} -> "Fixed"

manifestStatusFromInt :: Int32 -> Either String ManifestStatus
manifestStatusFromInt = \case
  0 -> Right Existing
  1 -> Right Added
  2 -> Right Deleted
  n -> Left $ "Iceberg.Read: invalid manifest status: " ++ show n

manifestContentFromInt :: Int32 -> Either String ManifestContent
manifestContentFromInt = \case
  0 -> Right DataContent
  1 -> Right DeletesContent
  n -> Left $ "Iceberg.Read: invalid manifest content: " ++ show n

parseFileFormat :: Text -> Either String FileFormat
parseFileFormat t =
  case T.toLower t of
    "avro" -> Right AvroFormat
    "parquet" -> Right ParquetFormat
    "orc" -> Right OrcFormat
    _ -> Left $ "Iceberg.Read: unknown file_format: " ++ T.unpack t

-- ============================================================
-- Position deletes
-- ============================================================

-- | Construct 'PositionDelete' values from already-decoded Parquet columns.
-- The two vectors must have the same length.
positionDeletesFromColumns :: V.Vector Text -> V.Vector Int64 -> V.Vector PositionDelete
positionDeletesFromColumns paths positions =
  V.zipWith PositionDelete paths positions

-- | Remove rows from @rows@ whose indices appear in position deletes
-- targeting @targetPath@.
applyPositionDeletes :: V.Vector PositionDelete -> Text -> V.Vector a -> V.Vector a
applyPositionDeletes deletes targetPath rows =
  let positions = V.foldl' (\acc pd ->
        if pdFilePath pd == targetPath
          then IntSet.insert (fromIntegral (pdPosition pd)) acc
          else acc) IntSet.empty deletes
  in if IntSet.null positions
     then rows
     else V.ifilter (\i _ -> not (IntSet.member i positions)) rows

-- ============================================================
-- Scan planning
-- ============================================================

-- | Result of 'planScan': the resolved snapshot, schema, and the set
-- of manifest / data file paths that need to be read.
data ScanPlan = ScanPlan
  { spSnapshot        :: Snapshot
  , spSchema          :: Schema
  , spManifestPaths   :: !(V.Vector Text)
  , spDataFilePaths   :: !(V.Vector Text)
  , spDeleteFilePaths :: !(V.Vector Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Build a scan plan: resolve the current snapshot and schema, read the
-- manifest list, read every manifest file, and collect all data file paths.
--
-- The third argument is a callback that fetches manifest file bytes given
-- a manifest path (e.g. from S3, local disk, etc.).
planScan
  :: TableMetadata
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Either String ScanPlan
planScan tm manifestListBytes readManifestByPath = do
  snap   <- maybe (Left "planScan: no current snapshot") Right
              (currentSnapshot tm)
  schema <- maybe (Left "planScan: no current schema") Right
              (currentSchema tm)
  (_, manifestFiles) <- readManifestList manifestListBytes
  let mfPaths = manifestFilePaths manifestFiles
  entryVecs <- V.mapM readAndParse mfPaths
  let allEntries = V.concat (V.toList entryVecs)
  Right ScanPlan
    { spSnapshot        = snap
    , spSchema          = schema
    , spManifestPaths   = mfPaths
    , spDataFilePaths   = V.map meFilePath allEntries
    , spDeleteFilePaths = V.empty
    }
  where
    readAndParse path = do
      bs <- readManifestByPath path
      (_, entries) <- readManifestEntries bs
      Right entries

-- | A single scannable data file together with the delete files that apply
-- to it. Mirrors Java's @FileScanTask@.
data FileScanTask = FileScanTask
  { fstDataFile      :: !ManifestEntry
  , fstDeleteFiles   :: !(V.Vector ManifestEntry)
  , fstSpecId        :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Project a 'ManifestEntry' to the 'Expr.FileMetrics' record used by the
-- pruning evaluators. Statistics that aren't present on the manifest entry
-- are returned as 'Nothing' (i.e. unknown), which 'Expression' treats
-- conservatively.
fileMetricsFromEntry :: ManifestEntry -> Expr.FileMetrics
fileMetricsFromEntry me = case meDataFile me of
  Just df -> Expr.FileMetrics
    { Expr.fmRecordCount = meRecordCount me
    , Expr.fmValueCounts = dataFileValueCounts df
    , Expr.fmNullCounts  = dataFileNullValueCounts df
    , Expr.fmNanCounts   = dataFileNanValueCounts df
    , Expr.fmLowerBounds = dataFileLowerBounds df
    , Expr.fmUpperBounds = dataFileUpperBounds df
    }
  Nothing -> Expr.emptyFileMetrics (meRecordCount me)

-- | Like 'planScan' but also collects delete file paths from delete manifests.
planScanWithDeletes
  :: TableMetadata
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Either String ScanPlan
planScanWithDeletes tm manifestListBytes readManifestByPath = do
  snap   <- maybe (Left "planScanWithDeletes: no current snapshot") Right
              (currentSnapshot tm)
  schema <- maybe (Left "planScanWithDeletes: no current schema") Right
              (currentSchema tm)
  (_, manifestFiles) <- readManifestList manifestListBytes
  let allPaths  = manifestFilePaths manifestFiles
      dataPaths = dataManifestPaths manifestFiles
      delMfs    = applicableDeletes snap manifestFiles
      delPaths  = V.map mfPath delMfs
  dataEntryVecs <- V.mapM readAndParse dataPaths
  delEntryVecs  <- V.mapM readAndParse delPaths
  let allDataEntries = V.concat (V.toList dataEntryVecs)
      allDelEntries  = V.concat (V.toList delEntryVecs)
  Right ScanPlan
    { spSnapshot        = snap
    , spSchema          = schema
    , spManifestPaths   = allPaths
    , spDataFilePaths   = V.map meFilePath allDataEntries
    , spDeleteFilePaths = V.map meFilePath allDelEntries
    }
  where
    readAndParse path = do
      bs <- readManifestByPath path
      (_, entries) <- readManifestEntries bs
      Right entries

-- | Like 'planScanWithDeletes' but additionally evaluates a row-level
-- predicate against the column statistics of each data file, dropping
-- files that cannot match. The result is one 'FileScanTask' per surviving
-- data file with its applicable delete files attached.
planScanWithFilter
  :: TableMetadata
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Expr.Expression
  -> Either String (V.Vector FileScanTask, Snapshot, Schema)
planScanWithFilter tm manifestListBytes readManifestByPath expr = do
  snap   <- maybe (Left "planScanWithFilter: no current snapshot") Right
              (currentSnapshot tm)
  schema <- maybe (Left "planScanWithFilter: no current schema") Right
              (currentSchema tm)
  planScanWithFilterAt snap schema manifestListBytes readManifestByPath expr

-- | Like 'planScanWithFilter' but driven by an explicit snapshot id rather
-- than the table's current snapshot pointer. Useful for time-travel reads.
planScanAtSnapshot
  :: TableMetadata
  -> Int64
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Expr.Expression
  -> Either String (V.Vector FileScanTask, Snapshot, Schema)
planScanAtSnapshot tm sid manifestListBytes readManifestByPath expr = do
  snap <- case Iceberg.Snapshot.snapshotById tm sid of
            Just s  -> Right s
            Nothing -> Left $ "planScanAtSnapshot: no such snapshot " ++ show sid
  let schemaForSnap = case snapSchemaId snap >>= Iceberg.SchemaEvolution.schemaById tm of
        Just s -> Just s
        Nothing -> Iceberg.SchemaEvolution.currentSchema tm
  schema <- maybe (Left "planScanAtSnapshot: no schema available") Right schemaForSnap
  planScanWithFilterAt snap schema manifestListBytes readManifestByPath expr

-- | Resolve the snapshot whose @timestamp_ms@ is the largest value not
-- exceeding the supplied target, then plan a filtered scan at that snapshot.
planScanAsOfTime
  :: TableMetadata
  -> Int64
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Expr.Expression
  -> Either String (V.Vector FileScanTask, Snapshot, Schema)
planScanAsOfTime tm targetMs manifestListBytes readManifestByPath expr =
  case Iceberg.Snapshot.snapshotAsOfTime tm targetMs of
    Just s -> planScanAtSnapshot tm (snapId s) manifestListBytes readManifestByPath expr
    Nothing -> Left $ "planScanAsOfTime: no snapshot at or before " ++ show targetMs

planScanWithFilterAt
  :: Snapshot
  -> Schema
  -> ByteString
  -> (Text -> Either String ByteString)
  -> Expr.Expression
  -> Either String (V.Vector FileScanTask, Snapshot, Schema)
planScanWithFilterAt snap schema manifestListBytes readManifestByPath expr = do
  (_, manifestFiles) <- readManifestList manifestListBytes
  let dataMfs   = V.filter (\mf -> mfContent mf == DataContent) manifestFiles
      delMfs    = applicableDeletes snap manifestFiles
  dataEntryVecs <- V.mapM (readAndInherit readManifestByPath) dataMfs
  delEntryVecs  <- V.mapM (readAndInherit readManifestByPath) delMfs
  let allDataEntries = V.concat (V.toList dataEntryVecs)
      allDelEntries  = V.concat (V.toList delEntryVecs)
      keep me = Expr.evaluateInclusive schema (fileMetricsFromEntry me) expr
      surviving = V.filter keep allDataEntries
      tasks = V.map (\me -> FileScanTask
                       { fstDataFile    = me
                       , fstDeleteFiles = applicableDeleteEntries me allDelEntries
                       , fstSpecId      = 0
                       }) surviving
  Right (tasks, snap, schema)
  where
    readAndInherit readMf manifest = do
      bs <- readMf (mfPath manifest)
      (_, entries) <- readManifestEntries bs
      Right (V.map (inheritSequenceNumbers manifest) entries)

-- | Find delete-manifest entries that are applicable to a given data-file
-- entry. The Iceberg spec says a delete file applies when its sequence
-- number is at most that of the data file.
applicableDeleteEntries
  :: ManifestEntry
  -> V.Vector ManifestEntry
  -> V.Vector ManifestEntry
applicableDeleteEntries dataEntry =
  V.filter $ \del -> case (meSequenceNumber dataEntry, meSequenceNumber del) of
    (Just dataSeq, Just delSeq) -> delSeq <= dataSeq
    _                           -> True
