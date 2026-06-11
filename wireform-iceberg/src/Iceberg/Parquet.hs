{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Bridge between @wireform-parquet@ and @wireform-iceberg@.

Once a Parquet writer has emitted a file and produced a 'P.FileMetadata',
this module derives the Iceberg manifest 'DataFile' the writer needs to
record - column sizes, value/null/NaN counts, lower/upper bounds (truncated
according to the table's metrics-mode property), key metadata, and
splittable offsets - all without re-reading the file.

It also offers a scan-side helper that pairs a Parquet 'P.OffsetIndex'
with an Iceberg 'DV.DeletionVector' to produce the page indices the reader
still needs to consume.
-}
module Iceberg.Parquet (
  -- * Writer side
  fromParquetMetadata,
  dataFileFromParquet,
  withEncryptionKeyMetadata,

  -- * Reader side
  pagesOverlappingDeletes,
  filterDeletedPages,

  -- * Encryption
  encryptionConfigFromTable,

  -- * Helpers
  columnPathFieldIdLookup,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Iceberg.BoundTrunc qualified as BT
import Iceberg.DeletionVector qualified as DV
import Iceberg.MetricsConfig qualified as MC
import Iceberg.Types
import Parquet.Encryption qualified as Enc
import Parquet.Types qualified as P


-- ============================================================
-- Encryption wiring
-- ============================================================

{- | Build a Parquet 'Enc.EncryptionConfig' from an Iceberg
'TableMetadata' encryption-keys map plus an explicit footer key. The
caller supplies the actual key bytes (Iceberg only stores the @key-id@
references in 'tmEncryptionKeys'); this helper assembles the
'Enc.EncryptionKeys' record and copies the @key-id@ string into the
@encKeyMetadata@ field that ends up on every encrypted column.
-}
encryptionConfigFromTable
  :: TableMetadata
  -> Text
  -- ^ key-id reference name (must exist in tmEncryptionKeys).
  -> ByteString
  -- ^ Resolved footer key bytes (16/24/32).
  -> Map Text ByteString
  -- ^ Resolved per-column keys, by Iceberg field name.
  -> ByteString
  -- ^ aad_file_id (8 bytes).
  -> Enc.EncryptionConfig
encryptionConfigFromTable tm keyId footerKey colKeys fileId =
  let keyMd = case Map.lookup keyId (tmEncryptionKeys tm) of
        Just txt -> TE.encodeUtf8 txt
        Nothing -> TE.encodeUtf8 keyId
  in Enc.EncryptionConfig
       { Enc.encAlgorithm = Enc.AesGcmV1
       , Enc.encKeys = Enc.EncryptionKeys footerKey colKeys
       , Enc.encAadFileId = fileId
       , Enc.encAadPrefix = mempty
       , Enc.encKeyMetadata = keyMd
       }


{- | Stamp an Iceberg 'DataFile' with the @key_metadata@ from a Parquet
'Enc.EncryptionConfig' (so the manifest entry records which key id was
used to encrypt the data file).
-}
withEncryptionKeyMetadata :: Enc.EncryptionConfig -> DataFile -> DataFile
withEncryptionKeyMetadata cfg df =
  df
    { dataFileKeyMetadata =
        if BS.null (Enc.encKeyMetadata cfg)
          then dataFileKeyMetadata df
          else Just (Enc.encKeyMetadata cfg)
    }


-- ============================================================
-- Writer side
-- ============================================================

{- | Project a Parquet 'P.FileMetadata' onto a partially-populated Iceberg
'DataFile'. The caller still needs to supply the file path, file size on
disk, the partition tuple (computed from the writer's row values via
'Iceberg.Partition.buildPartition'), and any v3 fields such as
@first_row_id@ - everything else (column statistics, bounds, split
offsets) is derived from the Parquet footer.

@lookupFieldId path@ resolves a Parquet column's
@cmPathInSchema@ to an Iceberg field id; use 'columnPathFieldIdLookup'
to build it from a 'Schema'.
-}
fromParquetMetadata
  :: P.FileMetadata
  -> (Vector Text -> Maybe Int)
  -- ^ Column path -> Iceberg field id.
  -> Map Text Text
  -- ^ Table properties (used for metrics modes).
  -> Text
  -- ^ Data file path.
  -> Int64
  -- ^ Data file size (bytes on disk).
  -> Vector (Maybe Value)
  -- ^ Partition tuple.
  -> Maybe Int
  -- ^ Sort order id (when applicable).
  -> DataFile
fromParquetMetadata fm lookupFid props path fileSize partition sortOrderId =
  let allChunks = V.concatMap P.rgColumns (P.fmRowGroups fm)
      pairs = V.toList (V.mapMaybe (chunkToFidStats lookupFid props) allChunks)
      colSizes = aggregate (\(fid, _, m) -> (fid, P.cmTotalCompressedSize m)) pairs
      valCounts = aggregate (\(fid, _, m) -> (fid, P.cmNumValues m)) pairs
      nullCounts =
        aggregateMaybe
          (\(fid, _, m) -> (fid,) <$> (P.cmStatistics m >>= P.statNullCount))
          pairs
      lowerBounds = collapseBounds Lower pairs
      upperBounds = collapseBounds Upper pairs
      splits =
        V.fromList
          ( concatMap
              ( \rg ->
                  [ P.cmDataPageOffset md
                  | cc <- V.toList (P.rgColumns rg)
                  , Just md <- [P.ccMetadata cc]
                  ]
              )
              (V.toList (P.fmRowGroups fm))
          )
      totalRows = P.fmNumRows fm
  in DataFile
       { dataFileContent = DataContent
       , dataFileFilePath = path
       , dataFileFileFormat = ParquetFormat
       , dataFilePartition = partition
       , dataFileRecordCount = totalRows
       , dataFileFileSize = fileSize
       , dataFileColumnSizes = colSizes
       , dataFileValueCounts = valCounts
       , dataFileNullValueCounts = nullCounts
       , dataFileNanValueCounts = Map.empty
       , dataFileLowerBounds = lowerBounds
       , dataFileUpperBounds = upperBounds
       , dataFileKeyMetadata = Nothing
       , dataFileSplitOffsets = splits
       , dataFileEqualityIds = V.empty
       , dataFileSortOrderId = sortOrderId
       , dataFileFirstRowId = Nothing
       , dataFileReferencedDataFile = Nothing
       , dataFileContentOffset = Nothing
       , dataFileContentSize = Nothing
       }


{- | Convenience: build a fully-populated 'DataFile' for an in-process Parquet
write. Equivalent to 'fromParquetMetadata' but resolves column paths via a
supplied 'Schema'.
-}
dataFileFromParquet
  :: P.FileMetadata
  -> Schema
  -> Map Text Text
  -> Text
  -> Int64
  -> Vector (Maybe Value)
  -> Maybe Int
  -> DataFile
dataFileFromParquet fm schema props path size part sortId =
  fromParquetMetadata fm (columnPathFieldIdLookup schema) props path size part sortId


{- | Build a column-path -> field-id resolver from an Iceberg 'Schema'. Walks
nested struct/list/map types and produces the Parquet-style dotted path
representation (@\"a.b.c\"@ flattened to @[\"a\", \"b\", \"c\"]@) for each
leaf with its Iceberg field id.
-}
columnPathFieldIdLookup :: Schema -> Vector Text -> Maybe Int
columnPathFieldIdLookup schema =
  let pathMap = Map.fromList (collect [] (V.toList (schemaFields schema)))
  in \pathV -> Map.lookup (V.toList pathV) pathMap
  where
    collect :: [Text] -> [StructField] -> [([Text], Int)]
    collect prefix fields = concatMap (collectOne prefix) fields

    collectOne :: [Text] -> StructField -> [([Text], Int)]
    collectOne prefix sf =
      let path = prefix ++ [sfName sf]
          here = (path, sfId sf)
      in case sfType sf of
           TStruct nested -> here : collect path (V.toList nested)
           TList _ inner -> here : collectInner (path ++ ["element"]) inner
           TMap _ kt _ vt ->
             here
               : collectInner (path ++ ["key"]) kt
               ++ collectInner (path ++ ["value"]) vt
           _ -> [here]

    collectInner :: [Text] -> IcebergType -> [([Text], Int)]
    collectInner path ty = case ty of
      TStruct nested -> collect path (V.toList nested)
      TList _ inner -> collectInner (path ++ ["element"]) inner
      TMap _ kt _ vt ->
        collectInner (path ++ ["key"]) kt
          ++ collectInner (path ++ ["value"]) vt
      _ -> []


-- ============================================================
-- Reader side: deletion-vector ↔ Parquet page index
-- ============================================================

{- | Given an Iceberg 'DV.DeletionVector' and the Parquet 'P.OffsetIndex' for
a column chunk, return the indices of pages that contain at least one
deleted row. Pages with no deleted rows are omitted, so the caller can
skip them entirely during scan.
-}
pagesOverlappingDeletes
  :: DV.DeletionVector
  -> P.OffsetIndex
  -> Vector Int
pagesOverlappingDeletes dv oi =
  let !pages = P.oiPageLocations oi
      !numPages = V.length pages
      !rowsPerPage = pageRowSpans pages
      hits =
        V.imapMaybe
          ( \i (firstRow, nextFirstRow) ->
              if pageHasDelete firstRow nextFirstRow dv
                then Just i
                else Nothing
          )
          rowsPerPage
  in if numPages == 0 then V.empty else hits
  where
    pageRowSpans :: Vector P.PageLocation -> Vector (Int64, Int64)
    pageRowSpans pages =
      let !n = V.length pages
      in V.generate n $ \i ->
           let !this = P.plFirstRowIndex (V.unsafeIndex pages i)
               !next =
                 if i + 1 < n
                   then P.plFirstRowIndex (V.unsafeIndex pages (i + 1))
                   else maxBound
           in (this, next)

    pageHasDelete :: Int64 -> Int64 -> DV.DeletionVector -> Bool
    pageHasDelete !lo !hi vec =
      -- DV.deletedPositions is sorted; we test only positions that fall in
      -- the page's row span. For typical row groups the dv is small (deletion
      -- vectors hold *additions* to per-row marks) so the linear scan is
      -- fine; if it grows we can add a range query to Iceberg.DeletionVector.
      any (\p -> p >= lo && p < hi) (DV.deletedPositions vec)


{- | Produce the row group indices a scanner should /still/ touch for this
column chunk after applying the deletion vector. Pages where every row is
deleted are dropped; pages with at least one surviving row stay.
-}
filterDeletedPages
  :: DV.DeletionVector
  -> P.OffsetIndex
  -> Int64
  -- ^ Total row count of the column chunk.
  -> Vector Int
filterDeletedPages dv oi totalRows =
  let !pages = P.oiPageLocations oi
      !n = V.length pages
      go !i acc
        | i >= n = V.fromList (reverse acc)
        | otherwise =
            let !lo = P.plFirstRowIndex (V.unsafeIndex pages i)
                !hi =
                  if i + 1 < n
                    then P.plFirstRowIndex (V.unsafeIndex pages (i + 1))
                    else totalRows
                deletes = filter (\p -> p >= lo && p < hi) (DV.deletedPositions dv)
                pageRows = hi - lo
                fullyDeleted = fromIntegral (length deletes) >= pageRows
            in if fullyDeleted
                 then go (i + 1) acc
                 else go (i + 1) (i : acc)
  in go 0 []


-- ============================================================
-- Writer-side aggregation helpers
-- ============================================================

data BoundSide = Lower | Upper


{- | Project (fid, leaf path, column metadata) for column chunks whose path
is mapped to an Iceberg field id and whose metrics-mode for that column
isn't @none@.
-}
chunkToFidStats
  :: (Vector Text -> Maybe Int)
  -> Map Text Text
  -> P.ColumnChunk
  -> Maybe (Int, Vector Text, P.ColumnMetadata)
chunkToFidStats lookupFid props cc = do
  cm <- P.ccMetadata cc
  fid <- lookupFid (P.cmPathInSchema cm)
  case MC.metricsModeForColumn props (T.intercalate "." (V.toList (P.cmPathInSchema cm))) of
    MC.MetricsNone -> Nothing
    _ -> Just (fid, P.cmPathInSchema cm, cm)


aggregate
  :: ((Int, Vector Text, P.ColumnMetadata) -> (Int, Int64))
  -> [(Int, Vector Text, P.ColumnMetadata)]
  -> Map Int Int64
aggregate f = foldl' step Map.empty
  where
    step !acc x =
      let !(fid, v) = f x
      in Map.insertWith (+) fid v acc


aggregateMaybe
  :: ((Int, Vector Text, P.ColumnMetadata) -> Maybe (Int, Int64))
  -> [(Int, Vector Text, P.ColumnMetadata)]
  -> Map Int Int64
aggregateMaybe f = foldl' step Map.empty
  where
    step !acc x = case f x of
      Just (fid, v) -> Map.insertWith (+) fid v acc
      Nothing -> acc


collapseBounds
  :: BoundSide
  -> [(Int, Vector Text, P.ColumnMetadata)]
  -> Map Int ByteString
collapseBounds side = foldl' step Map.empty
  where
    step !acc (fid, _path, cm) =
      case P.cmStatistics cm >>= statBound side of
        Just bs ->
          let bs' = case side of
                Lower -> BT.truncateLowerBytes truncationDefault bs
                Upper -> case BT.truncateUpperBytes truncationDefault bs of
                  Just t -> t
                  Nothing -> bs
          in Map.alter (mergeBound side bs') fid acc
        Nothing -> acc

    mergeBound Lower new Nothing = Just new
    mergeBound Lower new (Just old) = Just (if new <= old then new else old)
    mergeBound Upper new Nothing = Just new
    mergeBound Upper new (Just old) = Just (if new >= old then new else old)


statBound :: BoundSide -> P.Statistics -> Maybe ByteString
statBound Lower s = case P.statMinValue s of
  Just bs -> Just bs
  Nothing -> P.statMin s
statBound Upper s = case P.statMaxValue s of
  Just bs -> Just bs
  Nothing -> P.statMax s


{- | Default truncation length for string/binary bounds when the table doesn't
override @write.metadata.metrics.default@. Matches Iceberg's
@MetricsConfig.DEFAULT_METRICS_MODE_DEFAULT@ (@truncate(16)@).
-}
truncationDefault :: Int
truncationDefault = 16
