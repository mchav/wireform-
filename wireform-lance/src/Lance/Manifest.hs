{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
-- | Decoder for the Lance @lance.table.Manifest@ protobuf message.
--
-- A Lance @_versions/<n>.manifest@ file is a serialised
-- @lance.table.Manifest@ protobuf message followed by a 16-byte
-- fixed footer (decoded by 'Lance.Format.parseManifestFooter').
-- The protobuf body lives at @manifest_position .. (file_size - 16)@.
--
-- This module is a thin IO facade over the auto-generated typed
-- decoders in "Lance.Pb.Lance.Table" / "Lance.Pb.Lance.File".
-- The generated modules are produced by @cabal run gen-lance-pb@
-- from @proto/lance/{file,table}.proto@; do not edit them by hand.
module Lance.Manifest
  ( -- * Top-level
    decodeManifest
  , readDatasetManifest
    -- * Active data files
  , datasetActiveDataFiles
  , datasetActiveDataFilePaths
  , datasetWriterVersion
  , datasetTimestampMillis
    -- * Schema (typed)
  , LanceSchemaField (..)
  , datasetSchemaFields
    -- * Per-column metadata in a Lance v2 data file
  , decodeColumnMetadata
  , readColumnMetadataAt
    -- * Re-exports of the generated types
  , Pb.Manifest (..)
  , Pb.Manifest'WriterVersion (..)
  , Pb.Manifest'DataStorageFormat (..)
  , Pb.DataFragment (..)
  , Pb.DataFile (..)
  , Pb.DeletionFile (..)
  , Pb.DeletionFile'DeletionFileType (..)
  , Pb.UUID (..)
  , PbF2.ColumnMetadata (..)
  , PbF2.ColumnMetadata'Page (..)
  , PbF2.Encoding (..)
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T

import qualified Proto.Decode as ProtoDecode
import qualified Proto.Google.Protobuf.Timestamp as PbTs

import qualified Lance.Format as LF
import Lance.Format
  ( LanceManifestFooter (..)
  , manifestFooterSize
  )
import Lance.IO (openLanceManifest, LanceDataset (..))
import qualified Lance.Pb.Lance.File  as PbFile
import qualified Lance.Pb.Lance.File2 as PbF2
import qualified Lance.Pb.Lance.Table as Pb

-- | Decode the bytes of a serialised @lance.table.Manifest@
-- message into the typed record. The bytes are the slice of a
-- manifest file from @manifest_position@ to the start of the
-- trailing 16-byte footer.
decodeManifest :: ByteString -> Either String Pb.Manifest
decodeManifest bs = case ProtoDecode.decodeMessage bs of
  Left e  -> Left ("Lance.Manifest: " ++ show e)
  Right m -> Right m

-- | Read a manifest file off disk: parse the 16-byte footer to
-- find @manifest_position@, slice out the protobuf body, and
-- decode it. Returns @(footer, manifest)@ on success.
--
-- The manifest body on disk is u32-length-prefixed: the first
-- 4 bytes at @manifest_position@ are the little-endian length
-- of the serialised @Manifest@ message that follows. We strip
-- that prefix before handing the bytes to the protobuf decoder.
readDatasetManifest
  :: FilePath
  -> IO (Either String (LanceManifestFooter, Pb.Manifest))
readDatasetManifest fp = do
  res <- openLanceManifest fp
  case res of
    Left err           -> pure (Left err)
    Right (footer, bs) -> do
      let total      = BS.length bs
          startPos   = fromIntegral (lmfManifestPosition footer) :: Int
          bodyEnd    = total - manifestFooterSize
          rawLen     = bodyEnd - startPos
      if startPos < 0 || rawLen < 4 || startPos + rawLen > total
        then pure (Left "Lance.Manifest: manifest body out of range")
        else
          let raw      = BS.take rawLen (BS.drop startPos bs)
              prefixed = decodeU32LE (BS.take 4 raw)
              body     = BS.take prefixed (BS.drop 4 raw)
           in if prefixed > rawLen - 4
                then pure (Left "Lance.Manifest: u32 length prefix exceeds body")
                else case decodeManifest body of
                  Left e  -> pure (Left e)
                  Right m -> pure (Right (footer, m))

-- Decode a 4-byte little-endian u32 from the head of a
-- 'ByteString' as an 'Int'. Caller must ensure the slice is
-- exactly 4 bytes.
decodeU32LE :: BS.ByteString -> Int
decodeU32LE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Int
      b1 = fromIntegral (BS.index bs 1) :: Int
      b2 = fromIntegral (BS.index bs 2) :: Int
      b3 = fromIntegral (BS.index bs 3) :: Int
   in b0 + b1 * 0x100 + b2 * 0x10000 + b3 * 0x1000000

-- | Convenience: for a 'LanceDataset' that's already been
-- opened via 'Lance.IO.openLanceDataset' or
-- 'Lance.IO.openLanceDatasetAt', read the active manifest's
-- protobuf body and return the relative @path@s of every
-- active 'DataFile' in fragment order.
--
-- Returns an empty list for an uninitialised dataset (no
-- @_versions/@) and an error for a dataset whose latest
-- manifest can't be decoded.
datasetActiveDataFiles :: LanceDataset -> IO (Either String [Text])
datasetActiveDataFiles ds = withActiveManifest ds $ \m -> do
  frag <- V.toList (Pb.manifestFragments m)
  file <- V.toList (Pb.dataFragmentFiles frag)
  pure (Pb.dataFilePath file)

-- | Like 'datasetActiveDataFiles' but joins each path against
-- the dataset root + @data/@ prefix so callers get absolute
-- (or workspace-rooted) on-disk paths ready to feed to a
-- file reader.
datasetActiveDataFilePaths :: LanceDataset -> IO (Either String [FilePath])
datasetActiveDataFilePaths ds = do
  res <- datasetActiveDataFiles ds
  pure $ case res of
    Left e   -> Left e
    Right ps -> Right (map (\p -> ldRoot ds ++ "/data/" ++ T.unpack p) ps)

-- | Library + version of the writer that produced the active
-- manifest. Surfaces 'Pb.manifestWriterVersion' as a flat
-- @(library, version)@ pair (or 'Nothing' if the table was
-- written without a writer-version field, which is the case
-- for very old Lance datasets).
datasetWriterVersion :: LanceDataset -> IO (Either String (Maybe (Text, Text)))
datasetWriterVersion ds = withActiveManifestM ds $ \m ->
  case Pb.manifestWriterVersion m of
    Nothing -> Nothing
    Just wv -> Just ( Pb.manifestWriterVersionLibrary wv
                    , Pb.manifestWriterVersionVersion wv )

-- | Version-creation 'google.protobuf.Timestamp', flattened to
-- millis since the unix epoch. Useful for surfacing
-- ds.versions()[i].timestamp without pulling in a full
-- timestamp library.
datasetTimestampMillis :: LanceDataset -> IO (Either String (Maybe Int64))
datasetTimestampMillis ds = withActiveManifestM ds $ \m ->
  case Pb.manifestTimestamp m of
    Nothing -> Nothing
    Just ts ->
      -- google.protobuf.Timestamp is { seconds : int64, nanos : int32 }.
      let !s  = fromIntegral (PbTs.timestampSeconds ts)     :: Int64
          !ns = fromIntegral (PbTs.timestampNanos   ts)     :: Int64
       in Just (s * 1000 + ns `div` 1_000_000)

-- ============================================================
-- Schema readout
-- ============================================================

-- | One field from the dataset's typed schema. Lance stores
-- schemas as a flat list of 'Pb.Field' records with parent /
-- child relationships expressed via @id@ + @parent_id@; this
-- helper rolls up the most useful fields into a flat shape.
data LanceSchemaField = LanceSchemaField
  { lsfName        :: !Text
    -- ^ Fully-qualified field name (Lance writes nested
    -- fields as e.g. @\"items.value\"@ already).
  , lsfId          :: !Int
  , lsfParentId    :: !Int
    -- ^ @-1@ (or another sentinel) for top-level fields.
  , lsfLogicalType :: !Text
    -- ^ Arrow logical-type tag — see the @logical_type@
    -- comment in @file.proto@ for the grammar (e.g.
    -- @\"int64\"@, @\"string\"@, @\"list.struct\"@,
    -- @\"decimal:128:10:2\"@).
  , lsfNullable    :: !Bool
  } deriving (Show, Eq)

-- | Every field in the dataset's schema, in manifest order
-- (which is also the canonical field-id order Lance writes).
datasetSchemaFields :: LanceDataset -> IO (Either String [LanceSchemaField])
datasetSchemaFields ds = withActiveManifest ds $ \m ->
  V.toList $ V.map fieldToTyped (Pb.manifestFields m)
  where
    fieldToTyped :: PbFile.Field -> LanceSchemaField
    fieldToTyped f = LanceSchemaField
      { lsfName        = PbFile.fieldName f
      , lsfId          = fromIntegral (PbFile.fieldId f)
      , lsfParentId    = fromIntegral (PbFile.fieldParentId f)
      , lsfLogicalType = PbFile.fieldLogicalType f
      , lsfNullable    = PbFile.fieldNullable f
      }

-- ============================================================
-- Internal: load the active manifest body
-- ============================================================

-- | Read the active manifest off disk, decode it, and run a
-- pure list-producing function over the typed @Manifest@.
withActiveManifest
  :: LanceDataset
  -> (Pb.Manifest -> [a])
  -> IO (Either String [a])
withActiveManifest ds f = case ldVersions ds of
  []         -> pure (Right [])
  ((_, p):_) -> do
    res <- try (readDatasetManifest p)
            :: IO (Either SomeException (Either String (LanceManifestFooter, Pb.Manifest)))
    case res of
      Left e         -> pure (Left ("Lance.Manifest: " ++ show e))
      Right (Left e) -> pure (Left e)
      Right (Right (_, m)) -> pure (Right (f m))

-- | Like 'withActiveManifest' but for callers that produce a
-- single 'Maybe' result (e.g. 'datasetWriterVersion').
withActiveManifestM
  :: LanceDataset
  -> (Pb.Manifest -> Maybe a)
  -> IO (Either String (Maybe a))
withActiveManifestM ds f = case ldVersions ds of
  []         -> pure (Right Nothing)
  ((_, p):_) -> do
    res <- try (readDatasetManifest p)
            :: IO (Either SomeException (Either String (LanceManifestFooter, Pb.Manifest)))
    case res of
      Left e         -> pure (Left ("Lance.Manifest: " ++ show e))
      Right (Left e) -> pure (Left e)
      Right (Right (_, m)) -> pure (Right (f m))

-- ============================================================
-- Per-column metadata in a Lance v2 data file
-- ============================================================
--
-- A Lance v2 data file stores one 'ColumnMetadata' protobuf
-- message per column at the byte range named by the file's
-- column metadata offset table (see 'parseColumnOffsetTable'
-- in "Lance.Format"). The schema lives in
-- @lance.file.v2.ColumnMetadata@ — auto-generated as
-- 'PbF2.ColumnMetadata'.
--
-- Each column metadata carries:
--
--   * 'columnMetadataEncoding' — the column-level encoding
--     descriptor.
--   * 'columnMetadataPages' — one 'Page' per data page in the
--     column. Each page records its (buffer_offsets,
--     buffer_sizes, length, encoding, priority) tuple — the
--     inputs a data-page reader needs to map a row range to
--     a slice of the file.
--   * 'columnMetadataBufferOffsets' / 'columnMetadataBufferSizes'
--     — column-level buffers (statistics, dictionaries, …).

-- | Decode the bytes of a serialised
-- @lance.file.v2.ColumnMetadata@ message into the typed record.
decodeColumnMetadata :: ByteString -> Either String PbF2.ColumnMetadata
decodeColumnMetadata bs = case ProtoDecode.decodeMessage bs of
  Left e  -> Left ("Lance.Manifest: " ++ show e)
  Right m -> Right m

-- | Read column @col@'s metadata out of a Lance data file at
-- @filePath@. Walks the file's footer to locate the
-- column-metadata offset table, slices out the requested
-- column's bytes, and decodes them via 'decodeColumnMetadata'.
readColumnMetadataAt
  :: FilePath
  -> Int     -- ^ column index (must be < footer.num_columns)
  -> IO (Either String PbF2.ColumnMetadata)
readColumnMetadataAt fp col = do
  res <- try (BS.readFile fp) :: IO (Either SomeException ByteString)
  case res of
    Left e   -> pure (Left ("Lance.Manifest: " ++ show e))
    Right bs ->
      pure $ do
        lf    <- LF.readLanceFile bs
        slice <- LF.extractColumnMetadataBytes lf col
        decodeColumnMetadata slice
