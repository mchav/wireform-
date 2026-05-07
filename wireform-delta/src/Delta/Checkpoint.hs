{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Decoder for Delta Lake @*.checkpoint.parquet@ files.
--
-- A Delta checkpoint Parquet file folds the entire active log
-- of a table into a single columnar snapshot. Each row carries
-- exactly one of the action variants (@add@ / @remove@ /
-- @metaData@ / @protocol@ / @txn@ / …) under a top-level
-- struct column whose name matches the action; all other
-- action structs on that row are null. By reading these
-- columns, consumers can reconstruct a 'Delta.Log.TableSnapshot'
-- without walking the @NNNN.json@ commit files from version 0.
--
-- This module decodes the four action columns most readers
-- need: @add@, @remove@, @metaData@, and @protocol@. The
-- spec also defines @txn@, @domainMetadata@, and @sidecar@
-- columns; these are surfaced as 'ActionOther' so callers can
-- see they exist without losing them.
--
-- /Implementation notes:/
--
--   * The Parquet schema has duplicate leaf names across struct
--     parents (@add.path@, @remove.path@, @sidecar.path@, …).
--     We resolve each leaf by its full schema path
--     (@'cmPathInSchema'@) rather than by leaf name, sidestepping
--     the @parquetFileArrowSchema@ flattening that would
--     conflate them.
--   * Per-leaf reading goes through the existing
--     @readGenericXxxOptionalColumnChunk@ family, which handles
--     PLAIN, PLAIN_DICTIONARY and RLE_DICTIONARY in a single
--     pass. Delta checkpoints written by delta-rs / Spark are
--     dictionary-encoded for the high-cardinality string
--     columns and PLAIN for the low-cardinality bool / int
--     ones; both shapes thread through the generic reader.
--   * For each row we look at the @add.path@ / @remove.path@ /
--     @metaData.id@ / @protocol.minReaderVersion@ leaves: if
--     the row's value is 'Just _' the corresponding struct is
--     present, so we attribute the row to that variant.
--   * Map-typed and list-typed leaves (partitionValues, tags,
--     partitionColumns, readerFeatures, writerFeatures) are
--     /not/ decoded yet. See the docstring on the per-action
--     decoder for what's surfaced and what's left blank.
module Delta.Checkpoint
  ( -- * Decode
    decodeCheckpointFile
  , readCheckpointFile
    -- * Replay
  , checkpointToActions
  , snapshotFromCheckpoint
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64)

import qualified Parquet.Levels as PL
import qualified Parquet.Read   as PR
import qualified Parquet.Types  as P

import Delta.Log
  ( DeltaAction (..)
  , AddAction (..)
  , RemoveAction (..)
  , MetaDataAction (..)
  , ProtocolAction (..)
  , TableSnapshot
  , snapshotFromActions
  )

-- ============================================================
-- Decode
-- ============================================================

-- | Decode every action row in a Delta checkpoint Parquet
-- payload into a flat list of 'DeltaAction's. Rows whose
-- variant we don't recognise (@txn@, @domainMetadata@,
-- @sidecar@) are surfaced as 'ActionOther' rather than
-- dropped, so callers can see they exist.
decodeCheckpointFile :: ByteString -> Either String [DeltaAction]
decodeCheckpointFile bs = do
  pf <- PR.loadParquetFile bs
  let rgs = V.toList (P.fmRowGroups (PR.pfFooter pf))
  rgActions <- mapM (rowGroupActions pf) rgs
  Right (concat rgActions)

-- | Convenience: read the checkpoint file off disk and decode
-- it. Wraps the underlying bytes-loading exception in 'Either'
-- so callers don't have to install their own handler.
readCheckpointFile :: FilePath -> IO (Either String [DeltaAction])
readCheckpointFile fp = do
  res <- try (BS.readFile fp) :: IO (Either SomeException ByteString)
  case res of
    Left e   -> pure (Left ("Delta.Checkpoint: " ++ show e))
    Right bs -> pure (decodeCheckpointFile bs)

-- ============================================================
-- Replay
-- ============================================================

-- | Re-emit the action stream in row order. Useful when the
-- caller wants to feed a checkpoint into the same fold they
-- use for a JSON commit walk.
checkpointToActions :: ByteString -> Either String [DeltaAction]
checkpointToActions = decodeCheckpointFile

-- | Apply every checkpoint row to 'snapshotFromActions',
-- yielding a 'TableSnapshot' at the checkpoint version.
snapshotFromCheckpoint :: ByteString -> Either String TableSnapshot
snapshotFromCheckpoint = fmap snapshotFromActions . decodeCheckpointFile

-- ============================================================
-- Internal: per-row-group walker
-- ============================================================

-- | Decode every row in one row-group into a list of
-- 'DeltaAction's, in row order. Reads only the leaves we
-- actually need; the rest of the column chunks are never
-- touched.
rowGroupActions
  :: PR.ParquetFile
  -> P.RowGroup
  -> Either String [DeltaAction]
rowGroupActions pf rg = do
  let n = fromIntegral (P.rgNumRows rg) :: Int
  -- 'add' columns
  addPath_     <- readByteArrayOpt pf rg ["add", "path"]
  addSize_     <- readInt64Opt     pf rg ["add", "size"]
  addModTime_  <- readInt64Opt     pf rg ["add", "modificationTime"]
  addDataChg_  <- readBoolOpt      pf rg ["add", "dataChange"]
  addStats_    <- readByteArrayOpt pf rg ["add", "stats"]
  -- 'remove' columns
  remPath_     <- readByteArrayOpt pf rg ["remove", "path"]
  remDelTs_    <- readInt64Opt     pf rg ["remove", "deletionTimestamp"]
  remDataChg_  <- readBoolOpt      pf rg ["remove", "dataChange"]
  -- 'metaData' columns
  metaId_      <- readByteArrayOpt pf rg ["metaData", "id"]
  metaName_    <- readByteArrayOpt pf rg ["metaData", "name"]
  metaSchema_  <- readByteArrayOpt pf rg ["metaData", "schemaString"]
  metaProv_    <- readByteArrayOpt pf rg ["metaData", "format", "provider"]
  -- 'protocol' columns
  protMinR_    <- readInt32Opt     pf rg ["protocol", "minReaderVersion"]
  protMinW_    <- readInt32Opt     pf rg ["protocol", "minWriterVersion"]
  -- 'txn' presence (just the appId leaf; we don't decode the body)
  txnApp_      <- readByteArrayOpt pf rg ["txn", "appId"]
  -- 'domainMetadata' / 'sidecar' presence
  domDom_      <- readByteArrayOpt pf rg ["domainMetadata", "domain"]
  sideP_       <- readByteArrayOpt pf rg ["sidecar", "path"]

  pure [ rowAction i
           addPath_ addSize_ addModTime_ addDataChg_ addStats_
           remPath_ remDelTs_ remDataChg_
           metaId_ metaName_ metaSchema_ metaProv_
           protMinR_ protMinW_
           txnApp_ domDom_ sideP_
       | i <- [0 .. n - 1]
       ]

-- | Look at row @i@ across the per-leaf vectors and emit
-- exactly one 'DeltaAction'. The first non-null variant wins;
-- the spec guarantees only one is set per row.
rowAction
  :: Int
  -> V.Vector (Maybe ByteString) -- add.path
  -> V.Vector (Maybe Int64)      -- add.size
  -> V.Vector (Maybe Int64)      -- add.modificationTime
  -> V.Vector (Maybe Bool)       -- add.dataChange
  -> V.Vector (Maybe ByteString) -- add.stats
  -> V.Vector (Maybe ByteString) -- remove.path
  -> V.Vector (Maybe Int64)      -- remove.deletionTimestamp
  -> V.Vector (Maybe Bool)       -- remove.dataChange
  -> V.Vector (Maybe ByteString) -- metaData.id
  -> V.Vector (Maybe ByteString) -- metaData.name
  -> V.Vector (Maybe ByteString) -- metaData.schemaString
  -> V.Vector (Maybe ByteString) -- metaData.format.provider
  -> V.Vector (Maybe Int32)      -- protocol.minReaderVersion
  -> V.Vector (Maybe Int32)      -- protocol.minWriterVersion
  -> V.Vector (Maybe ByteString) -- txn.appId
  -> V.Vector (Maybe ByteString) -- domainMetadata.domain
  -> V.Vector (Maybe ByteString) -- sidecar.path
  -> DeltaAction
rowAction i ap aSz aMt aDc aSt rp rDt rDc mId mNm mSc mPr pR pW tA dD sP
  | Just path <- atIdx ap i = ActionAdd AddAction
      { addPath             = decodeText path
      , addSize             = fromIntegralMaybe (atIdx aSz i)
      , addModificationTime = fromIntegralMaybe (atIdx aMt i)
      , addDataChange       = fromMaybe True (atIdx aDc i)
      , addStats            = fmap decodeText (atIdx aSt i)
      , addPartitionValues  = Map.empty
          -- partitionValues is a 'map<string, string>' whose
          -- key_value entries live under three list-encoded
          -- leaves. Decoding them needs a list-aware reader on
          -- top of the per-leaf machinery; out of scope for the
          -- MVP. The active file path / size / stats are still
          -- complete.
      , addTags             = Map.empty
      , addDeletionVector   = Nothing
      }
  | Just path <- atIdx rp i = ActionRemove RemoveAction
      { removePath              = decodeText path
      , removeDeletionTimestamp = fmap fromIntegral (atIdx rDt i)
      , removeDataChange        = fromMaybe True (atIdx rDc i)
      , removeExtendedFileMetadata = Nothing
      , removeSize              = Nothing
      , removePartitionValues   = Map.empty
      }
  | Just metaIdBs <- atIdx mId i = ActionMetaData MetaDataAction
      { mdId               = decodeText metaIdBs
      , mdName             = fmap decodeText (atIdx mNm i)
      , mdDescription      = Nothing
      , mdFormat           = case atIdx mPr i of
          Just provider ->
            -- The format struct's 'options' map is again a
            -- list-encoded map column we don't decode here.
            -- Surfacing the 'provider' alone is enough for
            -- callers that want to know whether the table is
            -- Parquet-backed.
            Just (decodeText provider, Map.empty)
          Nothing -> Nothing
      , mdSchemaString     = maybe "" decodeText (atIdx mSc i)
      , mdPartitionColumns = []
      , mdConfiguration    = Map.empty
      , mdCreatedTime      = Nothing
      }
  | Just minR <- atIdx pR i = ActionProtocol ProtocolAction
      { pMinReaderVersion = fromIntegral minR
      , pMinWriterVersion = maybe 0 fromIntegral (atIdx pW i)
      , pReaderFeatures   = []
      , pWriterFeatures   = []
      }
  | Just _ <- atIdx tA i = ActionOther "txn"
  | Just _ <- atIdx dD i = ActionOther "domainMetadata"
  | Just _ <- atIdx sP i = ActionOther "sidecar"
  | otherwise            = ActionOther "<empty-row>"

atIdx :: V.Vector (Maybe a) -> Int -> Maybe a
atIdx v i = case v V.!? i of
  Just (Just x) -> Just x
  _             -> Nothing

decodeText :: ByteString -> Text
decodeText = TE.decodeUtf8

-- | Project @Maybe Int64@ values to a non-negative @Word64@,
-- defaulting to 0 when absent. Negative @Int64@s collapse to
-- 0 too (Delta size / modificationTime fields can't legally
-- be negative).
fromIntegralMaybe :: Maybe Int64 -> Word64
fromIntegralMaybe = maybe 0 (\n -> if n < 0 then 0 else fromIntegral n)

-- ============================================================
-- Per-leaf readers (path-based, encoding-agnostic)
-- ============================================================

-- | Look up the column chunk in @rg@ whose @cmPathInSchema@
-- matches @path@. The Delta checkpoint's schema has duplicate
-- leaf names ('add.path', 'remove.path'), so we /must/ match
-- on the full path rather than just the leaf-name.
findChunk
  :: P.RowGroup
  -> [Text]
  -> Either String P.ColumnChunk
findChunk rg path =
  let target = V.fromList path
   in case V.find (\cc -> case P.ccMetadata cc of
                            Just cm -> P.cmPathInSchema cm == target
                            Nothing -> False)
                   (P.rgColumns rg) of
        Just cc -> Right cc
        Nothing -> Left ("Delta.Checkpoint: column not found: "
                         ++ show path)

-- | Compute (maxRep, maxDef) for a leaf path, then read it as
-- @V.Vector (Maybe a)@ by dispatching on the physical type.
-- Missing leaves (e.g. an older Delta protocol without the
-- @sidecar@ column) come back as the zero-length all-Nothing
-- vector so per-row presence checks stay sound.
readByteArrayOpt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe ByteString))
readByteArrayOpt = readLeaf PR.readGenericByteArrayOptionalColumnChunk

readInt64Opt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Int64))
readInt64Opt = readLeaf PR.readGenericInt64OptionalColumnChunk

readInt32Opt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Int32))
readInt32Opt = readLeaf PR.readGenericInt32OptionalColumnChunk

readBoolOpt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Bool))
readBoolOpt = readLeaf PR.readGenericBoolOptionalColumnChunk

-- | Generic per-leaf reader: looks up the chunk by path,
-- decompresses + decodes via the supplied per-type reader,
-- and pads to the row-group row count. If the leaf isn't in
-- the schema (an older Delta protocol that doesn't write
-- e.g. @add.deletionVector.*@), we return an all-Nothing
-- vector of the right length so per-row presence checks stay
-- sound.
readLeaf
  :: (P.Compression -> Int -> Int -> ByteString
        -> Either String (V.Vector (Maybe a)))
  -> PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe a))
readLeaf reader pf rg path =
  let !nRows = fromIntegral (P.rgNumRows rg) :: Int
   in case findChunk rg path of
        Left  _  -> Right (V.replicate nRows Nothing)
        Right cc -> do
          cm <- case P.ccMetadata cc of
            Just m  -> Right m
            Nothing -> Left "Delta.Checkpoint: column chunk has no metadata"
          (maxRep, maxDef) <- PL.maxLevelsForColumnPath
                                (P.fmSchema (PR.pfFooter pf))
                                (V.fromList path)
          chunkSlice <- columnChunkBytes pf cm
          reader (P.cmCodec cm) maxRep maxDef chunkSlice

-- | Slice the bytes of a column chunk out of a 'ParquetFile'.
-- Mirrors what 'PR.columnChunkSlice' does but works directly
-- from a 'ColumnMetadata' so we don't have to thread the
-- (rgIdx, colIdx) pair through the per-leaf path.
columnChunkBytes
  :: PR.ParquetFile
  -> P.ColumnMetadata
  -> Either String ByteString
columnChunkBytes pf cm =
  let !start = case P.cmDictionaryPageOffset cm of
        Just dpOff | dpOff > 0 -> fromIntegral dpOff :: Int
        _                      -> fromIntegral (P.cmDataPageOffset cm) :: Int
      !len   = fromIntegral (P.cmTotalCompressedSize cm) :: Int
      !bs    = PR.pfBytes pf
   in if start < 0 || len < 0 || start + len > BS.length bs
        then Left "Delta.Checkpoint: column chunk out of range"
        else Right (BS.take len (BS.drop start bs))
