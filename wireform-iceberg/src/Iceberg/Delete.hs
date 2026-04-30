{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Iceberg position-delete and equality-delete file writers.
--
-- Both produce a Parquet file with a fixed shape and the Iceberg
-- 'DeleteFile' manifest entry describing it. The Parquet file is
-- written via "Parquet.Write" so it's byte-compatible with what
-- parquet-mr / arrow-rs / iceberg-python emit. The 'DeleteFile' the
-- caller gets back is ready to drop into the manifest writer.
--
-- = Position deletes (V2 + V3)
--
-- File layout: two required columns @file_path: string@ (field id
-- 'positionDeleteFilePathFieldId' = 2147483546) and @pos: long@
-- (field id 'positionDeletePosFieldId' = 2147483545). A V3 optional
-- @row: struct@ column is /not/ emitted by this writer; pass
-- 'writeRowGroup = False' if you want a v2 file.
--
-- = Equality deletes (V2 + V3)
--
-- File layout: one column per equality-id, in the same order the
-- caller specifies. The schema's leaf @field_id@s are exactly the
-- equality-ids, which is how Iceberg readers know which row matches.
module Iceberg.Delete
  ( -- * Position deletes
    PositionDeleteRow (..)
  , writePositionDeleteFile
  , positionDeleteFilePathFieldId
  , positionDeletePosFieldId
    -- * Equality deletes
  , EqualityDeleteSchema (..)
  , writeEqualityDeleteFile
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Iceberg.Types
  ( DeleteFile (..)
  , DeleteFileContent (..)
  , FileFormat (..)
  )

import qualified Parquet.Types as P
import qualified Parquet.Write as PW

-- ============================================================
-- Position deletes
-- ============================================================

-- | One position-delete record: which file, which row.
data PositionDeleteRow = PositionDeleteRow
  { pdrFilePath :: !Text
  , pdrPos      :: !Int64
  } deriving (Show, Eq)

-- | Reserved Iceberg field id for the @file_path@ column of a
-- position-delete file. @2147483546 = Integer.MAX_VALUE - 101@.
positionDeleteFilePathFieldId :: Int32
positionDeleteFilePathFieldId = 2147483546

-- | Reserved Iceberg field id for the @pos@ column. @= MAX_VALUE - 102@.
positionDeletePosFieldId :: Int32
positionDeletePosFieldId = 2147483545

-- | Build a position-delete Parquet file from the given delete
-- records, plus the manifest 'DeleteFile' entry for it.
--
-- The records must be sorted by @(file_path, pos)@ (the spec mandates
-- this so that scan planners can binary-search). This function does
-- /not/ sort for you; pass a presorted vector.
writePositionDeleteFile
  :: Text                       -- ^ Output object-store path (recorded on the DeleteFile).
  -> V.Vector PositionDeleteRow
  -> (ByteString, DeleteFile)
writePositionDeleteFile outputPath rows =
  let !filePathCol = V.map (TE.encodeUtf8 . pdrFilePath) rows
      !posCol      = VP.fromList (V.toList (V.map pdrPos rows))
      !schema = V.fromList
        [ P.SchemaElement "table"
            Nothing Nothing (Just 2) Nothing Nothing Nothing
        , (P.SchemaElement "file_path"
            (Just P.Required) (Just P.PTByteArray)
            Nothing Nothing Nothing Nothing)
            { P.seFieldId      = Just positionDeleteFilePathFieldId
            , P.seConvertedType = Just P.CTUtf8
            }
        , (P.SchemaElement "pos"
            (Just P.Required) (Just P.PTInt64)
            Nothing Nothing Nothing Nothing)
            { P.seFieldId = Just positionDeletePosFieldId
            }
        ]
      !cols = V.fromList
        [ PW.ColByteArray filePathCol
        , PW.ColInt64 posCol
        ]
      !fileBytes = PW.buildParquetFile schema (V.singleton cols)
      !df = DeleteFile
        { dfFilePath        = outputPath
        , dfFileFormat      = ParquetFormat
        , dfContent         = PositionDeletes
        , dfRecordCount     = fromIntegral (V.length rows)
        , dfFileSizeInBytes = fromIntegral (BS.length fileBytes)
        , dfEqualityFieldIds = V.empty
        , dfPartition       = mempty
        , dfSequenceNumber  = Nothing
        }
   in (fileBytes, df)

-- ============================================================
-- Equality deletes
-- ============================================================

-- | Description of one equality-delete column: the Iceberg @field_id@
-- the column matches and the Parquet primitive type to write it as.
-- The order of these in 'writeEqualityDeleteFile' must match the
-- order of 'PW.ColumnData' columns the caller provides.
data EqualityDeleteSchema = EqualityDeleteSchema
  { edsFieldId   :: !Int32
  , edsFieldName :: !Text
  , edsType      :: !P.ParquetType
  } deriving (Show, Eq)

-- | Build an equality-delete Parquet file. Each row in 'columns' is
-- a row of equality-key values; if any row of the target table
-- matches all those key values it is considered deleted.
--
-- @length columns == length schemaCols@; the @i@-th column writes the
-- value for @schemaCols !! i@.
writeEqualityDeleteFile
  :: Text                             -- ^ Output path.
  -> [EqualityDeleteSchema]           -- ^ Column descriptors.
  -> V.Vector PW.ColumnData           -- ^ Per-column data, same order.
  -> Either String (ByteString, DeleteFile)
writeEqualityDeleteFile outputPath schemaCols cols
  | length schemaCols /= V.length cols =
      Left "Iceberg.Delete: column count mismatch between schema and data"
  | null schemaCols =
      Left "Iceberg.Delete: equality-delete file requires at least one column"
  | otherwise = do
      let !rootElem = P.SchemaElement "table"
            Nothing Nothing (Just (fromIntegral (length schemaCols)))
            Nothing Nothing Nothing
          !leaves = V.fromList (map mkLeaf schemaCols)
          !schema = V.cons rootElem leaves
          !nRows  = if V.null cols then 0
                                   else PW.columnDataLength (V.unsafeIndex cols 0)
      if not (V.all (\c -> PW.columnDataLength c == nRows) cols)
        then Left "Iceberg.Delete: equality-delete columns must have equal row counts"
        else do
          let !fileBytes = PW.buildParquetFile schema (V.singleton cols)
              !df = DeleteFile
                { dfFilePath        = outputPath
                , dfFileFormat      = ParquetFormat
                , dfContent         = EqualityDeletes
                , dfRecordCount     = fromIntegral nRows
                , dfFileSizeInBytes = fromIntegral (BS.length fileBytes)
                , dfEqualityFieldIds = V.fromList (map edsFieldId schemaCols)
                , dfPartition       = mempty
                , dfSequenceNumber  = Nothing
                }
          Right (fileBytes, df)
  where
    mkLeaf :: EqualityDeleteSchema -> P.SchemaElement
    mkLeaf eds = (P.SchemaElement (edsFieldName eds)
                    (Just P.Required) (Just (edsType eds))
                    Nothing Nothing Nothing Nothing)
                 { P.seFieldId = Just (edsFieldId eds)
                 , P.seConvertedType = case edsType eds of
                     P.PTByteArray -> Just P.CTUtf8
                     _             -> Nothing
                 }
