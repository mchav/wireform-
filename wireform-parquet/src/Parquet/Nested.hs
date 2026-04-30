{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Writer-side support for one specific nested column shape that
-- Iceberg actually emits in the V1/V2/V3 specs:
--
-- @
-- optional group <name> (LIST) {
--   repeated group list {
--     optional <T> element;
--   }
-- }
-- @
--
-- ("optional list of optional primitive"). This is the shape Iceberg
-- uses for partition-tuple lists, V3 default-value arrays, and any
-- @list<T?>?@ user column.
--
-- Higher-order nesting (lists of structs, lists of lists, maps) is a
-- straight generalisation of this same Dremel-style shred, but the
-- error-prone parts are getting the (def, rep) sequence right and
-- pyarrow-compatible. We deliberately support only the shape this
-- module's docstring describes here so the implementation can be
-- ground-truthed against pyarrow byte-for-byte; richer nesting can
-- layer on top later without changing this entry point.
--
-- Usage sketch:
--
-- @
-- let xs = V.fromList
--           [ Just (V.fromList [Just 1, Nothing, Just 3])
--           , Just V.empty
--           , Nothing
--           ]
--     leaf = encodeOptionalListOptionalI32 xs
--     -- 'leaf' carries (path, maxDef=3, maxRep=1, defLevels, repLevels,
--     --                 plain-encoded present values, value count).
-- @
--
-- Pair the resulting 'NestedLeaf' with a list-shaped schema (build
-- with 'optionalListSchemaSegments') and the writer emits a
-- DATA_PAGE_V2 the way pyarrow / parquet-mr do.
module Parquet.Nested
  ( NestedLeaf (..)
  , encodeOptionalListOptionalI32
  , encodeOptionalListOptionalI64
  , encodeOptionalListOptionalDouble
  , optionalListSchemaSegments
  , buildOptionalListFile
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import GHC.Float (castDoubleToWord64)

import qualified Data.ByteString as BS
import Data.Word (Word8)

import qualified Parquet.LevelsEncode as LE
import qualified Parquet.Page as PP
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , ConvertedType (..)
  , Encoding (..)
  , FileMetadata (..)
  , LogicalType (..)
  , ParquetType (..)
  , Repetition (..)
  , RowGroup (..)
  , Statistics (..)
  , SchemaElement (..)
  )
import qualified Parquet.Write as PW

-- | One leaf column after Dremel shredding, ready to be written as a
-- @DATA_PAGE_V2@ page. Stream lengths satisfy
-- @VP.length nlDefLevels == VP.length nlRepLevels == /total events/@,
-- and 'nlValueCount' is the number of events with @def == nlMaxDef@
-- (the only ones that contribute a value to 'nlValueBytes').
data NestedLeaf = NestedLeaf
  { nlPath        :: !(V.Vector Text)
  , nlMaxDef      :: !Int
  , nlMaxRep      :: !Int
  , nlDefLevels   :: !(VP.Vector Int32)
  , nlRepLevels   :: !(VP.Vector Int32)
  , nlValueBytes  :: !ByteString
  , nlValueCount  :: !Int
  } deriving (Show, Eq)

-- ============================================================
-- The single shape: optional list of optional primitive
-- ============================================================
--
-- For 'list<T?>?' the spec gives:
--   maxDef = 3 (xs optional, list repeated, element optional)
--   maxRep = 1
--
-- Per-row events (Dremel shred):
--   - 'Nothing'                  -> [(def=0, rep=0)]
--   - 'Just []'                  -> [(def=1, rep=0)]
--   - 'Just [Nothing, Just v..]' -> first element rep=0; rest rep=1.
--                                    Element def is 2 for 'Nothing',
--                                    3 for 'Just v'.
--
-- Value bytes are PLAIN encoding of the present values in order.

-- | Specialised encoder for @optional list&lt;optional INT32&gt;@.
encodeOptionalListOptionalI32
  :: Text                                        -- ^ outer column name
  -> V.Vector (Maybe (V.Vector (Maybe Int32)))   -- ^ row-major data
  -> NestedLeaf
encodeOptionalListOptionalI32 col =
  encodeOptionalListOptional col (B.int32LE) 4

encodeOptionalListOptionalI64
  :: Text
  -> V.Vector (Maybe (V.Vector (Maybe Int64)))
  -> NestedLeaf
encodeOptionalListOptionalI64 col =
  encodeOptionalListOptional col (B.int64LE) 8

encodeOptionalListOptionalDouble
  :: Text
  -> V.Vector (Maybe (V.Vector (Maybe Double)))
  -> NestedLeaf
encodeOptionalListOptionalDouble col =
  encodeOptionalListOptional col (B.word64LE . castDoubleToWord64) 8

encodeOptionalListOptional
  :: Text
  -> (a -> B.Builder)
  -> Int                       -- ^ size hint per value (for builder)
  -> V.Vector (Maybe (V.Vector (Maybe a)))
  -> NestedLeaf
encodeOptionalListOptional outerName encOne _hint rows =
  let (defs, reps, valueCount, valueBytes) = shredOptionalListOptional encOne rows
      path = V.fromList [outerName, "list", "element"]
   in NestedLeaf
        { nlPath        = path
        , nlMaxDef      = 3
        , nlMaxRep      = 1
        , nlDefLevels   = defs
        , nlRepLevels   = reps
        , nlValueBytes  = valueBytes
        , nlValueCount  = valueCount
        }

shredOptionalListOptional
  :: (a -> B.Builder)
  -> V.Vector (Maybe (V.Vector (Maybe a)))
  -> (VP.Vector Int32, VP.Vector Int32, Int, ByteString)
shredOptionalListOptional encOne rows =
  let (defL, repL, vc, body) = V.foldl' step ([], [], 0, mempty) rows
      defV = VP.fromList (reverse defL)
      repV = VP.fromList (reverse repL)
   in (defV, repV, vc, BL.toStrict (B.toLazyByteString body))
  where
    step (!ds, !rs, !cnt, !bld) row = case row of
      Nothing -> (0 : ds, 0 : rs, cnt, bld)
      Just inner
        | V.null inner -> (1 : ds, 0 : rs, cnt, bld)
        | otherwise    ->
            V.ifoldl' (\(!ds', !rs', !c', !b') i mv ->
                          let r = if i == 0 then 0 else 1
                              (d, c'', b'') = case mv of
                                Nothing -> (2, c', b')
                                Just v  -> (3, c' + 1, b' <> encOne v)
                           in (d : ds', r : rs', c'', b''))
                      (ds, rs, cnt, bld) inner

-- ============================================================
-- Schema helpers
-- ============================================================

-- | Three SchemaElements describing a Parquet @optional list of
-- optional <T>@ subtree, in the order Parquet's flattened schema
-- expects:
--
-- @
--   optional group <name> (LIST) { num_children = 1 }
--   repeated group list             { num_children = 1 }
--   optional <T> element
-- @
--
-- Append these to a row-group root SchemaElement (which itself has
-- @num_children@ counting top-level columns) to get a complete schema.
optionalListSchemaSegments
  :: Text         -- ^ outer column name
  -> ParquetType  -- ^ leaf primitive type
  -> Maybe Int32  -- ^ outer column field-id (Iceberg-required)
  -> Maybe Int32  -- ^ leaf element field-id (Iceberg-required)
  -> [SchemaElement]
optionalListSchemaSegments outerName leafType outerFid leafFid =
  [ SchemaElement
      { seName          = outerName
      , seRepetition    = Just Optional
      , seType          = Nothing
      , seNumChildren   = Just 1
      , seConvertedType = Just CTList
      , seLogicalType   = Just LTList
      , seFieldId       = outerFid
      }
  , SchemaElement
      { seName          = "list"
      , seRepetition    = Just Repeated
      , seType          = Nothing
      , seNumChildren   = Just 1
      , seConvertedType = Nothing
      , seLogicalType   = Nothing
      , seFieldId       = Nothing
      }
  , SchemaElement
      { seName          = "element"
      , seRepetition    = Just Optional
      , seType          = Just leafType
      , seNumChildren   = Nothing
      , seConvertedType = Nothing
      , seLogicalType   = Nothing
      , seFieldId       = leafFid
      }
  ]

-- | Build a complete Parquet file containing exactly one column - a
-- top-level @optional list&lt;optional T&gt;@. The Parquet physical
-- type the leaf will be written as is taken from @leafType@.
--
-- The page emitted is DATA_PAGE_V2 with explicit definition- and
-- repetition-level lengths, followed by the PLAIN-encoded present
-- values from 'nlValueBytes'. The column chunk's @num_values@ is the
-- /total/ event count (def stream length), per the V2 spec, not the
-- present-value count.
buildOptionalListFile
  :: ParquetType
  -> NestedLeaf
  -> Int            -- ^ number of /top-level/ rows (so the file's
                    --   @fmNumRows@ is correct even for all-null /
                    --   all-empty columns).
  -> BS.ByteString
buildOptionalListFile leafType leaf numRows =
  let !defStream = LE.encodeRLEHybrid 2 (nlDefLevels leaf)
      !repStream = LE.encodeRLEHybrid 1 (nlRepLevels leaf)
      !defLen = BS.length defStream
      !repLen = BS.length repStream
      !valBytes = nlValueBytes leaf
      !numEvents = VP.length (nlDefLevels leaf)
      !totalUncomp = repLen + defLen + BS.length valBytes
      !hdr = PP.PageHeader
        { PP.phType = PP.PtDataPageV2 PP.DataPageHeaderV2
            { PP.dph2NumValues    = fromIntegral numEvents
            , PP.dph2NumNulls     = fromIntegral (numEvents - nlValueCount leaf)
            , PP.dph2NumRows      = fromIntegral numRows
            , PP.dph2Encoding     = 0  -- PLAIN
            , PP.dph2DefLevelsLen = fromIntegral defLen
            , PP.dph2RepLevelsLen = fromIntegral repLen
            , PP.dph2IsCompressed = False
            }
        , PP.phUncompressedPageSize = Just (fromIntegral totalUncomp)
        , PP.phCompressedPageSize   = Just (fromIntegral totalUncomp)
        }
      !pageBytes = PW.encodePageHeader hdr
                   <> repStream <> defStream <> valBytes

      -- Schema: row group root + the three list-segment elements.
      !schema = V.fromList
        ( SchemaElement
            { seName          = "schema"
            , seRepetition    = Nothing
            , seType          = Nothing
            , seNumChildren   = Just 1
            , seConvertedType = Nothing
            , seLogicalType   = Nothing
            , seFieldId       = Nothing
            }
        : optionalListSchemaSegments
            (V.unsafeIndex (nlPath leaf) 0)
            leafType Nothing Nothing
        )

      !colChunk = ColumnChunk
        { ccFilePath          = Nothing
        , ccFileOffset        = 4
        , ccMetadata          = Just ColumnMetadata
            { cmType                  = leafType
            , cmEncodings             = V.singleton Plain
            , cmPathInSchema          = nlPath leaf
            , cmCodec                 = Uncompressed
            , cmNumValues             = fromIntegral numEvents
            , cmTotalUncompressedSize = fromIntegral (BS.length pageBytes)
            , cmTotalCompressedSize   = fromIntegral (BS.length pageBytes)
            , cmDataPageOffset        = 4
            , cmStatistics            = Just Statistics
                { statMin = Nothing, statMax = Nothing
                , statNullCount = Just (fromIntegral (numEvents - nlValueCount leaf))
                , statDistinctCount = Nothing
                , statMinValue = Nothing, statMaxValue = Nothing
                }
            , cmBloomFilterOffset     = Nothing
            , cmBloomFilterLength     = Nothing
            }
        , ccOffsetIndexOffset = Nothing
        , ccOffsetIndexLength = Nothing
        , ccColumnIndexOffset = Nothing
        , ccColumnIndexLength = Nothing
        }
      !rg = RowGroup
        { rgColumns       = V.singleton colChunk
        , rgTotalByteSize = fromIntegral (BS.length pageBytes)
        , rgNumRows       = fromIntegral numRows
        }
      !fm = FileMetadata
        { fmVersion   = 2
        , fmSchema    = schema
        , fmNumRows   = fromIntegral numRows
        , fmRowGroups = V.singleton rg
        , fmCreatedBy = Just "wireform"
        }
   in PW.writeParquetFile fm (V.singleton (V.singleton pageBytes))

-- silence unused 'Word8' import
_unusedW8 :: Word8
_unusedW8 = 0
