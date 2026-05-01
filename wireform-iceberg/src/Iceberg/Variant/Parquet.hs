{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | High-level bridge between "Iceberg.Variant" and the Parquet
-- nested writer in "Parquet.Nested".
--
-- A Variant column in Parquet is a 2-leaf binary group:
--
-- @
-- optional group v (VARIANT(1)) {
--   required binary metadata;
--   required binary value;
-- }
-- @
--
-- This module turns a row-major @V.Vector (Maybe Variant)@ into the
-- @{metadata, value}@ byte pairs the Parquet writer needs and gives
-- you a complete file plus convenience entry points.
module Iceberg.Variant.Parquet
  ( -- * Single-column entry point
    buildVariantParquetFile
  , variantToNestedRow
    -- * Multi-column / mixed-shape entry point
  , VariantColumn (..)
  , buildVariantParquetFileMulti
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Vector as V

import qualified Iceberg.Variant as IV
import qualified Parquet.Nested as PN

-- | Shred one logical 'Iceberg.Variant.Variant' (or @Nothing@) into
-- the row a 'PN.NSVariant' / @optional NSVariant@ schema expects.
-- The encoding goes through 'Iceberg.Variant.encodeVariant' so the
-- underlying byte-strings are spec-compliant.
variantToNestedRow :: Maybe IV.Variant -> PN.NestedRow
variantToNestedRow Nothing  = PN.NRNull
variantToNestedRow (Just v) =
  let (m, x) = IV.encodeVariant v
   in PN.NRVariantBytes m x

-- | Build a Parquet file with exactly one top-level optional Variant
-- column.
--
-- @
-- buildVariantParquetFile \"payload\" rows
-- @
--
-- emits a file with schema
--
-- @
-- optional group payload (VARIANT(1)) {
--   required binary metadata;
--   required binary value;
-- }
-- @
--
-- and one row per element of @rows@. 'Nothing' rows produce a null
-- Variant via the outer optional layer; @Just v@ rows are encoded
-- via 'IV.encodeVariant'.
buildVariantParquetFile
  :: Text                       -- ^ column name
  -> V.Vector (Maybe IV.Variant)
  -> Either String ByteString
buildVariantParquetFile colName values =
  let schema = PN.NSOptional PN.NSVariant
      rows   = V.map variantToNestedRow values
   in PN.buildNestedFile (V.singleton (colName, schema))
        (V.singleton rows)

-- | One column of 'buildVariantParquetFileMulti'.
data VariantColumn = VariantColumn
  { vcName   :: !Text
  , vcValues :: !(V.Vector (Maybe IV.Variant))
  } deriving (Show, Eq)

-- | Build a Parquet file with multiple top-level optional Variant
-- columns, all sharing the same row count.
--
-- Convenient when a Spark / Iceberg dataset has several semi-
-- structured columns side-by-side (e.g. @event_payload@ +
-- @user_attributes@).
buildVariantParquetFileMulti
  :: V.Vector VariantColumn
  -> Either String ByteString
buildVariantParquetFileMulti columns
  | V.null columns =
      Left "Iceberg.Variant.Parquet: at least one column required"
  | otherwise =
      let !schemas = V.map
            (\vc -> (vcName vc, PN.NSOptional PN.NSVariant)) columns
          !rowsPerColumn = V.map
            (\vc -> V.map variantToNestedRow (vcValues vc)) columns
       in PN.buildNestedFile schemas rowsPerColumn

-- silence -Widentities for unused 'BS.empty' import; we re-export the
-- module-level alias for completeness.
_unusedBs :: ByteString
_unusedBs = BS.empty
