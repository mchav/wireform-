-- | Convenience re-exports for Apache Parquet.
--
-- @
-- import qualified Wireform.Parquet as Parquet
-- @
--
-- == Quick start
--
-- The high-level API in "Parquet.HighLevel" consolidates the
-- Parquet writer's many flavours into a single record of options:
--
-- @
-- let bytes = Parquet.'encodeParquet' Parquet.'defaultWriteOptions'
--                                    schema rowGroups
-- @
--
-- @rowGroups@ is a @[V.Vector ColumnData]@; one entry per row
-- group, each entry one 'ColumnData' per leaf schema column.
-- 'WriteOptions' carries compression, page version, page-index
-- emission, per-column encryption, and footer encryption (PARE
-- mode) — defaults are modern-Parquet-recommended.
--
-- Reading is currently lazy / type-dispatched (see
-- 'Parquet.HighLevel.decodeParquet' returning 'ParquetFile'); use
-- the specialised readers in "Parquet.Read" to project individual
-- columns.
module Wireform.Parquet
  ( -- * High-level API (most callers want this)
    module Parquet.HighLevel
    -- * Schema + footer types
  , module Parquet.Types
  , module Parquet.Footer
  , module Parquet.Page
  , module Parquet.PageIndex
    -- * Lower-level reader / writer / level / bloom modules
  , module Parquet.Read
  , module Parquet.Levels
  , module Parquet.Write
  , module Parquet.BloomFilter
  ) where

import Parquet.BloomFilter
import Parquet.Footer
import Parquet.HighLevel
import Parquet.Levels
import Parquet.Page
import Parquet.PageIndex
import Parquet.Read
import Parquet.Types
import Parquet.Write
