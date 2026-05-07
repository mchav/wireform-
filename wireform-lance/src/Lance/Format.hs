{-# LANGUAGE OverloadedStrings #-}
-- | Apache Lance columnar format reader (skeleton).
--
-- Lance is a single-file columnar format from LanceDB designed
-- around fast random row access for vector-search workloads.
-- Each Lance file contains:
--
-- @
-- <magic \"LANC\">
-- <data pages>
-- <column metadata>
-- <file metadata (FlatBuffers)>
-- <footer offset (i64)> <magic \"LANC\">
-- @
--
-- The metadata format is FlatBuffers with the same general
-- shape as Arrow's @Schema@ — columns, page descriptors,
-- statistics, encryption.
--
-- This module is a /skeleton/ exposing the magic constants
-- and a stubbed reader entry point. Filling in the FlatBuffers
-- decoder would mirror what 'Arrow.FlatBufferIPC' does for
-- Arrow IPC; the per-page decoders would parallel the
-- 'Parquet.Read' family.
module Lance.Format
  ( lanceMagic
  , LanceFile (..)
  , readLanceFile
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | The 4-byte magic at the start and end of every Lance file.
lanceMagic :: ByteString
lanceMagic = BS.pack [0x4C, 0x41, 0x4E, 0x43]  -- "LANC"

-- | Placeholder for a parsed Lance file. Real implementation
-- would carry a schema (FlatBuffers), per-column page index,
-- and the raw bytes (or an mmapped handle).
data LanceFile = LanceFile
  { lfBytes :: !ByteString
  } deriving (Show, Eq)

-- | Validate the magic envelope of a Lance file. Returns the
-- parsed handle on success; the FlatBuffers metadata decoder
-- is a follow-up.
readLanceFile :: ByteString -> Either String LanceFile
readLanceFile bs
  | BS.length bs < 12 = Left "Lance.Format: file too short"
  | BS.take 4 bs /= lanceMagic =
      Left "Lance.Format: missing leading LANC magic"
  | BS.takeEnd 4 bs /= lanceMagic =
      Left "Lance.Format: missing trailing LANC magic"
  | otherwise = Right LanceFile { lfBytes = bs }
