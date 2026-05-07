{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Delta Lake transaction log reader (skeleton).
--
-- Delta Lake is a Parquet-on-top-of-transaction-log table
-- format. Each table directory has a @_delta_log/@ subdirectory
-- containing newline-delimited JSON files
-- (@00000000000000000000.json@, @00000000000000000001.json@,
-- …) and periodic @*.checkpoint.parquet@ snapshots. Each line
-- in a JSON log file is one of a small set of @Action@s:
--
-- @
-- {"add":      {"path": ..., "size": ..., "stats": ..., ... }}
-- {"remove":   {"path": ..., "deletionTimestamp": ..., ... }}
-- {"metaData": {"id": ..., "schemaString": ..., ... }}
-- {"protocol": {"minReaderVersion": ..., "minWriterVersion": ... }}
-- {"commitInfo": {"timestamp": ..., "operation": ..., ... }}
-- {"txn":      {"appId": ..., "version": ..., ... }}
-- {"cdc":      {"path": ..., ...}}
-- @
--
-- This module is a /skeleton/: it parses the JSON
-- representation into typed 'DeltaAction' values and exposes
-- the @add@ + @remove@ derivation that gives the active file
-- set at a given table version. Time-travel, schema evolution,
-- column mapping, deletion vectors, and CDC are out of scope
-- for the initial pass.
module Delta.Log
  ( -- * Actions
    DeltaAction (..)
  , AddAction (..)
  , RemoveAction (..)
  , MetaDataAction (..)
  , ProtocolAction (..)
    -- * Log parsing
  , parseLogFile
  , parseLogLine
    -- * Active file set
  , activeFiles
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , (.:), (.:?)
  , withObject
  , decode
  , fromJSON
  , Result (..)
  )
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | One line of a Delta log file. The JSON shape is a
-- single-key object whose key picks the variant.
data DeltaAction
  = ActionAdd      !AddAction
  | ActionRemove   !RemoveAction
  | ActionMetaData !MetaDataAction
  | ActionProtocol !ProtocolAction
  | ActionOther    !Text  -- ^ commitInfo / txn / cdc / unknown — the variant tag
  deriving (Show, Eq)

-- | The @add@ action: a Parquet (or other format) data file is
-- now part of the table.
data AddAction = AddAction
  { addPath           :: !Text
  , addSize           :: !Word64
  , addModificationTime :: !Word64
  , addDataChange     :: !Bool
  , addStats          :: !(Maybe Text)
    -- ^ JSON-encoded statistics (numRecords, minValues, maxValues, nullCount).
  , addPartitionValues :: !(Map.Map Text (Maybe Text))
  } deriving (Show, Eq)

-- | The @remove@ action: a previously-added file is no longer
-- in the table. Combined with subsequent @add@s, this is how
-- Delta encodes overwrites.
data RemoveAction = RemoveAction
  { removePath              :: !Text
  , removeDeletionTimestamp :: !(Maybe Word64)
  , removeDataChange        :: !Bool
  } deriving (Show, Eq)

-- | The @metaData@ action: table-level schema + properties.
-- Carried in the very first commit of a table; subsequent
-- @metaData@s are schema-evolution boundaries.
data MetaDataAction = MetaDataAction
  { mdId               :: !Text
  , mdName             :: !(Maybe Text)
  , mdSchemaString     :: !Text  -- ^ JSON-encoded Spark schema.
  , mdPartitionColumns :: ![Text]
  } deriving (Show, Eq)

-- | The @protocol@ action: minimum reader / writer versions
-- the table demands. Readers below 'pMinReaderVersion' must
-- refuse to open the table.
data ProtocolAction = ProtocolAction
  { pMinReaderVersion :: !Int
  , pMinWriterVersion :: !Int
  } deriving (Show, Eq)

-- ============================================================
-- Aeson instances
-- ============================================================

instance FromJSON AddAction where
  parseJSON = withObject "AddAction" $ \o -> AddAction
    <$> o .:  "path"
    <*> o .:  "size"
    <*> o .:  "modificationTime"
    <*> o .:  "dataChange"
    <*> o .:? "stats"
    <*> (o .:? "partitionValues" >>= maybe (pure Map.empty) pure)

instance FromJSON RemoveAction where
  parseJSON = withObject "RemoveAction" $ \o -> RemoveAction
    <$> o .:  "path"
    <*> o .:? "deletionTimestamp"
    <*> (maybe True id <$> o .:? "dataChange")

instance FromJSON MetaDataAction where
  parseJSON = withObject "MetaDataAction" $ \o -> MetaDataAction
    <$> o .:  "id"
    <*> o .:? "name"
    <*> o .:  "schemaString"
    <*> (maybe [] id <$> o .:? "partitionColumns")

instance FromJSON ProtocolAction where
  parseJSON = withObject "ProtocolAction" $ \o -> ProtocolAction
    <$> o .:  "minReaderVersion"
    <*> o .:  "minWriterVersion"

-- ============================================================
-- Parser
-- ============================================================

-- | Parse one NDJSON line of a Delta log file.
parseLogLine :: BL.ByteString -> Maybe DeltaAction
parseLogLine line = do
  v <- decode line
  case v of
    Object o ->
      let pluck :: FromJSON a => Text -> Maybe a
          pluck k = do
            inner <- AKM.lookup (AK.fromText k) o
            case fromJSON inner of
              Success a -> Just a
              Error _   -> Nothing
      in case Nothing of
           _ | Just a  <- pluck "add"      -> Just (ActionAdd a)
             | Just r  <- pluck "remove"   -> Just (ActionRemove r)
             | Just md <- pluck "metaData" -> Just (ActionMetaData md)
             | Just p  <- pluck "protocol" -> Just (ActionProtocol p)
             | otherwise -> Just (ActionOther (firstKey o))
    _ -> Nothing
  where
    firstKey o = case AKM.toList o of
      ((k, _) : _) -> AK.toText k
      []           -> T.pack "<empty>"

-- | Parse every NDJSON line of a single log file.
parseLogFile :: BL.ByteString -> [DeltaAction]
parseLogFile bs =
  [ a
  | line <- BL.split 0x0A bs  -- '\n'
  , not (BL.null line)
  , Just a <- [parseLogLine line]
  ]

-- ============================================================
-- Active file derivation
-- ============================================================

-- | Apply every action in order to derive the table's current
-- active file set. @add@s grow the set; @remove@s prune it.
-- Returns the surviving 'AddAction's (which carry path,
-- partition values, stats).
activeFiles :: [DeltaAction] -> [AddAction]
activeFiles = Map.elems . foldl step Map.empty
  where
    step !acc (ActionAdd a)    = Map.insert (addPath a) a acc
    step !acc (ActionRemove r) = Map.delete (removePath r) acc
    step !acc _                = acc
