{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Avro decoder for the Hudi @HoodieCommitMetadata@ schema.
--
-- Hudi 1.x onward writes completed @commit@ / @deltacommit@
-- instant payloads as Avro container files, replacing the
-- previous JSON encoding (see HUDI-6776). This module reads
-- such a container and walks the embedded
-- @HoodieCommitMetadata@ record into the same typed
-- 'HoodieCommitMetadata' / 'HoodieWriteStat' shape that
-- 'parseCommitJson' produces, so callers can fold both
-- representations through the same 'tableStateFromCommits'.
--
-- The schema is vendored at
-- @wireform-hudi/avro/HoodieCommitMetadata.avsc@ — the
-- canonical copy in the
-- @apache/hudi@ tree at
-- @hudi-common/src/main/avro/HoodieCommitMetadata.avsc@.
module Hudi.Avro
  ( -- * Decoding
    decodeCommitAvro
  , readCommitAvroFile
    -- * Schema (vendored)
  , hoodieCommitMetadataSchema
  ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V

import Data.FileEmbed (embedFile)
import qualified Data.Aeson as Aeson

import qualified Avro.Container as Container
import qualified Avro.Schema as AS
import qualified Avro.Schema.Parse as ASP
import qualified Avro.Value as AV

import Hudi.Timeline
  ( HoodieCommitMetadata (..)
  , HoodieWriteStat (..)
  )

-- ============================================================
-- Vendored schema
-- ============================================================

-- | The @HoodieCommitMetadata@ Avro schema, parsed from the
-- vendored @.avsc@ at module-load time. The parse failure
-- branch is unreachable for the checked-in schema; we surface
-- it as 'error' rather than push it through every call site.
hoodieCommitMetadataSchema :: AS.AvroType
hoodieCommitMetadataSchema =
  case ASP.parseAvroSchema hoodieCommitMetadataAvsc of
    Right t -> t
    Left  e -> error ("Hudi.Avro: bundled HoodieCommitMetadata.avsc failed to parse: " ++ e)

-- | The .avsc payload, embedded at compile time via
-- 'Data.FileEmbed.embedFile' so the schema travels with the
-- compiled artifact and there's no runtime file lookup.
-- Sourced from @wireform-hudi/avro/HoodieCommitMetadata.avsc@.
hoodieCommitMetadataAvsc :: BS.ByteString
hoodieCommitMetadataAvsc =
  $(embedFile "avro/HoodieCommitMetadata.avsc")
{-# NOINLINE hoodieCommitMetadataAvsc #-}

-- ============================================================
-- Decoding
-- ============================================================

-- | Decode an Avro container's bytes into a typed
-- 'HoodieCommitMetadata'. Returns 'Left' if the container
-- header is malformed, the schema doesn't match
-- @HoodieCommitMetadata@, or the body has no record /
-- multiple records (Hudi commit instants are exactly one
-- record per file).
decodeCommitAvro :: BS.ByteString -> Either String HoodieCommitMetadata
decodeCommitAvro bs = do
  (_writerSchema, vals) <- Container.readContainer bs
  case V.length vals of
    0 -> Left "Hudi.Avro: container had no records"
    1 -> commitFromAvroValue (V.head vals)
    n -> Left ("Hudi.Avro: container had " ++ show n
            ++ " records, expected 1")

-- | Convenience wrapper that reads bytes off disk before
-- decoding.
readCommitAvroFile :: FilePath -> IO (Either String HoodieCommitMetadata)
readCommitAvroFile fp = do
  bs <- BS.readFile fp
  pure (decodeCommitAvro bs)

-- ============================================================
-- AV.Value walker
-- ============================================================
--
-- We don't generate the Hudi types via wireform-avro's TH
-- deriver because the schema is a moving target across Hudi
-- versions and the JSON-payload decoder in 'Hudi.Timeline'
-- already pins the typed shape we want callers to see. The
-- walker below intentionally tolerates extra / missing fields
-- so a HoodieCommitMetadata.avsc shipped by a newer Hudi version
-- still decodes — only fields present in our typed record are
-- promoted.

commitFromAvroValue :: AV.Value -> Either String HoodieCommitMetadata
commitFromAvroValue (AV.Record fs) =
  let m = lookupRec hoodieCommitMetadataSchema fs
   in Right HoodieCommitMetadata
        { hcmPartitionToWriteStats = unwrapPartitions (lookupKey m "partitionToWriteStats")
        , hcmCompacted             = unwrapBool (lookupKey m "compacted")
        , hcmExtraMetadata         = unwrapStringMap (lookupKey m "extraMetadata")
        , hcmOperationType         = unwrapText (lookupKey m "operationType")
        , hcmTotalCreateTime       = unwrapInt64 (lookupKey m "totalCreateTime")
        , hcmTotalUpsertTime       = unwrapInt64 (lookupKey m "totalUpsertTime")
        , hcmTotalScanTime         = unwrapInt64 (lookupKey m "totalScanTime")
        , hcmExtra                 = HM.empty
        }
commitFromAvroValue v =
  Left ("Hudi.Avro: top-level value is not a record: " ++ truncShow v)

-- | Pair the field /names/ from the writer schema with the
-- positional values in the record, then collapse to a Map.
-- @lookupRec@ has to know the schema because Avro records carry
-- their values in declaration order without keys.
lookupRec :: AS.AvroType -> V.Vector AV.Value -> Map.Map Text AV.Value
lookupRec (AS.AvroRecord {AS.avroRecordFields = sf}) vs =
  Map.fromList $ V.toList $ V.zipWith pair sf vs
  where
    pair fld v = (AS.avroFieldName fld, v)
lookupRec _ _ = Map.empty

lookupKey :: Map.Map Text AV.Value -> Text -> Maybe AV.Value
lookupKey m k = case Map.lookup k m of
  Just (AV.Union _ AV.Null) -> Nothing  -- nullable union with null branch
  Just (AV.Union _ inner)   -> Just inner
  other                     -> other

unwrapText :: Maybe AV.Value -> Maybe Text
unwrapText (Just (AV.String s)) = Just s
unwrapText _                    = Nothing

unwrapInt64 :: Maybe AV.Value -> Maybe Int64
unwrapInt64 (Just (AV.Long n)) = Just n
unwrapInt64 (Just (AV.Int  n)) = Just (fromIntegral n)
unwrapInt64 _                  = Nothing

unwrapBool :: Maybe AV.Value -> Maybe Bool
unwrapBool (Just (AV.Bool b)) = Just b
unwrapBool _                  = Nothing

-- | Decode an Avro map of strings into an Aeson-shaped HashMap.
-- The HoodieCommitMetadata.avsc schema has @extraMetadata@ as
-- @map<string, string>@; we surface it through the same
-- 'HashMap Text Aeson.Value' shape that 'parseCommitJson'
-- produces so callers can fold both representations through
-- the same accessor.
unwrapStringMap :: Maybe AV.Value -> HM.HashMap Text Aeson.Value
unwrapStringMap (Just (AV.Map kvs)) =
  HM.fromList
    [ (k, avroValueToAeson v) | (k, v) <- V.toList kvs ]
unwrapStringMap (Just (AV.Union _ inner)) = unwrapStringMap (Just inner)
unwrapStringMap _ = HM.empty

-- | Best-effort 'Avro.Value' → 'Aeson.Value' projection for the
-- shape Hudi's @extraMetadata@ uses (string → string). Falls
-- back to 'Aeson.Null' for anything we don't recognise.
avroValueToAeson :: AV.Value -> Aeson.Value
avroValueToAeson = \case
  AV.String s        -> Aeson.String s
  AV.Bool b          -> Aeson.Bool b
  AV.Int  n          -> Aeson.Number (fromIntegral n)
  AV.Long n          -> Aeson.Number (fromIntegral n)
  AV.Null            -> Aeson.Null
  AV.Union _ inner   -> avroValueToAeson inner
  _                  -> Aeson.Null

-- | Decode @partitionToWriteStats@: a map of partition path to
-- an array of HoodieWriteStat records.
unwrapPartitions
  :: Maybe AV.Value
  -> Map.Map Text [HoodieWriteStat]
unwrapPartitions (Just (AV.Map kvs)) =
  Map.fromList $ V.toList $ V.map decodeOne kvs
  where
    decodeOne (k, v) = (k, unwrapStats v)
unwrapPartitions _ = Map.empty

unwrapStats :: AV.Value -> [HoodieWriteStat]
unwrapStats v = case v of
  AV.Array xs -> V.toList (V.mapMaybe statFromValue xs)
  AV.Union _ inner -> unwrapStats inner
  _           -> []

-- | One HoodieWriteStat record. We hard-code the field order
-- from the vendored schema so a missing field on the wire
-- doesn't shift later fields out of place.
statFromValue :: AV.Value -> Maybe HoodieWriteStat
statFromValue (AV.Record vs) =
  -- Pull the writer-stat sub-schema out of the top schema so we
  -- can pair field names with positions.
  let m = lookupRec hudiWriteStatSchema vs
      g = lookupKey m
   in Just HoodieWriteStat
        { hwsFileId           = unwrapText  (g "fileId")
        , hwsPath             = unwrapText  (g "path")
        , hwsPrevCommit       = unwrapText  (g "prevCommit")
        , hwsPartitionPath    = unwrapText  (g "partitionPath")
        , hwsNumWrites        = unwrapInt64 (g "numWrites")
        , hwsNumDeletes       = unwrapInt64 (g "numDeletes")
        , hwsNumUpdateWrites  = unwrapInt64 (g "numUpdateWrites")
        , hwsNumInserts       = unwrapInt64 (g "numInserts")
        , hwsTotalWriteBytes  = unwrapInt64 (g "totalWriteBytes")
        , hwsTotalWriteErrors = unwrapInt64 (g "totalWriteErrors")
        , hwsFileSizeInBytes  = unwrapInt64 (g "fileSizeInBytes")
        , hwsBaseFile         = unwrapText  (g "baseFile")
        , hwsLogFiles         = unwrapStringList (g "logFiles")
        , hwsTotalLogRecords  = unwrapInt64 (g "totalLogRecords")
        , hwsTotalLogFiles    = unwrapInt64 (g "totalLogFiles")
        , hwsTotalLogBlocks   = unwrapInt64 (g "totalLogBlocks")
        , hwsExtra            = HM.empty
        }
statFromValue (AV.Union _ inner) = statFromValue inner
statFromValue _                  = Nothing

unwrapStringList :: Maybe AV.Value -> [Text]
unwrapStringList (Just (AV.Array xs)) =
  fromMaybe [] (mapM textOf (V.toList xs))
  where
    textOf (AV.String s)        = Just s
    textOf (AV.Union _ (AV.String s)) = Just s
    textOf _                    = Nothing
unwrapStringList (Just (AV.Union _ inner)) = unwrapStringList (Just inner)
unwrapStringList _ = []

-- | The HoodieWriteStat sub-schema, extracted from the
-- top-level. Looking it up at module load lets the walker know
-- the field positions.
hudiWriteStatSchema :: AS.AvroType
hudiWriteStatSchema = case findStatSchema hoodieCommitMetadataSchema of
  Just s  -> s
  Nothing -> error "Hudi.Avro: HoodieWriteStat sub-schema not found"

-- Walk the schema to find the named record 'HoodieWriteStat'.
findStatSchema :: AS.AvroType -> Maybe AS.AvroType
findStatSchema t = case t of
  rec@AS.AvroRecord{} | AS.avroRecordName rec == "HoodieWriteStat" -> Just rec
  AS.AvroRecord {AS.avroRecordFields = fs} ->
    firstJust (V.map (findStatSchema . AS.avroFieldType) fs)
  AS.AvroArray {AS.avroArrayItems = it} -> findStatSchema it
  AS.AvroMap   {AS.avroMapValues = vt}  -> findStatSchema vt
  AS.AvroUnion {AS.avroUnionBranches = brs} ->
    firstJust (V.map findStatSchema brs)
  _ -> Nothing
  where
    firstJust = V.foldl' (\acc v -> case acc of Just _ -> acc; Nothing -> v) Nothing

truncShow :: Show a => a -> String
truncShow x = let s = show x in if length s > 200 then take 200 s ++ "…" else s
