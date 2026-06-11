{-# LANGUAGE OverloadedStrings #-}

{- | Constraint validation for Iceberg metadata.

The Iceberg spec carries version-specific invariants:

- V1 tables omit @last-sequence-number@ and snapshot @sequence-number@.
- V2+ tables require those fields and require manifests to record
  @content@, @sequence_number@, @min_sequence_number@.
- V3 tables additionally need @next-row-id@ when row lineage is in use.
- Identifier-field-ids must point at primitive, required, top-level (or
  non-optional struct nested) fields, never at floats, doubles, or fields
  inside maps/lists.

'validateMetadata' returns either a list of human-readable violations or
the input metadata. 'validateIdentifierFields' is exposed separately for
use during schema evolution.
-}
module Iceberg.Validate (
  ValidationResult (..),
  validateMetadata,
  validateIdentifierFields,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Iceberg.Types


data ValidationResult
  = ValidationOk
  | ValidationErrors ![Text]
  deriving (Show, Eq)


{- | Check structural and version invariants of a 'TableMetadata'. The
returned list aggregates all violations rather than failing on the first.
-}
validateMetadata :: TableMetadata -> ValidationResult
validateMetadata tm =
  let errs =
        concat
          [ checkFormatVersion tm
          , checkV2Invariants tm
          , checkV3Invariants tm
          , checkSchemaList tm
          , checkPartitionSpecs tm
          , checkSnapshotRefs tm
          , checkIdentifierFields tm
          ]
  in if null errs then ValidationOk else ValidationErrors errs


checkFormatVersion :: TableMetadata -> [Text]
checkFormatVersion tm
  | tmFormatVersion tm < 1 = ["format-version must be >= 1"]
  | tmFormatVersion tm > 3 =
      [ "unsupported format-version: "
          <> T.pack (show (tmFormatVersion tm))
      ]
  | otherwise = []


checkV2Invariants :: TableMetadata -> [Text]
checkV2Invariants tm
  | tmFormatVersion tm < 2 = []
  | otherwise =
      concat
        [ [ "snapshot "
              <> T.pack (show (snapId s))
              <> " missing sequence-number for v2"
          | s <- V.toList (tmSnapshots tm)
          , snapSequenceNumber s == 0 && snapParentId s /= Nothing
          ]
        , [ "snapshot "
              <> T.pack (show (snapId s))
              <> " has sequence-number greater than table last-sequence-number"
          | s <- V.toList (tmSnapshots tm)
          , snapSequenceNumber s > tmLastSequenceNumber tm
          ]
        ]


checkV3Invariants :: TableMetadata -> [Text]
checkV3Invariants tm
  | tmFormatVersion tm < 3 = []
  | otherwise =
      concat
        [ [ "format-version=3 table missing next-row-id"
          | tmNextRowId tm == Nothing
          , not (V.null (tmSnapshots tm))
          ]
        ]


checkSchemaList :: TableMetadata -> [Text]
checkSchemaList tm =
  concat
    [ [ "current-schema-id "
          <> T.pack (show (tmCurrentSchemaId tm))
          <> " not found in schemas"
      | not
          ( any
              (\s -> schemaId s == tmCurrentSchemaId tm)
              (V.toList (tmSchemas tm))
          )
      ]
    , [ "duplicate schema id: " <> T.pack (show sid)
      | sid <- duplicates (map schemaId (V.toList (tmSchemas tm)))
      ]
    ]


checkPartitionSpecs :: TableMetadata -> [Text]
checkPartitionSpecs tm =
  concat
    [ [ "default-spec-id "
          <> T.pack (show (tmDefaultSpecId tm))
          <> " not found in partition-specs"
      | not
          ( any
              (\s -> psSpecId s == tmDefaultSpecId tm)
              (V.toList (tmPartitionSpecs tm))
          )
      ]
    , [ "duplicate partition-spec id: " <> T.pack (show sid)
      | sid <- duplicates (map psSpecId (V.toList (tmPartitionSpecs tm)))
      ]
    ]


checkSnapshotRefs :: TableMetadata -> [Text]
checkSnapshotRefs tm =
  concat
    [ [ "snapshot ref \""
          <> name
          <> "\" points at unknown snapshot "
          <> T.pack (show (srSnapshotId r))
      | (name, r) <- Map.toList (tmSnapshotRefs tm)
      , not (any (\s -> snapId s == srSnapshotId r) (V.toList (tmSnapshots tm)))
      ]
    , [ "snapshot ref \"" <> name <> "\" has unknown type \"" <> srType r <> "\""
      | (name, r) <- Map.toList (tmSnapshotRefs tm)
      , srType r /= "branch" && srType r /= "tag"
      ]
    ]


checkIdentifierFields :: TableMetadata -> [Text]
checkIdentifierFields tm =
  concat
    [validateIdentifierFields s | s <- V.toList (tmSchemas tm)]


{- | Verify identifier-field-ids satisfy the Iceberg constraints: each id
must reference a top-level primitive, required, non-floating field.
-}
validateIdentifierFields :: Schema -> [Text]
validateIdentifierFields schema =
  let topLevel = V.toList (schemaFields schema)
      byId = Map.fromList [(sfId sf, sf) | sf <- topLevel]
      checks =
        [ violation
        | fid <- V.toList (schemaIdentifierFieldIds schema)
        , Just violation <- [check fid byId]
        ]
  in checks
  where
    check fid byId = case Map.lookup fid byId of
      Nothing ->
        Just $
          "identifier-field-ids: no such top-level field id "
            <> T.pack (show fid)
      Just sf
        | not (sfRequired sf) ->
            Just $
              "identifier-field-id "
                <> T.pack (show fid)
                <> " is on optional field \""
                <> sfName sf
                <> "\""
        | sfType sf == TFloat || sfType sf == TDouble ->
            Just $
              "identifier-field-id "
                <> T.pack (show fid)
                <> " uses float/double, which is not allowed"
        | not (isPrimitive (sfType sf)) ->
            Just $
              "identifier-field-id "
                <> T.pack (show fid)
                <> " is on non-primitive field"
        | otherwise -> Nothing


isPrimitive :: IcebergType -> Bool
isPrimitive ty = case ty of
  TStruct {} -> False
  TList {} -> False
  TMap {} -> False
  TVariant -> False
  TGeometry {} -> False
  TGeography {} -> False
  _ -> True


duplicates :: Ord a => [a] -> [a]
duplicates xs = [k | (k, n) <- Map.toList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]), n > 1]
