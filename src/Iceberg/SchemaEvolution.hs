-- | Schema evolution operations for Iceberg tables.
--
-- Provides lookup by schema ID, recursive field search, and column
-- projection (keeping a subset of top-level fields by their IDs).
module Iceberg.SchemaEvolution
  ( schemaById
  , currentSchema
  , findFieldById
  , projectSchema
  , pruneByFieldIds
  ) where

import qualified Data.IntSet as IntSet
import qualified Data.Vector as V

import Iceberg.Types

-- | Look up a schema by its ID in the schemas list.
schemaById :: TableMetadata -> Int -> Maybe Schema
schemaById tm sid = V.find (\s -> schemaId s == sid) (tmSchemas tm)

-- | The schema matching 'tmCurrentSchemaId'.
currentSchema :: TableMetadata -> Maybe Schema
currentSchema tm = schemaById tm (tmCurrentSchemaId tm)

-- | Find a field by its field ID, searching recursively into nested
-- structs, list element types, and map key/value types.
findFieldById :: Schema -> Int -> Maybe StructField
findFieldById schema fid = searchFields (schemaFields schema)
  where
    searchFields :: V.Vector StructField -> Maybe StructField
    searchFields fields =
      case V.find (\sf -> sfId sf == fid) fields of
        Just sf -> Just sf
        Nothing -> searchNested 0 fields

    searchNested :: Int -> V.Vector StructField -> Maybe StructField
    searchNested !i fields
      | i >= V.length fields = Nothing
      | otherwise =
          case searchType (sfType (V.unsafeIndex fields i)) of
            Just sf -> Just sf
            Nothing -> searchNested (i + 1) fields

    searchType :: IcebergType -> Maybe StructField
    searchType (TStruct inner) = searchFields inner
    searchType (TList _ elemTy) = searchType elemTy
    searchType (TMap _ keyTy _ valTy) =
      case searchType keyTy of
        Just sf -> Just sf
        Nothing -> searchType valTy
    searchType _ = Nothing

-- | Keep only top-level fields whose IDs appear in the given list.
-- Returns 'Left' if no requested field IDs match any top-level field.
projectSchema :: Schema -> [Int] -> Either String Schema
projectSchema schema fieldIds =
  let idSet = IntSet.fromList fieldIds
      projected = V.filter (\sf -> IntSet.member (sfId sf) idSet) (schemaFields schema)
  in if V.null projected && not (null fieldIds)
     then Left "projectSchema: none of the requested field IDs found in schema"
     else Right schema { schemaFields = projected }

-- | Filter manifest entries to only those that might contain data for
-- the given field IDs. Currently a no-op because 'ManifestEntry' does
-- not decode column-level statistics (column_sizes, value_counts,
-- null_value_counts, lower_bounds, upper_bounds). When those fields are
-- added to the manifest decoder, this function can use them for pruning.
pruneByFieldIds :: V.Vector ManifestEntry -> Schema -> [Int] -> V.Vector ManifestEntry
pruneByFieldIds entries _schema _fieldIds = entries
