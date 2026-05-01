{-# LANGUAGE OverloadedStrings #-}
-- | Schema evolution compatibility checks.
--
-- Iceberg defines a small algebra of valid schema evolutions: adding a
-- nullable field, dropping a nullable field, renaming a field (keeping its
-- id), reordering fields, and a closed set of primitive type promotions
-- (e.g. @int -> long@, @float -> double@, @decimal(P,S) -> decimal(P',S)@
-- when @P' > P@).
--
-- This module checks whether evolving from one 'Schema' to another would
-- be accepted by the Iceberg compatibility rules. It is also useful for
-- catching drift between the table-level schema and Parquet writer
-- schemas before commit.
module Iceberg.SchemaCompat
  ( EvolutionResult(..)
  , validateEvolution
  , isPromotionAllowed
  ) where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Iceberg.Types

-- | Result of an evolution check.
data EvolutionResult
  = EvolutionOk
  | EvolutionErrors ![Text]
  deriving (Show, Eq)

-- | Validate that @new@ is a permissible Iceberg evolution of @old@.
validateEvolution :: Schema -> Schema -> EvolutionResult
validateEvolution old new =
  let oldMap = flatFieldMap old
      newMap = flatFieldMap new
      addErrors = checkAdditions oldMap newMap
      delErrors = checkDeletions oldMap newMap
      changeErrors = checkChanges oldMap newMap
      errs = addErrors ++ delErrors ++ changeErrors
   in if null errs then EvolutionOk else EvolutionErrors errs

-- | Whether a primitive type promotion is allowed by the Iceberg spec.
isPromotionAllowed :: IcebergType -> IcebergType -> Bool
isPromotionAllowed a b | a == b = True
isPromotionAllowed TInt TLong              = True
isPromotionAllowed TFloat TDouble          = True
isPromotionAllowed TDate TTimestamp        = True
isPromotionAllowed TDate TTimestampNs      = True
isPromotionAllowed (TDecimal pa s) (TDecimal pb s') | s == s' && pb >= pa = True
-- Unknown can promote to any type per the V3 spec.
isPromotionAllowed TUnknown _              = True
isPromotionAllowed _ _                      = False

-- ============================================================
-- Internal helpers
-- ============================================================

-- | Flatten a schema to (field-id -> (full-path, field)) for both top-level
-- and nested fields. Path components are dot-joined for diagnostics.
flatFieldMap :: Schema -> Map Int (Text, StructField)
flatFieldMap s = goFields T.empty Map.empty (schemaFields s)
  where
    goFields prefix acc fs = V.foldl' (goField prefix) acc fs
    goField prefix acc sf =
      let path = if T.null prefix then sfName sf else prefix <> "." <> sfName sf
          acc' = Map.insert (sfId sf) (path, sf) acc
       in case sfType sf of
            TStruct nested -> goFields path acc' nested
            _ -> acc'

checkAdditions :: Map Int (Text, StructField) -> Map Int (Text, StructField) -> [Text]
checkAdditions old new =
  [ "required field added without default: " <> path
  | (fid, (path, sf)) <- Map.toList new
  , Map.notMember fid old
  , sfRequired sf
  , sfInitialDefault sf == Nothing
  ]

checkDeletions :: Map Int (Text, StructField) -> Map Int (Text, StructField) -> [Text]
checkDeletions old new =
  [ "required field deleted: " <> path
  | (fid, (path, sf)) <- Map.toList old
  , Map.notMember fid new
  , sfRequired sf
  ]

checkChanges :: Map Int (Text, StructField) -> Map Int (Text, StructField) -> [Text]
checkChanges old new = foldl' check [] (Map.toList old)
  where
    check acc (fid, (oldPath, oldSf)) = case Map.lookup fid new of
      Nothing -> acc
      Just (_, newSf) ->
        let promoErr
              | not (isPromotionAllowed (sfType oldSf) (sfType newSf)) =
                  ["disallowed type change at " <> oldPath
                   <> ": " <> T.pack (show (sfType oldSf))
                   <> " -> " <> T.pack (show (sfType newSf))]
              | otherwise = []
            requiredErr
              | not (sfRequired oldSf) && sfRequired newSf =
                  ["nullable field made required: " <> oldPath]
              | otherwise = []
         in acc ++ promoErr ++ requiredErr
