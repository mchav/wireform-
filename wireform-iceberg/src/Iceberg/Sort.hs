-- | Sort-key evaluation: project a row's typed values through a
-- 'SortOrder' to produce a comparable key plus per-field nulls/asc-desc
-- metadata. Used by writers to bucket rows into sorted files.
module Iceberg.Sort
  ( SortKey(..)
  , buildSortKey
  , compareSortKeys
  ) where

import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Avro.Value as AV
import qualified Iceberg.Transform as Tr
import Iceberg.Types

-- | A single row's sort key: one transformed value per sort field, in
-- 'soFields' order, paired with the field's direction and null-ordering.
newtype SortKey = SortKey
  { unSortKey :: Vector (Maybe AV.Value, SortDirection, NullOrder)
  } deriving (Show, Eq)

-- | Compute a row's 'SortKey' from a 'SortOrder' and a lookup function from
-- source field id to value.
buildSortKey
  :: SortOrder
  -> Schema
  -> (Int -> Maybe AV.Value)
  -> Either Tr.TransformError SortKey
buildSortKey so schema lookupSrc = do
  slots <- V.mapM mkSlot (soFields so)
  Right (SortKey slots)
  where
    mkSlot sf = case lookupSrc (sortSourceId sf) of
      Nothing -> Right (Nothing, sortDirection sf, sortNullOrder sf)
      Just v  -> case sourceTypeOf schema (sortSourceId sf) of
        Just srcTy -> case Tr.applyTransform (sortTransform sf) srcTy v of
          Right out -> Right (Just out, sortDirection sf, sortNullOrder sf)
          Left  e   -> Left e
        Nothing -> Right (Just v, sortDirection sf, sortNullOrder sf)

sourceTypeOf :: Schema -> Int -> Maybe IcebergType
sourceTypeOf schema fid =
  fmap sfType (V.find (\sf -> sfId sf == fid) (schemaFields schema))

-- | Lexicographic comparison of two 'SortKey's, honoring per-field
-- direction and null ordering. The keys must come from the same
-- 'SortOrder' (i.e. have the same shape).
compareSortKeys :: SortKey -> SortKey -> Ordering
compareSortKeys (SortKey a) (SortKey b) = go 0
  where
    !n = min (V.length a) (V.length b)
    go !i
      | i >= n    = compare (V.length a) (V.length b)
      | otherwise =
          let (av, dir, nullOrder) = V.unsafeIndex a i
              (bv, _,   _)         = V.unsafeIndex b i
              ord = compareSlot av bv nullOrder
              ord' = case dir of
                Asc  -> ord
                Desc -> invertOrdering ord
           in case ord' of
                EQ -> go (i + 1)
                _  -> ord'

compareSlot :: Maybe AV.Value -> Maybe AV.Value -> NullOrder -> Ordering
compareSlot Nothing  Nothing  _          = EQ
compareSlot Nothing  (Just _) NullsFirst = LT
compareSlot Nothing  (Just _) NullsLast  = GT
compareSlot (Just _) Nothing  NullsFirst = GT
compareSlot (Just _) Nothing  NullsLast  = LT
compareSlot (Just x) (Just y) _          = avCompare x y

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering GT = LT
invertOrdering EQ = EQ

-- | Compare two Avro values structurally. Iceberg's spec defines a total
-- order on each primitive type; this implementation is conservative and
-- compares like-for-like values, falling back to 'compare' on the wire form
-- for anything more exotic.
avCompare :: AV.Value -> AV.Value -> Ordering
avCompare (AV.Bool a)   (AV.Bool b)   = compare a b
avCompare (AV.Int a)    (AV.Int b)    = compare a b
avCompare (AV.Long a)   (AV.Long b)   = compare a b
avCompare (AV.Float a)  (AV.Float b)  = compare a b
avCompare (AV.Double a) (AV.Double b) = compare a b
avCompare (AV.String a) (AV.String b) = compare a b
avCompare (AV.Bytes a)  (AV.Bytes b)  = compare a b
avCompare (AV.Fixed a)  (AV.Fixed b)  = compare a b
avCompare a b = compare (show a) (show b)
