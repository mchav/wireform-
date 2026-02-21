{-# LANGUAGE BangPatterns #-}
-- | Message merge semantics as defined by the protobuf specification.
--
-- When decoding, if the same field number appears multiple times:
--
-- * Scalar fields: last value wins
-- * Repeated fields: values are appended
-- * Message fields: recursively merged
-- * Map fields: entries are merged (last value wins per key)
-- * Oneof fields: last set field wins
--
-- This module provides the 'Mergeable' typeclass and utilities for
-- implementing merge in generated code. This is important for:
--
-- * Proper decoding of messages split across multiple wire segments
-- * Combining partial updates (like PATCH semantics)
-- * Conformance with the protobuf specification
module Proto.Merge
  ( -- * Merge typeclass
    Mergeable (..)

    -- * Merge helpers
  , mergeOptional
  , mergeRepeated
  , mergeRepeatedU
  , mergeMap
  , mergeScalar
  , mergeBool
  , mergeBytes
  , mergeText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | Typeclass for types that support protobuf merge semantics.
--
-- 'mergeFrom' combines two values according to the proto merge rules.
-- The second argument is the "incoming" value (from a newer decode
-- or update), and its fields take precedence over the first.
class Mergeable a where
  mergeFrom :: a -> a -> a

-- | Merge optional (Maybe) fields: incoming Just wins, otherwise keep existing.
mergeOptional :: Mergeable a => Maybe a -> Maybe a -> Maybe a
mergeOptional existing incoming = case (existing, incoming) of
  (_, Just b)         -> Just (maybe b (\a -> mergeFrom a b) existing)
  (Just a, Nothing)   -> Just a
  (Nothing, Nothing)  -> Nothing

-- | Merge repeated fields by appending.
mergeRepeated :: V.Vector a -> V.Vector a -> V.Vector a
mergeRepeated = (V.++)

-- | Merge unboxed repeated fields by appending.
mergeRepeatedU :: VU.Unbox a => VU.Vector a -> VU.Vector a -> VU.Vector a
mergeRepeatedU = (VU.++)

-- | Merge map fields: keys from incoming override existing.
mergeMap :: Ord k => Map k v -> Map k v -> Map k v
mergeMap existing incoming = Map.union incoming existing

-- | Merge scalar fields: last (non-default) value wins.
mergeScalar :: (Eq a, Num a) => a -> a -> a
mergeScalar existing incoming
  | incoming == 0 = existing
  | otherwise     = incoming

-- | Merge bool fields.
mergeBool :: Bool -> Bool -> Bool
mergeBool existing incoming
  | incoming  = incoming
  | otherwise = existing

-- | Merge bytes fields: incoming non-empty wins.
mergeBytes :: ByteString -> ByteString -> ByteString
mergeBytes existing incoming
  | BS.null incoming = existing
  | otherwise        = incoming

-- | Merge text fields: incoming non-empty wins.
mergeText :: Text -> Text -> Text
mergeText existing incoming
  | T.null incoming = existing
  | otherwise       = incoming
