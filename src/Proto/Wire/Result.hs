{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
-- | Combined decode result type merging the Maybe (end-of-input) and
-- Either (error) layers into a single three-way unboxed sum.
module Proto.Wire.Result
  ( DecRes#(..)
  ) where

import GHC.Exts (Int#)

-- | Combined three-way decode result.
--
-- * 'Done#' — end of input (carries offset)
-- * 'Ok#' — decoded a value (carries value + new offset)
-- * 'Err#' — error (carries error description)
--
-- This is used for operations like "get tag or end-of-input" where
-- nesting Decoder (UMaybe Tag) would require two case splits.
data DecRes# a
  = Done# Int#
  | Ok# a Int#
  | Err# String
