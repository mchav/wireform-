{-# LANGUAGE TemplateHaskell #-}

-- | Splice site for the top-level-enum + packed-scalar
-- 'loadProto' regression.
module Test.Proto.Derive.TopEnumInstances
  ( Status (..)
  , Account (..)
  , PackedBag (..)
  , defaultAccount
  , defaultPackedBag
  ) where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Vector as V

import Proto.TH (loadProto)

-- Keep GHC from optimising away the imports the loadProto splice
-- transitively needs.
_unused :: (V.Vector Int, T.Text, Int32)
_unused = (V.empty, T.empty, 0)

$(loadProto "test/data/topenum_regression.proto")
