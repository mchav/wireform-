{-# LANGUAGE TemplateHaskell #-}

{- | Splice site for the top-level-enum + packed-scalar
'loadProto' regression.
-}
module Test.Proto.TH.Derive.TopEnumInstances (
  Status (..),
  Account (..),
  PackedBag (..),
  defaultAccount,
  defaultPackedBag,
) where

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.TH (loadProto)


-- Keep GHC from optimising away the imports the loadProto splice
-- transitively needs.
_unused :: (V.Vector Int, T.Text, Int32)
_unused = (V.empty, T.empty, 0)


$(loadProto "test/data/topenum_regression.proto")
