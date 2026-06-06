{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Splice site for the top-level-enum + packed-scalar
'loadProto' regression.
-}
module Test.Proto.Derive.TopEnumInstances (
  Status (..),
  Account (..),
  PackedBag (..),
  defaultAccount,
  defaultPackedBag,
) where

import Data.Int (Int32)
import Data.Reflection (Given (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH (loadProto)

-- TH-generated JSON instances carry a 'Given ExtensionRegistry' constraint
-- for proto2 extensions; this test target has none, so satisfy it with
-- the empty registry.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry


-- Keep GHC from optimising away the imports the loadProto splice
-- transitively needs.
_unused :: (V.Vector Int, T.Text, Int32)
_unused = (V.empty, T.empty, 0)


$(loadProto "test/data/topenum_regression.proto")
