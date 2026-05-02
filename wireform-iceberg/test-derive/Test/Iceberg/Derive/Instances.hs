{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time probes that confirm 'icebergSchemaFor' refuses
-- sum-type shapes.
module Test.Iceberg.Derive.Instances
  ( sumTypeSchemaSucceeded
  ) where

import Language.Haskell.TH (recover)
import Language.Haskell.TH.Syntax (lift)

import Iceberg.Derive (icebergSchemaFor)

import Test.Iceberg.Derive.Types (Variant)

-- | Splice-time probe: 'True' iff @icebergSchemaFor ''Variant@
-- succeeded (which it must not, since 'Variant' is a sum).
sumTypeSchemaSucceeded :: Bool
sumTypeSchemaSucceeded = $(recover (lift False) (do
  _ <- icebergSchemaFor ''Variant
  lift True))
