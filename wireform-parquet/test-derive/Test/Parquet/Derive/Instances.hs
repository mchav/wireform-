{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Instance derivations + a compile-time probe that checks
'deriveParquet' refuses sum-type shapes.
-}
module Test.Parquet.Derive.Instances (
  sumTypeDeriveSucceeded,
) where

import Language.Haskell.TH (recover)
import Language.Haskell.TH.Syntax (lift)
import Parquet.Derive (deriveParquet)
import Test.Parquet.Derive.Types


deriveParquet ''Sale
deriveParquet ''Order


sumTypeDeriveSucceeded :: Bool
sumTypeDeriveSucceeded =
  $( recover
       (lift False)
       ( do
           _ <- deriveParquet ''Color
           lift True
       )
   )
