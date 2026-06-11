{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HTML.Derive.Instances () where

import HTML.Derive
import Test.HTML.Derive.Types


deriveHTML ''User
deriveHTML ''Color
deriveHTML ''Shape
