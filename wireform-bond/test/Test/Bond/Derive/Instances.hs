{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bond.Derive.Instances () where

import Bond.Derive
import Test.Bond.Derive.Types


deriveBond ''Profile
deriveBond ''Tag
deriveBond ''Color
deriveBond ''Shape
