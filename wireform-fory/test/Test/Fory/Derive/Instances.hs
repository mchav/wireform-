{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Fory.Derive.Instances () where

import Fory.Derive

import Test.Fory.Derive.Types

deriveFory ''Profile
deriveFory ''Tag
deriveFory ''Color
deriveFory ''Shape
