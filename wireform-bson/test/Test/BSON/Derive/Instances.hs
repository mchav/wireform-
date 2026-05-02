{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.BSON.Derive.Instances () where

import BSON.Derive

import Test.BSON.Derive.Types

deriveBSON ''Profile
deriveBSON ''Tag
deriveBSON ''Color
deriveBSON ''Shape
