{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ion.Derive.Instances () where

import Ion.Derive

import Test.Ion.Derive.Types

deriveIon ''Profile
deriveIon ''Tag
deriveIon ''Color
deriveIon ''Shape
