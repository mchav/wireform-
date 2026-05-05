{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Fury.Derive.Instances () where

import Fury.Derive

import Test.Fury.Derive.Types

deriveFury ''Profile
deriveFury ''Tag
deriveFury ''Color
deriveFury ''Shape
