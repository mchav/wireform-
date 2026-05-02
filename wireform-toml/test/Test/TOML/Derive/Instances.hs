{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.TOML.Derive.Instances () where

import TOML.Derive

import Test.TOML.Derive.Types

deriveTOML ''Profile
deriveTOML ''Tag
deriveTOML ''Color
deriveTOML ''Shape
