{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.YAML.Derive.Instances () where

import YAML.Derive

import Test.YAML.Derive.Types

deriveYAML ''Profile
deriveYAML ''Tag
deriveYAML ''Color
deriveYAML ''Shape
