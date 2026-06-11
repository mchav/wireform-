{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.YAML.Derive.Instances () where

import Test.YAML.Derive.Types
import YAML.Derive


deriveYAML ''Profile
deriveYAML ''Tag
deriveYAML ''Color
deriveYAML ''Shape
