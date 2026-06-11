{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Avro.Derive.Instances () where

import Avro.Derive
import Test.Avro.Derive.Types


deriveAvro ''Profile
deriveAvro ''Tag
deriveAvro ''Color
deriveAvro ''Shape
