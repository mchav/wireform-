{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.XML.Derive.Instances () where

import Test.XML.Derive.Types
import XML.Derive


deriveXML ''User
deriveXML ''Status
deriveXML ''Color
deriveXML ''Shape
