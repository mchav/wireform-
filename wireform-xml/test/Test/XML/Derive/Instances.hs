{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.XML.Derive.Instances () where

import XML.Derive

import Test.XML.Derive.Types

deriveXML ''User
deriveXML ''Status
deriveXML ''Color
deriveXML ''Shape
