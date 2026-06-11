{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.EDN.Derive.Instances () where

import EDN.Derive
import Test.EDN.Derive.Types


deriveEDN ''Profile
deriveEDN ''Tag
deriveEDN ''Color
deriveEDN ''Shape
