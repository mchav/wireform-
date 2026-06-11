{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ORC.Derive.Instances () where

import ORC.Derive
import Test.ORC.Derive.Types


deriveORC ''OrderId
deriveORC ''Sale
