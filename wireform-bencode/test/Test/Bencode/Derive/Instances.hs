{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bencode.Derive.Instances () where

import Bencode.Derive
import Test.Bencode.Derive.Types


deriveBencode ''Profile
deriveBencode ''Tag
deriveBencode ''Color
deriveBencode ''Shape
