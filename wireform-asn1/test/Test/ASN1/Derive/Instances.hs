{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ASN1.Derive.Instances () where

import ASN1.Derive

import Test.ASN1.Derive.Types

deriveASN1 ''Person
deriveASN1 ''Wrapper
deriveASN1 ''Color
deriveASN1 ''Shape
