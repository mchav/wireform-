{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.CBOR.Derive.Instances () where

import CBOR.Derive

import Test.CBOR.Derive.Types

deriveCBOR ''Profile
deriveCBOR ''Tag
deriveCBOR ''Color
deriveCBOR ''Shape
