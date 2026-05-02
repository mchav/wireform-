{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.FlatBuffers.Derive.Instances () where

import FlatBuffers.Derive

import Test.FlatBuffers.Derive.Types

deriveFlatBuffers ''Position
deriveFlatBuffers ''Tag
deriveFlatBuffers ''Color
