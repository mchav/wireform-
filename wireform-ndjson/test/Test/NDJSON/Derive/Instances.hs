{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.NDJSON.Derive.Instances () where

import NDJSON.Derive

import Test.NDJSON.Derive.Types

deriveNDJSON ''Event
