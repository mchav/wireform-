{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.CSV.Derive.Instances () where

import CSV.Derive

import Test.CSV.Derive.Types

deriveCSV ''Person
