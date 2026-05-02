{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.CSV.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified CSV.Class as C
import qualified CSV.Derive as CD

import Test.CSV.Derive.Instances ()
import Test.CSV.Derive.Types

tests :: TestTree
tests = testGroup "CSV.Derive"
  [ testCase "row encoding drops skipped fields" $
      C.toCSVRow (Person "Alice" 30 "a@x" "private") @?=
        V.fromList ["Alice", "30", "a@x"]

  , testCase "round-trip applies defaults for skipped" $
      case C.fromCSVRow (V.fromList ["Bob", "25", "b@y"]) of
        Right p -> do
          personName  p @?= "Bob"
          personAge   p @?= 25
          personEmail p @?= "b@y"
          personNotes p @?= defaultNotes
        Left e  -> fail e

  , testCase "csvHeaderFor uses renames, drops skipped" $
      $(CD.csvHeaderFor ''Person) @?=
        V.fromList ["name", "person_age", "email"]

  , testCase "missing cell yields Left" $
      case C.fromCSVRow (V.fromList ["Bob", "25"]) :: Either String Person of
        Left _  -> pure ()
        Right p -> fail ("unexpected " ++ show p)
  ]
