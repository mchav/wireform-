{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.Lens (lensTests) where

import Data.Int (Int32, Int64)
import Test.Tasty
import Test.Tasty.HUnit

import Proto.Lens
import Proto.Google.Protobuf.Timestamp (Timestamp(..), defaultTimestamp)

lensTests :: TestTree
lensTests = testGroup "Proto.Lens"
  [ testGroup "view (get)"
      [ testCase "view seconds" $ do
          let ts = Timestamp 42 99
          view (field @"seconds") ts @?= (42 :: Int64)

      , testCase "view nanos" $ do
          let ts = Timestamp 42 99
          view (field @"nanos") ts @?= (99 :: Int32)

      , testCase "^. operator" $ do
          let ts = Timestamp 100 200
          (ts ^. field @"seconds") @?= (100 :: Int64)
      ]

  , testGroup "set"
      [ testCase "set seconds" $ do
          let ts = defaultTimestamp
              ts' = set (field @"seconds") 42 ts
          seconds ts' @?= 42
          nanos ts' @?= 0

      , testCase ".~ operator" $ do
          let ts = defaultTimestamp
              ts' = ts & field @"seconds" .~ 42
                       & field @"nanos" .~ 99
          seconds ts' @?= 42
          nanos ts' @?= 99
      ]

  , testGroup "over (modify)"
      [ testCase "over seconds (+1)" $ do
          let ts = Timestamp 42 0
              ts' = over (field @"seconds") (+1) ts
          seconds ts' @?= 43

      , testCase "%~ operator" $ do
          let ts = Timestamp 10 20
              ts' = ts & field @"nanos" %~ (*2)
          nanos ts' @?= 40
      ]

  , testGroup "composition"
      [ testCase "set then view" $ do
          let ts = set (field @"seconds") 123 defaultTimestamp
          view (field @"seconds") ts @?= (123 :: Int64)

      , testCase "chained set with &" $ do
          let ts = defaultTimestamp
                & field @"seconds" .~ 1000
                & field @"nanos" .~ 500
          ts @?= Timestamp 1000 500
      ]
  ]
