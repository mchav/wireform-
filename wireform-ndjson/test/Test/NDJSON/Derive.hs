{-# LANGUAGE OverloadedStrings #-}

module Test.NDJSON.Derive (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified NDJSON.Decode as ND
import qualified NDJSON.Encode as NE

import Test.NDJSON.Derive.Instances ()
import Test.NDJSON.Derive.Types

tests :: TestTree
tests = testGroup "NDJSON.Derive"
  [ testCase "round-trip Vector via encodeRecords/decodeRecords" $ do
      let xs = V.fromList
            [ Event 1 "a" 1.5
            , Event 2 "b" 2.5
            , Event 3 "c" 3.5
            ]
      let bs = NE.encodeRecords xs
      ND.decodeRecords bs @?= Right xs

  , testCase "wire format honours rename modifiers" $ do
      let bs = NE.encodeRecords (V.singleton (Event 7 "x" 1.0))
      let line = head (BS8.split '\n' bs)
      assertBool "event_id present"
        (BS.isInfixOf "event_id" line)
      assertBool "name present"
        (BS.isInfixOf "\"name\":" line)
      assertBool "score present (StripPrefix + SnakeCase)"
        (BS.isInfixOf "\"score\":" line)
      assertBool "eventId absent"
        (not (BS.isInfixOf "eventId" line))
      assertBool "eventScore absent"
        (not (BS.isInfixOf "eventScore" line))
  ]
