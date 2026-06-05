{-# LANGUAGE OverloadedStrings #-}

module Test.NDJSON.Derive (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import Test.Syd

import qualified NDJSON.Decode as ND
import qualified NDJSON.Encode as NE

import Test.NDJSON.Derive.Instances ()
import Test.NDJSON.Derive.Types

tests :: Spec
tests = describe "NDJSON.Derive" $ sequence_
  [ it "round-trip Vector via encodeRecords/decodeRecords" $ do
      let xs = V.fromList
            [ Event 1 "a" 1.5
            , Event 2 "b" 2.5
            , Event 3 "c" 3.5
            ]
      let bs = NE.encodeRecords xs
      ND.decodeRecords bs `shouldBe` Right xs

  , it "wire format honours rename modifiers" $ do
      let bs = NE.encodeRecords (V.singleton (Event 7 "x" 1.0))
      let line = head (BS8.split '\n' bs)
      (BS.isInfixOf "event_id" line) `shouldBe` True
      (BS.isInfixOf "\"name\":" line) `shouldBe` True
      (BS.isInfixOf "\"score\":" line) `shouldBe` True
      (not (BS.isInfixOf "eventId" line)) `shouldBe` True
      (not (BS.isInfixOf "eventScore" line)) `shouldBe` True
  ]
