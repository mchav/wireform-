{-# LANGUAGE OverloadedStrings #-}

module Test.NDJSON.Derive (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Vector qualified as V
import NDJSON.Decode qualified as ND
import NDJSON.Encode qualified as NE
import Test.NDJSON.Derive.Instances ()
import Test.NDJSON.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "NDJSON.Derive" $
    sequence_
      [ it "round-trip Vector via encodeRecords/decodeRecords" $ do
          let xs =
                V.fromList
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
