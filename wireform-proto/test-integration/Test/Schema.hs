{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Test.Schema (schemaTests) where

import Data.Int (Int32, Int64)
import Proto.Google.Protobuf.Timestamp
import Test.Syd


schemaTests :: Spec
schemaTests =
  describe "Schema (generated record fields)" $
    sequence_
      [ it "generated field names" $ do
          let ts = defaultTimestamp {timestampSeconds = 42, timestampNanos = 99}
          timestampSeconds ts `shouldBe` (42 :: Int64)
          timestampNanos ts `shouldBe` (99 :: Int32)
      , it "default value" $ do
          timestampSeconds defaultTimestamp `shouldBe` (0 :: Int64)
          timestampNanos defaultTimestamp `shouldBe` (0 :: Int32)
      , it "record update" $ do
          let ts = defaultTimestamp {timestampSeconds = 100, timestampNanos = 200}
              ts' = ts {timestampSeconds = 300}
          timestampSeconds ts' `shouldBe` (300 :: Int64)
          timestampNanos ts' `shouldBe` (200 :: Int32)
      ]
