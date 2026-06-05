{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Test.Lens (lensTests) where

import Data.Int (Int32, Int64)
import Test.Syd

import Proto.Google.Protobuf.Timestamp (Timestamp(..), defaultTimestamp)

lensTests :: Spec
lensTests = describe "Proto.Lens (record field access)" $ sequence_
  [ it "read timestampSeconds" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
      timestampSeconds ts `shouldBe` (42 :: Int64)

  , it "read timestampNanos" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
      timestampNanos ts `shouldBe` (99 :: Int32)

  , it "update via record syntax" $ do
      let ts = defaultTimestamp { timestampSeconds = 42, timestampNanos = 99 }
          ts' = ts { timestampSeconds = 100, timestampNanos = 200 }
      timestampSeconds ts' `shouldBe` 100
      timestampNanos ts' `shouldBe` 200

  , it "default values" $ do
      timestampSeconds defaultTimestamp `shouldBe` 0
      timestampNanos defaultTimestamp `shouldBe` 0
  ]
