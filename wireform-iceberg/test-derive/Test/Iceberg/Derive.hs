{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Iceberg.Derive (tests) where

import Data.Text (Text)
import qualified Data.Vector as V
import Test.Syd

import qualified Iceberg.Types as I
import Iceberg.Derive (icebergFieldsFor, icebergSchemaFor)

import Test.Iceberg.Derive.Instances (sumTypeSchemaSucceeded)
import Test.Iceberg.Derive.Types

tests :: Spec
tests = describe "Iceberg.Derive" $ sequence_
  [ schemaTests
  , fieldsTests
  , spliceTests
  ]

personSchema :: I.Schema
personSchema = $(icebergSchemaFor ''Person)

saleSchema :: I.Schema
saleSchema = $(icebergSchemaFor ''Sale)

taggedSchema :: I.Schema
taggedSchema = $(icebergSchemaFor ''Tagged)

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: Spec
schemaTests = describe "icebergSchemaFor" $ sequence_
  [ it "Person schema has three fields with auto IDs starting at 1" $ do
      let fs = I.schemaFields personSchema
      V.length fs `shouldBe` 3
      I.sfId   (V.unsafeIndex fs 0) `shouldBe` 1
      I.sfName (V.unsafeIndex fs 0) `shouldBe` "person_name"
      I.sfId   (V.unsafeIndex fs 1) `shouldBe` 2
      I.sfName (V.unsafeIndex fs 1) `shouldBe` "person_age"
      I.sfId   (V.unsafeIndex fs 2) `shouldBe` 3
      I.sfName (V.unsafeIndex fs 2) `shouldBe` "person_active"

  , it "Person field types map to the right IcebergType" $ do
      let fs = I.schemaFields personSchema
      I.sfType (V.unsafeIndex fs 0) `shouldBe` I.TString
      I.sfType (V.unsafeIndex fs 1) `shouldBe` I.TLong
      I.sfType (V.unsafeIndex fs 2) `shouldBe` I.TBoolean

  , it "all Person fields are required" $ do
      let fs = I.schemaFields personSchema
      I.sfRequired (V.unsafeIndex fs 0) `shouldBe` True
      I.sfRequired (V.unsafeIndex fs 1) `shouldBe` True
      I.sfRequired (V.unsafeIndex fs 2) `shouldBe` True

  , it "Sale Maybe field becomes optional with the inner type" $ do
      let fs = I.schemaFields saleSchema
      I.sfName     (V.unsafeIndex fs 2) `shouldBe` "sale_region"
      I.sfRequired (V.unsafeIndex fs 2) `shouldBe` False
      I.sfType     (V.unsafeIndex fs 2) `shouldBe` I.TString

  , it "rename modifier reflected on Sale field names" $ do
      let fs = I.schemaFields saleSchema
      I.sfName (V.unsafeIndex fs 0) `shouldBe` "amount"
      I.sfName (V.unsafeIndex fs 1) `shouldBe` "product"

  , it "tag modifier overrides the auto-assigned field id" $ do
      let fs = I.schemaFields taggedSchema
      I.sfId (V.unsafeIndex fs 0) `shouldBe` 100
      I.sfId (V.unsafeIndex fs 1) `shouldBe` 200

  , it "schemaId defaults to 0 and identifier-fields is empty" $ do
      I.schemaId               personSchema `shouldBe` 0
      I.schemaIdentifierFieldIds personSchema `shouldBe` V.empty
  ]

-- ---------------------------------------------------------------------------
-- Fields-only splice
-- ---------------------------------------------------------------------------

personFields :: [(Text, I.IcebergType)]
personFields = $(icebergFieldsFor ''Person)

fieldsTests :: Spec
fieldsTests = describe "icebergFieldsFor" $ sequence_
  [ it "Person produces the bare (name, type) pair list" $ do
      length personFields `shouldBe` 3
      map fst personFields `shouldBe` ["person_name", "person_age", "person_active"]
      map snd personFields `shouldBe` [I.TString, I.TLong, I.TBoolean]
  ]

-- ---------------------------------------------------------------------------
-- Splice-time refusal
-- ---------------------------------------------------------------------------

spliceTests :: Spec
spliceTests = describe "splice-time" $ sequence_
  [ it "icebergSchemaFor refuses sum types" $
      (not sumTypeSchemaSucceeded) `shouldBe` True
  ]
