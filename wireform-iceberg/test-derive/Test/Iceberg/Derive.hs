{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Iceberg.Derive (tests) where

import Data.Text (Text)
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Iceberg.Types as I
import Iceberg.Derive (icebergFieldsFor, icebergSchemaFor)

import Test.Iceberg.Derive.Instances (sumTypeSchemaSucceeded)
import Test.Iceberg.Derive.Types

tests :: TestTree
tests = testGroup "Iceberg.Derive"
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

schemaTests :: TestTree
schemaTests = testGroup "icebergSchemaFor"
  [ testCase "Person schema has three fields with auto IDs starting at 1" $ do
      let fs = I.schemaFields personSchema
      V.length fs @?= 3
      I.sfId   (V.unsafeIndex fs 0) @?= 1
      I.sfName (V.unsafeIndex fs 0) @?= "person_name"
      I.sfId   (V.unsafeIndex fs 1) @?= 2
      I.sfName (V.unsafeIndex fs 1) @?= "person_age"
      I.sfId   (V.unsafeIndex fs 2) @?= 3
      I.sfName (V.unsafeIndex fs 2) @?= "person_active"

  , testCase "Person field types map to the right IcebergType" $ do
      let fs = I.schemaFields personSchema
      I.sfType (V.unsafeIndex fs 0) @?= I.TString
      I.sfType (V.unsafeIndex fs 1) @?= I.TLong
      I.sfType (V.unsafeIndex fs 2) @?= I.TBoolean

  , testCase "all Person fields are required" $ do
      let fs = I.schemaFields personSchema
      I.sfRequired (V.unsafeIndex fs 0) @?= True
      I.sfRequired (V.unsafeIndex fs 1) @?= True
      I.sfRequired (V.unsafeIndex fs 2) @?= True

  , testCase "Sale Maybe field becomes optional with the inner type" $ do
      let fs = I.schemaFields saleSchema
      I.sfName     (V.unsafeIndex fs 2) @?= "sale_region"
      I.sfRequired (V.unsafeIndex fs 2) @?= False
      I.sfType     (V.unsafeIndex fs 2) @?= I.TString

  , testCase "rename modifier reflected on Sale field names" $ do
      let fs = I.schemaFields saleSchema
      I.sfName (V.unsafeIndex fs 0) @?= "amount"
      I.sfName (V.unsafeIndex fs 1) @?= "product"

  , testCase "tag modifier overrides the auto-assigned field id" $ do
      let fs = I.schemaFields taggedSchema
      I.sfId (V.unsafeIndex fs 0) @?= 100
      I.sfId (V.unsafeIndex fs 1) @?= 200

  , testCase "schemaId defaults to 0 and identifier-fields is empty" $ do
      I.schemaId               personSchema @?= 0
      I.schemaIdentifierFieldIds personSchema @?= V.empty
  ]

-- ---------------------------------------------------------------------------
-- Fields-only splice
-- ---------------------------------------------------------------------------

personFields :: [(Text, I.IcebergType)]
personFields = $(icebergFieldsFor ''Person)

fieldsTests :: TestTree
fieldsTests = testGroup "icebergFieldsFor"
  [ testCase "Person produces the bare (name, type) pair list" $ do
      length personFields @?= 3
      map fst personFields @?= ["person_name", "person_age", "person_active"]
      map snd personFields @?= [I.TString, I.TLong, I.TBoolean]
  ]

-- ---------------------------------------------------------------------------
-- Splice-time refusal
-- ---------------------------------------------------------------------------

spliceTests :: TestTree
spliceTests = testGroup "splice-time"
  [ testCase "icebergSchemaFor refuses sum types" $
      assertBool "icebergSchemaFor ''Variant must fail at splice time"
        (not sumTypeSchemaSucceeded)
  ]
