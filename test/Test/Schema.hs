{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.Schema (schemaTests) where

import Data.Int (Int32, Int64)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Proto.Schema
import Proto.Google.Protobuf.Timestamp

schemaTests :: TestTree
schemaTests = testGroup "Schema Metadata"
  [ testGroup "ProtoMessage"
      [ testCase "message name" $
          protoMessageName (Proxy :: Proxy Timestamp) @?= "google.protobuf.Timestamp"

      , testCase "package name" $
          protoPackageName (Proxy :: Proxy Timestamp) @?= "google.protobuf"

      , testCase "default value" $ do
          let def = protoDefaultValue :: Timestamp
          seconds def @?= 0
          nanos def @?= 0

      , testCase "field descriptors by number" $ do
          let descs = protoFieldDescriptors (Proxy :: Proxy Timestamp)
          Map.size descs @?= 2
          assertBool "Has field 1" (Map.member 1 descs)
          assertBool "Has field 2" (Map.member 2 descs)

      , testCase "field descriptor names" $ do
          messageFieldNames (Proxy :: Proxy Timestamp) @?= ["seconds", "nanos"]

      , testCase "field descriptor numbers" $ do
          messageFieldNumbers (Proxy :: Proxy Timestamp) @?= [1, 2]

      , testCase "lookup field by name" $ do
          case lookupFieldDescriptor "seconds" (Proxy :: Proxy Timestamp) of
            Nothing -> assertFailure "Expected field 'seconds'"
            Just (SomeField fd) -> do
              fdName fd @?= "seconds"
              fdNumber fd @?= 1
              fdTypeDesc fd @?= ScalarType Int64Field
              fdLabel fd @?= LabelOptional

      , testCase "lookup field by number" $ do
          case fieldDescriptorByNumber 2 (Proxy :: Proxy Timestamp) of
            Nothing -> assertFailure "Expected field 2"
            Just (SomeField fd) -> do
              fdName fd @?= "nanos"
              fdNumber fd @?= 2
              fdTypeDesc fd @?= ScalarType Int32Field

      , testCase "field descriptor not found" $ do
          case lookupFieldDescriptor "nonexistent" (Proxy :: Proxy Timestamp) of
            Nothing -> pure ()
            Just _  -> assertFailure "Should not find nonexistent"
          case fieldDescriptorByNumber 99 (Proxy :: Proxy Timestamp) of
            Nothing -> pure ()
            Just _  -> assertFailure "Should not find field 99"
      ]

  , testGroup "HasField"
      [ testCase "getField seconds" $ do
          let ts = Timestamp 1234567890 500
          getField @Timestamp @"seconds" ts @?= (1234567890 :: Int64)

      , testCase "getField nanos" $ do
          let ts = Timestamp 100 999
          getField @Timestamp @"nanos" ts @?= (999 :: Int32)

      , testCase "setField seconds" $ do
          let ts = Timestamp 0 0
              ts' = setField @Timestamp @"seconds" 42 ts
          seconds ts' @?= 42
          nanos ts' @?= 0

      , testCase "setField nanos" $ do
          let ts = Timestamp 100 200
              ts' = setField @Timestamp @"nanos" 999 ts
          seconds ts' @?= 100
          nanos ts' @?= 999

      , testCase "field descriptor from HasField" $ do
          let fd = fieldDescriptor (Proxy :: Proxy Timestamp) (Proxy :: Proxy "seconds")
          fdName fd @?= "seconds"
          fdNumber fd @?= 1
      ]

  , testGroup "HasField getter/setter roundtrip"
      [ testCase "get seconds via HasField" $ do
          let ts = Timestamp 42 99
          getField @Timestamp @"seconds" ts @?= (42 :: Int64)

      , testCase "set nanos via HasField" $ do
          let ts = Timestamp 42 99
              ts' = setField @Timestamp @"nanos" 777 ts
          nanos ts' @?= 777
          seconds ts' @?= 42
      ]
  ]
