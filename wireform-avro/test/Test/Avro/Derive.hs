{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Avro.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Avro.Class as A
import Avro.Derive (avroSchemaFor)
import qualified Avro.Schema as AS
import qualified Avro.Value as AV

import Test.Avro.Derive.Instances ()
import Test.Avro.Derive.Types

tests :: TestTree
tests = testGroup "Avro.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  , schemaTests
  ]

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode produces a positional Record (skipping skipped fields)" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case A.toAvro p of
        AV.Record vs -> do
          V.length vs @?= 3
          (vs V.! 0) @?= AV.String "Alice"
          (vs V.! 1) @?= AV.Long 30
          (vs V.! 2) @?= AV.String "a@x"
        v -> fail ("expected Record, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case A.fromAvro (A.toAvro p) of
        Right p' -> do
          profileName    p' @?= profileName p
          profileAge     p' @?= profileAge p
          profileEmail   p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      A.toAvro (Tag 42) @?= AV.Long 42
  , testCase "round-trip" $
      A.fromAvro (A.toAvro (Tag 7)) @?= Right (Tag 7)
  ]

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red is ordinal 0"        $ A.toAvro Red      @?= AV.Enum 0
  , testCase "Green is ordinal 1"      $ A.toAvro Green    @?= AV.Enum 1
  , testCase "DarkBlue is ordinal 2"   $ A.toAvro DarkBlue @?= AV.Enum 2
  , testCase "round-trip every variant" $
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "out-of-range fails" $
      case A.fromAvro (AV.Enum 99) :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = A.fromAvro (A.toAvro c) @?= Right c

-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> Union 0 Null" $
      A.toAvro Origin @?= AV.Union 0 AV.Null

  , testCase "Circle (unary) -> Union 1 (inner)" $
      A.toAvro (Circle 1.5) @?= AV.Union 1 (AV.Double 1.5)

  , testCase "Rect (n-ary) -> Union 2 (Record [...])" $
      A.toAvro (Rect 2 3) @?=
        AV.Union 2 (AV.Record (V.fromList [AV.Double 2, AV.Double 3]))

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown branch index fails" $ do
      case A.fromAvro (AV.Union 99 AV.Null) :: Either String Shape of
        Left _  -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = A.fromAvro (A.toAvro s) @?= Right s

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: TestTree
schemaTests = testGroup "schema"
  [ testCase "Profile schema lists renamed field names in declaration order" $
      case $(avroSchemaFor ''Profile) of
        AS.AvroRecord{..} -> do
          avroRecordName @?= "Profile"
          V.length avroRecordFields @?= 3
          map AS.avroFieldName (V.toList avroRecordFields)
            @?= ["name", "profile_age", "email"]
          assertBool "all field types are primitives" $
            all isPrimitive (V.toList avroRecordFields)
        _ -> fail "expected AvroRecord"

  , testCase "Color schema lists renamed symbols in declaration order" $
      case $(avroSchemaFor ''Color) of
        AS.AvroEnum{..} -> do
          avroEnumName @?= "Color"
          V.toList avroEnumSymbols @?= ["red", "green", "dark-blue"]
        _ -> fail "expected AvroEnum"

  , testCase "Shape schema is a Union with one branch per ctor" $
      case $(avroSchemaFor ''Shape) of
        AS.AvroUnion{..} -> do
          V.length avroUnionBranches @?= 3
          avroUnionBranches V.! 0
            @?= AS.AvroPrimitive AS.AvroNull
          avroUnionBranches V.! 1
            @?= AS.AvroPrimitive AS.AvroDouble
        _ -> fail "expected AvroUnion"

  , testCase "Tag (newtype) schema is the inner type's schema" $
      $(avroSchemaFor ''Tag) @?= AS.AvroPrimitive AS.AvroLong
  ]
  where
    isPrimitive :: AS.AvroField -> Bool
    isPrimitive f = case AS.avroFieldType f of
      AS.AvroPrimitive _ -> True
      _                  -> False
