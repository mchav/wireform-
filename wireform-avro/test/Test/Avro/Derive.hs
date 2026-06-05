{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Avro.Derive (spec) where

import qualified Data.Vector as V
import Test.Syd

import qualified Avro.Class as A
import Avro.Derive (avroSchemaFor)
import qualified Avro.Schema as AS
import qualified Avro.Value as AV

import Test.Avro.Derive.Instances ()
import Test.Avro.Derive.Types

spec :: Spec
spec = describe "Avro.Derive" $ do
  recordTests
  newtypeTests
  enumTests
  sumTests
  schemaTests

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests = describe "record" $ do
  it "encode produces a positional Record (skipping skipped fields)" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case A.toAvro p of
      AV.Record vs -> do
        V.length vs `shouldBe` 3
        (vs V.! 0) `shouldBe` AV.String "Alice"
        (vs V.! 1) `shouldBe` AV.Long 30
        (vs V.! 2) `shouldBe` AV.String "a@x"
      v -> expectationFailure ("expected Record, got " ++ show v)

  it "round-trip fills skipped from defaults" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case A.fromAvro (A.toAvro p) of
      Right p' -> do
        profileName    p' `shouldBe` profileName p
        profileAge     p' `shouldBe` profileAge p
        profileEmail   p' `shouldBe` profileEmail p
        profilePrivate p' `shouldBe` defaultPrivate
      Left e -> expectationFailure e

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests = describe "newtype" $ do
  it "pass-through" $
    A.toAvro (Tag 42) `shouldBe` AV.Long 42
  it "round-trip" $
    A.fromAvro (A.toAvro (Tag 7)) `shouldBe` Right (Tag 7)

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests = describe "enum" $ do
  it "Red is ordinal 0"        $ A.toAvro Red      `shouldBe` AV.Enum 0
  it "Green is ordinal 1"      $ A.toAvro Green    `shouldBe` AV.Enum 1
  it "DarkBlue is ordinal 2"   $ A.toAvro DarkBlue `shouldBe` AV.Enum 2
  it "round-trip every variant" $
    mapM_ rt [Red, Green, DarkBlue]
  it "out-of-range fails" $
    case A.fromAvro (AV.Enum 99) :: Either String Color of
      Left _  -> pure ()
      Right c -> expectationFailure ("unexpected " ++ show c)
  where
    rt :: Color -> IO ()
    rt c = A.fromAvro (A.toAvro c) `shouldBe` Right c

-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: Spec
sumTests = describe "sum" $ do
  it "Origin (nullary) -> Union 0 Null" $
    A.toAvro Origin `shouldBe` AV.Union 0 AV.Null

  it "Circle (unary) -> Union 1 (inner)" $
    A.toAvro (Circle 1.5) `shouldBe` AV.Union 1 (AV.Double 1.5)

  it "Rect (n-ary) -> Union 2 (Record [...])" $
    A.toAvro (Rect 2 3) `shouldBe`
      AV.Union 2 (AV.Record (V.fromList [AV.Double 2, AV.Double 3]))

  it "round-trip Origin" $ rt Origin
  it "round-trip Circle" $ rt (Circle 2.5)
  it "round-trip Rect"   $ rt (Rect 4 5)

  it "unknown branch index fails" $
    case A.fromAvro (AV.Union 99 AV.Null) :: Either String Shape of
      Left _  -> pure ()
      Right s -> expectationFailure ("unexpected " ++ show s)
  where
    rt :: Shape -> IO ()
    rt s = A.fromAvro (A.toAvro s) `shouldBe` Right s

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: Spec
schemaTests = describe "schema" $ do
  it "Profile schema lists renamed field names in declaration order" $
    case $(avroSchemaFor ''Profile) of
      AS.AvroRecord{..} -> do
        avroRecordName `shouldBe` "Profile"
        V.length avroRecordFields `shouldBe` 3
        map AS.avroFieldName (V.toList avroRecordFields)
          `shouldBe` ["name", "profile_age", "email"]
        all isPrimitive (V.toList avroRecordFields) `shouldBe` True
      _ -> expectationFailure "expected AvroRecord"

  it "Color schema lists renamed symbols in declaration order" $
    case $(avroSchemaFor ''Color) of
      AS.AvroEnum{..} -> do
        avroEnumName `shouldBe` "Color"
        V.toList avroEnumSymbols `shouldBe` ["red", "green", "dark-blue"]
      _ -> expectationFailure "expected AvroEnum"

  it "Shape schema is a Union with one branch per ctor" $
    case $(avroSchemaFor ''Shape) of
      AS.AvroUnion{..} -> do
        V.length avroUnionBranches `shouldBe` 3
        avroUnionBranches V.! 0
          `shouldBe` AS.AvroPrimitive AS.AvroNull
        avroUnionBranches V.! 1
          `shouldBe` AS.AvroPrimitive AS.AvroDouble
      _ -> expectationFailure "expected AvroUnion"

  it "Tag (newtype) schema is the inner type's schema" $
    $(avroSchemaFor ''Tag) `shouldBe` AS.AvroPrimitive AS.AvroLong
  where
    isPrimitive :: AS.AvroField -> Bool
    isPrimitive f = case AS.avroFieldType f of
      AS.AvroPrimitive _ -> True
      _                  -> False
