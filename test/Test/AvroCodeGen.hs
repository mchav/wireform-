module Test.AvroCodeGen (avroCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Avro.Schema
import Avro.CodeGen (generateAvroTypes)

avroCodeGenTests :: TestTree
avroCodeGenTests = testGroup "Avro.CodeGen"
  [ testRecordCodeGen
  , testEnumCodeGen
  , testNullableUnionCodeGen
  , testNestedRecordCodeGen
  , testArrayAndMapCodeGen
  ]

personSchema :: AvroType
personSchema = AvroRecord
  { avroRecordName = "Person"
  , avroRecordNamespace = Nothing
  , avroRecordDoc = Nothing
  , avroRecordAliases = V.empty
  , avroRecordProps = Map.empty
  , avroRecordFields = V.fromList
    [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
    , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
    , AvroField "email" (AvroUnion { avroUnionBranches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString] }) Nothing Nothing V.empty Nothing Map.empty
    ]
  }

testRecordCodeGen :: TestTree
testRecordCodeGen = testCase "generates record data type + instances from Avro schema" $ do
  let code = generateAvroTypes personSchema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "contains personEmail field" ("personEmail" `T.isInfixOf` code)
  assertBool "personName is Text" ("!Text" `T.isInfixOf` code)
  assertBool "personAge is Int32" ("!Int32" `T.isInfixOf` code)
  assertBool "personEmail is Maybe Text" ("Maybe Text" `T.isInfixOf` code)
  assertBool "contains ToAvro instance" ("instance ToAvro Person" `T.isInfixOf` code)
  assertBool "contains FromAvro instance" ("instance FromAvro Person" `T.isInfixOf` code)
  assertBool "contains deriving Show Eq" ("Show, Eq" `T.isInfixOf` code)
  assertBool "contains deriving NFData" ("NFData" `T.isInfixOf` code)

colorSchema :: AvroType
colorSchema = AvroEnum
  { avroEnumName = "Color"
  , avroEnumNamespace = Nothing
  , avroEnumDoc = Nothing
  , avroEnumAliases = V.empty
  , avroEnumSymbols = V.fromList ["RED", "GREEN", "BLUE"]
  , avroEnumDefault = Nothing
  }

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "generates enum sum type from Avro schema" $ do
  let code = generateAvroTypes colorSchema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)
  assertBool "contains ToAvro instance" ("instance ToAvro Color" `T.isInfixOf` code)
  assertBool "contains FromAvro instance" ("instance FromAvro Color" `T.isInfixOf` code)
  assertBool "contains Enum, Bounded deriving" ("Enum, Bounded" `T.isInfixOf` code)

testNullableUnionCodeGen :: TestTree
testNullableUnionCodeGen = testCase "union [null, T] produces Maybe T" $ do
  let schema = AvroRecord
        { avroRecordName = "OptionalField"
        , avroRecordNamespace = Nothing
        , avroRecordDoc = Nothing
        , avroRecordAliases = V.empty
        , avroRecordProps = Map.empty
        , avroRecordFields = V.fromList
          [ AvroField "value" (AvroUnion { avroUnionBranches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroLong] }) Nothing Nothing V.empty Nothing Map.empty
          ]
        }
      code = generateAvroTypes schema
  assertBool "contains Maybe Int64" ("Maybe Int64" `T.isInfixOf` code)

testNestedRecordCodeGen :: TestTree
testNestedRecordCodeGen = testCase "nested records produce separate data types" $ do
  let addressSchema = AvroRecord
        { avroRecordName = "Address"
        , avroRecordNamespace = Nothing
        , avroRecordDoc = Nothing
        , avroRecordAliases = V.empty
        , avroRecordProps = Map.empty
        , avroRecordFields = V.fromList
          [ AvroField "street" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "city" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          ]
        }
      schema = AvroRecord
        { avroRecordName = "Employee"
        , avroRecordNamespace = Nothing
        , avroRecordDoc = Nothing
        , avroRecordAliases = V.empty
        , avroRecordProps = Map.empty
        , avroRecordFields = V.fromList
          [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "address" addressSchema Nothing Nothing V.empty Nothing Map.empty
          ]
        }
      code = generateAvroTypes schema
  assertBool "contains data Address" ("data Address = Address" `T.isInfixOf` code)
  assertBool "contains data Employee" ("data Employee = Employee" `T.isInfixOf` code)
  assertBool "contains addressStreet" ("addressStreet" `T.isInfixOf` code)
  assertBool "contains addressCity" ("addressCity" `T.isInfixOf` code)
  assertBool "contains employeeName" ("employeeName" `T.isInfixOf` code)
  assertBool "contains employeeAddress" ("employeeAddress" `T.isInfixOf` code)

testArrayAndMapCodeGen :: TestTree
testArrayAndMapCodeGen = testCase "arrays -> Vector, maps -> Map Text" $ do
  let schema = AvroRecord
        { avroRecordName = "Container"
        , avroRecordNamespace = Nothing
        , avroRecordDoc = Nothing
        , avroRecordAliases = V.empty
        , avroRecordProps = Map.empty
        , avroRecordFields = V.fromList
          [ AvroField "tags" (AvroArray { avroArrayItems = AvroPrimitive AvroString }) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "metadata" (AvroMap { avroMapValues = AvroPrimitive AvroString }) Nothing Nothing V.empty Nothing Map.empty
          ]
        }
      code = generateAvroTypes schema
  assertBool "contains Vector" ("Vector" `T.isInfixOf` code)
  assertBool "contains Map Text" ("Map Text" `T.isInfixOf` code)
