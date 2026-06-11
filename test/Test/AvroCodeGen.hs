module Test.AvroCodeGen (avroCodeGenTests) where

import Avro.CodeGen (generateAvroTypes)
import Avro.Schema
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


avroCodeGenTests :: Spec
avroCodeGenTests =
  describe "Avro.CodeGen" $
    sequence_
      [ testRecordCodeGen
      , testEnumCodeGen
      , testNullableUnionCodeGen
      , testNestedRecordCodeGen
      , testArrayAndMapCodeGen
      ]


personSchema :: AvroType
personSchema =
  AvroRecord
    { avroRecordName = "Person"
    , avroRecordNamespace = Nothing
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "email" (AvroUnion {avroUnionBranches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString]}) Nothing Nothing V.empty Nothing Map.empty
          ]
    }


testRecordCodeGen :: Spec
testRecordCodeGen = it "generates record data type + instances from Avro schema" $ do
  let code = generateAvroTypes personSchema
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("personEmail" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int32" `T.isInfixOf` code) `shouldBe` True
  ("Maybe Text" `T.isInfixOf` code) `shouldBe` True
  ("instance ToAvro Person" `T.isInfixOf` code) `shouldBe` True
  ("instance FromAvro Person" `T.isInfixOf` code) `shouldBe` True
  ("Show, Eq" `T.isInfixOf` code) `shouldBe` True
  ("NFData" `T.isInfixOf` code) `shouldBe` True


colorSchema :: AvroType
colorSchema =
  AvroEnum
    { avroEnumName = "Color"
    , avroEnumNamespace = Nothing
    , avroEnumDoc = Nothing
    , avroEnumAliases = V.empty
    , avroEnumSymbols = V.fromList ["RED", "GREEN", "BLUE"]
    , avroEnumDefault = Nothing
    }


testEnumCodeGen :: Spec
testEnumCodeGen = it "generates enum sum type from Avro schema" $ do
  let code = generateAvroTypes colorSchema
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True
  ("instance ToAvro Color" `T.isInfixOf` code) `shouldBe` True
  ("instance FromAvro Color" `T.isInfixOf` code) `shouldBe` True
  ("Enum, Bounded" `T.isInfixOf` code) `shouldBe` True


testNullableUnionCodeGen :: Spec
testNullableUnionCodeGen = it "union [null, T] produces Maybe T" $ do
  let schema =
        AvroRecord
          { avroRecordName = "OptionalField"
          , avroRecordNamespace = Nothing
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ AvroField "value" (AvroUnion {avroUnionBranches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroLong]}) Nothing Nothing V.empty Nothing Map.empty
                ]
          }
      code = generateAvroTypes schema
  ("Maybe Int64" `T.isInfixOf` code) `shouldBe` True


testNestedRecordCodeGen :: Spec
testNestedRecordCodeGen = it "nested records produce separate data types" $ do
  let addressSchema =
        AvroRecord
          { avroRecordName = "Address"
          , avroRecordNamespace = Nothing
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ AvroField "street" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "city" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                ]
          }
      schema =
        AvroRecord
          { avroRecordName = "Employee"
          , avroRecordNamespace = Nothing
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "address" addressSchema Nothing Nothing V.empty Nothing Map.empty
                ]
          }
      code = generateAvroTypes schema
  ("data Address = Address" `T.isInfixOf` code) `shouldBe` True
  ("data Employee = Employee" `T.isInfixOf` code) `shouldBe` True
  ("addressStreet" `T.isInfixOf` code) `shouldBe` True
  ("addressCity" `T.isInfixOf` code) `shouldBe` True
  ("employeeName" `T.isInfixOf` code) `shouldBe` True
  ("employeeAddress" `T.isInfixOf` code) `shouldBe` True


testArrayAndMapCodeGen :: Spec
testArrayAndMapCodeGen = it "arrays -> Vector, maps -> Map Text" $ do
  let schema =
        AvroRecord
          { avroRecordName = "Container"
          , avroRecordNamespace = Nothing
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ AvroField "tags" (AvroArray {avroArrayItems = AvroPrimitive AvroString}) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "metadata" (AvroMap {avroMapValues = AvroPrimitive AvroString}) Nothing Nothing V.empty Nothing Map.empty
                ]
          }
      code = generateAvroTypes schema
  ("Vector" `T.isInfixOf` code) `shouldBe` True
  ("Map Text" `T.isInfixOf` code) `shouldBe` True
