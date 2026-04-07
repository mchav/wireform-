module Test.ISLCodeGen (islCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Vector as V

import Ion.ISLSchema
import Ion.ISLCodeGen (generateISLTypes)

islCodeGenTests :: TestTree
islCodeGenTests = testGroup "Ion.ISLCodeGen"
  [ testStructCodeGen
  , testEnumCodeGen
  , testOptionalFieldCodeGen
  , testConstraintComments
  , testIonInstances
  ]

testStructCodeGen :: TestTree
testStructCodeGen = testCase "struct type produces record with ToIon/FromIon" $ do
  let schema = ISLSchema
        { islTypes = V.fromList
            [ ISLType
                { islTypeName = "person"
                , islBaseType = Just "struct"
                , islFields = Just (V.fromList
                    [ ISLField "name" (ISLType "name_type" (Just "string") Nothing Nothing Nothing)
                    , ISLField "age" (ISLType "age_type" (Just "int") Nothing Nothing Nothing)
                    ])
                , islValidValues = Nothing
                , islOccurs = Nothing
                }
            ]
        , islImports = V.empty
        }
      code = generateISLTypes schema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "name is Text" ("!Text" `T.isInfixOf` code)
  assertBool "age is Int64" ("!Int64" `T.isInfixOf` code)
  assertBool "contains ToIon instance" ("instance ToIon Person" `T.isInfixOf` code)
  assertBool "contains FromIon instance" ("instance FromIon Person" `T.isInfixOf` code)

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "enum constraint produces sum type" $ do
  let schema = ISLSchema
        { islTypes = V.fromList
            [ ISLType
                { islTypeName = "color"
                , islBaseType = Just "symbol"
                , islFields = Nothing
                , islValidValues = Just (EnumVal (V.fromList ["red", "green", "blue"]))
                , islOccurs = Nothing
                }
            ]
        , islImports = V.empty
        }
      code = generateISLTypes schema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)

testOptionalFieldCodeGen :: TestTree
testOptionalFieldCodeGen = testCase "optional occurs produces Maybe" $ do
  let schema = ISLSchema
        { islTypes = V.fromList
            [ ISLType
                { islTypeName = "config"
                , islBaseType = Just "struct"
                , islFields = Just (V.fromList
                    [ ISLField "host" (ISLType "t" (Just "string") Nothing Nothing Nothing)
                    , ISLField "port" (ISLType "t" (Just "int") Nothing Nothing (Just OOptional))
                    ])
                , islValidValues = Nothing
                , islOccurs = Nothing
                }
            ]
        , islImports = V.empty
        }
      code = generateISLTypes schema
  assertBool "contains Maybe Int64" ("Maybe Int64" `T.isInfixOf` code)

testConstraintComments :: TestTree
testConstraintComments = testCase "field constraints appear as comments" $ do
  let schema = ISLSchema
        { islTypes = V.fromList
            [ ISLType
                { islTypeName = "metric"
                , islBaseType = Just "struct"
                , islFields = Just (V.fromList
                    [ ISLField "value" (ISLType "t" (Just "int") Nothing (Just (RangeVal (Just 0) (Just 100))) Nothing)
                    ])
                , islValidValues = Nothing
                , islOccurs = Nothing
                }
            ]
        , islImports = V.empty
        }
      code = generateISLTypes schema
  assertBool "contains range comment" ("range:" `T.isInfixOf` code)

testIonInstances :: TestTree
testIonInstances = testCase "generates ToIon/FromIon instances for enums" $ do
  let schema = ISLSchema
        { islTypes = V.fromList
            [ ISLType
                { islTypeName = "status"
                , islBaseType = Just "symbol"
                , islFields = Nothing
                , islValidValues = Just (EnumVal (V.fromList ["active", "inactive"]))
                , islOccurs = Nothing
                }
            ]
        , islImports = V.empty
        }
      code = generateISLTypes schema
  assertBool "contains ToIon instance" ("instance ToIon Status" `T.isInfixOf` code)
  assertBool "contains FromIon instance" ("instance FromIon Status" `T.isInfixOf` code)
