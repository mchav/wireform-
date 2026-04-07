module Test.BondCodeGen (bondCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import Bond.Schema
import Bond.CodeGen (generateBondTypes)

bondCodeGenTests :: TestTree
bondCodeGenTests = testGroup "Bond.CodeGen"
  [ testStructCodeGen
  , testEnumCodeGen
  , testOptionalFieldCodeGen
  , testContainerFieldCodeGen
  ]

testStructCodeGen :: TestTree
testStructCodeGen = testCase "generates struct data type from Bond schema" $ do
  let schema = BondSchema
        { bondNamespace = Just "example"
        , bondImports = []
        , bondDecls =
            [ BondDeclStruct (BondStruct
                { bsName = "Person"
                , bsTypeParam = Nothing
                , bsFields =
                    [ BondField 1 BondRequired BFTString "name" Nothing mempty
                    , BondField 2 BondRequired BFTInt32 "age" Nothing mempty
                    ]
                , bsAttributes = mempty
                })
            ]
        }
      code = generateBondTypes schema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "personName is Text" ("!Text" `T.isInfixOf` code)
  assertBool "personAge is Int32" ("!Int32" `T.isInfixOf` code)
  assertBool "contains ToBond instance" ("instance ToBond Person" `T.isInfixOf` code)
  assertBool "contains FromBond instance" ("instance FromBond Person" `T.isInfixOf` code)

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "generates enum sum type from Bond schema" $ do
  let schema = BondSchema
        { bondNamespace = Nothing
        , bondImports = []
        , bondDecls =
            [ BondDeclEnum (BondEnum
                { beName = "Color"
                , beValues =
                    [ BondEnumValue "RED" (Just 0)
                    , BondEnumValue "GREEN" (Just 1)
                    , BondEnumValue "BLUE" (Just 2)
                    ]
                })
            ]
        }
      code = generateBondTypes schema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)
  assertBool "contains ToBond instance" ("instance ToBond Color" `T.isInfixOf` code)
  assertBool "contains FromBond instance" ("instance FromBond Color" `T.isInfixOf` code)

testOptionalFieldCodeGen :: TestTree
testOptionalFieldCodeGen = testCase "optional fields produce Maybe types" $ do
  let schema = BondSchema
        { bondNamespace = Nothing
        , bondImports = []
        , bondDecls =
            [ BondDeclStruct (BondStruct
                { bsName = "OptStruct"
                , bsTypeParam = Nothing
                , bsFields =
                    [ BondField 1 BondOptional BFTInt64 "value" Nothing mempty
                    ]
                , bsAttributes = mempty
                })
            ]
        }
      code = generateBondTypes schema
  assertBool "contains Maybe Int64" ("Maybe Int64" `T.isInfixOf` code)

testContainerFieldCodeGen :: TestTree
testContainerFieldCodeGen = testCase "containers produce Vector/Map types" $ do
  let schema = BondSchema
        { bondNamespace = Nothing
        , bondImports = []
        , bondDecls =
            [ BondDeclStruct (BondStruct
                { bsName = "Container"
                , bsTypeParam = Nothing
                , bsFields =
                    [ BondField 1 BondRequired (BFTList BFTString) "items" Nothing mempty
                    , BondField 2 BondRequired (BFTMap BFTString BFTInt32) "labels" Nothing mempty
                    ]
                , bsAttributes = mempty
                })
            ]
        }
      code = generateBondTypes schema
  assertBool "list -> Vector Text" ("Vector Text" `T.isInfixOf` code)
  assertBool "map -> Map Text Int32" ("Map Text Int32" `T.isInfixOf` code)
