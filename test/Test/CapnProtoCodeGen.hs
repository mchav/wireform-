module Test.CapnProtoCodeGen (capnProtoCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Vector as V

import CapnProto.Schema
import CapnProto.CodeGen (generateCapnProtoTypes)

capnProtoCodeGenTests :: TestTree
capnProtoCodeGenTests = testGroup "CapnProto.CodeGen"
  [ testStructCodeGen
  , testEnumCodeGen
  , testNestedStructCodeGen
  , testListFieldCodeGen
  ]

testStructCodeGen :: TestTree
testStructCodeGen = testCase "generates struct data type from Cap'n Proto schema" $ do
  let schema = CapnProtoSchema
        { csFileId = Nothing
        , csImports = V.empty
        , csDecls = V.fromList
            [ DStruct (StructDef
                { sdName = "Person"
                , sdFields = V.fromList
                    [ FieldDef "name" 0 CTText Nothing V.empty
                    , FieldDef "age" 1 CTInt32 Nothing V.empty
                    ]
                , sdNested = V.empty
                , sdUnions = V.empty
                })
            ]
        }
      code = generateCapnProtoTypes schema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "personName is Text" ("!Text" `T.isInfixOf` code)
  assertBool "personAge is Int32" ("!Int32" `T.isInfixOf` code)

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "generates enum sum type from Cap'n Proto schema" $ do
  let schema = CapnProtoSchema
        { csFileId = Nothing
        , csImports = V.empty
        , csDecls = V.fromList
            [ DEnum (EnumDef
                { edName = "Color"
                , edValues = V.fromList [("red", 0), ("green", 1), ("blue", 2)]
                })
            ]
        }
      code = generateCapnProtoTypes schema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)
  assertBool "contains Enum deriving" ("Enum" `T.isInfixOf` code)

testNestedStructCodeGen :: TestTree
testNestedStructCodeGen = testCase "nested structs become separate types" $ do
  let schema = CapnProtoSchema
        { csFileId = Nothing
        , csImports = V.empty
        , csDecls = V.fromList
            [ DStruct (StructDef
                { sdName = "Outer"
                , sdFields = V.fromList
                    [ FieldDef "value" 0 CTInt32 Nothing V.empty
                    ]
                , sdNested = V.fromList
                    [ DStruct (StructDef
                        { sdName = "Inner"
                        , sdFields = V.fromList
                            [ FieldDef "data" 0 CTData Nothing V.empty
                            ]
                        , sdNested = V.empty
                        , sdUnions = V.empty
                        })
                    ]
                , sdUnions = V.empty
                })
            ]
        }
      code = generateCapnProtoTypes schema
  assertBool "contains data Outer" ("data Outer = Outer" `T.isInfixOf` code)
  assertBool "contains data Inner" ("data Inner = Inner" `T.isInfixOf` code)

testListFieldCodeGen :: TestTree
testListFieldCodeGen = testCase "lists -> Vector fields" $ do
  let schema = CapnProtoSchema
        { csFileId = Nothing
        , csImports = V.empty
        , csDecls = V.fromList
            [ DStruct (StructDef
                { sdName = "WithList"
                , sdFields = V.fromList
                    [ FieldDef "items" 0 (CTList CTText) Nothing V.empty
                    ]
                , sdNested = V.empty
                , sdUnions = V.empty
                })
            ]
        }
      code = generateCapnProtoTypes schema
  assertBool "contains Vector Text" ("Vector Text" `T.isInfixOf` code)
