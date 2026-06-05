module Test.CapnProtoCodeGen (capnProtoCodeGenTests) where

import Test.Syd

import qualified Data.Text as T
import qualified Data.Vector as V

import CapnProto.Schema
import CapnProto.CodeGen (generateCapnProtoTypes)

capnProtoCodeGenTests :: Spec
capnProtoCodeGenTests = describe "CapnProto.CodeGen" $ sequence_
  [ testStructCodeGen
  , testEnumCodeGen
  , testNestedStructCodeGen
  , testListFieldCodeGen
  ]

testStructCodeGen :: Spec
testStructCodeGen = it "generates struct data type from Cap'n Proto schema" $ do
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
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int32" `T.isInfixOf` code) `shouldBe` True

testEnumCodeGen :: Spec
testEnumCodeGen = it "generates enum sum type from Cap'n Proto schema" $ do
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
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True
  ("Enum" `T.isInfixOf` code) `shouldBe` True

testNestedStructCodeGen :: Spec
testNestedStructCodeGen = it "nested structs become separate types" $ do
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
  ("data Outer = Outer" `T.isInfixOf` code) `shouldBe` True
  ("data Inner = Inner" `T.isInfixOf` code) `shouldBe` True

testListFieldCodeGen :: Spec
testListFieldCodeGen = it "lists -> Vector fields" $ do
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
  ("Vector Text" `T.isInfixOf` code) `shouldBe` True
