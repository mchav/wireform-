module Test.FlatBuffersCodeGen (flatBuffersCodeGenTests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import FlatBuffers.CodeGen (generateFlatBuffersTypes)
import FlatBuffers.Schema
import Test.Syd


flatBuffersCodeGenTests :: Spec
flatBuffersCodeGenTests =
  describe "FlatBuffers.CodeGen" $
    sequence_
      [ testTableCodeGen
      , testStructCodeGen
      , testEnumCodeGen
      , testUnionCodeGen
      , testTableOptionalFields
      ]


emptySchema :: FlatBuffersSchema
emptySchema =
  FlatBuffersSchema
    { fbsNamespace = Nothing
    , fbsIncludes = V.empty
    , fbsDecls = V.empty
    , fbsRootType = Nothing
    , fbsFileIdentifier = Nothing
    , fbsFileExtension = Nothing
    , fbsAttributes = V.empty
    }


testTableCodeGen :: Spec
testTableCodeGen = it "generates table record with Maybe for optional fields" $ do
  let schema =
        emptySchema
          { fbsDecls =
              V.fromList
                [ FBTable
                    ( TableDef
                        { tdName = "Monster"
                        , tdFields =
                            V.fromList
                              [ TableField "name" FTString Nothing False V.empty
                              , TableField "hp" FTInt (Just "100") False V.empty
                              ]
                        }
                    )
                ]
          }
      code = generateFlatBuffersTypes schema
  ("data Monster = Monster" `T.isInfixOf` code) `shouldBe` True
  ("monsterName" `T.isInfixOf` code) `shouldBe` True
  ("monsterHp" `T.isInfixOf` code) `shouldBe` True
  ("Maybe Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int32" `T.isInfixOf` code) `shouldBe` True


testStructCodeGen :: Spec
testStructCodeGen = it "generates strict struct record" $ do
  let schema =
        emptySchema
          { fbsDecls =
              V.fromList
                [ FBStruct
                    ( FBStructDef
                        { fsdName = "Vec3"
                        , fsdFields =
                            V.fromList
                              [ ("x", FTFloat)
                              , ("y", FTFloat)
                              , ("z", FTFloat)
                              ]
                        }
                    )
                ]
          }
      code = generateFlatBuffersTypes schema
  ("data Vec3 = Vec3" `T.isInfixOf` code) `shouldBe` True
  ("vec3X" `T.isInfixOf` code) `shouldBe` True
  ("vec3Y" `T.isInfixOf` code) `shouldBe` True
  ("vec3Z" `T.isInfixOf` code) `shouldBe` True
  ("!Float" `T.isInfixOf` code) `shouldBe` True


testEnumCodeGen :: Spec
testEnumCodeGen = it "generates enum sum type" $ do
  let schema =
        emptySchema
          { fbsDecls =
              V.fromList
                [ FBEnum
                    ( FBEnumDef
                        { fedName = "Color"
                        , fedUnderlyingType = FTByte
                        , fedValues =
                            V.fromList
                              [ ("Red", Just 0)
                              , ("Green", Just 1)
                              , ("Blue", Just 2)
                              ]
                        }
                    )
                ]
          }
      code = generateFlatBuffersTypes schema
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True


testUnionCodeGen :: Spec
testUnionCodeGen = it "generates union sum type" $ do
  let schema =
        emptySchema
          { fbsDecls =
              V.fromList
                [ FBUnion
                    ( FBUnionDef
                        { fudName = "Equipment"
                        , fudMembers = V.fromList ["Weapon", "Armor"]
                        }
                    )
                ]
          }
      code = generateFlatBuffersTypes schema
  ("data Equipment" `T.isInfixOf` code) `shouldBe` True
  ("EquipmentWeapon" `T.isInfixOf` code) `shouldBe` True
  ("EquipmentArmor" `T.isInfixOf` code) `shouldBe` True


testTableOptionalFields :: Spec
testTableOptionalFields = it "table fields without default get Maybe" $ do
  let schema =
        emptySchema
          { fbsDecls =
              V.fromList
                [ FBTable
                    ( TableDef
                        { tdName = "Opt"
                        , tdFields =
                            V.fromList
                              [ TableField "value" FTLong Nothing False V.empty
                              ]
                        }
                    )
                ]
          }
      code = generateFlatBuffersTypes schema
  ("Maybe Int64" `T.isInfixOf` code) `shouldBe` True
