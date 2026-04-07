module Test.FlatBuffersCodeGen (flatBuffersCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Vector as V

import FlatBuffers.Schema
import FlatBuffers.CodeGen (generateFlatBuffersTypes)

flatBuffersCodeGenTests :: TestTree
flatBuffersCodeGenTests = testGroup "FlatBuffers.CodeGen"
  [ testTableCodeGen
  , testStructCodeGen
  , testEnumCodeGen
  , testUnionCodeGen
  , testTableOptionalFields
  ]

emptySchema :: FlatBuffersSchema
emptySchema = FlatBuffersSchema
  { fbsNamespace = Nothing
  , fbsIncludes = V.empty
  , fbsDecls = V.empty
  , fbsRootType = Nothing
  , fbsFileIdentifier = Nothing
  , fbsFileExtension = Nothing
  , fbsAttributes = V.empty
  }

testTableCodeGen :: TestTree
testTableCodeGen = testCase "generates table record with Maybe for optional fields" $ do
  let schema = emptySchema
        { fbsDecls = V.fromList
            [ FBTable (TableDef
                { tdName = "Monster"
                , tdFields = V.fromList
                    [ TableField "name" FTString Nothing False V.empty
                    , TableField "hp" FTInt (Just "100") False V.empty
                    ]
                })
            ]
        }
      code = generateFlatBuffersTypes schema
  assertBool "contains data Monster" ("data Monster = Monster" `T.isInfixOf` code)
  assertBool "contains monsterName field" ("monsterName" `T.isInfixOf` code)
  assertBool "contains monsterHp field" ("monsterHp" `T.isInfixOf` code)
  assertBool "name is Maybe (no default)" ("Maybe Text" `T.isInfixOf` code)
  assertBool "hp is strict (has default)" ("!Int32" `T.isInfixOf` code)

testStructCodeGen :: TestTree
testStructCodeGen = testCase "generates strict struct record" $ do
  let schema = emptySchema
        { fbsDecls = V.fromList
            [ FBStruct (FBStructDef
                { fsdName = "Vec3"
                , fsdFields = V.fromList
                    [ ("x", FTFloat)
                    , ("y", FTFloat)
                    , ("z", FTFloat)
                    ]
                })
            ]
        }
      code = generateFlatBuffersTypes schema
  assertBool "contains data Vec3" ("data Vec3 = Vec3" `T.isInfixOf` code)
  assertBool "contains vec3X" ("vec3X" `T.isInfixOf` code)
  assertBool "contains vec3Y" ("vec3Y" `T.isInfixOf` code)
  assertBool "contains vec3Z" ("vec3Z" `T.isInfixOf` code)
  assertBool "strict Float" ("!Float" `T.isInfixOf` code)

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "generates enum sum type" $ do
  let schema = emptySchema
        { fbsDecls = V.fromList
            [ FBEnum (FBEnumDef
                { fedName = "Color"
                , fedUnderlyingType = FTByte
                , fedValues = V.fromList
                    [ ("Red", Just 0)
                    , ("Green", Just 1)
                    , ("Blue", Just 2)
                    ]
                })
            ]
        }
      code = generateFlatBuffersTypes schema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)

testUnionCodeGen :: TestTree
testUnionCodeGen = testCase "generates union sum type" $ do
  let schema = emptySchema
        { fbsDecls = V.fromList
            [ FBUnion (FBUnionDef
                { fudName = "Equipment"
                , fudMembers = V.fromList ["Weapon", "Armor"]
                })
            ]
        }
      code = generateFlatBuffersTypes schema
  assertBool "contains data Equipment" ("data Equipment" `T.isInfixOf` code)
  assertBool "contains EquipmentWeapon" ("EquipmentWeapon" `T.isInfixOf` code)
  assertBool "contains EquipmentArmor" ("EquipmentArmor" `T.isInfixOf` code)

testTableOptionalFields :: TestTree
testTableOptionalFields = testCase "table fields without default get Maybe" $ do
  let schema = emptySchema
        { fbsDecls = V.fromList
            [ FBTable (TableDef
                { tdName = "Opt"
                , tdFields = V.fromList
                    [ TableField "value" FTLong Nothing False V.empty
                    ]
                })
            ]
        }
      code = generateFlatBuffersTypes schema
  assertBool "contains Maybe Int64" ("Maybe Int64" `T.isInfixOf` code)
