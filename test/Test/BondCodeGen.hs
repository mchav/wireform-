module Test.BondCodeGen (bondCodeGenTests) where

import Test.Syd

import qualified Data.Text as T

import Bond.Schema
import Bond.CodeGen (generateBondTypes)

bondCodeGenTests :: Spec
bondCodeGenTests = describe "Bond.CodeGen" $ sequence_
  [ testStructCodeGen
  , testEnumCodeGen
  , testOptionalFieldCodeGen
  , testContainerFieldCodeGen
  ]

testStructCodeGen :: Spec
testStructCodeGen = it "generates struct data type from Bond schema" $ do
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
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int32" `T.isInfixOf` code) `shouldBe` True
  ("instance ToBond Person" `T.isInfixOf` code) `shouldBe` True
  ("instance FromBond Person" `T.isInfixOf` code) `shouldBe` True

testEnumCodeGen :: Spec
testEnumCodeGen = it "generates enum sum type from Bond schema" $ do
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
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True
  ("instance ToBond Color" `T.isInfixOf` code) `shouldBe` True
  ("instance FromBond Color" `T.isInfixOf` code) `shouldBe` True

testOptionalFieldCodeGen :: Spec
testOptionalFieldCodeGen = it "optional fields produce Maybe types" $ do
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
  ("Maybe Int64" `T.isInfixOf` code) `shouldBe` True

testContainerFieldCodeGen :: Spec
testContainerFieldCodeGen = it "containers produce Vector/Map types" $ do
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
  ("Vector Text" `T.isInfixOf` code) `shouldBe` True
  ("Map Text Int32" `T.isInfixOf` code) `shouldBe` True
