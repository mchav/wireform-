module Test.ISLCodeGen (islCodeGenTests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Ion.ISLCodeGen (generateISLTypes)
import Ion.ISLSchema
import Test.Syd


islCodeGenTests :: Spec
islCodeGenTests =
  describe "Ion.ISLCodeGen" $
    sequence_
      [ testStructCodeGen
      , testEnumCodeGen
      , testOptionalFieldCodeGen
      , testConstraintComments
      , testIonInstances
      ]


testStructCodeGen :: Spec
testStructCodeGen = it "struct type produces record with ToIon/FromIon" $ do
  let schema =
        ISLSchema
          { islTypes =
              V.fromList
                [ ISLType
                    { islTypeName = "person"
                    , islBaseType = Just "struct"
                    , islFields =
                        Just
                          ( V.fromList
                              [ ISLField "name" (ISLType "name_type" (Just "string") Nothing Nothing Nothing)
                              , ISLField "age" (ISLType "age_type" (Just "int") Nothing Nothing Nothing)
                              ]
                          )
                    , islValidValues = Nothing
                    , islOccurs = Nothing
                    }
                ]
          , islImports = V.empty
          }
      code = generateISLTypes schema
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int64" `T.isInfixOf` code) `shouldBe` True
  ("instance ToIon Person" `T.isInfixOf` code) `shouldBe` True
  ("instance FromIon Person" `T.isInfixOf` code) `shouldBe` True


testEnumCodeGen :: Spec
testEnumCodeGen = it "enum constraint produces sum type" $ do
  let schema =
        ISLSchema
          { islTypes =
              V.fromList
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
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True


testOptionalFieldCodeGen :: Spec
testOptionalFieldCodeGen = it "optional occurs produces Maybe" $ do
  let schema =
        ISLSchema
          { islTypes =
              V.fromList
                [ ISLType
                    { islTypeName = "config"
                    , islBaseType = Just "struct"
                    , islFields =
                        Just
                          ( V.fromList
                              [ ISLField "host" (ISLType "t" (Just "string") Nothing Nothing Nothing)
                              , ISLField "port" (ISLType "t" (Just "int") Nothing Nothing (Just OOptional))
                              ]
                          )
                    , islValidValues = Nothing
                    , islOccurs = Nothing
                    }
                ]
          , islImports = V.empty
          }
      code = generateISLTypes schema
  ("Maybe Int64" `T.isInfixOf` code) `shouldBe` True


testConstraintComments :: Spec
testConstraintComments = it "field constraints appear as comments" $ do
  let schema =
        ISLSchema
          { islTypes =
              V.fromList
                [ ISLType
                    { islTypeName = "metric"
                    , islBaseType = Just "struct"
                    , islFields =
                        Just
                          ( V.fromList
                              [ ISLField "value" (ISLType "t" (Just "int") Nothing (Just (RangeVal (Just 0) (Just 100))) Nothing)
                              ]
                          )
                    , islValidValues = Nothing
                    , islOccurs = Nothing
                    }
                ]
          , islImports = V.empty
          }
      code = generateISLTypes schema
  ("range:" `T.isInfixOf` code) `shouldBe` True


testIonInstances :: Spec
testIonInstances = it "generates ToIon/FromIon instances for enums" $ do
  let schema =
        ISLSchema
          { islTypes =
              V.fromList
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
  ("instance ToIon Status" `T.isInfixOf` code) `shouldBe` True
  ("instance FromIon Status" `T.isInfixOf` code) `shouldBe` True
