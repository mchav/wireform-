module Test.CDDLCodeGen (cddlCodeGenTests) where

import CBOR.CDDLCodeGen (generateCDDLTypes)
import CBOR.CDDLSchema
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


cddlCodeGenTests :: Spec
cddlCodeGenTests =
  describe "CBOR.CDDLCodeGen" $
    sequence_
      [ testMapRecordCodeGen
      , testArrayNewtypeCodeGen
      , testChoiceCodeGen
      , testOptionalFieldCodeGen
      , testCBORInstances
      ]


testMapRecordCodeGen :: Spec
testMapRecordCodeGen = it "map rule produces record type" $ do
  let schema =
        CDDLSchema
          ( V.fromList
              [ CDDLRule
                  "person"
                  ( CTMap
                      ( V.fromList
                          [ CDDLMember "name" CTTstr Once
                          , CDDLMember "age" CTUint Once
                          ]
                      )
                  )
              ]
          )
      code = generateCDDLTypes schema
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Word64" `T.isInfixOf` code) `shouldBe` True


testArrayNewtypeCodeGen :: Spec
testArrayNewtypeCodeGen = it "array rule produces newtype over Vector" $ do
  let schema =
        CDDLSchema
          ( V.fromList
              [ CDDLRule
                  "tags"
                  ( CTArray
                      ( V.fromList
                          [ CDDLMember "tag" CTTstr ZeroOrMore
                          ]
                      )
                  )
              ]
          )
      code = generateCDDLTypes schema
  ("newtype Tags = Tags" `T.isInfixOf` code) `shouldBe` True
  ("Vector" `T.isInfixOf` code) `shouldBe` True


testChoiceCodeGen :: Spec
testChoiceCodeGen = it "choice rule produces sum type" $ do
  let schema =
        CDDLSchema
          ( V.fromList
              [ CDDLRule "value" (CTChoice (V.fromList [CTTstr, CTUint, CTBool]))
              ]
          )
      code = generateCDDLTypes schema
  ("data Value" `T.isInfixOf` code) `shouldBe` True
  ("ValueAlt0" `T.isInfixOf` code) `shouldBe` True
  ("ValueAlt1" `T.isInfixOf` code) `shouldBe` True
  ("ValueAlt2" `T.isInfixOf` code) `shouldBe` True


testOptionalFieldCodeGen :: Spec
testOptionalFieldCodeGen = it "optional members produce Maybe" $ do
  let schema =
        CDDLSchema
          ( V.fromList
              [ CDDLRule
                  "config"
                  ( CTMap
                      ( V.fromList
                          [ CDDLMember "host" CTTstr Once
                          , CDDLMember "port" CTUint Optional
                          ]
                      )
                  )
              ]
          )
      code = generateCDDLTypes schema
  ("Maybe Word64" `T.isInfixOf` code) `shouldBe` True


testCBORInstances :: Spec
testCBORInstances = it "generates ToCBOR/FromCBOR stub instances" $ do
  let schema =
        CDDLSchema
          ( V.fromList
              [ CDDLRule
                  "msg"
                  ( CTMap
                      ( V.fromList
                          [ CDDLMember "body" CTBstr Once
                          ]
                      )
                  )
              ]
          )
      code = generateCDDLTypes schema
  ("instance ToCBOR Msg" `T.isInfixOf` code) `shouldBe` True
  ("instance FromCBOR Msg" `T.isInfixOf` code) `shouldBe` True
