module Test.ASN1CodeGen (asn1CodeGenTests) where

import Test.Syd

import qualified Data.Text as T
import qualified Data.Vector as V

import ASN1.Schema
import ASN1.CodeGen (generateASN1Types)

asn1CodeGenTests :: Spec
asn1CodeGenTests = describe "ASN1.CodeGen" $ sequence_
  [ testSequenceCodeGen
  , testChoiceCodeGen
  , testEnumeratedCodeGen
  , testOptionalField
  , testSequenceOfField
  ]

testSequenceCodeGen :: Spec
testSequenceCodeGen = it "SEQUENCE produces record type" $ do
  let modl = ASN1Module
        { asnModuleName = "TestModule"
        , asnTagMode = AutomaticTags
        , asnAssignments = V.fromList
            [ TypeAssignment "Person" (TDSequence (V.fromList
                [ ComponentType "name" TDUTF8String False
                , ComponentType "age" (TDInteger Nothing) False
                ]))
            ]
        }
      code = generateASN1Types modl
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("personAge" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("!Int64" `T.isInfixOf` code) `shouldBe` True

testChoiceCodeGen :: Spec
testChoiceCodeGen = it "CHOICE produces sum type" $ do
  let modl = ASN1Module
        { asnModuleName = "TestModule"
        , asnTagMode = AutomaticTags
        , asnAssignments = V.fromList
            [ TypeAssignment "Id" (TDChoice (V.fromList
                [ ComponentType "name" TDUTF8String False
                , ComponentType "number" (TDInteger Nothing) False
                ]))
            ]
        }
      code = generateASN1Types modl
  ("data Id" `T.isInfixOf` code) `shouldBe` True
  ("IdName" `T.isInfixOf` code) `shouldBe` True
  ("IdNumber" `T.isInfixOf` code) `shouldBe` True

testEnumeratedCodeGen :: Spec
testEnumeratedCodeGen = it "ENUMERATED produces enum type" $ do
  let modl = ASN1Module
        { asnModuleName = "TestModule"
        , asnTagMode = AutomaticTags
        , asnAssignments = V.fromList
            [ TypeAssignment "Status" (TDEnumerated (V.fromList
                [ ("active", Just 0)
                , ("inactive", Just 1)
                , ("deleted", Just 2)
                ]))
            ]
        }
      code = generateASN1Types modl
  ("data Status" `T.isInfixOf` code) `shouldBe` True
  ("StatusActive" `T.isInfixOf` code) `shouldBe` True
  ("StatusInactive" `T.isInfixOf` code) `shouldBe` True
  ("StatusDeleted" `T.isInfixOf` code) `shouldBe` True

testOptionalField :: Spec
testOptionalField = it "OPTIONAL fields produce Maybe types" $ do
  let modl = ASN1Module
        { asnModuleName = "TestModule"
        , asnTagMode = AutomaticTags
        , asnAssignments = V.fromList
            [ TypeAssignment "WithOpt" (TDSequence (V.fromList
                [ ComponentType "required_field" TDUTF8String False
                , ComponentType "optional_field" (TDOptional TDUTF8String) True
                ]))
            ]
        }
      code = generateASN1Types modl
  ("Maybe Text" `T.isInfixOf` code) `shouldBe` True

testSequenceOfField :: Spec
testSequenceOfField = it "SEQUENCE OF produces Vector type" $ do
  let modl = ASN1Module
        { asnModuleName = "TestModule"
        , asnTagMode = AutomaticTags
        , asnAssignments = V.fromList
            [ TypeAssignment "WithList" (TDSequence (V.fromList
                [ ComponentType "items" (TDSequenceOf TDUTF8String) False
                ]))
            ]
        }
      code = generateASN1Types modl
  ("Vector Text" `T.isInfixOf` code) `shouldBe` True
