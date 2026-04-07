module Test.ASN1CodeGen (asn1CodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Vector as V

import ASN1.Schema
import ASN1.CodeGen (generateASN1Types)

asn1CodeGenTests :: TestTree
asn1CodeGenTests = testGroup "ASN1.CodeGen"
  [ testSequenceCodeGen
  , testChoiceCodeGen
  , testEnumeratedCodeGen
  , testOptionalField
  , testSequenceOfField
  ]

testSequenceCodeGen :: TestTree
testSequenceCodeGen = testCase "SEQUENCE produces record type" $ do
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
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "personName is Text" ("!Text" `T.isInfixOf` code)
  assertBool "personAge is Int64" ("!Int64" `T.isInfixOf` code)

testChoiceCodeGen :: TestTree
testChoiceCodeGen = testCase "CHOICE produces sum type" $ do
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
  assertBool "contains data Id" ("data Id" `T.isInfixOf` code)
  assertBool "contains IdName" ("IdName" `T.isInfixOf` code)
  assertBool "contains IdNumber" ("IdNumber" `T.isInfixOf` code)

testEnumeratedCodeGen :: TestTree
testEnumeratedCodeGen = testCase "ENUMERATED produces enum type" $ do
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
  assertBool "contains data Status" ("data Status" `T.isInfixOf` code)
  assertBool "contains StatusActive" ("StatusActive" `T.isInfixOf` code)
  assertBool "contains StatusInactive" ("StatusInactive" `T.isInfixOf` code)
  assertBool "contains StatusDeleted" ("StatusDeleted" `T.isInfixOf` code)

testOptionalField :: TestTree
testOptionalField = testCase "OPTIONAL fields produce Maybe types" $ do
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
  assertBool "contains Maybe Text" ("Maybe Text" `T.isInfixOf` code)

testSequenceOfField :: TestTree
testSequenceOfField = testCase "SEQUENCE OF produces Vector type" $ do
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
  assertBool "contains Vector Text" ("Vector Text" `T.isInfixOf` code)
