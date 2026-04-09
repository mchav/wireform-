module Test.CDDLCodeGen (cddlCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Vector as V

import CBOR.CDDLSchema
import CBOR.CDDLCodeGen (generateCDDLTypes)

cddlCodeGenTests :: TestTree
cddlCodeGenTests = testGroup "CBOR.CDDLCodeGen"
  [ testMapRecordCodeGen
  , testArrayNewtypeCodeGen
  , testChoiceCodeGen
  , testOptionalFieldCodeGen
  , testCBORInstances
  ]

testMapRecordCodeGen :: TestTree
testMapRecordCodeGen = testCase "map rule produces record type" $ do
  let schema = CDDLSchema (V.fromList
        [ CDDLRule "person" (CTMap (V.fromList
            [ CDDLMember "name" CTTstr Once
            , CDDLMember "age" CTUint Once
            ]))
        ])
      code = generateCDDLTypes schema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "contains personAge field" ("personAge" `T.isInfixOf` code)
  assertBool "name is Text" ("!Text" `T.isInfixOf` code)
  assertBool "age is Word64" ("!Word64" `T.isInfixOf` code)

testArrayNewtypeCodeGen :: TestTree
testArrayNewtypeCodeGen = testCase "array rule produces newtype over Vector" $ do
  let schema = CDDLSchema (V.fromList
        [ CDDLRule "tags" (CTArray (V.fromList
            [ CDDLMember "tag" CTTstr ZeroOrMore
            ]))
        ])
      code = generateCDDLTypes schema
  assertBool "contains newtype Tags" ("newtype Tags = Tags" `T.isInfixOf` code)
  assertBool "contains Vector" ("Vector" `T.isInfixOf` code)

testChoiceCodeGen :: TestTree
testChoiceCodeGen = testCase "choice rule produces sum type" $ do
  let schema = CDDLSchema (V.fromList
        [ CDDLRule "value" (CTChoice (V.fromList [CTTstr, CTUint, CTBool]))
        ])
      code = generateCDDLTypes schema
  assertBool "contains data Value" ("data Value" `T.isInfixOf` code)
  assertBool "contains ValueAlt0" ("ValueAlt0" `T.isInfixOf` code)
  assertBool "contains ValueAlt1" ("ValueAlt1" `T.isInfixOf` code)
  assertBool "contains ValueAlt2" ("ValueAlt2" `T.isInfixOf` code)

testOptionalFieldCodeGen :: TestTree
testOptionalFieldCodeGen = testCase "optional members produce Maybe" $ do
  let schema = CDDLSchema (V.fromList
        [ CDDLRule "config" (CTMap (V.fromList
            [ CDDLMember "host" CTTstr Once
            , CDDLMember "port" CTUint Optional
            ]))
        ])
      code = generateCDDLTypes schema
  assertBool "contains Maybe Word64" ("Maybe Word64" `T.isInfixOf` code)

testCBORInstances :: TestTree
testCBORInstances = testCase "generates ToCBOR/FromCBOR stub instances" $ do
  let schema = CDDLSchema (V.fromList
        [ CDDLRule "msg" (CTMap (V.fromList
            [ CDDLMember "body" CTBstr Once
            ]))
        ])
      code = generateCDDLTypes schema
  assertBool "contains ToCBOR instance" ("instance ToCBOR Msg" `T.isInfixOf` code)
  assertBool "contains FromCBOR instance" ("instance FromCBOR Msg" `T.isInfixOf` code)
