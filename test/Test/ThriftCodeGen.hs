module Test.ThriftCodeGen (thriftCodeGenTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T

import Thrift.Schema
import Thrift.CodeGen (generateThriftTypes)

thriftCodeGenTests :: TestTree
thriftCodeGenTests = testGroup "Thrift.CodeGen"
  [ testStructCodeGen
  , testEnumCodeGen
  , testOptionalFieldCodeGen
  , testListSetMapCodeGen
  , testServiceCodeGen
  ]

testStructCodeGen :: TestTree
testStructCodeGen = testCase "generates struct data type from Thrift schema" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Person"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "name" TString Required Nothing
              , ThriftField 2 "age" TI32 Optional Nothing
              , ThriftField 3 "tags" (TList TString) Default Nothing
              ]
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  assertBool "contains data Person" ("data Person = Person" `T.isInfixOf` code)
  assertBool "contains personName field" ("personName" `T.isInfixOf` code)
  assertBool "personName is Text" ("!Text" `T.isInfixOf` code)
  assertBool "personAge is Maybe" ("Maybe Int32" `T.isInfixOf` code)
  assertBool "personTags is Vector" ("Vector" `T.isInfixOf` code)
  assertBool "contains ToThrift instance" ("instance ToThrift Person" `T.isInfixOf` code)
  assertBool "contains FromThrift instance" ("instance FromThrift Person" `T.isInfixOf` code)

testEnumCodeGen :: TestTree
testEnumCodeGen = testCase "generates enum sum type from Thrift schema" $ do
  let schema = ThriftSchema
        { tsStructs = []
        , tsEnums = [ThriftEnum
            { teName = "Color"
            , teValues = [("RED", 1), ("GREEN", 2), ("BLUE", 3)]
            }]
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  assertBool "contains data Color" ("data Color" `T.isInfixOf` code)
  assertBool "contains ColorRed" ("ColorRed" `T.isInfixOf` code)
  assertBool "contains ColorGreen" ("ColorGreen" `T.isInfixOf` code)
  assertBool "contains ColorBlue" ("ColorBlue" `T.isInfixOf` code)
  assertBool "contains ToThrift instance" ("instance ToThrift Color" `T.isInfixOf` code)
  assertBool "contains FromThrift instance" ("instance FromThrift Color" `T.isInfixOf` code)

testOptionalFieldCodeGen :: TestTree
testOptionalFieldCodeGen = testCase "optional fields produce Maybe types" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Optional"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "value" TI64 Optional Nothing
              ]
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  assertBool "contains Maybe Int64" ("Maybe Int64" `T.isInfixOf` code)

testListSetMapCodeGen :: TestTree
testListSetMapCodeGen = testCase "lists -> Vector, sets -> Vector, maps -> Map" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Container"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "items" (TList TString) Default Nothing
              , ThriftField 2 "unique_items" (TSet TI32) Default Nothing
              , ThriftField 3 "labels" (TMap TString TI64) Default Nothing
              ]
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  assertBool "list -> Vector Text" ("Vector Text" `T.isInfixOf` code)
  assertBool "set -> Vector Int32" ("Vector Int32" `T.isInfixOf` code)
  assertBool "map -> Map Text Int64" ("Map Text Int64" `T.isInfixOf` code)

testServiceCodeGen :: TestTree
testServiceCodeGen = testCase "services produce method descriptors" $ do
  let schema = ThriftSchema
        { tsStructs = []
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = [ThriftService
            { tsvName = "UserService"
            , tsvExtends = Nothing
            , tsvMethods =
              [ ThriftMethod "getUser" (Just (TStruct "User")) [ThriftField 1 "id" TI64 Required Nothing] [] False
              , ThriftMethod "deleteUser" Nothing [ThriftField 1 "id" TI64 Required Nothing] [] False
              ]
            }]
        }
      code = generateThriftTypes schema
  assertBool "contains service name" ("UserService" `T.isInfixOf` code)
  assertBool "contains method descriptor type" ("UserServiceMethod" `T.isInfixOf` code)
  assertBool "contains GetUser method" ("GetUser" `T.isInfixOf` code || "getUser" `T.isInfixOf` code)
  assertBool "contains service name binding" ("UserServiceServiceName" `T.isInfixOf` code)
