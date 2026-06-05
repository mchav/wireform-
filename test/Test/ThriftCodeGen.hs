module Test.ThriftCodeGen (thriftCodeGenTests) where

import Test.Syd

import qualified Data.Text as T
import qualified Data.Vector as V

import Thrift.Schema
import Thrift.CodeGen (generateThriftTypes)

thriftCodeGenTests :: Spec
thriftCodeGenTests = describe "Thrift.CodeGen" $ sequence_
  [ testStructCodeGen
  , testEnumCodeGen
  , testOptionalFieldCodeGen
  , testListSetMapCodeGen
  , testServiceCodeGen
  , testTypedefCodeGen
  , testConstCodeGen
  ]

testStructCodeGen :: Spec
testStructCodeGen = it "generates struct data type from Thrift schema" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Person"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "name" TString Required Nothing V.empty
              , ThriftField 2 "age" TI32 Optional Nothing V.empty
              , ThriftField 3 "tags" (TList TString) Default Nothing V.empty
              ]
            , tsAnnotations = V.empty
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  ("data Person = Person" `T.isInfixOf` code) `shouldBe` True
  ("personName" `T.isInfixOf` code) `shouldBe` True
  ("!Text" `T.isInfixOf` code) `shouldBe` True
  ("Maybe Int32" `T.isInfixOf` code) `shouldBe` True
  ("Vector" `T.isInfixOf` code) `shouldBe` True
  ("instance ToThrift Person" `T.isInfixOf` code) `shouldBe` True
  ("instance FromThrift Person" `T.isInfixOf` code) `shouldBe` True

testEnumCodeGen :: Spec
testEnumCodeGen = it "generates enum sum type from Thrift schema" $ do
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
  ("data Color" `T.isInfixOf` code) `shouldBe` True
  ("ColorRed" `T.isInfixOf` code) `shouldBe` True
  ("ColorGreen" `T.isInfixOf` code) `shouldBe` True
  ("ColorBlue" `T.isInfixOf` code) `shouldBe` True
  ("instance ToThrift Color" `T.isInfixOf` code) `shouldBe` True
  ("instance FromThrift Color" `T.isInfixOf` code) `shouldBe` True

testOptionalFieldCodeGen :: Spec
testOptionalFieldCodeGen = it "optional fields produce Maybe types" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Optional"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "value" TI64 Optional Nothing V.empty
              ]
            , tsAnnotations = V.empty
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  ("Maybe Int64" `T.isInfixOf` code) `shouldBe` True

testListSetMapCodeGen :: Spec
testListSetMapCodeGen = it "lists -> Vector, sets -> Vector, maps -> Map" $ do
  let schema = ThriftSchema
        { tsStructs = [ThriftStruct
            { tsName = "Container"
            , tsKind = StructNormal
            , tsFields =
              [ ThriftField 1 "items" (TList TString) Default Nothing V.empty
              , ThriftField 2 "unique_items" (TSet TI32) Default Nothing V.empty
              , ThriftField 3 "labels" (TMap TString TI64) Default Nothing V.empty
              ]
            , tsAnnotations = V.empty
            }]
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  ("Vector Text" `T.isInfixOf` code) `shouldBe` True
  ("Vector Int32" `T.isInfixOf` code) `shouldBe` True
  ("Map Text Int64" `T.isInfixOf` code) `shouldBe` True

testServiceCodeGen :: Spec
testServiceCodeGen = it "services produce method descriptors" $ do
  let schema = ThriftSchema
        { tsStructs = []
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts = []
        , tsServices = [ThriftService
            { tsvName = "UserService"
            , tsvExtends = Nothing
            , tsvMethods =
              [ ThriftMethod "getUser" (Just (TStruct "User")) [ThriftField 1 "id" TI64 Required Nothing V.empty] [] False
              , ThriftMethod "deleteUser" Nothing [ThriftField 1 "id" TI64 Required Nothing V.empty] [] False
              ]
            }]
        }
      code = generateThriftTypes schema
  ("UserService" `T.isInfixOf` code) `shouldBe` True
  ("UserServiceMethod" `T.isInfixOf` code) `shouldBe` True
  ("GetUser" `T.isInfixOf` code || "getUser" `T.isInfixOf` code) `shouldBe` True
  ("UserServiceServiceName" `T.isInfixOf` code) `shouldBe` True

testTypedefCodeGen :: Spec
testTypedefCodeGen = it "generates type synonym for typedef" $ do
  let schema = ThriftSchema
        { tsStructs = []
        , tsEnums = []
        , tsTypedefs =
          [ ThriftTypedef "UserId" TI64
          , ThriftTypedef "UserName" TString
          ]
        , tsConsts = []
        , tsServices = []
        }
      code = generateThriftTypes schema
  ("type UserId = Int64" `T.isInfixOf` code) `shouldBe` True
  ("type UserName = Text" `T.isInfixOf` code) `shouldBe` True

testConstCodeGen :: Spec
testConstCodeGen = it "generates constant values for consts" $ do
  let schema = ThriftSchema
        { tsStructs = []
        , tsEnums = []
        , tsTypedefs = []
        , tsConsts =
          [ ThriftConst "MAX_RETRIES" TI32 (TCVInt 3)
          , ThriftConst "DEFAULT_NAME" TString (TCVString "guest")
          ]
        , tsServices = []
        }
      code = generateThriftTypes schema
  ("maxRetries :: Int32" `T.isInfixOf` code) `shouldBe` True
  ("maxRetries = 3" `T.isInfixOf` code) `shouldBe` True
  ("defaultName :: Text" `T.isInfixOf` code) `shouldBe` True
  ("defaultName = \"guest\"" `T.isInfixOf` code) `shouldBe` True
