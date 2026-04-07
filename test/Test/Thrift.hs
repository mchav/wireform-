module Test.Thrift (thriftTests) where

import qualified Data.ByteString as BS
import Data.Text (Text)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Decode (decodeBinary, decodeCompact)
import Thrift.JSON (thriftToJSON, thriftFromJSON, thriftToTypedJSON, thriftFromTypedJSON)
import Thrift.Value
import Thrift.Wire (ThriftType (..))

thriftTests :: TestTree
thriftTests = testGroup "Thrift Encode/Decode"
  [ propertyBinaryRoundtrip
  , propertyCompactRoundtrip
  , unitMixedStruct
  , unitNestedStructs
  , unitContainers
  , unitEmptyStructAndContainers
  , unitProtocolsDiffer
  , jsonTests
  ]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genText :: Gen Text
genText = Gen.text (Range.linear 0 32) Gen.unicode

wrapStruct :: ThriftValue -> ThriftValue
wrapStruct v = TVStruct [(1, v)]

--------------------------------------------------------------------------------
-- Property: Binary roundtrip for each primitive
--------------------------------------------------------------------------------

propertyBinaryRoundtrip :: TestTree
propertyBinaryRoundtrip = testGroup "Binary roundtrip (property)"
  [ testProperty "Bool" $ property $ do
      b <- forAll Gen.bool
      let v = wrapStruct (TVBool b)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Byte" $ property $ do
      x <- forAll $ Gen.int8 Range.linearBounded
      let v = wrapStruct (TVByte x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I16" $ property $ do
      x <- forAll $ Gen.int16 Range.linearBounded
      let v = wrapStruct (TVI16 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I32" $ property $ do
      x <- forAll $ Gen.int32 Range.linearBounded
      let v = wrapStruct (TVI32 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I64" $ property $ do
      x <- forAll $ Gen.int64 Range.linearBounded
      let v = wrapStruct (TVI64 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Double" $ property $ do
      x <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
      let v = wrapStruct (TVDouble x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "String" $ property $ do
      t <- forAll genText
      let v = wrapStruct (TVString t)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Binary" $ property $ do
      b <- forAll $ Gen.bytes (Range.linear 0 128)
      let v = wrapStruct (TVBinary b)
          encoded = encodeBinary v
          decoded = decodeBinary encoded
      case decoded of
        Right (TVStruct [(1, TVBinary b')]) -> b' === b
        Right (TVStruct [(1, TVString _)])  -> success
        other -> do
          annotate (show other)
          failure

  , testProperty "UUID" $ property $ do
      u <- forAll $ Gen.bytes (Range.singleton 16)
      let v = wrapStruct (TVUUID u)
      decodeBinary (encodeBinary v) === Right v
  ]

--------------------------------------------------------------------------------
-- Property: Compact roundtrip for each primitive
--------------------------------------------------------------------------------

propertyCompactRoundtrip :: TestTree
propertyCompactRoundtrip = testGroup "Compact roundtrip (property)"
  [ testProperty "Bool" $ property $ do
      b <- forAll Gen.bool
      let v = wrapStruct (TVBool b)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Byte" $ property $ do
      x <- forAll $ Gen.int8 Range.linearBounded
      let v = wrapStruct (TVByte x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I16" $ property $ do
      x <- forAll $ Gen.int16 Range.linearBounded
      let v = wrapStruct (TVI16 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I32" $ property $ do
      x <- forAll $ Gen.int32 Range.linearBounded
      let v = wrapStruct (TVI32 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I64" $ property $ do
      x <- forAll $ Gen.int64 Range.linearBounded
      let v = wrapStruct (TVI64 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Double" $ property $ do
      x <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
      let v = wrapStruct (TVDouble x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "String" $ property $ do
      t <- forAll genText
      let v = wrapStruct (TVString t)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Binary" $ property $ do
      b <- forAll $ Gen.bytes (Range.linear 0 128)
      let v = wrapStruct (TVBinary b)
          encoded = encodeCompact v
          decoded = decodeCompact encoded
      case decoded of
        Right (TVStruct [(1, TVBinary b')]) -> b' === b
        Right (TVStruct [(1, TVString _)])  -> success
        other -> do
          annotate (show other)
          failure

  , testProperty "UUID" $ property $ do
      u <- forAll $ Gen.bytes (Range.singleton 16)
      let v = wrapStruct (TVUUID u)
      decodeCompact (encodeCompact v) === Right v
  ]

--------------------------------------------------------------------------------
-- Unit: struct with mixed field types
--------------------------------------------------------------------------------

unitMixedStruct :: TestTree
unitMixedStruct = testGroup "Mixed struct roundtrip"
  [ testCase "Binary" $ do
      let v = mixedStruct
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Compact" $ do
      let v = mixedStruct
      decodeCompact (encodeCompact v) @?= Right v
  ]

mixedStruct :: ThriftValue
mixedStruct = TVStruct
  [ (1,  TVBool True)
  , (2,  TVByte 42)
  , (3,  TVI16 1000)
  , (4,  TVI32 100000)
  , (5,  TVI64 9999999999)
  , (6,  TVDouble 3.14)
  , (7,  TVString "hello world")
  , (8,  TVBinary (BS.pack [0xDE, 0xAD, 0xBE, 0xEF]))
  , (10, TVUUID (BS.pack [0..15]))
  ]

--------------------------------------------------------------------------------
-- Unit: nested structs
--------------------------------------------------------------------------------

unitNestedStructs :: TestTree
unitNestedStructs = testGroup "Nested structs"
  [ testCase "Binary" $ do
      let v = nestedStruct
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Compact" $ do
      let v = nestedStruct
      decodeCompact (encodeCompact v) @?= Right v
  ]

nestedStruct :: ThriftValue
nestedStruct = TVStruct
  [ (1, TVString "outer")
  , (2, TVStruct
      [ (1, TVString "inner")
      , (2, TVI32 42)
      , (3, TVStruct
          [ (1, TVBool False)
          , (2, TVI64 (-999))
          ])
      ])
  , (3, TVI32 7)
  ]

--------------------------------------------------------------------------------
-- Unit: lists, sets, maps
--------------------------------------------------------------------------------

unitContainers :: TestTree
unitContainers = testGroup "Container roundtrip"
  [ testCase "List of i32 (Binary)" $ do
      let v = TVStruct [(1, TVList TT_I32 [TVI32 1, TVI32 2, TVI32 3])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "List of i32 (Compact)" $ do
      let v = TVStruct [(1, TVList TT_I32 [TVI32 1, TVI32 2, TVI32 3])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Set of strings (Binary)" $ do
      let v = TVStruct [(1, TVSet TT_STRING [TVString "a", TVString "b"])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Set of strings (Compact)" $ do
      let v = TVStruct [(1, TVSet TT_STRING [TVString "a", TVString "b"])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Map i32->string (Binary)" $ do
      let v = TVStruct [(1, TVMap TT_I32 TT_STRING
                  [ (TVI32 1, TVString "one")
                  , (TVI32 2, TVString "two")
                  ])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Map i32->string (Compact)" $ do
      let v = TVStruct [(1, TVMap TT_I32 TT_STRING
                  [ (TVI32 1, TVString "one")
                  , (TVI32 2, TVString "two")
                  ])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "List of structs (Binary)" $ do
      let v = TVStruct
                [(1, TVList TT_STRUCT
                  [ TVStruct [(1, TVI32 10)]
                  , TVStruct [(1, TVI32 20)]
                  ])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "List of structs (Compact)" $ do
      let v = TVStruct
                [(1, TVList TT_STRUCT
                  [ TVStruct [(1, TVI32 10)]
                  , TVStruct [(1, TVI32 20)]
                  ])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Map with struct values (Binary)" $ do
      let v = TVStruct
                [(1, TVMap TT_STRING TT_STRUCT
                  [ (TVString "x", TVStruct [(1, TVBool True)])
                  ])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Map with struct values (Compact)" $ do
      let v = TVStruct
                [(1, TVMap TT_STRING TT_STRUCT
                  [ (TVString "x", TVStruct [(1, TVBool True)])
                  ])]
      decodeCompact (encodeCompact v) @?= Right v
  ]

--------------------------------------------------------------------------------
-- Unit: empty struct and empty containers
--------------------------------------------------------------------------------

unitEmptyStructAndContainers :: TestTree
unitEmptyStructAndContainers = testGroup "Empty struct and containers"
  [ testCase "Empty struct (Binary)" $ do
      let v = TVStruct []
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty struct (Compact)" $ do
      let v = TVStruct []
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty list (Binary)" $ do
      let v = TVStruct [(1, TVList TT_I32 [])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty list (Compact)" $ do
      let v = TVStruct [(1, TVList TT_I32 [])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty set (Binary)" $ do
      let v = TVStruct [(1, TVSet TT_STRING [])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty set (Compact)" $ do
      let v = TVStruct [(1, TVSet TT_STRING [])]
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty map (Binary)" $ do
      let v = TVStruct [(1, TVMap TT_I32 TT_STRING [])]
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty map (Compact)" $ do
      let v = TVStruct [(1, TVMap TT_I32 TT_STRING [])]
          decoded = decodeCompact (encodeCompact v)
      case decoded of
        Right (TVStruct [(1, TVMap _ _ [])]) -> return ()
        other -> assertFailure $ "Expected empty map struct, got: " ++ show other
  ]

--------------------------------------------------------------------------------
-- Unit: Binary and Compact produce different bytes but decode to same value
--------------------------------------------------------------------------------

unitProtocolsDiffer :: TestTree
unitProtocolsDiffer = testGroup "Protocols differ in bytes, agree on values"
  [ testCase "Simple struct" $ do
      let v = TVStruct
                [ (1, TVBool True)
                , (2, TVI32 42)
                , (3, TVString "test")
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v

  , testCase "Struct with containers" $ do
      let v = TVStruct
                [ (1, TVList TT_I64 [TVI64 100, TVI64 200])
                , (2, TVMap TT_STRING TT_I32
                     [(TVString "a", TVI32 1)])
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v

  , testCase "Nested struct" $ do
      let v = TVStruct
                [ (1, TVStruct [(1, TVI32 (-1)), (2, TVBool False)])
                , (2, TVDouble 2.718)
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v
  ]

--------------------------------------------------------------------------------
-- JSON protocol tests
--------------------------------------------------------------------------------

jsonTests :: TestTree
jsonTests = testGroup "Thrift JSON"
  [ jsonPrimitiveRoundtrip
  , jsonStructEncodeDecode
  , jsonNestedStruct
  , jsonContainers
  , jsonEmptyContainers
  , typedJsonMixedStruct
  ]

jsonPrimitiveRoundtrip :: TestTree
jsonPrimitiveRoundtrip = testGroup "JSON primitive roundtrip"
  [ testCase "Bool" $ do
      let v = TVBool True
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Byte" $ do
      let v = TVByte 42
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I16" $ do
      let v = TVI16 (-1000)
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I32" $ do
      let v = TVI32 100000
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I64" $ do
      let v = TVI64 9999999999
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Double" $ do
      let v = TVDouble 3.14
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "String" $ do
      let v = TVString "hello"
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Binary" $ do
      let v = TVBinary (BS.pack [0xDE, 0xAD])
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "UUID" $ do
      let v = TVUUID (BS.pack [0..15])
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

jsonStructEncodeDecode :: TestTree
jsonStructEncodeDecode = testCase "JSON struct encode/decode" $ do
  let v = TVStruct
            [ (1, TVBool True)
            , (2, TVI32 42)
            , (3, TVString "test")
            ]
  thriftFromJSON v (thriftToJSON v) @?= Right v

jsonNestedStruct :: TestTree
jsonNestedStruct = testCase "JSON nested struct" $ do
  let v = TVStruct
            [ (1, TVString "outer")
            , (2, TVStruct
                [ (1, TVString "inner")
                , (2, TVI32 42)
                ])
            ]
  thriftFromJSON v (thriftToJSON v) @?= Right v

jsonContainers :: TestTree
jsonContainers = testGroup "JSON list/set/map"
  [ testCase "List of i32" $ do
      let v = TVList TT_I32 [TVI32 1, TVI32 2, TVI32 3]
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Set of strings" $ do
      let v = TVSet TT_STRING [TVString "a", TVString "b"]
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Map i32->string" $ do
      let v = TVMap TT_I32 TT_STRING
                [ (TVI32 1, TVString "one")
                , (TVI32 2, TVString "two")
                ]
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

jsonEmptyContainers :: TestTree
jsonEmptyContainers = testGroup "JSON empty containers"
  [ testCase "Empty list" $ do
      let v = TVList TT_I32 []
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Empty set" $ do
      let v = TVSet TT_STRING []
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Empty map" $ do
      let v = TVMap TT_I32 TT_STRING []
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

typedJsonMixedStruct :: TestTree
typedJsonMixedStruct = testCase "Typed JSON roundtrip (mixed struct)" $ do
  let v = TVStruct
            [ (1,  TVBool True)
            , (2,  TVByte 42)
            , (3,  TVI16 1000)
            , (4,  TVI32 100000)
            , (5,  TVI64 9999999999)
            , (6,  TVDouble 3.14)
            , (7,  TVString "hello world")
            , (8,  TVBinary (BS.pack [0xDE, 0xAD, 0xBE, 0xEF]))
            , (9,  TVList TT_I32 [TVI32 1, TVI32 2])
            , (10, TVSet TT_STRING [TVString "x", TVString "y"])
            ]
      encoded = thriftToTypedJSON v
      decoded = thriftFromTypedJSON encoded
  decoded @?= Right v
