{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Test.Class (classTests) where

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit

import qualified MsgPack.Value as MV
import qualified MsgPack.Class as MC

import qualified CBOR.Value as CV
import qualified CBOR.Class as CC

import qualified BSON.Value as BV
import qualified BSON.Class as BC

import qualified EDN.Value as EV
import qualified EDN.Class as EC

import qualified Ion.Value as IV
import qualified Ion.Class as IC

-- Sample record type for Generic deriving tests
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (MC.ToMsgPack, MC.FromMsgPack)
    deriving anyclass (CC.ToCBOR, CC.FromCBOR)
    deriving anyclass (BC.ToBSON, BC.FromBSON)
    deriving anyclass (EC.ToEDN, EC.FromEDN)
    deriving anyclass (IC.ToIon, IC.FromIon)

data Address = Address
  { street :: Text
  , city   :: Text
  , zip_   :: Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (MC.ToMsgPack, MC.FromMsgPack)
    deriving anyclass (CC.ToCBOR, CC.FromCBOR)
    deriving anyclass (BC.ToBSON, BC.FromBSON)
    deriving anyclass (IC.ToIon, IC.FromIon)

classTests :: TestTree
classTests = testGroup "Typeclass encode/decode"
  [ msgPackClassTests
  , cborClassTests
  , bsonClassTests
  , ednClassTests
  , ionClassTests
  , msgPackGenericTests
  , cborGenericTests
  , bsonGenericTests
  , ednGenericTests
  , ionGenericTests
  ]

--------------------------------------------------------------------------------
-- MsgPack typeclass instances
--------------------------------------------------------------------------------

msgPackClassTests :: TestTree
msgPackClassTests = testGroup "MsgPack.Class instances"
  [ testCase "Bool roundtrip" $ do
      MC.fromMsgPack (MC.toMsgPack True) @?= Right True
      MC.fromMsgPack (MC.toMsgPack False) @?= Right False

  , testCase "Int roundtrip" $ do
      MC.fromMsgPack (MC.toMsgPack (42 :: Int)) @?= Right (42 :: Int)
      MC.fromMsgPack (MC.toMsgPack (-7 :: Int)) @?= Right (-7 :: Int)

  , testCase "Text roundtrip" $
      MC.fromMsgPack (MC.toMsgPack ("hello" :: Text)) @?= Right ("hello" :: Text)

  , testCase "Maybe roundtrip" $ do
      MC.fromMsgPack (MC.toMsgPack (Just (42 :: Int))) @?= Right (Just (42 :: Int))
      MC.fromMsgPack (MC.toMsgPack (Nothing :: Maybe Int)) @?= Right (Nothing :: Maybe Int)

  , testCase "List roundtrip" $
      MC.fromMsgPack (MC.toMsgPack [1, 2, 3 :: Int]) @?= Right [1, 2, 3 :: Int]

  , testCase "Vector roundtrip" $ do
      let v = V.fromList [10, 20, 30 :: Int]
      MC.fromMsgPack (MC.toMsgPack v) @?= Right v

  , testCase "Map roundtrip" $ do
      let m = Map.fromList [("a" :: Text, 1 :: Int), ("b", 2)]
      MC.fromMsgPack (MC.toMsgPack m) @?= Right m

  , testCase "(a,b) roundtrip" $
      MC.fromMsgPack (MC.toMsgPack ("x" :: Text, 42 :: Int)) @?= Right ("x" :: Text, 42 :: Int)

  , testCase "() roundtrip" $
      MC.fromMsgPack (MC.toMsgPack ()) @?= Right ()

  , testCase "Double roundtrip" $
      MC.fromMsgPack (MC.toMsgPack (3.14 :: Double)) @?= Right (3.14 :: Double)

  , testCase "Binary encode/decode via ByteString" $ do
      let bs = MC.encodeMsgPack (42 :: Int)
      MC.decodeMsgPack bs @?= Right (42 :: Int)
  ]

--------------------------------------------------------------------------------
-- CBOR typeclass instances
--------------------------------------------------------------------------------

cborClassTests :: TestTree
cborClassTests = testGroup "CBOR.Class instances"
  [ testCase "Bool roundtrip" $ do
      CC.fromCBOR (CC.toCBOR True) @?= Right True
      CC.fromCBOR (CC.toCBOR False) @?= Right False

  , testCase "Int roundtrip" $ do
      CC.fromCBOR (CC.toCBOR (42 :: Int)) @?= Right (42 :: Int)
      CC.fromCBOR (CC.toCBOR (-7 :: Int)) @?= Right (-7 :: Int)

  , testCase "Text roundtrip" $
      CC.fromCBOR (CC.toCBOR ("hello" :: Text)) @?= Right ("hello" :: Text)

  , testCase "Maybe roundtrip" $ do
      CC.fromCBOR (CC.toCBOR (Just (42 :: Int))) @?= Right (Just (42 :: Int))
      CC.fromCBOR (CC.toCBOR (Nothing :: Maybe Int)) @?= Right (Nothing :: Maybe Int)

  , testCase "List roundtrip" $
      CC.fromCBOR (CC.toCBOR [1, 2, 3 :: Int]) @?= Right [1, 2, 3 :: Int]

  , testCase "Binary encode/decode via ByteString" $ do
      let bs = CC.encodeCBOR (42 :: Int)
      CC.decodeCBOR bs @?= Right (42 :: Int)
  ]

--------------------------------------------------------------------------------
-- BSON typeclass instances
--------------------------------------------------------------------------------

bsonClassTests :: TestTree
bsonClassTests = testGroup "BSON.Class instances"
  [ testCase "Bool roundtrip" $ do
      BC.fromBSON (BC.toBSON True) @?= Right True
      BC.fromBSON (BC.toBSON False) @?= Right False

  , testCase "Int32 roundtrip" $ do
      BC.fromBSON (BC.toBSON (42 :: Int)) @?= Right (42 :: Int)
      BC.fromBSON (BC.toBSON (-7 :: Int)) @?= Right (-7 :: Int)

  , testCase "Text roundtrip" $
      BC.fromBSON (BC.toBSON ("hello" :: Text)) @?= Right ("hello" :: Text)

  , testCase "Double roundtrip" $
      BC.fromBSON (BC.toBSON (3.14 :: Double)) @?= Right (3.14 :: Double)

  , testCase "List roundtrip" $
      BC.fromBSON (BC.toBSON [1, 2, 3 :: Int]) @?= Right [1, 2, 3 :: Int]
  ]

--------------------------------------------------------------------------------
-- EDN typeclass instances
--------------------------------------------------------------------------------

ednClassTests :: TestTree
ednClassTests = testGroup "EDN.Class instances"
  [ testCase "Bool roundtrip" $ do
      EC.fromEDN (EC.toEDN True) @?= Right True
      EC.fromEDN (EC.toEDN False) @?= Right False

  , testCase "Int roundtrip" $
      EC.fromEDN (EC.toEDN (42 :: Int)) @?= Right (42 :: Int)

  , testCase "Text roundtrip" $
      EC.fromEDN (EC.toEDN ("hello" :: Text)) @?= Right ("hello" :: Text)

  , testCase "List roundtrip" $
      EC.fromEDN (EC.toEDN [1, 2, 3 :: Int]) @?= Right [1, 2, 3 :: Int]
  ]

--------------------------------------------------------------------------------
-- Ion typeclass instances
--------------------------------------------------------------------------------

ionClassTests :: TestTree
ionClassTests = testGroup "Ion.Class instances"
  [ testCase "Bool roundtrip" $ do
      IC.fromIon (IC.toIon True) @?= Right True
      IC.fromIon (IC.toIon False) @?= Right False

  , testCase "Int roundtrip" $
      IC.fromIon (IC.toIon (42 :: Int)) @?= Right (42 :: Int)

  , testCase "Text roundtrip" $
      IC.fromIon (IC.toIon ("hello" :: Text)) @?= Right ("hello" :: Text)

  , testCase "List roundtrip" $
      IC.fromIon (IC.toIon [1, 2, 3 :: Int]) @?= Right [1, 2, 3 :: Int]
  ]

--------------------------------------------------------------------------------
-- MsgPack Generic deriving
--------------------------------------------------------------------------------

msgPackGenericTests :: TestTree
msgPackGenericTests = testGroup "MsgPack Generic deriving"
  [ testCase "Person to/from MsgPack value" $ do
      let p = Person "Alice" 30
          v = MC.toMsgPack p
          expected = MV.Map (V.fromList
            [ (MV.String "name", MV.String "Alice")
            , (MV.String "age", MV.Word 30)
            ])
      v @?= expected
      MC.fromMsgPack v @?= Right p

  , testCase "Person binary roundtrip" $ do
      let p = Person "Bob" 25
      MC.decodeMsgPack (MC.encodeMsgPack p) @?= Right p

  , testCase "Address generic roundtrip" $ do
      let a = Address "123 Main St" "Springfield" 62701
      MC.fromMsgPack (MC.toMsgPack a) @?= Right a

  , testCase "Address binary roundtrip" $ do
      let a = Address "456 Oak Ave" "Shelbyville" 62702
      MC.decodeMsgPack (MC.encodeMsgPack a) @?= Right a
  ]

--------------------------------------------------------------------------------
-- CBOR Generic deriving
--------------------------------------------------------------------------------

cborGenericTests :: TestTree
cborGenericTests = testGroup "CBOR Generic deriving"
  [ testCase "Person to/from CBOR value" $ do
      let p = Person "Alice" 30
          v = CC.toCBOR p
          expected = CV.Map (V.fromList
            [ (CV.TextString "name", CV.TextString "Alice")
            , (CV.TextString "age", CV.UInt 30)
            ])
      v @?= expected
      CC.fromCBOR v @?= Right p

  , testCase "Person binary roundtrip" $ do
      let p = Person "Bob" 25
      CC.decodeCBOR (CC.encodeCBOR p) @?= Right p

  , testCase "Address generic roundtrip" $ do
      let a = Address "123 Main St" "Springfield" 62701
      CC.fromCBOR (CC.toCBOR a) @?= Right a
  ]

--------------------------------------------------------------------------------
-- BSON Generic deriving
--------------------------------------------------------------------------------

bsonGenericTests :: TestTree
bsonGenericTests = testGroup "BSON Generic deriving"
  [ testCase "Person to/from BSON value" $ do
      let p = Person "Alice" 30
          v = BC.toBSON p
      case v of
        BV.Document _ -> pure ()
        _ -> assertFailure "expected Document"
      BC.fromBSON v @?= Right p

  , testCase "Person binary roundtrip" $ do
      let p = Person "Bob" 25
      BC.decodeBSON (BC.encodeBSON p) @?= Right p

  , testCase "Address generic roundtrip" $ do
      let a = Address "123 Main St" "Springfield" 62701
      BC.fromBSON (BC.toBSON a) @?= Right a
  ]

--------------------------------------------------------------------------------
-- EDN Generic deriving
--------------------------------------------------------------------------------

ednGenericTests :: TestTree
ednGenericTests = testGroup "EDN Generic deriving"
  [ testCase "Person to/from EDN value" $ do
      let p = Person "Alice" 30
          v = EC.toEDN p
      case v of
        EV.Map _ -> pure ()
        _ -> assertFailure "expected Map"
      EC.fromEDN v @?= Right p

  , testCase "Person text roundtrip" $ do
      let p = Person "Bob" 25
      EC.decodeEDN (EC.encodeEDN p) @?= Right p
  ]

--------------------------------------------------------------------------------
-- Ion Generic deriving
--------------------------------------------------------------------------------

ionGenericTests :: TestTree
ionGenericTests = testGroup "Ion Generic deriving"
  [ testCase "Person to/from Ion value" $ do
      let p = Person "Alice" 30
          v = IC.toIon p
      case v of
        IV.Struct _ -> pure ()
        _ -> assertFailure "expected Struct"
      IC.fromIon v @?= Right p

  , testCase "Person binary roundtrip" $ do
      let p = Person "Bob" 25
      IC.decodeIon (IC.encodeIon p) @?= Right p

  , testCase "Address generic roundtrip" $ do
      let a = Address "123 Main St" "Springfield" 62701
      IC.fromIon (IC.toIon a) @?= Right a
  ]
