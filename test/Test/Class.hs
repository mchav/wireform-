{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Class (classTests) where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.Product as FProduct
import qualified Data.Functor.Sum as FSum
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
import Data.Ord (Down(..))
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Version (makeVersion)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

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
  , aesonParityTests
  , directEncodingTests
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

--------------------------------------------------------------------------------
-- Aeson-parity instances (NonEmpty, Either, Set, Seq, IntMap, IntSet, HashMap,
-- HashSet, 3- and 4-tuples, Identity, Const, Down, Version, Ratio, Char,
-- Natural, lazy Text, etc.) -- spot-check round-trips across formats.
--------------------------------------------------------------------------------

aesonParityTests :: TestTree
aesonParityTests = testGroup "Aeson-parity round-trips"
  [ msgPackParityTests
  , cborParityTests
  , bsonParityTests
  , ednParityTests
  , ionParityTests
  , functorNewtypeTests
  ]

-- Helpers to make explicit type signatures on the recovered value the only
-- annotation site needed.
rtMP :: forall a. (MC.ToMsgPack a, MC.FromMsgPack a, Eq a, Show a) => a -> Assertion
rtMP x = MC.fromMsgPack (MC.toMsgPack x) @?= Right x

rtCC :: forall a. (CC.ToCBOR a, CC.FromCBOR a, Eq a, Show a) => a -> Assertion
rtCC x = CC.fromCBOR (CC.toCBOR x) @?= Right x

rtBC :: forall a. (BC.ToBSON a, BC.FromBSON a, Eq a, Show a) => a -> Assertion
rtBC x = BC.fromBSON (BC.toBSON x) @?= Right x

rtEC :: forall a. (EC.ToEDN a, EC.FromEDN a, Eq a, Show a) => a -> Assertion
rtEC x = EC.fromEDN (EC.toEDN x) @?= Right x

rtIC :: forall a. (IC.ToIon a, IC.FromIon a, Eq a, Show a) => a -> Assertion
rtIC x = IC.fromIon (IC.toIon x) @?= Right x

msgPackParityTests :: TestTree
msgPackParityTests = testGroup "MsgPack"
  [ testCase "Integer roundtrip"   $ do rtMP (12345678901234567 :: Integer); rtMP (-1 :: Integer)
  , testCase "Natural roundtrip"   $ rtMP (42 :: Natural)
  , testCase "Lazy Text roundtrip" $ rtMP (TL.pack "hello lazy world")
  , testCase "NonEmpty roundtrip"  $ rtMP (1 :| [2, 3 :: Int])
  , testCase "Either roundtrip"    $ do
      rtMP (Left  "left"  :: Either Text Int)
      rtMP (Right 99      :: Either Text Int)
  , testCase "Set roundtrip"     $ rtMP (Set.fromList [1, 2, 3 :: Int])
  , testCase "Seq roundtrip"     $ rtMP (Seq.fromList [10, 20 :: Int])
  , testCase "IntMap roundtrip"  $ rtMP (IntMap.fromList [(1, "a" :: Text), (2, "b")])
  , testCase "IntSet roundtrip"  $ rtMP (IntSet.fromList [3, 4, 5])
  , testCase "HashMap roundtrip" $ rtMP (HM.fromList [("k" :: Text, 1 :: Int)])
  , testCase "HashSet roundtrip" $ rtMP (HS.fromList [1, 2 :: Int])
  , testCase "3-tuple roundtrip" $ rtMP ("x" :: Text, 1 :: Int, True)
  , testCase "4-tuple roundtrip" $ rtMP (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int)
  , testCase "Identity roundtrip" $ rtMP (Identity (5 :: Int))
  , testCase "Down roundtrip"     $ rtMP (Down (7 :: Int))
  , testCase "Version roundtrip"  $ rtMP (makeVersion [1, 2, 3])
  , testCase "Ratio roundtrip"    $ rtMP (3 % 4 :: Rational)
  ]

cborParityTests :: TestTree
cborParityTests = testGroup "CBOR"
  [ testCase "Integer roundtrip"  $ do rtCC (98765432109876 :: Integer); rtCC (-3 :: Integer)
  , testCase "Natural roundtrip"  $ rtCC (42 :: Natural)
  , testCase "Lazy Text roundtrip" $ rtCC (TL.pack "lazy")
  , testCase "NonEmpty roundtrip" $ rtCC (1 :| [2 :: Int])
  , testCase "Either roundtrip"   $ do
      rtCC (Left  "x" :: Either Text Int)
      rtCC (Right 1   :: Either Text Int)
  , testCase "Set roundtrip"     $ rtCC (Set.fromList [1, 2 :: Int])
  , testCase "Seq roundtrip"     $ rtCC (Seq.fromList ["a" :: Text, "b"])
  , testCase "IntMap roundtrip"  $ rtCC (IntMap.fromList [(1, True), (2, False)])
  , testCase "IntSet roundtrip"  $ rtCC (IntSet.fromList [10, 20])
  , testCase "HashMap roundtrip" $ rtCC (HM.fromList [("k" :: Text, 5 :: Int)])
  , testCase "HashSet roundtrip" $ rtCC (HS.fromList [1, 2 :: Int])
  , testCase "3-tuple roundtrip" $ rtCC ("x" :: Text, 1 :: Int, True)
  , testCase "4-tuple roundtrip" $ rtCC (1 :: Int, "b" :: Text, True, 4 :: Int)
  , testCase "Identity roundtrip" $ rtCC (Identity ("hi" :: Text))
  , testCase "Down roundtrip"     $ rtCC (Down (7 :: Int))
  , testCase "Version roundtrip"  $ rtCC (makeVersion [2])
  , testCase "Ratio roundtrip"    $ rtCC (5 % 7 :: Rational)
  ]

bsonParityTests :: TestTree
bsonParityTests = testGroup "BSON"
  [ testCase "Integer (small) roundtrip" $ rtBC (123 :: Integer)
  , testCase "Natural roundtrip"   $ rtBC (42 :: Natural)
  , testCase "NonEmpty roundtrip"  $ rtBC (1 :| [2 :: Int])
  , testCase "Either roundtrip"    $ do
      rtBC (Left  "x" :: Either Text Int)
      rtBC (Right 7   :: Either Text Int)
  , testCase "Set roundtrip"      $ rtBC (Set.fromList [1, 2 :: Int])
  , testCase "Seq roundtrip"      $ rtBC (Seq.fromList [10, 20 :: Int])
  , testCase "IntMap roundtrip"   $ rtBC (IntMap.fromList [(1, "a" :: Text)])
  , testCase "IntSet roundtrip"   $ rtBC (IntSet.fromList [3, 4])
  , testCase "3-tuple roundtrip"  $ rtBC ("x" :: Text, 1 :: Int, True)
  , testCase "Identity roundtrip" $ rtBC (Identity (5 :: Int))
  , testCase "Down roundtrip"     $ rtBC (Down (7 :: Int))
  , testCase "Version roundtrip"  $ rtBC (makeVersion [1, 2])
  ]

ednParityTests :: TestTree
ednParityTests = testGroup "EDN"
  [ testCase "Char roundtrip"      $ rtEC 'x'
  , testCase "Integer roundtrip"   $ rtEC (12345 :: Integer)
  , testCase "Natural roundtrip"   $ rtEC (42 :: Natural)
  , testCase "Lazy Text roundtrip" $ rtEC (TL.pack "lazy")
  , testCase "NonEmpty roundtrip"  $ rtEC (1 :| [2 :: Int])
  , testCase "Either roundtrip"    $ do
      rtEC (Left  "x" :: Either Text Int)
      rtEC (Right 1   :: Either Text Int)
  , testCase "Set roundtrip"     $ rtEC (Set.fromList [1, 2 :: Int])
  , testCase "Seq roundtrip"     $ rtEC (Seq.fromList ["a" :: Text])
  , testCase "IntMap roundtrip"  $ rtEC (IntMap.fromList [(1, True)])
  , testCase "IntSet roundtrip"  $ rtEC (IntSet.fromList [10])
  , testCase "HashSet roundtrip" $ rtEC (HS.fromList [1, 2 :: Int])
  , testCase "3-tuple roundtrip" $ rtEC ("x" :: Text, 1 :: Int, True)
  , testCase "Identity roundtrip" $ rtEC (Identity ("hi" :: Text))
  , testCase "Version roundtrip"  $ rtEC (makeVersion [3])
  ]

ionParityTests :: TestTree
ionParityTests = testGroup "Ion"
  [ testCase "Char roundtrip"      $ rtIC 'x'
  , testCase "Integer (small) roundtrip" $ rtIC (12345 :: Integer)
  , testCase "Natural roundtrip"   $ rtIC (42 :: Natural)
  , testCase "Lazy Text roundtrip" $ rtIC (TL.pack "lazy")
  , testCase "NonEmpty roundtrip"  $ rtIC (1 :| [2 :: Int])
  , testCase "Either roundtrip"    $ do
      rtIC (Left  "x" :: Either Text Int)
      rtIC (Right 1   :: Either Text Int)
  , testCase "Set roundtrip"     $ rtIC (Set.fromList [1, 2 :: Int])
  , testCase "Seq roundtrip"     $ rtIC (Seq.fromList [10, 20 :: Int])
  , testCase "IntMap roundtrip"  $ rtIC (IntMap.fromList [(1, "a" :: Text)])
  , testCase "IntSet roundtrip"  $ rtIC (IntSet.fromList [10])
  , testCase "HashMap (Text k) roundtrip" $ rtIC (HM.fromList [("k" :: Text, 1 :: Int)])
  , testCase "HashSet roundtrip" $ rtIC (HS.fromList [1, 2 :: Int])
  , testCase "3-tuple roundtrip" $ rtIC ("x" :: Text, 1 :: Int, True)
  , testCase "Identity roundtrip" $ rtIC (Identity (5 :: Int))
  , testCase "Down roundtrip"     $ rtIC (Down (7 :: Int))
  , testCase "Version roundtrip"  $ rtIC (makeVersion [1])
  ]

--------------------------------------------------------------------------------
-- Functor / monoid newtype round-trips. We exercise one format per
-- class-shape (binary tagged-map, AST-wrapped, native union) so the
-- coverage is broad without exploding the test count.
--------------------------------------------------------------------------------

functorNewtypeTests :: TestTree
functorNewtypeTests = testGroup "Functor / monoid newtypes"
  [ testGroup "MsgPack"
      [ testCase "Sum"           $ rtMP (Mon.Sum     (5 :: Int))
      , testCase "Product"       $ rtMP (Mon.Product (6 :: Int))
      , testCase "Dual"          $ rtMP (Mon.Dual    ("x" :: Text))
      , testCase "All"           $ rtMP (Mon.All True)
      , testCase "Any"           $ rtMP (Mon.Any True)
      , testCase "First"         $ rtMP (Mon.First (Just (1 :: Int)))
      , testCase "Last"          $ rtMP (Mon.Last  (Just (2 :: Int)))
      , testCase "Min"           $ rtMP (Semi.Min (3 :: Int))
      , testCase "Max"           $ rtMP (Semi.Max (4 :: Int))
      , testCase "Semi.First"    $ rtMP (Semi.First (5 :: Int))
      , testCase "Semi.Last"     $ rtMP (Semi.Last  (6 :: Int))
      , testCase "WrappedMonoid" $ rtMP (Semi.WrapMonoid ("hi" :: Text))
      , testCase "Arg"           $ rtMP (Semi.Arg (1 :: Int) ("x" :: Text))
      , testCase "Compose"       $ rtMP (Compose [Just (1 :: Int), Nothing, Just 2])
      , testCase "Functor.Product" $
          rtMP (FProduct.Pair (Identity (1 :: Int)) (Identity (2 :: Int)))
      , testCase "Functor.Sum (InL)" $
          rtMP (FSum.InL (Identity (1 :: Int)) :: FSum.Sum Identity Identity Int)
      , testCase "Functor.Sum (InR)" $
          rtMP (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
      ]
  , testGroup "CBOR"
      [ testCase "Sum"           $ rtCC (Mon.Sum     (5 :: Int))
      , testCase "Min"           $ rtCC (Semi.Min    (3 :: Int))
      , testCase "Arg"           $ rtCC (Semi.Arg (1 :: Int) ("x" :: Text))
      , testCase "Compose"       $ rtCC (Compose [Just (1 :: Int), Nothing])
      , testCase "Functor.Product" $
          rtCC (FProduct.Pair (Identity (1 :: Int)) (Identity (2 :: Int)))
      , testCase "Functor.Sum (InR)" $
          rtCC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
      ]
  , testGroup "BSON"
      [ testCase "Sum"           $ rtBC (Mon.Sum (5 :: Int))
      , testCase "Arg"           $ rtBC (Semi.Arg (1 :: Int) ("x" :: Text))
      , testCase "Functor.Sum (InL)" $
          rtBC (FSum.InL (Identity (1 :: Int)) :: FSum.Sum Identity Identity Int)
      ]
  , testGroup "EDN"
      [ testCase "Sum"           $ rtEC (Mon.Sum (5 :: Int))
      , testCase "Compose"       $ rtEC (Compose [Just (1 :: Int)])
      , testCase "Functor.Sum (InR)" $
          rtEC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
      ]
  , testGroup "Ion"
      [ testCase "Sum"           $ rtIC (Mon.Sum (5 :: Int))
      , testCase "Arg"           $ rtIC (Semi.Arg (1 :: Int) ("x" :: Text))
      , testCase "Functor.Sum (InR)" $
          rtIC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
      ]
  ]

--------------------------------------------------------------------------------
-- Direct (toEncoding) parity: the direct path must produce the same
-- bytes as the AST path.
--------------------------------------------------------------------------------

directEncodingTests :: TestTree
directEncodingTests = testGroup "Direct toEncoding parity"
  [ testGroup "MsgPack"
      [ testCase "Bool"     $ MC.encodeMsgPackDirect True              @?= MC.encodeMsgPack True
      , testCase "Int"      $ MC.encodeMsgPackDirect (123 :: Int)      @?= MC.encodeMsgPack (123 :: Int)
      , testCase "Negative" $ MC.encodeMsgPackDirect (-7 :: Int)       @?= MC.encodeMsgPack (-7 :: Int)
      , testCase "Text"     $ MC.encodeMsgPackDirect ("hello" :: Text) @?= MC.encodeMsgPack ("hello" :: Text)
      , testCase "List"     $ MC.encodeMsgPackDirect [1,2,3 :: Int]    @?= MC.encodeMsgPack [1,2,3 :: Int]
      , testCase "Maybe"    $ MC.encodeMsgPackDirect (Just (5 :: Int)) @?= MC.encodeMsgPack (Just (5 :: Int))
      , testCase "Map"      $ MC.encodeMsgPackDirect (Map.fromList [("a" :: Text, 1 :: Int)])
                                @?= MC.encodeMsgPack (Map.fromList [("a" :: Text, 1 :: Int)])
      , testCase "Person (default through Value)" $
          MC.encodeMsgPackDirect (Person "Alice" 30) @?= MC.encodeMsgPack (Person "Alice" 30)
      ]
  , testGroup "CBOR"
      [ testCase "Bool"     $ CC.encodeCBORDirect True              @?= CC.encodeCBOR True
      , testCase "Int"      $ CC.encodeCBORDirect (123 :: Int)      @?= CC.encodeCBOR (123 :: Int)
      , testCase "Negative" $ CC.encodeCBORDirect (-7 :: Int)       @?= CC.encodeCBOR (-7 :: Int)
      , testCase "Text"     $ CC.encodeCBORDirect ("hello" :: Text) @?= CC.encodeCBOR ("hello" :: Text)
      , testCase "List"     $ CC.encodeCBORDirect [1,2,3 :: Int]    @?= CC.encodeCBOR [1,2,3 :: Int]
      , testCase "Maybe"    $ CC.encodeCBORDirect (Just (5 :: Int)) @?= CC.encodeCBOR (Just (5 :: Int))
      , testCase "Map"      $ CC.encodeCBORDirect (Map.fromList [("a" :: Text, 1 :: Int)])
                                @?= CC.encodeCBOR (Map.fromList [("a" :: Text, 1 :: Int)])
      , testCase "Person (default through Value)" $
          CC.encodeCBORDirect (Person "Alice" 30) @?= CC.encodeCBOR (Person "Alice" 30)
      ]
  , testGroup "BSON"
      [ testCase "Person (Encoding wraps Value)" $
          BC.encodeBSONDirect (Person "Alice" 30) @?= BC.encodeBSON (Person "Alice" 30)
      ]
  , testGroup "EDN"
      [ testCase "Bool" $ EC.encodeEDNDirect True              @?= EC.encodeEDN True
      , testCase "Int"  $ EC.encodeEDNDirect (5 :: Int)        @?= EC.encodeEDN (5 :: Int)
      , testCase "Text" $ EC.encodeEDNDirect ("hello" :: Text) @?= EC.encodeEDN ("hello" :: Text)
      , testCase "List" $ EC.encodeEDNDirect [1,2 :: Int]      @?= EC.encodeEDN [1,2 :: Int]
      , testCase "Person (default through Value)" $
          EC.encodeEDNDirect (Person "Alice" 30) @?= EC.encodeEDN (Person "Alice" 30)
      ]
  , testGroup "Ion"
      [ testCase "Person (Encoding wraps Value)" $
          IC.encodeIonDirect (Person "Alice" 30) @?= IC.encodeIon (Person "Alice" 30)
      ]
  ]
