{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Class (classTests) where

import BSON.Class qualified as BC
import BSON.Value qualified as BV
import Bencode.Encode qualified as BencodeE
import Bencode.Encoding qualified as BencodeEncoding
import Bencode.Value qualified as BencodeV
import CBOR.Class qualified as CC
import CBOR.Value qualified as CV
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FProduct
import Data.Functor.Sum qualified as FSum
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Mon
import Data.Ord (Down (..))
import Data.Ratio ((%))
import Data.Semigroup qualified as Semi
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Version (makeVersion)
import EDN.Class qualified as EC
import EDN.Value qualified as EV
import GHC.Generics (Generic)
import Ion.Class qualified as IC
import Ion.Value qualified as IV
import MsgPack.Class qualified as MC
import MsgPack.Value qualified as MV
import Numeric.Natural (Natural)
import Test.Syd


-- Sample record type for Generic deriving tests
data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (MC.ToMsgPack, MC.FromMsgPack)
  deriving anyclass (CC.ToCBOR, CC.FromCBOR)
  deriving anyclass (BC.ToBSON, BC.FromBSON)
  deriving anyclass (EC.ToEDN, EC.FromEDN)
  deriving anyclass (IC.ToIon, IC.FromIon)


data Address = Address
  { street :: Text
  , city :: Text
  , zip_ :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (MC.ToMsgPack, MC.FromMsgPack)
  deriving anyclass (CC.ToCBOR, CC.FromCBOR)
  deriving anyclass (BC.ToBSON, BC.FromBSON)
  deriving anyclass (IC.ToIon, IC.FromIon)


classTests :: Spec
classTests =
  describe "Typeclass encode/decode" $
    sequence_
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

msgPackClassTests :: Spec
msgPackClassTests =
  describe "MsgPack.Class instances" $
    sequence_
      [ it "Bool roundtrip" $ do
          MC.fromMsgPack (MC.toMsgPack True) `shouldBe` Right True
          MC.fromMsgPack (MC.toMsgPack False) `shouldBe` Right False
      , it "Int roundtrip" $ do
          MC.fromMsgPack (MC.toMsgPack (42 :: Int)) `shouldBe` Right (42 :: Int)
          MC.fromMsgPack (MC.toMsgPack (-7 :: Int)) `shouldBe` Right (-7 :: Int)
      , it "Text roundtrip" $
          MC.fromMsgPack (MC.toMsgPack ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)
      , it "Maybe roundtrip" $ do
          MC.fromMsgPack (MC.toMsgPack (Just (42 :: Int))) `shouldBe` Right (Just (42 :: Int))
          MC.fromMsgPack (MC.toMsgPack (Nothing :: Maybe Int)) `shouldBe` Right (Nothing :: Maybe Int)
      , it "List roundtrip" $
          MC.fromMsgPack (MC.toMsgPack [1, 2, 3 :: Int]) `shouldBe` Right [1, 2, 3 :: Int]
      , it "Vector roundtrip" $ do
          let v = V.fromList [10, 20, 30 :: Int]
          MC.fromMsgPack (MC.toMsgPack v) `shouldBe` Right v
      , it "Map roundtrip" $ do
          let m = Map.fromList [("a" :: Text, 1 :: Int), ("b", 2)]
          MC.fromMsgPack (MC.toMsgPack m) `shouldBe` Right m
      , it "(a,b) roundtrip" $
          MC.fromMsgPack (MC.toMsgPack ("x" :: Text, 42 :: Int)) `shouldBe` Right ("x" :: Text, 42 :: Int)
      , it "() roundtrip" $
          MC.fromMsgPack (MC.toMsgPack ()) `shouldBe` Right ()
      , it "Double roundtrip" $
          MC.fromMsgPack (MC.toMsgPack (3.14 :: Double)) `shouldBe` Right (3.14 :: Double)
      , it "Binary encode/decode via ByteString" $ do
          let bs = MC.encodeMsgPack (42 :: Int)
          MC.decodeMsgPack bs `shouldBe` Right (42 :: Int)
      ]


--------------------------------------------------------------------------------
-- CBOR typeclass instances
--------------------------------------------------------------------------------

cborClassTests :: Spec
cborClassTests =
  describe "CBOR.Class instances" $
    sequence_
      [ it "Bool roundtrip" $ do
          CC.fromCBOR (CC.toCBOR True) `shouldBe` Right True
          CC.fromCBOR (CC.toCBOR False) `shouldBe` Right False
      , it "Int roundtrip" $ do
          CC.fromCBOR (CC.toCBOR (42 :: Int)) `shouldBe` Right (42 :: Int)
          CC.fromCBOR (CC.toCBOR (-7 :: Int)) `shouldBe` Right (-7 :: Int)
      , it "Text roundtrip" $
          CC.fromCBOR (CC.toCBOR ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)
      , it "Maybe roundtrip" $ do
          CC.fromCBOR (CC.toCBOR (Just (42 :: Int))) `shouldBe` Right (Just (42 :: Int))
          CC.fromCBOR (CC.toCBOR (Nothing :: Maybe Int)) `shouldBe` Right (Nothing :: Maybe Int)
      , it "List roundtrip" $
          CC.fromCBOR (CC.toCBOR [1, 2, 3 :: Int]) `shouldBe` Right [1, 2, 3 :: Int]
      , it "Binary encode/decode via ByteString" $ do
          let bs = CC.encodeCBOR (42 :: Int)
          CC.decodeCBOR bs `shouldBe` Right (42 :: Int)
      ]


--------------------------------------------------------------------------------
-- BSON typeclass instances
--------------------------------------------------------------------------------

bsonClassTests :: Spec
bsonClassTests =
  describe "BSON.Class instances" $
    sequence_
      [ it "Bool roundtrip" $ do
          BC.fromBSON (BC.toBSON True) `shouldBe` Right True
          BC.fromBSON (BC.toBSON False) `shouldBe` Right False
      , it "Int32 roundtrip" $ do
          BC.fromBSON (BC.toBSON (42 :: Int)) `shouldBe` Right (42 :: Int)
          BC.fromBSON (BC.toBSON (-7 :: Int)) `shouldBe` Right (-7 :: Int)
      , it "Text roundtrip" $
          BC.fromBSON (BC.toBSON ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)
      , it "Double roundtrip" $
          BC.fromBSON (BC.toBSON (3.14 :: Double)) `shouldBe` Right (3.14 :: Double)
      , it "List roundtrip" $
          BC.fromBSON (BC.toBSON [1, 2, 3 :: Int]) `shouldBe` Right [1, 2, 3 :: Int]
      ]


--------------------------------------------------------------------------------
-- EDN typeclass instances
--------------------------------------------------------------------------------

ednClassTests :: Spec
ednClassTests =
  describe "EDN.Class instances" $
    sequence_
      [ it "Bool roundtrip" $ do
          EC.fromEDN (EC.toEDN True) `shouldBe` Right True
          EC.fromEDN (EC.toEDN False) `shouldBe` Right False
      , it "Int roundtrip" $
          EC.fromEDN (EC.toEDN (42 :: Int)) `shouldBe` Right (42 :: Int)
      , it "Text roundtrip" $
          EC.fromEDN (EC.toEDN ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)
      , it "List roundtrip" $
          EC.fromEDN (EC.toEDN [1, 2, 3 :: Int]) `shouldBe` Right [1, 2, 3 :: Int]
      ]


--------------------------------------------------------------------------------
-- Ion typeclass instances
--------------------------------------------------------------------------------

ionClassTests :: Spec
ionClassTests =
  describe "Ion.Class instances" $
    sequence_
      [ it "Bool roundtrip" $ do
          IC.fromIon (IC.toIon True) `shouldBe` Right True
          IC.fromIon (IC.toIon False) `shouldBe` Right False
      , it "Int roundtrip" $
          IC.fromIon (IC.toIon (42 :: Int)) `shouldBe` Right (42 :: Int)
      , it "Text roundtrip" $
          IC.fromIon (IC.toIon ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)
      , it "List roundtrip" $
          IC.fromIon (IC.toIon [1, 2, 3 :: Int]) `shouldBe` Right [1, 2, 3 :: Int]
      ]


--------------------------------------------------------------------------------
-- MsgPack Generic deriving
--------------------------------------------------------------------------------

msgPackGenericTests :: Spec
msgPackGenericTests =
  describe "MsgPack Generic deriving" $
    sequence_
      [ it "Person to/from MsgPack value" $ do
          let p = Person "Alice" 30
              v = MC.toMsgPack p
              expected =
                MV.Map
                  ( V.fromList
                      [ (MV.String "name", MV.String "Alice")
                      , (MV.String "age", MV.Word 30)
                      ]
                  )
          v `shouldBe` expected
          MC.fromMsgPack v `shouldBe` Right p
      , it "Person binary roundtrip" $ do
          let p = Person "Bob" 25
          MC.decodeMsgPack (MC.encodeMsgPack p) `shouldBe` Right p
      , it "Address generic roundtrip" $ do
          let a = Address "123 Main St" "Springfield" 62701
          MC.fromMsgPack (MC.toMsgPack a) `shouldBe` Right a
      , it "Address binary roundtrip" $ do
          let a = Address "456 Oak Ave" "Shelbyville" 62702
          MC.decodeMsgPack (MC.encodeMsgPack a) `shouldBe` Right a
      ]


--------------------------------------------------------------------------------
-- CBOR Generic deriving
--------------------------------------------------------------------------------

cborGenericTests :: Spec
cborGenericTests =
  describe "CBOR Generic deriving" $
    sequence_
      [ it "Person to/from CBOR value" $ do
          let p = Person "Alice" 30
              v = CC.toCBOR p
              expected =
                CV.Map
                  ( V.fromList
                      [ (CV.TextString "name", CV.TextString "Alice")
                      , (CV.TextString "age", CV.UInt 30)
                      ]
                  )
          v `shouldBe` expected
          CC.fromCBOR v `shouldBe` Right p
      , it "Person binary roundtrip" $ do
          let p = Person "Bob" 25
          CC.decodeCBOR (CC.encodeCBOR p) `shouldBe` Right p
      , it "Address generic roundtrip" $ do
          let a = Address "123 Main St" "Springfield" 62701
          CC.fromCBOR (CC.toCBOR a) `shouldBe` Right a
      ]


--------------------------------------------------------------------------------
-- BSON Generic deriving
--------------------------------------------------------------------------------

bsonGenericTests :: Spec
bsonGenericTests =
  describe "BSON Generic deriving" $
    sequence_
      [ it "Person to/from BSON value" $ do
          let p = Person "Alice" 30
              v = BC.toBSON p
          case v of
            BV.Document _ -> pure ()
            _ -> expectationFailure "expected Document"
          BC.fromBSON v `shouldBe` Right p
      , it "Person binary roundtrip" $ do
          let p = Person "Bob" 25
          BC.decodeBSON (BC.encodeBSON p) `shouldBe` Right p
      , it "Address generic roundtrip" $ do
          let a = Address "123 Main St" "Springfield" 62701
          BC.fromBSON (BC.toBSON a) `shouldBe` Right a
      ]


--------------------------------------------------------------------------------
-- EDN Generic deriving
--------------------------------------------------------------------------------

ednGenericTests :: Spec
ednGenericTests =
  describe "EDN Generic deriving" $
    sequence_
      [ it "Person to/from EDN value" $ do
          let p = Person "Alice" 30
              v = EC.toEDN p
          case v of
            EV.Map _ -> pure ()
            _ -> expectationFailure "expected Map"
          EC.fromEDN v `shouldBe` Right p
      , it "Person text roundtrip" $ do
          let p = Person "Bob" 25
          EC.decodeEDN (EC.encodeEDN p) `shouldBe` Right p
      ]


--------------------------------------------------------------------------------
-- Ion Generic deriving
--------------------------------------------------------------------------------

ionGenericTests :: Spec
ionGenericTests =
  describe "Ion Generic deriving" $
    sequence_
      [ it "Person to/from Ion value" $ do
          let p = Person "Alice" 30
              v = IC.toIon p
          case v of
            IV.Struct _ -> pure ()
            _ -> expectationFailure "expected Struct"
          IC.fromIon v `shouldBe` Right p
      , it "Person binary roundtrip" $ do
          let p = Person "Bob" 25
          IC.decodeIon (IC.encodeIon p) `shouldBe` Right p
      , it "Address generic roundtrip" $ do
          let a = Address "123 Main St" "Springfield" 62701
          IC.fromIon (IC.toIon a) `shouldBe` Right a
      ]


--------------------------------------------------------------------------------
-- Aeson-parity instances (NonEmpty, Either, Set, Seq, IntMap, IntSet, HashMap,
-- HashSet, 3- and 4-tuples, Identity, Const, Down, Version, Ratio, Char,
-- Natural, lazy Text, etc.) -- spot-check round-trips across formats.
--------------------------------------------------------------------------------

aesonParityTests :: Spec
aesonParityTests =
  describe "Aeson-parity round-trips" $
    sequence_
      [ msgPackParityTests
      , cborParityTests
      , bsonParityTests
      , ednParityTests
      , ionParityTests
      , functorNewtypeTests
      ]


-- Helpers to make explicit type signatures on the recovered value the only
-- annotation site needed.
rtMP :: forall a. (MC.ToMsgPack a, MC.FromMsgPack a, Eq a, Show a) => a -> IO ()
rtMP x = MC.fromMsgPack (MC.toMsgPack x) `shouldBe` Right x


rtCC :: forall a. (CC.ToCBOR a, CC.FromCBOR a, Eq a, Show a) => a -> IO ()
rtCC x = CC.fromCBOR (CC.toCBOR x) `shouldBe` Right x


rtBC :: forall a. (BC.ToBSON a, BC.FromBSON a, Eq a, Show a) => a -> IO ()
rtBC x = BC.fromBSON (BC.toBSON x) `shouldBe` Right x


rtEC :: forall a. (EC.ToEDN a, EC.FromEDN a, Eq a, Show a) => a -> IO ()
rtEC x = EC.fromEDN (EC.toEDN x) `shouldBe` Right x


rtIC :: forall a. (IC.ToIon a, IC.FromIon a, Eq a, Show a) => a -> IO ()
rtIC x = IC.fromIon (IC.toIon x) `shouldBe` Right x


msgPackParityTests :: Spec
msgPackParityTests =
  describe "MsgPack" $
    sequence_
      [ it "Integer roundtrip" $ do rtMP (12345678901234567 :: Integer); rtMP (-1 :: Integer)
      , it "Natural roundtrip" $ rtMP (42 :: Natural)
      , it "Lazy Text roundtrip" $ rtMP (TL.pack "hello lazy world")
      , it "NonEmpty roundtrip" $ rtMP (1 :| [2, 3 :: Int])
      , it "Either roundtrip" $ do
          rtMP (Left "left" :: Either Text Int)
          rtMP (Right 99 :: Either Text Int)
      , it "Set roundtrip" $ rtMP (Set.fromList [1, 2, 3 :: Int])
      , it "Seq roundtrip" $ rtMP (Seq.fromList [10, 20 :: Int])
      , it "IntMap roundtrip" $ rtMP (IntMap.fromList [(1, "a" :: Text), (2, "b")])
      , it "IntSet roundtrip" $ rtMP (IntSet.fromList [3, 4, 5])
      , it "HashMap roundtrip" $ rtMP (HM.fromList [("k" :: Text, 1 :: Int)])
      , it "HashSet roundtrip" $ rtMP (HS.fromList [1, 2 :: Int])
      , it "3-tuple roundtrip" $ rtMP ("x" :: Text, 1 :: Int, True)
      , it "4-tuple roundtrip" $ rtMP (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int)
      , it "Identity roundtrip" $ rtMP (Identity (5 :: Int))
      , it "Down roundtrip" $ rtMP (Down (7 :: Int))
      , it "Version roundtrip" $ rtMP (makeVersion [1, 2, 3])
      , it "Ratio roundtrip" $ rtMP (3 % 4 :: Rational)
      ]


cborParityTests :: Spec
cborParityTests =
  describe "CBOR" $
    sequence_
      [ it "Integer roundtrip" $ do rtCC (98765432109876 :: Integer); rtCC (-3 :: Integer)
      , it "Natural roundtrip" $ rtCC (42 :: Natural)
      , it "Lazy Text roundtrip" $ rtCC (TL.pack "lazy")
      , it "NonEmpty roundtrip" $ rtCC (1 :| [2 :: Int])
      , it "Either roundtrip" $ do
          rtCC (Left "x" :: Either Text Int)
          rtCC (Right 1 :: Either Text Int)
      , it "Set roundtrip" $ rtCC (Set.fromList [1, 2 :: Int])
      , it "Seq roundtrip" $ rtCC (Seq.fromList ["a" :: Text, "b"])
      , it "IntMap roundtrip" $ rtCC (IntMap.fromList [(1, True), (2, False)])
      , it "IntSet roundtrip" $ rtCC (IntSet.fromList [10, 20])
      , it "HashMap roundtrip" $ rtCC (HM.fromList [("k" :: Text, 5 :: Int)])
      , it "HashSet roundtrip" $ rtCC (HS.fromList [1, 2 :: Int])
      , it "3-tuple roundtrip" $ rtCC ("x" :: Text, 1 :: Int, True)
      , it "4-tuple roundtrip" $ rtCC (1 :: Int, "b" :: Text, True, 4 :: Int)
      , it "Identity roundtrip" $ rtCC (Identity ("hi" :: Text))
      , it "Down roundtrip" $ rtCC (Down (7 :: Int))
      , it "Version roundtrip" $ rtCC (makeVersion [2])
      , it "Ratio roundtrip" $ rtCC (5 % 7 :: Rational)
      ]


bsonParityTests :: Spec
bsonParityTests =
  describe "BSON" $
    sequence_
      [ it "Integer (small) roundtrip" $ rtBC (123 :: Integer)
      , it "Natural roundtrip" $ rtBC (42 :: Natural)
      , it "NonEmpty roundtrip" $ rtBC (1 :| [2 :: Int])
      , it "Either roundtrip" $ do
          rtBC (Left "x" :: Either Text Int)
          rtBC (Right 7 :: Either Text Int)
      , it "Set roundtrip" $ rtBC (Set.fromList [1, 2 :: Int])
      , it "Seq roundtrip" $ rtBC (Seq.fromList [10, 20 :: Int])
      , it "IntMap roundtrip" $ rtBC (IntMap.fromList [(1, "a" :: Text)])
      , it "IntSet roundtrip" $ rtBC (IntSet.fromList [3, 4])
      , it "3-tuple roundtrip" $ rtBC ("x" :: Text, 1 :: Int, True)
      , it "Identity roundtrip" $ rtBC (Identity (5 :: Int))
      , it "Down roundtrip" $ rtBC (Down (7 :: Int))
      , it "Version roundtrip" $ rtBC (makeVersion [1, 2])
      ]


ednParityTests :: Spec
ednParityTests =
  describe "EDN" $
    sequence_
      [ it "Char roundtrip" $ rtEC 'x'
      , it "Integer roundtrip" $ rtEC (12345 :: Integer)
      , it "Natural roundtrip" $ rtEC (42 :: Natural)
      , it "Lazy Text roundtrip" $ rtEC (TL.pack "lazy")
      , it "NonEmpty roundtrip" $ rtEC (1 :| [2 :: Int])
      , it "Either roundtrip" $ do
          rtEC (Left "x" :: Either Text Int)
          rtEC (Right 1 :: Either Text Int)
      , it "Set roundtrip" $ rtEC (Set.fromList [1, 2 :: Int])
      , it "Seq roundtrip" $ rtEC (Seq.fromList ["a" :: Text])
      , it "IntMap roundtrip" $ rtEC (IntMap.fromList [(1, True)])
      , it "IntSet roundtrip" $ rtEC (IntSet.fromList [10])
      , it "HashSet roundtrip" $ rtEC (HS.fromList [1, 2 :: Int])
      , it "3-tuple roundtrip" $ rtEC ("x" :: Text, 1 :: Int, True)
      , it "Identity roundtrip" $ rtEC (Identity ("hi" :: Text))
      , it "Version roundtrip" $ rtEC (makeVersion [3])
      ]


ionParityTests :: Spec
ionParityTests =
  describe "Ion" $
    sequence_
      [ it "Char roundtrip" $ rtIC 'x'
      , it "Integer (small) roundtrip" $ rtIC (12345 :: Integer)
      , it "Natural roundtrip" $ rtIC (42 :: Natural)
      , it "Lazy Text roundtrip" $ rtIC (TL.pack "lazy")
      , it "NonEmpty roundtrip" $ rtIC (1 :| [2 :: Int])
      , it "Either roundtrip" $ do
          rtIC (Left "x" :: Either Text Int)
          rtIC (Right 1 :: Either Text Int)
      , it "Set roundtrip" $ rtIC (Set.fromList [1, 2 :: Int])
      , it "Seq roundtrip" $ rtIC (Seq.fromList [10, 20 :: Int])
      , it "IntMap roundtrip" $ rtIC (IntMap.fromList [(1, "a" :: Text)])
      , it "IntSet roundtrip" $ rtIC (IntSet.fromList [10])
      , it "HashMap (Text k) roundtrip" $ rtIC (HM.fromList [("k" :: Text, 1 :: Int)])
      , it "HashSet roundtrip" $ rtIC (HS.fromList [1, 2 :: Int])
      , it "3-tuple roundtrip" $ rtIC ("x" :: Text, 1 :: Int, True)
      , it "Identity roundtrip" $ rtIC (Identity (5 :: Int))
      , it "Down roundtrip" $ rtIC (Down (7 :: Int))
      , it "Version roundtrip" $ rtIC (makeVersion [1])
      ]


--------------------------------------------------------------------------------
-- Functor / monoid newtype round-trips. We exercise one format per
-- class-shape (binary tagged-map, AST-wrapped, native union) so the
-- coverage is broad without exploding the test count.
--------------------------------------------------------------------------------

functorNewtypeTests :: Spec
functorNewtypeTests =
  describe "Functor / monoid newtypes" $
    sequence_
      [ describe "MsgPack" $
          sequence_
            [ it "Sum" $ rtMP (Mon.Sum (5 :: Int))
            , it "Product" $ rtMP (Mon.Product (6 :: Int))
            , it "Dual" $ rtMP (Mon.Dual ("x" :: Text))
            , it "All" $ rtMP (Mon.All True)
            , it "Any" $ rtMP (Mon.Any True)
            , it "First" $ rtMP (Mon.First (Just (1 :: Int)))
            , it "Last" $ rtMP (Mon.Last (Just (2 :: Int)))
            , it "Min" $ rtMP (Semi.Min (3 :: Int))
            , it "Max" $ rtMP (Semi.Max (4 :: Int))
            , it "Semi.First" $ rtMP (Semi.First (5 :: Int))
            , it "Semi.Last" $ rtMP (Semi.Last (6 :: Int))
            , it "WrappedMonoid" $ rtMP (Semi.WrapMonoid ("hi" :: Text))
            , it "Arg" $ rtMP (Semi.Arg (1 :: Int) ("x" :: Text))
            , it "Compose" $ rtMP (Compose [Just (1 :: Int), Nothing, Just 2])
            , it "Functor.Product" $
                rtMP (FProduct.Pair (Identity (1 :: Int)) (Identity (2 :: Int)))
            , it "Functor.Sum (InL)" $
                rtMP (FSum.InL (Identity (1 :: Int)) :: FSum.Sum Identity Identity Int)
            , it "Functor.Sum (InR)" $
                rtMP (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
            ]
      , describe "CBOR" $
          sequence_
            [ it "Sum" $ rtCC (Mon.Sum (5 :: Int))
            , it "Min" $ rtCC (Semi.Min (3 :: Int))
            , it "Arg" $ rtCC (Semi.Arg (1 :: Int) ("x" :: Text))
            , it "Compose" $ rtCC (Compose [Just (1 :: Int), Nothing])
            , it "Functor.Product" $
                rtCC (FProduct.Pair (Identity (1 :: Int)) (Identity (2 :: Int)))
            , it "Functor.Sum (InR)" $
                rtCC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
            ]
      , describe "BSON" $
          sequence_
            [ it "Sum" $ rtBC (Mon.Sum (5 :: Int))
            , it "Arg" $ rtBC (Semi.Arg (1 :: Int) ("x" :: Text))
            , it "Functor.Sum (InL)" $
                rtBC (FSum.InL (Identity (1 :: Int)) :: FSum.Sum Identity Identity Int)
            ]
      , describe "EDN" $
          sequence_
            [ it "Sum" $ rtEC (Mon.Sum (5 :: Int))
            , it "Compose" $ rtEC (Compose [Just (1 :: Int)])
            , it "Functor.Sum (InR)" $
                rtEC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
            ]
      , describe "Ion" $
          sequence_
            [ it "Sum" $ rtIC (Mon.Sum (5 :: Int))
            , it "Arg" $ rtIC (Semi.Arg (1 :: Int) ("x" :: Text))
            , it "Functor.Sum (InR)" $
                rtIC (FSum.InR (Identity (2 :: Int)) :: FSum.Sum Identity Identity Int)
            ]
      ]


--------------------------------------------------------------------------------
-- Direct (toEncoding) parity: the direct path must produce the same
-- bytes as the AST path.
--------------------------------------------------------------------------------

directEncodingTests :: Spec
directEncodingTests =
  describe "Direct toEncoding parity" $
    sequence_
      [ describe "MsgPack" $
          sequence_
            [ it "Bool" $ MC.encodeMsgPackDirect True `shouldBe` MC.encodeMsgPack True
            , it "Int" $ MC.encodeMsgPackDirect (123 :: Int) `shouldBe` MC.encodeMsgPack (123 :: Int)
            , it "Negative" $ MC.encodeMsgPackDirect (-7 :: Int) `shouldBe` MC.encodeMsgPack (-7 :: Int)
            , it "Text" $ MC.encodeMsgPackDirect ("hello" :: Text) `shouldBe` MC.encodeMsgPack ("hello" :: Text)
            , it "List" $ MC.encodeMsgPackDirect [1, 2, 3 :: Int] `shouldBe` MC.encodeMsgPack [1, 2, 3 :: Int]
            , it "Maybe" $ MC.encodeMsgPackDirect (Just (5 :: Int)) `shouldBe` MC.encodeMsgPack (Just (5 :: Int))
            , it "Map" $
                MC.encodeMsgPackDirect (Map.fromList [("a" :: Text, 1 :: Int)])
                  `shouldBe` MC.encodeMsgPack (Map.fromList [("a" :: Text, 1 :: Int)])
            , it "Person (default through Value)" $
                MC.encodeMsgPackDirect (Person "Alice" 30) `shouldBe` MC.encodeMsgPack (Person "Alice" 30)
            ]
      , describe "CBOR" $
          sequence_
            [ it "Bool" $ CC.encodeCBORDirect True `shouldBe` CC.encodeCBOR True
            , it "Int" $ CC.encodeCBORDirect (123 :: Int) `shouldBe` CC.encodeCBOR (123 :: Int)
            , it "Negative" $ CC.encodeCBORDirect (-7 :: Int) `shouldBe` CC.encodeCBOR (-7 :: Int)
            , it "Text" $ CC.encodeCBORDirect ("hello" :: Text) `shouldBe` CC.encodeCBOR ("hello" :: Text)
            , it "List" $ CC.encodeCBORDirect [1, 2, 3 :: Int] `shouldBe` CC.encodeCBOR [1, 2, 3 :: Int]
            , it "Maybe" $ CC.encodeCBORDirect (Just (5 :: Int)) `shouldBe` CC.encodeCBOR (Just (5 :: Int))
            , it "Map" $
                CC.encodeCBORDirect (Map.fromList [("a" :: Text, 1 :: Int)])
                  `shouldBe` CC.encodeCBOR (Map.fromList [("a" :: Text, 1 :: Int)])
            , it "Person (default through Value)" $
                CC.encodeCBORDirect (Person "Alice" 30) `shouldBe` CC.encodeCBOR (Person "Alice" 30)
            ]
      , describe "BSON" $
          sequence_
            [ it "Person (Encoding wraps Value)" $
                BC.encodeBSONDirect (Person "Alice" 30) `shouldBe` BC.encodeBSON (Person "Alice" 30)
            ]
      , describe "EDN" $
          sequence_
            [ it "Bool" $ EC.encodeEDNDirect True `shouldBe` EC.encodeEDN True
            , it "Int" $ EC.encodeEDNDirect (5 :: Int) `shouldBe` EC.encodeEDN (5 :: Int)
            , it "Text" $ EC.encodeEDNDirect ("hello" :: Text) `shouldBe` EC.encodeEDN ("hello" :: Text)
            , it "List" $ EC.encodeEDNDirect [1, 2 :: Int] `shouldBe` EC.encodeEDN [1, 2 :: Int]
            , it "Person (default through Value)" $
                EC.encodeEDNDirect (Person "Alice" 30) `shouldBe` EC.encodeEDN (Person "Alice" 30)
            ]
      , describe "Ion" $
          sequence_
            [ it "Person (Encoding wraps Value)" $
                IC.encodeIonDirect (Person "Alice" 30) `shouldBe` IC.encodeIon (Person "Alice" 30)
            ]
      , describe "Bencode (BEP-3 dict-key sort)" $
          sequence_
            [ it "encode sorts dict keys (raw byte order)" $ do
                -- "z" should land after "a" regardless of insertion order.
                let v =
                      BencodeV.BDict
                        ( V.fromList
                            [ ("z", BencodeV.BInteger 26)
                            , ("a", BencodeV.BInteger 1)
                            ]
                        )
                    expected =
                      BencodeE.encode
                        ( BencodeV.BDict
                            ( V.fromList
                                [ ("a", BencodeV.BInteger 1)
                                , ("z", BencodeV.BInteger 26)
                                ]
                            )
                        )
                BencodeE.encode v `shouldBe` expected
            , it "encode sorts by raw byte order, not numeric" $ do
                -- "10" sorts before "2" lex / byte-wise even though 2 < 10.
                let v =
                      BencodeV.BDict
                        ( V.fromList
                            [ ("2", BencodeV.BInteger 2)
                            , ("10", BencodeV.BInteger 10)
                            ]
                        )
                    encoded = BencodeE.encode v
                encoded `shouldBe` "d2:10i10e1:2i2ee"
            , it "encode sorts nested dicts" $ do
                let v =
                      BencodeV.BDict
                        ( V.fromList
                            [
                              ( "outer"
                              , BencodeV.BDict
                                  ( V.fromList
                                      [ ("z", BencodeV.BInteger 1)
                                      , ("a", BencodeV.BInteger 2)
                                      ]
                                  )
                              )
                            ]
                        )
                    encoded = BencodeE.encode v
                encoded `shouldBe` "d5:outerd1:ai2e1:zi1eee"
            , it "direct encoding sorts dict keys too" $ do
                let direct =
                      BencodeEncoding.encodingToByteString
                        ( BencodeEncoding.dictFromList
                            [ ("z", BencodeEncoding.int 1)
                            , ("a", BencodeEncoding.int 2)
                            ]
                        )
                direct `shouldBe` "d1:ai2e1:zi1ee"
            ]
      ]
