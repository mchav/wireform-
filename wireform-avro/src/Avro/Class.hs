{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

{- | Typeclass-based Avro serialization.

Provides 'ToAvro' and 'FromAvro' typeclasses for converting Haskell
values to\/from 'Avro.Value.Value'. Instances are provided for common
types: 'Bool', 'Int', 'Int32', 'Int64', 'Float', 'Double', 'Text',
'ByteString', 'Vector', and 'Maybe' (as Avro unions).
-}
module Avro.Class (
  ToAvro (..),
  FromAvro (..),
) where

import Avro.Encoding (Encoding)
import Avro.Encoding qualified as Enc
import Avro.Value qualified as AV
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FProduct
import Data.Functor.Sum qualified as FSum
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Mon
import Data.Ord (Down (..))
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Semigroup qualified as Semi
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)


class ToAvro a where
  toAvro :: a -> AV.Value


  {- | aeson-style direct encoder. Avro requires a schema to write
  the wire bytes, so 'Encoding' wraps an 'AV.Value'; the API is
  provided for parity with the other formats.
  -}
  toEncoding :: a -> Encoding
  toEncoding = Enc.value . toAvro


class FromAvro a where
  fromAvro :: AV.Value -> Either String a


instance ToAvro Bool where
  toAvro = AV.Bool


instance FromAvro Bool where
  fromAvro (AV.Bool b) = Right b
  fromAvro _ = Left "FromAvro Bool: expected Bool"


instance ToAvro Int where
  toAvro n = AV.Long (fromIntegral n)


instance FromAvro Int where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int: expected Int or Long"


instance ToAvro Int8 where
  toAvro n = AV.Int (fromIntegral n)


instance FromAvro Int8 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int8: expected Int or Long"


instance ToAvro Int16 where
  toAvro n = AV.Int (fromIntegral n)


instance FromAvro Int16 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int16: expected Int or Long"


instance ToAvro Int32 where
  toAvro = AV.Int


instance FromAvro Int32 where
  fromAvro (AV.Int n) = Right n
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int32: expected Int or Long"


instance ToAvro Int64 where
  toAvro = AV.Long


instance FromAvro Int64 where
  fromAvro (AV.Long n) = Right n
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int64: expected Long or Int"


instance ToAvro Word where
  toAvro n = AV.Long (fromIntegral n)


instance FromAvro Word where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word: expected Long or Int"


instance ToAvro Word8 where
  toAvro n = AV.Int (fromIntegral n)


instance FromAvro Word8 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word8: expected Int or Long"


instance ToAvro Word16 where
  toAvro n = AV.Int (fromIntegral n)


instance FromAvro Word16 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word16: expected Int or Long"


instance ToAvro Word32 where
  toAvro n = AV.Long (fromIntegral n)


instance FromAvro Word32 where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word32: expected Long or Int"


instance ToAvro Word64 where
  toAvro n = AV.Long (fromIntegral n)


instance FromAvro Word64 where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word64: expected Long or Int"


instance ToAvro Float where
  toAvro = AV.Float


instance FromAvro Float where
  fromAvro (AV.Float f) = Right f
  fromAvro (AV.Double d) = Right (realToFrac d)
  fromAvro _ = Left "FromAvro Float: expected Float or Double"


instance ToAvro Double where
  toAvro = AV.Double


instance FromAvro Double where
  fromAvro (AV.Double d) = Right d
  fromAvro (AV.Float f) = Right (realToFrac f)
  fromAvro _ = Left "FromAvro Double: expected Double or Float"


instance ToAvro Text where
  toAvro = AV.String


instance FromAvro Text where
  fromAvro (AV.String t) = Right t
  fromAvro _ = Left "FromAvro Text: expected String"


instance ToAvro ByteString where
  toAvro = AV.Bytes


instance FromAvro ByteString where
  fromAvro (AV.Bytes bs) = Right bs
  fromAvro (AV.Fixed bs) = Right bs
  fromAvro _ = Left "FromAvro ByteString: expected Bytes or Fixed"


instance ToAvro () where
  toAvro () = AV.Null


instance FromAvro () where
  fromAvro AV.Null = Right ()
  fromAvro _ = Left "FromAvro (): expected Null"


instance ToAvro a => ToAvro (Maybe a) where
  toAvro Nothing = AV.Null
  toAvro (Just x) = toAvro x


instance FromAvro a => FromAvro (Maybe a) where
  fromAvro AV.Null = Right Nothing
  fromAvro v = Just <$> fromAvro v


instance ToAvro a => ToAvro [a] where
  toAvro xs = AV.Array (V.fromList (map toAvro xs))


instance FromAvro a => FromAvro [a] where
  fromAvro (AV.Array vs) = traverse fromAvro (V.toList vs)
  fromAvro _ = Left "FromAvro [a]: expected Array"


instance ToAvro a => ToAvro (Vector a) where
  toAvro xs = AV.Array (V.map toAvro xs)


instance FromAvro a => FromAvro (Vector a) where
  fromAvro (AV.Array vs) = V.mapM fromAvro vs
  fromAvro _ = Left "FromAvro Vector: expected Array"


instance (ToAvro a, ToAvro b) => ToAvro (a, b) where
  toAvro (a, b) = AV.Array (V.fromList [toAvro a, toAvro b])


instance (FromAvro a, FromAvro b) => FromAvro (a, b) where
  fromAvro (AV.Array vs)
    | V.length vs == 2 = (,) <$> fromAvro (vs V.! 0) <*> fromAvro (vs V.! 1)
  fromAvro _ = Left "FromAvro (a,b): expected Array of length 2"


-- Aeson-parity instances ---------------------------------------------------

instance ToAvro Char where
  toAvro c = AV.String (T.singleton c)


instance FromAvro Char where
  fromAvro (AV.String t) | T.length t == 1 = Right (T.head t)
  fromAvro _ = Left "FromAvro Char: expected single-character String"


instance ToAvro Integer where
  toAvro = AV.Long . fromInteger


instance FromAvro Integer where
  fromAvro (AV.Long n) = Right (toInteger n)
  fromAvro (AV.Int n) = Right (toInteger n)
  fromAvro _ = Left "FromAvro Integer: expected Int or Long"


instance ToAvro Natural where
  toAvro = AV.Long . fromIntegral


instance FromAvro Natural where
  fromAvro (AV.Long n) | n >= 0 = Right (fromIntegral n)
  fromAvro (AV.Int n) | n >= 0 = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Natural: expected non-negative Int or Long"


instance ToAvro TL.Text where
  toAvro = AV.String . TL.toStrict


instance FromAvro TL.Text where
  fromAvro v = TL.fromStrict <$> fromAvro v


instance ToAvro BSL.ByteString where
  toAvro = AV.Bytes . BSL.toStrict


instance FromAvro BSL.ByteString where
  fromAvro v = BSL.fromStrict <$> fromAvro v


instance ToAvro a => ToAvro (NonEmpty a) where
  toAvro = toAvro . NE.toList


instance FromAvro a => FromAvro (NonEmpty a) where
  fromAvro v = do
    xs <- fromAvro v
    case xs of
      [] -> Left "FromAvro NonEmpty: empty array"
      (y : ys) -> Right (y :| ys)


-- | 'Either' encodes as an Avro union with branches in [Left, Right] order.
instance (ToAvro a, ToAvro b) => ToAvro (Either a b) where
  toAvro (Left x) = AV.Union 0 (toAvro x)
  toAvro (Right x) = AV.Union 1 (toAvro x)


instance (FromAvro a, FromAvro b) => FromAvro (Either a b) where
  fromAvro (AV.Union 0 v) = Left <$> fromAvro v
  fromAvro (AV.Union 1 v) = Right <$> fromAvro v
  fromAvro _ = Left "FromAvro Either: expected Union 0/1"


instance (Ord a, ToAvro a) => ToAvro (Set a) where
  toAvro = AV.Array . V.fromList . fmap toAvro . Set.toList


instance (Ord a, FromAvro a) => FromAvro (Set a) where
  fromAvro v = Set.fromList <$> fromAvro v


instance ToAvro a => ToAvro (Seq a) where
  toAvro s = AV.Array (V.fromList (fmap toAvro (foldr (:) [] s)))


instance FromAvro a => FromAvro (Seq a) where
  fromAvro v = Seq.fromList <$> fromAvro v


{- | A 'Map' keyed by 'Text' encodes as an Avro map. Other key types
fall back to an array of pairs to preserve faithful round-tripping.
-}
instance ToAvro v => ToAvro (Map Text v) where
  toAvro m = AV.Map (V.fromList [(k, toAvro v) | (k, v) <- Map.toList m])


instance FromAvro v => FromAvro (Map Text v) where
  fromAvro (AV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) k <$> fromAvro v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromAvro _ = Left "FromAvro (Map Text v): expected Map"


instance ToAvro v => ToAvro (HashMap Text v) where
  toAvro m = AV.Map (V.fromList [(k, toAvro v) | (k, v) <- HM.toList m])


instance FromAvro v => FromAvro (HashMap Text v) where
  fromAvro (AV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) k <$> fromAvro v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromAvro _ = Left "FromAvro (HashMap Text v): expected Map"


instance ToAvro v => ToAvro (IntMap v) where
  toAvro m = AV.Map (V.fromList [(T.pack (show k), toAvro v) | (k, v) <- IntMap.toList m])


instance FromAvro v => FromAvro (IntMap v) where
  fromAvro (AV.Map kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case reads (T.unpack k) of
        [(i, "")] -> (,) i <$> fromAvro v
        _ -> Left "FromAvro IntMap: cannot parse Int key"
  fromAvro _ = Left "FromAvro IntMap: expected Map"


instance ToAvro IntSet where
  toAvro = AV.Array . V.fromList . fmap toAvro . IntSet.toList


instance FromAvro IntSet where
  fromAvro v = IntSet.fromList <$> fromAvro v


instance (Hashable a, ToAvro a) => ToAvro (HashSet a) where
  toAvro = AV.Array . V.fromList . fmap toAvro . HS.toList


instance (Eq a, Hashable a, FromAvro a) => FromAvro (HashSet a) where
  fromAvro v = HS.fromList <$> fromAvro v


instance (ToAvro a, ToAvro b, ToAvro c) => ToAvro (a, b, c) where
  toAvro (a, b, c) = AV.Array (V.fromList [toAvro a, toAvro b, toAvro c])


instance (FromAvro a, FromAvro b, FromAvro c) => FromAvro (a, b, c) where
  fromAvro (AV.Array vs)
    | V.length vs == 3 =
        (,,) <$> fromAvro (vs V.! 0) <*> fromAvro (vs V.! 1) <*> fromAvro (vs V.! 2)
  fromAvro _ = Left "FromAvro (a,b,c): expected Array of length 3"


instance (ToAvro a, ToAvro b, ToAvro c, ToAvro d) => ToAvro (a, b, c, d) where
  toAvro (a, b, c, d) = AV.Array (V.fromList [toAvro a, toAvro b, toAvro c, toAvro d])


instance (FromAvro a, FromAvro b, FromAvro c, FromAvro d) => FromAvro (a, b, c, d) where
  fromAvro (AV.Array vs)
    | V.length vs == 4 =
        (,,,)
          <$> fromAvro (vs V.! 0)
          <*> fromAvro (vs V.! 1)
          <*> fromAvro (vs V.! 2)
          <*> fromAvro (vs V.! 3)
  fromAvro _ = Left "FromAvro (a,b,c,d): expected Array of length 4"


instance ToAvro a => ToAvro (Identity a) where
  toAvro (Identity x) = toAvro x


instance FromAvro a => FromAvro (Identity a) where
  fromAvro v = Identity <$> fromAvro v


instance ToAvro a => ToAvro (Const a b) where
  toAvro (Const x) = toAvro x


instance FromAvro a => FromAvro (Const a b) where
  fromAvro v = Const <$> fromAvro v


instance ToAvro a => ToAvro (Down a) where
  toAvro (Down x) = toAvro x


instance FromAvro a => FromAvro (Down a) where
  fromAvro v = Down <$> fromAvro v


instance ToAvro Version where
  toAvro = toAvro . versionBranch


instance FromAvro Version where
  fromAvro v = makeVersion <$> fromAvro v


instance (Integral a, ToAvro a) => ToAvro (Ratio a) where
  toAvro r = AV.Array (V.fromList [toAvro (numerator r), toAvro (denominator r)])


instance (Integral a, FromAvro a) => FromAvro (Ratio a) where
  fromAvro (AV.Array vs)
    | V.length vs == 2 = do
        n <- fromAvro (vs V.! 0)
        d <- fromAvro (vs V.! 1)
        if d == 0
          then Left "FromAvro Ratio: zero denominator"
          else Right (n % d)
  fromAvro _ = Left "FromAvro Ratio: expected Array of length 2"


-- Functor / monoid newtype instances --------------------------------------

instance ToAvro a => ToAvro (Mon.Sum a) where
  toAvro = toAvro . Mon.getSum


instance FromAvro a => FromAvro (Mon.Sum a) where
  fromAvro v = Mon.Sum <$> fromAvro v


instance ToAvro a => ToAvro (Mon.Product a) where
  toAvro = toAvro . Mon.getProduct


instance FromAvro a => FromAvro (Mon.Product a) where
  fromAvro v = Mon.Product <$> fromAvro v


instance ToAvro a => ToAvro (Mon.Dual a) where
  toAvro = toAvro . Mon.getDual


instance FromAvro a => FromAvro (Mon.Dual a) where
  fromAvro v = Mon.Dual <$> fromAvro v


instance ToAvro Mon.All where
  toAvro = toAvro . Mon.getAll


instance FromAvro Mon.All where
  fromAvro v = Mon.All <$> fromAvro v


instance ToAvro Mon.Any where
  toAvro = toAvro . Mon.getAny


instance FromAvro Mon.Any where
  fromAvro v = Mon.Any <$> fromAvro v


instance ToAvro a => ToAvro (Mon.First a) where
  toAvro = toAvro . Mon.getFirst


instance FromAvro a => FromAvro (Mon.First a) where
  fromAvro v = Mon.First <$> fromAvro v


instance ToAvro a => ToAvro (Mon.Last a) where
  toAvro = toAvro . Mon.getLast


instance FromAvro a => FromAvro (Mon.Last a) where
  fromAvro v = Mon.Last <$> fromAvro v


instance ToAvro a => ToAvro (Semi.Min a) where
  toAvro = toAvro . Semi.getMin


instance FromAvro a => FromAvro (Semi.Min a) where
  fromAvro v = Semi.Min <$> fromAvro v


instance ToAvro a => ToAvro (Semi.Max a) where
  toAvro = toAvro . Semi.getMax


instance FromAvro a => FromAvro (Semi.Max a) where
  fromAvro v = Semi.Max <$> fromAvro v


instance ToAvro a => ToAvro (Semi.First a) where
  toAvro = toAvro . Semi.getFirst


instance FromAvro a => FromAvro (Semi.First a) where
  fromAvro v = Semi.First <$> fromAvro v


instance ToAvro a => ToAvro (Semi.Last a) where
  toAvro = toAvro . Semi.getLast


instance FromAvro a => FromAvro (Semi.Last a) where
  fromAvro v = Semi.Last <$> fromAvro v


instance ToAvro a => ToAvro (Semi.WrappedMonoid a) where
  toAvro = toAvro . Semi.unwrapMonoid


instance FromAvro a => FromAvro (Semi.WrappedMonoid a) where
  fromAvro v = Semi.WrapMonoid <$> fromAvro v


instance (ToAvro a, ToAvro b) => ToAvro (Semi.Arg a b) where
  toAvro (Semi.Arg a b) = AV.Array (V.fromList [toAvro a, toAvro b])


instance (FromAvro a, FromAvro b) => FromAvro (Semi.Arg a b) where
  fromAvro (AV.Array vs)
    | V.length vs == 2 = Semi.Arg <$> fromAvro (vs V.! 0) <*> fromAvro (vs V.! 1)
  fromAvro _ = Left "FromAvro Arg: expected Array of length 2"


instance ToAvro (f (g a)) => ToAvro (Compose f g a) where
  toAvro = toAvro . getCompose


instance FromAvro (f (g a)) => FromAvro (Compose f g a) where
  fromAvro v = Compose <$> fromAvro v


instance (ToAvro (f a), ToAvro (g a)) => ToAvro (FProduct.Product f g a) where
  toAvro (FProduct.Pair x y) = AV.Array (V.fromList [toAvro x, toAvro y])


instance (FromAvro (f a), FromAvro (g a)) => FromAvro (FProduct.Product f g a) where
  fromAvro (AV.Array vs)
    | V.length vs == 2 = FProduct.Pair <$> fromAvro (vs V.! 0) <*> fromAvro (vs V.! 1)
  fromAvro _ = Left "FromAvro Functor.Product: expected Array of length 2"


-- | 'Functor.Sum' uses the native Avro union so it round-trips cleanly.
instance (ToAvro (f a), ToAvro (g a)) => ToAvro (FSum.Sum f g a) where
  toAvro (FSum.InL x) = AV.Union 0 (toAvro x)
  toAvro (FSum.InR x) = AV.Union 1 (toAvro x)


instance (FromAvro (f a), FromAvro (g a)) => FromAvro (FSum.Sum f g a) where
  fromAvro (AV.Union 0 v) = FSum.InL <$> fromAvro v
  fromAvro (AV.Union 1 v) = FSum.InR <$> fromAvro v
  fromAvro _ = Left "FromAvro Functor.Sum: expected Union 0/1"


instance ToAvro AV.Value where
  toAvro = id


instance FromAvro AV.Value where
  fromAvro = Right
