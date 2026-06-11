{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typeclass-based BSON serialization with GHC Generics support.

Provides 'ToBSON' and 'FromBSON' typeclasses for converting Haskell
records to\/from BSON documents. Records are encoded as BSON documents
with field names as keys. Derive instances via @DeriveGeneric@.

@
{\-\# LANGUAGE DeriveGeneric \#-\}
import GHC.Generics (Generic)
import BSON.Class

data User = User { name :: Text, age :: Int } deriving (Generic)
instance ToBSON User
instance FromBSON User

let bytes = encodeBSON (User \"Bob\" 25)
let Right user = decodeBSON bytes :: Either String User
@
-}
module BSON.Class (
  ToBSON (..),
  FromBSON (..),
  encodeBSON,
  encodeBSONDirect,
  decodeBSON,
  genericToEncoding,
  GToBSON (..),
  GFromBSON (..),
) where

import BSON.Decode qualified as BD
import BSON.Encode qualified as BE
import BSON.Encoding (Encoding)
import BSON.Encoding qualified as Enc
import BSON.Value qualified as BV
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FProduct
import Data.Functor.Sum qualified as FSum
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
import GHC.Generics
import Numeric.Natural (Natural)


class ToBSON a where
  toBSON :: a -> BV.Value
  default toBSON :: (Generic a, GToBSON (Rep a)) => a -> BV.Value
  toBSON = gToBSON . from


  {- | aeson-style direct encoder. BSON's wire format is
  length-prefixed at every level so 'Encoding' wraps a fully-built
  'BV.Value' (see 'BSON.Encoding'); the API is in place for parity
  and so future direct-write paths can be slotted in without an
  API break.
  -}
  toEncoding :: a -> Encoding
  toEncoding = Enc.value . toBSON


class FromBSON a where
  fromBSON :: BV.Value -> Either String a
  default fromBSON :: (Generic a, GFromBSON (Rep a)) => BV.Value -> Either String a
  fromBSON v = to <$> gFromBSON v


encodeBSON :: ToBSON a => a -> ByteString
encodeBSON = BE.encode . toBSON


-- | Encode directly via 'toEncoding'.
encodeBSONDirect :: ToBSON a => a -> ByteString
encodeBSONDirect = Enc.encodingToByteString . toEncoding


genericToEncoding :: (Generic a, GToBSON (Rep a)) => a -> Encoding
genericToEncoding = Enc.value . gToBSON . from


decodeBSON :: FromBSON a => ByteString -> Either String a
decodeBSON bs = BD.decode bs >>= fromBSON


instance ToBSON Bool where
  toBSON = BV.Bool


instance FromBSON Bool where
  fromBSON (BV.Bool b) = Right b
  fromBSON _ = Left "FromBSON Bool: expected Bool"


instance ToBSON Int where
  toBSON n
    | n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32) =
        BV.Int32 (fromIntegral n)
    | otherwise = BV.Int64 (fromIntegral n)


instance FromBSON Int where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Double d) = Right (round d)
  fromBSON _ = Left "FromBSON Int: expected Int32, Int64, or Double"


instance ToBSON Int8 where
  toBSON n = BV.Int32 (fromIntegral n)


instance FromBSON Int8 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int8: expected Int32 or Int64"


instance ToBSON Int16 where
  toBSON n = BV.Int32 (fromIntegral n)


instance FromBSON Int16 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int16: expected Int32 or Int64"


instance ToBSON Int32 where
  toBSON = BV.Int32


instance FromBSON Int32 where
  fromBSON (BV.Int32 n) = Right n
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int32: expected Int32 or Int64"


instance ToBSON Int64 where
  toBSON = BV.Int64


instance FromBSON Int64 where
  fromBSON (BV.Int64 n) = Right n
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int64: expected Int64 or Int32"


instance ToBSON Word where
  toBSON n = BV.Int64 (fromIntegral n)


instance FromBSON Word where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word: expected Int64 or Int32"


instance ToBSON Word8 where
  toBSON n = BV.Int32 (fromIntegral n)


instance FromBSON Word8 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word8: expected Int32 or Int64"


instance ToBSON Word16 where
  toBSON n = BV.Int32 (fromIntegral n)


instance FromBSON Word16 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word16: expected Int32 or Int64"


instance ToBSON Word32 where
  toBSON n = BV.Int64 (fromIntegral n)


instance FromBSON Word32 where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word32: expected Int64 or Int32"


instance ToBSON Word64 where
  toBSON n = BV.Int64 (fromIntegral n)


instance FromBSON Word64 where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word64: expected Int64 or Int32"


instance ToBSON Float where
  toBSON f = BV.Double (realToFrac f)


instance FromBSON Float where
  fromBSON (BV.Double d) = Right (realToFrac d)
  fromBSON _ = Left "FromBSON Float: expected Double"


instance ToBSON Double where
  toBSON = BV.Double


instance FromBSON Double where
  fromBSON (BV.Double d) = Right d
  fromBSON _ = Left "FromBSON Double: expected Double"


instance ToBSON Text where
  toBSON = BV.String


instance FromBSON Text where
  fromBSON (BV.String t) = Right t
  fromBSON _ = Left "FromBSON Text: expected String"


instance ToBSON ByteString where
  toBSON = BV.Binary 0x00


instance FromBSON ByteString where
  fromBSON (BV.Binary _sub bs) = Right bs
  fromBSON _ = Left "FromBSON ByteString: expected Binary"


instance ToBSON () where
  toBSON () = BV.Null


instance FromBSON () where
  fromBSON BV.Null = Right ()
  fromBSON _ = Left "FromBSON (): expected Null"


instance ToBSON a => ToBSON (Maybe a) where
  toBSON Nothing = BV.Null
  toBSON (Just x) = toBSON x


instance FromBSON a => FromBSON (Maybe a) where
  fromBSON BV.Null = Right Nothing
  fromBSON v = Just <$> fromBSON v


instance ToBSON a => ToBSON [a] where
  toBSON xs = BV.Array (V.fromList (map toBSON xs))


instance FromBSON a => FromBSON [a] where
  fromBSON (BV.Array vs) = traverse fromBSON (V.toList vs)
  fromBSON _ = Left "FromBSON [a]: expected Array"


instance ToBSON a => ToBSON (Vector a) where
  toBSON xs = BV.Array (V.map toBSON xs)


instance FromBSON a => FromBSON (Vector a) where
  fromBSON (BV.Array vs) = V.mapM fromBSON vs
  fromBSON _ = Left "FromBSON Vector: expected Array"


instance (ToBSON a, ToBSON b) => ToBSON (a, b) where
  toBSON (a, b) = BV.Array (V.fromList [toBSON a, toBSON b])


instance (FromBSON a, FromBSON b) => FromBSON (a, b) where
  fromBSON (BV.Array vs)
    | V.length vs == 2 = (,) <$> fromBSON (vs V.! 0) <*> fromBSON (vs V.! 1)
  fromBSON _ = Left "FromBSON (a,b): expected Array of length 2"


instance (ToBSON k, ToBSON v) => ToBSON (Map k v) where
  toBSON m = BV.Array (V.fromList [BV.Array (V.fromList [toBSON k, toBSON v']) | (k, v') <- Map.toList m])


instance (Ord k, FromBSON k, FromBSON v) => FromBSON (Map k v) where
  fromBSON (BV.Array vs) = do
    pairs <- traverse decodePair (V.toList vs)
    Right (Map.fromList pairs)
    where
      decodePair (BV.Array kv)
        | V.length kv == 2 = (,) <$> fromBSON (kv V.! 0) <*> fromBSON (kv V.! 1)
      decodePair _ = Left "FromBSON Map: expected Array of pairs"
  fromBSON _ = Left "FromBSON Map: expected Array"


-- Aeson-parity instances ---------------------------------------------------

instance ToBSON Integer where
  toBSON n
    | n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32) =
        BV.Int32 (fromInteger n)
    | n >= fromIntegral (minBound :: Int64) && n <= fromIntegral (maxBound :: Int64) =
        BV.Int64 (fromInteger n)
    | otherwise = BV.String (T.pack (show n))


instance FromBSON Integer where
  fromBSON (BV.Int32 n) = Right (toInteger n)
  fromBSON (BV.Int64 n) = Right (toInteger n)
  fromBSON (BV.String t) = case reads (T.unpack t) of
    [(n, "")] -> Right n
    _ -> Left "FromBSON Integer: cannot parse string"
  fromBSON _ = Left "FromBSON Integer: expected Int32, Int64, or String"


instance ToBSON Natural where
  toBSON = toBSON . toInteger


instance FromBSON Natural where
  fromBSON v = do
    n <- fromBSON v :: Either String Integer
    if n < 0
      then Left "FromBSON Natural: negative integer"
      else Right (fromInteger n)


instance ToBSON TL.Text where
  toBSON = BV.String . TL.toStrict


instance FromBSON TL.Text where
  fromBSON v = TL.fromStrict <$> fromBSON v


instance ToBSON BSL.ByteString where
  toBSON = BV.Binary 0x00 . BSL.toStrict


instance FromBSON BSL.ByteString where
  fromBSON v = BSL.fromStrict <$> fromBSON v


instance ToBSON a => ToBSON (NonEmpty a) where
  toBSON = toBSON . NE.toList


instance FromBSON a => FromBSON (NonEmpty a) where
  fromBSON v = do
    xs <- fromBSON v
    case xs of
      [] -> Left "FromBSON NonEmpty: empty list"
      (y : ys) -> Right (y :| ys)


instance (ToBSON a, ToBSON b) => ToBSON (Either a b) where
  toBSON (Left x) = BV.Document (V.singleton ("Left", toBSON x))
  toBSON (Right x) = BV.Document (V.singleton ("Right", toBSON x))


instance (FromBSON a, FromBSON b) => FromBSON (Either a b) where
  fromBSON (BV.Document kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("Left", v) -> Left <$> fromBSON v
        ("Right", v) -> Right <$> fromBSON v
        _ -> Left "FromBSON Either: expected Left/Right key"
  fromBSON _ = Left "FromBSON Either: expected single-key Document"


instance (Ord a, ToBSON a) => ToBSON (Set a) where
  toBSON = BV.Array . V.fromList . fmap toBSON . Set.toList


instance (Ord a, FromBSON a) => FromBSON (Set a) where
  fromBSON v = Set.fromList <$> fromBSON v


instance ToBSON a => ToBSON (Seq a) where
  toBSON s = BV.Array (V.fromList (fmap toBSON (foldr (:) [] s)))


instance FromBSON a => FromBSON (Seq a) where
  fromBSON v = Seq.fromList <$> fromBSON v


instance ToBSON v => ToBSON (IntMap v) where
  toBSON m = BV.Document (V.fromList [(T.pack (show k), toBSON v) | (k, v) <- IntMap.toList m])


instance FromBSON v => FromBSON (IntMap v) where
  fromBSON (BV.Document kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case reads (T.unpack k) of
        [(i, "")] -> (,) i <$> fromBSON v
        _ -> Left "FromBSON IntMap: cannot parse Int key"
  fromBSON _ = Left "FromBSON IntMap: expected Document"


instance ToBSON IntSet where
  toBSON = BV.Array . V.fromList . fmap toBSON . IntSet.toList


instance FromBSON IntSet where
  fromBSON v = IntSet.fromList <$> fromBSON v


instance (ToBSON a, ToBSON b, ToBSON c) => ToBSON (a, b, c) where
  toBSON (a, b, c) = BV.Array (V.fromList [toBSON a, toBSON b, toBSON c])


instance (FromBSON a, FromBSON b, FromBSON c) => FromBSON (a, b, c) where
  fromBSON (BV.Array vs)
    | V.length vs == 3 =
        (,,)
          <$> fromBSON (vs V.! 0)
          <*> fromBSON (vs V.! 1)
          <*> fromBSON (vs V.! 2)
  fromBSON _ = Left "FromBSON (a,b,c): expected Array of length 3"


instance (ToBSON a, ToBSON b, ToBSON c, ToBSON d) => ToBSON (a, b, c, d) where
  toBSON (a, b, c, d) = BV.Array (V.fromList [toBSON a, toBSON b, toBSON c, toBSON d])


instance (FromBSON a, FromBSON b, FromBSON c, FromBSON d) => FromBSON (a, b, c, d) where
  fromBSON (BV.Array vs)
    | V.length vs == 4 =
        (,,,)
          <$> fromBSON (vs V.! 0)
          <*> fromBSON (vs V.! 1)
          <*> fromBSON (vs V.! 2)
          <*> fromBSON (vs V.! 3)
  fromBSON _ = Left "FromBSON (a,b,c,d): expected Array of length 4"


instance ToBSON a => ToBSON (Identity a) where
  toBSON (Identity x) = toBSON x


instance FromBSON a => FromBSON (Identity a) where
  fromBSON v = Identity <$> fromBSON v


instance ToBSON a => ToBSON (Const a b) where
  toBSON (Const x) = toBSON x


instance FromBSON a => FromBSON (Const a b) where
  fromBSON v = Const <$> fromBSON v


instance ToBSON a => ToBSON (Down a) where
  toBSON (Down x) = toBSON x


instance FromBSON a => FromBSON (Down a) where
  fromBSON v = Down <$> fromBSON v


instance ToBSON Version where
  toBSON = toBSON . versionBranch


instance FromBSON Version where
  fromBSON v = makeVersion <$> fromBSON v


instance (Integral a, ToBSON a) => ToBSON (Ratio a) where
  toBSON r = BV.Array (V.fromList [toBSON (numerator r), toBSON (denominator r)])


instance (Integral a, FromBSON a) => FromBSON (Ratio a) where
  fromBSON (BV.Array vs)
    | V.length vs == 2 = do
        n <- fromBSON (vs V.! 0)
        d <- fromBSON (vs V.! 1)
        if d == 0
          then Left "FromBSON Ratio: zero denominator"
          else Right (n % d)
  fromBSON _ = Left "FromBSON Ratio: expected Array of length 2"


-- Functor / monoid newtype instances --------------------------------------

instance ToBSON a => ToBSON (Mon.Sum a) where
  toBSON = toBSON . Mon.getSum


instance FromBSON a => FromBSON (Mon.Sum a) where
  fromBSON v = Mon.Sum <$> fromBSON v


instance ToBSON a => ToBSON (Mon.Product a) where
  toBSON = toBSON . Mon.getProduct


instance FromBSON a => FromBSON (Mon.Product a) where
  fromBSON v = Mon.Product <$> fromBSON v


instance ToBSON a => ToBSON (Mon.Dual a) where
  toBSON = toBSON . Mon.getDual


instance FromBSON a => FromBSON (Mon.Dual a) where
  fromBSON v = Mon.Dual <$> fromBSON v


instance ToBSON Mon.All where
  toBSON = toBSON . Mon.getAll


instance FromBSON Mon.All where
  fromBSON v = Mon.All <$> fromBSON v


instance ToBSON Mon.Any where
  toBSON = toBSON . Mon.getAny


instance FromBSON Mon.Any where
  fromBSON v = Mon.Any <$> fromBSON v


instance ToBSON a => ToBSON (Mon.First a) where
  toBSON = toBSON . Mon.getFirst


instance FromBSON a => FromBSON (Mon.First a) where
  fromBSON v = Mon.First <$> fromBSON v


instance ToBSON a => ToBSON (Mon.Last a) where
  toBSON = toBSON . Mon.getLast


instance FromBSON a => FromBSON (Mon.Last a) where
  fromBSON v = Mon.Last <$> fromBSON v


instance ToBSON a => ToBSON (Semi.Min a) where
  toBSON = toBSON . Semi.getMin


instance FromBSON a => FromBSON (Semi.Min a) where
  fromBSON v = Semi.Min <$> fromBSON v


instance ToBSON a => ToBSON (Semi.Max a) where
  toBSON = toBSON . Semi.getMax


instance FromBSON a => FromBSON (Semi.Max a) where
  fromBSON v = Semi.Max <$> fromBSON v


instance ToBSON a => ToBSON (Semi.First a) where
  toBSON = toBSON . Semi.getFirst


instance FromBSON a => FromBSON (Semi.First a) where
  fromBSON v = Semi.First <$> fromBSON v


instance ToBSON a => ToBSON (Semi.Last a) where
  toBSON = toBSON . Semi.getLast


instance FromBSON a => FromBSON (Semi.Last a) where
  fromBSON v = Semi.Last <$> fromBSON v


instance ToBSON a => ToBSON (Semi.WrappedMonoid a) where
  toBSON = toBSON . Semi.unwrapMonoid


instance FromBSON a => FromBSON (Semi.WrappedMonoid a) where
  fromBSON v = Semi.WrapMonoid <$> fromBSON v


instance (ToBSON a, ToBSON b) => ToBSON (Semi.Arg a b) where
  toBSON (Semi.Arg a b) = BV.Array (V.fromList [toBSON a, toBSON b])


instance (FromBSON a, FromBSON b) => FromBSON (Semi.Arg a b) where
  fromBSON (BV.Array vs)
    | V.length vs == 2 = Semi.Arg <$> fromBSON (vs V.! 0) <*> fromBSON (vs V.! 1)
  fromBSON _ = Left "FromBSON Arg: expected Array of length 2"


instance ToBSON (f (g a)) => ToBSON (Compose f g a) where
  toBSON = toBSON . getCompose


instance FromBSON (f (g a)) => FromBSON (Compose f g a) where
  fromBSON v = Compose <$> fromBSON v


instance (ToBSON (f a), ToBSON (g a)) => ToBSON (FProduct.Product f g a) where
  toBSON (FProduct.Pair x y) = BV.Array (V.fromList [toBSON x, toBSON y])


instance (FromBSON (f a), FromBSON (g a)) => FromBSON (FProduct.Product f g a) where
  fromBSON (BV.Array vs)
    | V.length vs == 2 = FProduct.Pair <$> fromBSON (vs V.! 0) <*> fromBSON (vs V.! 1)
  fromBSON _ = Left "FromBSON Functor.Product: expected Array of length 2"


instance (ToBSON (f a), ToBSON (g a)) => ToBSON (FSum.Sum f g a) where
  toBSON (FSum.InL x) = BV.Document (V.singleton ("InL", toBSON x))
  toBSON (FSum.InR x) = BV.Document (V.singleton ("InR", toBSON x))


instance (FromBSON (f a), FromBSON (g a)) => FromBSON (FSum.Sum f g a) where
  fromBSON (BV.Document kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("InL", v) -> FSum.InL <$> fromBSON v
        ("InR", v) -> FSum.InR <$> fromBSON v
        _ -> Left "FromBSON Functor.Sum: expected InL/InR key"
  fromBSON _ = Left "FromBSON Functor.Sum: expected single-key Document"


instance ToBSON BV.Value where
  toBSON = id


instance FromBSON BV.Value where
  fromBSON = Right


-- GHC.Generics support

class GToBSON f where
  gToBSON :: f p -> BV.Value


class GFromBSON f where
  gFromBSON :: BV.Value -> Either String (f p)


instance GToBSON f => GToBSON (M1 D c f) where
  gToBSON (M1 x) = gToBSON x


instance GFromBSON f => GFromBSON (M1 D c f) where
  gFromBSON v = M1 <$> gFromBSON v


instance (Constructor c, GToBSONFields f) => GToBSON (M1 C c f) where
  gToBSON (M1 x) =
    let fields = gToBSONFields x
    in BV.Document (V.fromList fields)


instance (Constructor c, GFromBSONFields f) => GFromBSON (M1 C c f) where
  gFromBSON (BV.Document kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromBSONFields lkup
  gFromBSON _ = Left "GFromBSON: expected Document for record type"


lookupField :: Text -> Vector (Text, BV.Value) -> Maybe BV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)


class GToBSONFields f where
  gToBSONFields :: f p -> [(Text, BV.Value)]


class GFromBSONFields f where
  gFromBSONFields :: (Text -> Maybe BV.Value) -> Either String (f p)


instance (GToBSONFields a, GToBSONFields b) => GToBSONFields (a :*: b) where
  gToBSONFields (a :*: b) = gToBSONFields a ++ gToBSONFields b


instance (GFromBSONFields a, GFromBSONFields b) => GFromBSONFields (a :*: b) where
  gFromBSONFields lkup = (:*:) <$> gFromBSONFields lkup <*> gFromBSONFields lkup


instance (Selector s, ToBSON a) => GToBSONFields (M1 S s (K1 i a)) where
  gToBSONFields m@(M1 (K1 x)) = [(T.pack (selName m), toBSON x)]


instance (Selector s, FromBSON a) => GFromBSONFields (M1 S s (K1 i a)) where
  gFromBSONFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromBSON: missing field " ++ T.unpack name
         Just v -> M1 . K1 <$> fromBSON v
