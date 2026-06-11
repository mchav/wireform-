{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typeclass-based Amazon Ion serialization with GHC Generics support.

Provides 'ToIon' and 'FromIon' typeclasses for converting Haskell
records to\/from Ion structs. Derive instances via @DeriveGeneric@.

@
{\-\# LANGUAGE DeriveGeneric \#-\}
import GHC.Generics (Generic)
import Ion.Class

data Metric = Metric { name :: Text, value :: Double } deriving (Generic)
instance ToIon Metric
instance FromIon Metric

let bytes = encodeIon (Metric \"cpu\" 0.95)
let Right m = decodeIon bytes :: Either String Metric
@
-}
module Ion.Class (
  ToIon (..),
  FromIon (..),
  encodeIon,
  encodeIonDirect,
  decodeIon,
  genericToEncoding,
  GToIon (..),
  GFromIon (..),
) where

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
import GHC.Generics
import Ion.Decode qualified as ID
import Ion.Encode qualified as IE
import Ion.Encoding (Encoding)
import Ion.Encoding qualified as Enc
import Ion.Value qualified as IV
import Numeric.Natural (Natural)


class ToIon a where
  toIon :: a -> IV.Value
  default toIon :: (Generic a, GToIon (Rep a)) => a -> IV.Value
  toIon = gToIon . from


  {- | aeson-style direct encoder. Ion's TLV layout makes a streaming
  'Builder' awkward; 'Encoding' wraps a fully-built 'IV.Value'.
  -}
  toEncoding :: a -> Encoding
  toEncoding = Enc.value . toIon


class FromIon a where
  fromIon :: IV.Value -> Either String a
  default fromIon :: (Generic a, GFromIon (Rep a)) => IV.Value -> Either String a
  fromIon v = to <$> gFromIon v


encodeIon :: ToIon a => a -> ByteString
encodeIon = IE.encode . toIon


encodeIonDirect :: ToIon a => a -> ByteString
encodeIonDirect = Enc.encodingToByteString . toEncoding


genericToEncoding :: (Generic a, GToIon (Rep a)) => a -> Encoding
genericToEncoding = Enc.value . gToIon . from


decodeIon :: FromIon a => ByteString -> Either String a
decodeIon bs = ID.decode bs >>= fromIon


instance ToIon Bool where
  toIon = IV.Bool


instance FromIon Bool where
  fromIon (IV.Bool b) = Right b
  fromIon _ = Left "FromIon Bool: expected Bool"


instance ToIon Int where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Int where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int: expected Int"


instance ToIon Int8 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Int8 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int8: expected Int"


instance ToIon Int16 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Int16 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int16: expected Int"


instance ToIon Int32 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Int32 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int32: expected Int"


instance ToIon Int64 where
  toIon = IV.Int


instance FromIon Int64 where
  fromIon (IV.Int n) = Right n
  fromIon _ = Left "FromIon Int64: expected Int"


instance ToIon Word where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Word where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word: expected Int"


instance ToIon Word8 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Word8 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word8: expected Int"


instance ToIon Word16 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Word16 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word16: expected Int"


instance ToIon Word32 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Word32 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word32: expected Int"


instance ToIon Word64 where
  toIon n = IV.Int (fromIntegral n)


instance FromIon Word64 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word64: expected Int"


instance ToIon Float where
  toIon f = IV.Float (realToFrac f)


instance FromIon Float where
  fromIon (IV.Float d) = Right (realToFrac d)
  fromIon _ = Left "FromIon Float: expected Float"


instance ToIon Double where
  toIon = IV.Float


instance FromIon Double where
  fromIon (IV.Float d) = Right d
  fromIon _ = Left "FromIon Double: expected Float"


instance ToIon Text where
  toIon = IV.String


instance FromIon Text where
  fromIon (IV.String t) = Right t
  fromIon (IV.Symbol t) = Right t
  fromIon _ = Left "FromIon Text: expected String or Symbol"


instance ToIon ByteString where
  toIon = IV.Blob


instance FromIon ByteString where
  fromIon (IV.Blob bs) = Right bs
  fromIon (IV.Clob bs) = Right bs
  fromIon _ = Left "FromIon ByteString: expected Blob or Clob"


instance ToIon () where
  toIon () = IV.Null


instance FromIon () where
  fromIon IV.Null = Right ()
  fromIon _ = Left "FromIon (): expected Null"


instance ToIon a => ToIon (Maybe a) where
  toIon Nothing = IV.Null
  toIon (Just x) = toIon x


instance FromIon a => FromIon (Maybe a) where
  fromIon IV.Null = Right Nothing
  fromIon v = Just <$> fromIon v


instance ToIon a => ToIon [a] where
  toIon xs = IV.List (V.fromList (map toIon xs))


instance FromIon a => FromIon [a] where
  fromIon (IV.List vs) = traverse fromIon (V.toList vs)
  fromIon _ = Left "FromIon [a]: expected List"


instance ToIon a => ToIon (Vector a) where
  toIon xs = IV.List (V.map toIon xs)


instance FromIon a => FromIon (Vector a) where
  fromIon (IV.List vs) = V.mapM fromIon vs
  fromIon _ = Left "FromIon Vector: expected List"


instance (ToIon a, ToIon b) => ToIon (a, b) where
  toIon (a, b) = IV.List (V.fromList [toIon a, toIon b])


instance (FromIon a, FromIon b) => FromIon (a, b) where
  fromIon (IV.List vs)
    | V.length vs == 2 = (,) <$> fromIon (vs V.! 0) <*> fromIon (vs V.! 1)
  fromIon _ = Left "FromIon (a,b): expected List of length 2"


instance (ToIon k, ToIon v) => ToIon (Map k v) where
  toIon m = IV.List (V.fromList [IV.List (V.fromList [toIon k, toIon v']) | (k, v') <- Map.toList m])


instance (Ord k, FromIon k, FromIon v) => FromIon (Map k v) where
  fromIon (IV.List vs) = do
    pairs <- traverse decodePair (V.toList vs)
    Right (Map.fromList pairs)
    where
      decodePair (IV.List kv)
        | V.length kv == 2 = (,) <$> fromIon (kv V.! 0) <*> fromIon (kv V.! 1)
      decodePair _ = Left "FromIon Map: expected List of pairs"
  fromIon (IV.Struct kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromIon (IV.String k) <*> fromIon v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromIon _ = Left "FromIon Map: expected List or Struct"


-- Aeson-parity instances ---------------------------------------------------

instance ToIon Char where
  toIon c = IV.String (T.singleton c)


instance FromIon Char where
  fromIon (IV.String t) | T.length t == 1 = Right (T.head t)
  fromIon (IV.Symbol t) | T.length t == 1 = Right (T.head t)
  fromIon _ = Left "FromIon Char: expected single-character String"


instance ToIon Integer where
  toIon = IV.Int . fromInteger


instance FromIon Integer where
  fromIon (IV.Int n) = Right (toInteger n)
  fromIon _ = Left "FromIon Integer: expected Int"


instance ToIon Natural where
  toIon = IV.Int . fromIntegral


instance FromIon Natural where
  fromIon (IV.Int n) | n >= 0 = Right (fromIntegral n)
  fromIon _ = Left "FromIon Natural: expected non-negative Int"


instance ToIon TL.Text where
  toIon = IV.String . TL.toStrict


instance FromIon TL.Text where
  fromIon v = TL.fromStrict <$> fromIon v


instance ToIon BSL.ByteString where
  toIon = IV.Blob . BSL.toStrict


instance FromIon BSL.ByteString where
  fromIon v = BSL.fromStrict <$> fromIon v


instance ToIon a => ToIon (NonEmpty a) where
  toIon = toIon . NE.toList


instance FromIon a => FromIon (NonEmpty a) where
  fromIon v = do
    xs <- fromIon v
    case xs of
      [] -> Left "FromIon NonEmpty: empty list"
      (y : ys) -> Right (y :| ys)


instance (ToIon a, ToIon b) => ToIon (Either a b) where
  toIon (Left x) = IV.Struct (V.singleton ("Left", toIon x))
  toIon (Right x) = IV.Struct (V.singleton ("Right", toIon x))


instance (FromIon a, FromIon b) => FromIon (Either a b) where
  fromIon (IV.Struct kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("Left", v) -> Left <$> fromIon v
        ("Right", v) -> Right <$> fromIon v
        _ -> Left "FromIon Either: expected Left/Right key"
  fromIon _ = Left "FromIon Either: expected single-key Struct"


instance (Ord a, ToIon a) => ToIon (Set a) where
  toIon = IV.List . V.fromList . fmap toIon . Set.toList


instance (Ord a, FromIon a) => FromIon (Set a) where
  fromIon v = Set.fromList <$> fromIon v


instance ToIon a => ToIon (Seq a) where
  toIon s = IV.List (V.fromList (fmap toIon (foldr (:) [] s)))


instance FromIon a => FromIon (Seq a) where
  fromIon v = Seq.fromList <$> fromIon v


instance ToIon v => ToIon (IntMap v) where
  toIon m = IV.Struct (V.fromList [(T.pack (show k), toIon v) | (k, v) <- IntMap.toList m])


instance FromIon v => FromIon (IntMap v) where
  fromIon (IV.Struct kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case reads (T.unpack k) of
        [(i, "")] -> (,) i <$> fromIon v
        _ -> Left "FromIon IntMap: cannot parse Int key"
  fromIon _ = Left "FromIon IntMap: expected Struct"


instance ToIon IntSet where
  toIon = IV.List . V.fromList . fmap toIon . IntSet.toList


instance FromIon IntSet where
  fromIon v = IntSet.fromList <$> fromIon v


instance ToIon v => ToIon (HashMap Text v) where
  toIon m = IV.Struct (V.fromList [(k, toIon v) | (k, v) <- HM.toList m])


instance FromIon v => FromIon (HashMap Text v) where
  fromIon (IV.Struct kvs) = do
    pairs <- traverse (\(k, v) -> (,) k <$> fromIon v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromIon _ = Left "FromIon (HashMap Text v): expected Struct"


instance (Hashable a, ToIon a) => ToIon (HashSet a) where
  toIon = IV.List . V.fromList . fmap toIon . HS.toList


instance (Eq a, Hashable a, FromIon a) => FromIon (HashSet a) where
  fromIon v = HS.fromList <$> fromIon v


instance (ToIon a, ToIon b, ToIon c) => ToIon (a, b, c) where
  toIon (a, b, c) = IV.List (V.fromList [toIon a, toIon b, toIon c])


instance (FromIon a, FromIon b, FromIon c) => FromIon (a, b, c) where
  fromIon (IV.List vs)
    | V.length vs == 3 =
        (,,) <$> fromIon (vs V.! 0) <*> fromIon (vs V.! 1) <*> fromIon (vs V.! 2)
  fromIon _ = Left "FromIon (a,b,c): expected List of length 3"


instance (ToIon a, ToIon b, ToIon c, ToIon d) => ToIon (a, b, c, d) where
  toIon (a, b, c, d) = IV.List (V.fromList [toIon a, toIon b, toIon c, toIon d])


instance (FromIon a, FromIon b, FromIon c, FromIon d) => FromIon (a, b, c, d) where
  fromIon (IV.List vs)
    | V.length vs == 4 =
        (,,,)
          <$> fromIon (vs V.! 0)
          <*> fromIon (vs V.! 1)
          <*> fromIon (vs V.! 2)
          <*> fromIon (vs V.! 3)
  fromIon _ = Left "FromIon (a,b,c,d): expected List of length 4"


instance ToIon a => ToIon (Identity a) where
  toIon (Identity x) = toIon x


instance FromIon a => FromIon (Identity a) where
  fromIon v = Identity <$> fromIon v


instance ToIon a => ToIon (Const a b) where
  toIon (Const x) = toIon x


instance FromIon a => FromIon (Const a b) where
  fromIon v = Const <$> fromIon v


instance ToIon a => ToIon (Down a) where
  toIon (Down x) = toIon x


instance FromIon a => FromIon (Down a) where
  fromIon v = Down <$> fromIon v


instance ToIon Version where
  toIon = toIon . versionBranch


instance FromIon Version where
  fromIon v = makeVersion <$> fromIon v


instance (Integral a, ToIon a) => ToIon (Ratio a) where
  toIon r = IV.List (V.fromList [toIon (numerator r), toIon (denominator r)])


instance (Integral a, FromIon a) => FromIon (Ratio a) where
  fromIon (IV.List vs)
    | V.length vs == 2 = do
        n <- fromIon (vs V.! 0)
        d <- fromIon (vs V.! 1)
        if d == 0
          then Left "FromIon Ratio: zero denominator"
          else Right (n % d)
  fromIon _ = Left "FromIon Ratio: expected List of length 2"


-- Functor / monoid newtype instances --------------------------------------

instance ToIon a => ToIon (Mon.Sum a) where
  toIon = toIon . Mon.getSum


instance FromIon a => FromIon (Mon.Sum a) where
  fromIon v = Mon.Sum <$> fromIon v


instance ToIon a => ToIon (Mon.Product a) where
  toIon = toIon . Mon.getProduct


instance FromIon a => FromIon (Mon.Product a) where
  fromIon v = Mon.Product <$> fromIon v


instance ToIon a => ToIon (Mon.Dual a) where
  toIon = toIon . Mon.getDual


instance FromIon a => FromIon (Mon.Dual a) where
  fromIon v = Mon.Dual <$> fromIon v


instance ToIon Mon.All where
  toIon = toIon . Mon.getAll


instance FromIon Mon.All where
  fromIon v = Mon.All <$> fromIon v


instance ToIon Mon.Any where
  toIon = toIon . Mon.getAny


instance FromIon Mon.Any where
  fromIon v = Mon.Any <$> fromIon v


instance ToIon a => ToIon (Mon.First a) where
  toIon = toIon . Mon.getFirst


instance FromIon a => FromIon (Mon.First a) where
  fromIon v = Mon.First <$> fromIon v


instance ToIon a => ToIon (Mon.Last a) where
  toIon = toIon . Mon.getLast


instance FromIon a => FromIon (Mon.Last a) where
  fromIon v = Mon.Last <$> fromIon v


instance ToIon a => ToIon (Semi.Min a) where
  toIon = toIon . Semi.getMin


instance FromIon a => FromIon (Semi.Min a) where
  fromIon v = Semi.Min <$> fromIon v


instance ToIon a => ToIon (Semi.Max a) where
  toIon = toIon . Semi.getMax


instance FromIon a => FromIon (Semi.Max a) where
  fromIon v = Semi.Max <$> fromIon v


instance ToIon a => ToIon (Semi.First a) where
  toIon = toIon . Semi.getFirst


instance FromIon a => FromIon (Semi.First a) where
  fromIon v = Semi.First <$> fromIon v


instance ToIon a => ToIon (Semi.Last a) where
  toIon = toIon . Semi.getLast


instance FromIon a => FromIon (Semi.Last a) where
  fromIon v = Semi.Last <$> fromIon v


instance ToIon a => ToIon (Semi.WrappedMonoid a) where
  toIon = toIon . Semi.unwrapMonoid


instance FromIon a => FromIon (Semi.WrappedMonoid a) where
  fromIon v = Semi.WrapMonoid <$> fromIon v


instance (ToIon a, ToIon b) => ToIon (Semi.Arg a b) where
  toIon (Semi.Arg a b) = IV.List (V.fromList [toIon a, toIon b])


instance (FromIon a, FromIon b) => FromIon (Semi.Arg a b) where
  fromIon (IV.List vs)
    | V.length vs == 2 = Semi.Arg <$> fromIon (vs V.! 0) <*> fromIon (vs V.! 1)
  fromIon _ = Left "FromIon Arg: expected List of length 2"


instance ToIon (f (g a)) => ToIon (Compose f g a) where
  toIon = toIon . getCompose


instance FromIon (f (g a)) => FromIon (Compose f g a) where
  fromIon v = Compose <$> fromIon v


instance (ToIon (f a), ToIon (g a)) => ToIon (FProduct.Product f g a) where
  toIon (FProduct.Pair x y) = IV.List (V.fromList [toIon x, toIon y])


instance (FromIon (f a), FromIon (g a)) => FromIon (FProduct.Product f g a) where
  fromIon (IV.List vs)
    | V.length vs == 2 = FProduct.Pair <$> fromIon (vs V.! 0) <*> fromIon (vs V.! 1)
  fromIon _ = Left "FromIon Functor.Product: expected List of length 2"


instance (ToIon (f a), ToIon (g a)) => ToIon (FSum.Sum f g a) where
  toIon (FSum.InL x) = IV.Struct (V.singleton ("InL", toIon x))
  toIon (FSum.InR x) = IV.Struct (V.singleton ("InR", toIon x))


instance (FromIon (f a), FromIon (g a)) => FromIon (FSum.Sum f g a) where
  fromIon (IV.Struct kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("InL", v) -> FSum.InL <$> fromIon v
        ("InR", v) -> FSum.InR <$> fromIon v
        _ -> Left "FromIon Functor.Sum: expected InL/InR key"
  fromIon _ = Left "FromIon Functor.Sum: expected single-key Struct"


instance ToIon IV.Value where
  toIon = id


instance FromIon IV.Value where
  fromIon = Right


-- GHC.Generics support

class GToIon f where
  gToIon :: f p -> IV.Value


class GFromIon f where
  gFromIon :: IV.Value -> Either String (f p)


instance GToIon f => GToIon (M1 D c f) where
  gToIon (M1 x) = gToIon x


instance GFromIon f => GFromIon (M1 D c f) where
  gFromIon v = M1 <$> gFromIon v


instance (Constructor c, GToIonFields f) => GToIon (M1 C c f) where
  gToIon (M1 x) =
    let fields = gToIonFields x
    in IV.Struct (V.fromList fields)


instance (Constructor c, GFromIonFields f) => GFromIon (M1 C c f) where
  gFromIon (IV.Struct kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromIonFields lkup
  gFromIon _ = Left "GFromIon: expected Struct for record type"


lookupField :: Text -> Vector (Text, IV.Value) -> Maybe IV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)


class GToIonFields f where
  gToIonFields :: f p -> [(Text, IV.Value)]


class GFromIonFields f where
  gFromIonFields :: (Text -> Maybe IV.Value) -> Either String (f p)


instance (GToIonFields a, GToIonFields b) => GToIonFields (a :*: b) where
  gToIonFields (a :*: b) = gToIonFields a ++ gToIonFields b


instance (GFromIonFields a, GFromIonFields b) => GFromIonFields (a :*: b) where
  gFromIonFields lkup = (:*:) <$> gFromIonFields lkup <*> gFromIonFields lkup


instance (Selector s, ToIon a) => GToIonFields (M1 S s (K1 i a)) where
  gToIonFields m@(M1 (K1 x)) = [(T.pack (selName m), toIon x)]


instance (Selector s, FromIon a) => GFromIonFields (M1 S s (K1 i a)) where
  gFromIonFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromIon: missing field " ++ T.unpack name
         Just v -> M1 . K1 <$> fromIon v
