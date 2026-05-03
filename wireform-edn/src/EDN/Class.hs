{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based EDN serialization with GHC Generics support.
--
-- Provides 'ToEDN' and 'FromEDN' typeclasses for converting Haskell
-- values to\/from EDN. Records are encoded as EDN maps with keyword keys.
-- Derive instances automatically via @DeriveGeneric@.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import EDN.Class
--
-- data Point = Point { x :: Double, y :: Double } deriving (Generic)
-- instance ToEDN Point
-- instance FromEDN Point
--
-- let bs = encodeEDN (Point 1.0 2.0)
-- let Right pt = decodeEDN bs :: Either String Point
-- @
module EDN.Class
  ( ToEDN(..)
  , FromEDN(..)
  , encodeEDN
  , encodeEDNDirect
  , decodeEDN
  , genericToEncoding
  , GToEDN(..)
  , GFromEDN(..)
  , GToEDNEncoding(..)
  , GToEDNEncodingFields(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.Product as FProduct
import qualified Data.Functor.Sum as FSum
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

import qualified EDN.Value as EV
import qualified EDN.Encode as EE
import qualified EDN.Decode as ED
import EDN.Encoding (Encoding)
import qualified EDN.Encoding as Enc
import qualified Data.Text.Lazy.Builder as TLB

class ToEDN a where
  toEDN :: a -> EV.Value
  default toEDN :: (Generic a, GToEDN (Rep a)) => a -> EV.Value
  toEDN = gToEDN . from

  toEncoding :: a -> Encoding
  toEncoding = valueToEncoding . toEDN

class FromEDN a where
  fromEDN :: EV.Value -> Either String a
  default fromEDN :: (Generic a, GFromEDN (Rep a)) => EV.Value -> Either String a
  fromEDN v = to <$> gFromEDN v

encodeEDN :: ToEDN a => a -> Text
encodeEDN = EE.encode . toEDN

-- | Encode directly via 'toEncoding'.
encodeEDNDirect :: ToEDN a => a -> Text
encodeEDNDirect = Enc.encodingToText . toEncoding

decodeEDN :: FromEDN a => Text -> Either String a
decodeEDN t = ED.decode t >>= fromEDN

genericToEncoding :: (Generic a, GToEDNEncoding (Rep a)) => a -> Encoding
genericToEncoding = gToEncoding . from

valueToEncoding :: EV.Value -> Encoding
valueToEncoding v = Enc.Encoding (TLB.fromText (EE.encode v))

instance ToEDN Bool where
  toEDN = EV.Bool
  toEncoding = Enc.bool

instance FromEDN Bool where
  fromEDN (EV.Bool b) = Right b
  fromEDN _ = Left "FromEDN Bool: expected Bool"

instance ToEDN Int where
  toEDN n = EV.Integer (fromIntegral n)
  toEncoding = Enc.int

instance FromEDN Int where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int: expected Integer"

instance ToEDN Int8 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int8 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int8: expected Integer"

instance ToEDN Int16 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int16 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int16: expected Integer"

instance ToEDN Int32 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int32 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int32: expected Integer"

instance ToEDN Int64 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int64 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int64: expected Integer"

instance ToEDN Word where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word: expected Integer"

instance ToEDN Word8 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word8 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word8: expected Integer"

instance ToEDN Word16 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word16 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word16: expected Integer"

instance ToEDN Word32 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word32 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word32: expected Integer"

instance ToEDN Word64 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word64 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word64: expected Integer"

instance ToEDN Float where
  toEDN f = EV.Float (realToFrac f)

instance FromEDN Float where
  fromEDN (EV.Float d) = Right (realToFrac d)
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Float: expected Float"

instance ToEDN Double where
  toEDN = EV.Float
  toEncoding = Enc.double

instance FromEDN Double where
  fromEDN (EV.Float d) = Right d
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Double: expected Float"

instance ToEDN Text where
  toEDN = EV.String
  toEncoding = Enc.string

instance FromEDN Text where
  fromEDN (EV.String t) = Right t
  fromEDN _ = Left "FromEDN Text: expected String"

instance ToEDN ByteString where
  toEDN _ = EV.Nil

instance FromEDN ByteString where
  fromEDN _ = Left "FromEDN ByteString: EDN has no binary type"

instance ToEDN () where
  toEDN () = EV.Nil
  toEncoding () = Enc.nil

instance FromEDN () where
  fromEDN EV.Nil = Right ()
  fromEDN _ = Left "FromEDN (): expected Nil"

instance ToEDN a => ToEDN (Maybe a) where
  toEDN Nothing = EV.Nil
  toEDN (Just x) = toEDN x
  toEncoding Nothing  = Enc.nil
  toEncoding (Just x) = toEncoding x

instance FromEDN a => FromEDN (Maybe a) where
  fromEDN EV.Nil = Right Nothing
  fromEDN v = Just <$> fromEDN v

instance ToEDN a => ToEDN [a] where
  toEDN xs = EV.Vector (V.fromList (map toEDN xs))
  toEncoding xs = Enc.vectorFromList (fmap toEncoding xs)

instance FromEDN a => FromEDN [a] where
  fromEDN (EV.Vector vs) = traverse fromEDN (V.toList vs)
  fromEDN (EV.List vs) = traverse fromEDN (V.toList vs)
  fromEDN _ = Left "FromEDN [a]: expected Vector or List"

instance ToEDN a => ToEDN (Vector a) where
  toEDN xs = EV.Vector (V.map toEDN xs)

instance FromEDN a => FromEDN (Vector a) where
  fromEDN (EV.Vector vs) = V.mapM fromEDN vs
  fromEDN (EV.List vs) = V.mapM fromEDN vs
  fromEDN _ = Left "FromEDN Vector: expected Vector or List"

instance (ToEDN a, ToEDN b) => ToEDN (a, b) where
  toEDN (a, b) = EV.Vector (V.fromList [toEDN a, toEDN b])

instance (FromEDN a, FromEDN b) => FromEDN (a, b) where
  fromEDN (EV.Vector vs)
    | V.length vs == 2 = (,) <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1)
  fromEDN _ = Left "FromEDN (a,b): expected Vector of length 2"

instance (ToEDN k, ToEDN v) => ToEDN (Map k v) where
  toEDN m = EV.Map (V.fromList [(toEDN k, toEDN v') | (k, v') <- Map.toList m])

instance (Ord k, FromEDN k, FromEDN v) => FromEDN (Map k v) where
  fromEDN (EV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromEDN k <*> fromEDN v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromEDN _ = Left "FromEDN Map: expected Map"

-- Aeson-parity instances ---------------------------------------------------

instance ToEDN Char where
  toEDN = EV.Char

instance FromEDN Char where
  fromEDN (EV.Char c) = Right c
  fromEDN (EV.String t)
    | T.length t == 1 = Right (T.head t)
  fromEDN _ = Left "FromEDN Char: expected Char or single-char String"

instance ToEDN Integer where
  toEDN = EV.Integer

instance FromEDN Integer where
  fromEDN (EV.Integer n) = Right n
  fromEDN _ = Left "FromEDN Integer: expected Integer"

instance ToEDN Natural where
  toEDN = EV.Integer . toInteger

instance FromEDN Natural where
  fromEDN (EV.Integer n) | n >= 0 = Right (fromInteger n)
  fromEDN _ = Left "FromEDN Natural: expected non-negative Integer"

instance ToEDN TL.Text where
  toEDN = EV.String . TL.toStrict

instance FromEDN TL.Text where
  fromEDN v = TL.fromStrict <$> fromEDN v

-- | EDN has no native binary type; lazy 'ByteString' is mapped to 'Nil'
-- the same way as strict 'ByteString'.
instance ToEDN BSL.ByteString where
  toEDN _ = EV.Nil

instance FromEDN BSL.ByteString where
  fromEDN _ = Left "FromEDN ByteString: EDN has no binary type"

instance ToEDN a => ToEDN (NonEmpty a) where
  toEDN = toEDN . NE.toList

instance FromEDN a => FromEDN (NonEmpty a) where
  fromEDN v = do
    xs <- fromEDN v
    case xs of
      []     -> Left "FromEDN NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance (ToEDN a, ToEDN b) => ToEDN (Either a b) where
  toEDN (Left  x) = EV.Map (V.singleton (EV.Keyword Nothing "Left",  toEDN x))
  toEDN (Right x) = EV.Map (V.singleton (EV.Keyword Nothing "Right", toEDN x))

instance (FromEDN a, FromEDN b) => FromEDN (Either a b) where
  fromEDN (EV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (EV.Keyword _ "Left",  v) -> Left  <$> fromEDN v
        (EV.Keyword _ "Right", v) -> Right <$> fromEDN v
        (EV.String "Left",     v) -> Left  <$> fromEDN v
        (EV.String "Right",    v) -> Right <$> fromEDN v
        _                         -> Left "FromEDN Either: expected Left/Right key"
  fromEDN _ = Left "FromEDN Either: expected single-key Map"

instance (Ord a, ToEDN a) => ToEDN (Set a) where
  toEDN = EV.Set . V.fromList . fmap toEDN . Set.toList

instance (Ord a, FromEDN a) => FromEDN (Set a) where
  fromEDN (EV.Set vs) = Set.fromList <$> traverse fromEDN (V.toList vs)
  fromEDN v = Set.fromList <$> fromEDN v

instance ToEDN a => ToEDN (Seq a) where
  toEDN s = EV.Vector (V.fromList (fmap toEDN (foldr (:) [] s)))

instance FromEDN a => FromEDN (Seq a) where
  fromEDN v = Seq.fromList <$> fromEDN v

instance ToEDN v => ToEDN (IntMap v) where
  toEDN m = EV.Map (V.fromList (fmap (\(k, v) -> (toEDN k, toEDN v)) (IntMap.toList m)))

instance FromEDN v => FromEDN (IntMap v) where
  fromEDN (EV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromEDN k <*> fromEDN v) (V.toList kvs)
    Right (IntMap.fromList pairs)
  fromEDN _ = Left "FromEDN IntMap: expected Map"

instance ToEDN IntSet where
  toEDN = EV.Set . V.fromList . fmap toEDN . IntSet.toList

instance FromEDN IntSet where
  fromEDN (EV.Set vs) = IntSet.fromList <$> traverse fromEDN (V.toList vs)
  fromEDN v = IntSet.fromList <$> fromEDN v

instance (Hashable k, ToEDN k, ToEDN v) => ToEDN (HashMap k v) where
  toEDN m = EV.Map (V.fromList (fmap (\(k, v) -> (toEDN k, toEDN v)) (HM.toList m)))

instance (Eq k, Hashable k, FromEDN k, FromEDN v) => FromEDN (HashMap k v) where
  fromEDN (EV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromEDN k <*> fromEDN v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromEDN _ = Left "FromEDN HashMap: expected Map"

instance (Hashable a, ToEDN a) => ToEDN (HashSet a) where
  toEDN = EV.Set . V.fromList . fmap toEDN . HS.toList

instance (Eq a, Hashable a, FromEDN a) => FromEDN (HashSet a) where
  fromEDN (EV.Set vs) = HS.fromList <$> traverse fromEDN (V.toList vs)
  fromEDN v = HS.fromList <$> fromEDN v

instance (ToEDN a, ToEDN b, ToEDN c) => ToEDN (a, b, c) where
  toEDN (a, b, c) = EV.Vector (V.fromList [toEDN a, toEDN b, toEDN c])

instance (FromEDN a, FromEDN b, FromEDN c) => FromEDN (a, b, c) where
  fromEDN (EV.Vector vs)
    | V.length vs == 3 =
        (,,) <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1) <*> fromEDN (vs V.! 2)
  fromEDN _ = Left "FromEDN (a,b,c): expected Vector of length 3"

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d) => ToEDN (a, b, c, d) where
  toEDN (a, b, c, d) = EV.Vector (V.fromList [toEDN a, toEDN b, toEDN c, toEDN d])

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d) => FromEDN (a, b, c, d) where
  fromEDN (EV.Vector vs)
    | V.length vs == 4 =
        (,,,) <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1)
              <*> fromEDN (vs V.! 2) <*> fromEDN (vs V.! 3)
  fromEDN _ = Left "FromEDN (a,b,c,d): expected Vector of length 4"

instance ToEDN a => ToEDN (Identity a) where
  toEDN (Identity x) = toEDN x

instance FromEDN a => FromEDN (Identity a) where
  fromEDN v = Identity <$> fromEDN v

instance ToEDN a => ToEDN (Const a b) where
  toEDN (Const x) = toEDN x

instance FromEDN a => FromEDN (Const a b) where
  fromEDN v = Const <$> fromEDN v

instance ToEDN a => ToEDN (Down a) where
  toEDN (Down x) = toEDN x

instance FromEDN a => FromEDN (Down a) where
  fromEDN v = Down <$> fromEDN v

instance ToEDN Version where
  toEDN = toEDN . versionBranch

instance FromEDN Version where
  fromEDN v = makeVersion <$> fromEDN v

instance (Integral a, ToEDN a) => ToEDN (Ratio a) where
  toEDN r = EV.Vector (V.fromList [toEDN (numerator r), toEDN (denominator r)])

instance (Integral a, FromEDN a) => FromEDN (Ratio a) where
  fromEDN (EV.Vector vs)
    | V.length vs == 2 = do
        n <- fromEDN (vs V.! 0)
        d <- fromEDN (vs V.! 1)
        if d == 0
          then Left "FromEDN Ratio: zero denominator"
          else Right (n % d)
  fromEDN _ = Left "FromEDN Ratio: expected Vector of length 2"

-- Functor / monoid newtype instances --------------------------------------

instance ToEDN a => ToEDN (Mon.Sum a) where
  toEDN = toEDN . Mon.getSum
  toEncoding = toEncoding . Mon.getSum

instance FromEDN a => FromEDN (Mon.Sum a) where
  fromEDN v = Mon.Sum <$> fromEDN v

instance ToEDN a => ToEDN (Mon.Product a) where
  toEDN = toEDN . Mon.getProduct
  toEncoding = toEncoding . Mon.getProduct

instance FromEDN a => FromEDN (Mon.Product a) where
  fromEDN v = Mon.Product <$> fromEDN v

instance ToEDN a => ToEDN (Mon.Dual a) where
  toEDN = toEDN . Mon.getDual
  toEncoding = toEncoding . Mon.getDual

instance FromEDN a => FromEDN (Mon.Dual a) where
  fromEDN v = Mon.Dual <$> fromEDN v

instance ToEDN Mon.All where
  toEDN = toEDN . Mon.getAll
  toEncoding = toEncoding . Mon.getAll

instance FromEDN Mon.All where
  fromEDN v = Mon.All <$> fromEDN v

instance ToEDN Mon.Any where
  toEDN = toEDN . Mon.getAny
  toEncoding = toEncoding . Mon.getAny

instance FromEDN Mon.Any where
  fromEDN v = Mon.Any <$> fromEDN v

instance ToEDN a => ToEDN (Mon.First a) where
  toEDN = toEDN . Mon.getFirst
  toEncoding = toEncoding . Mon.getFirst

instance FromEDN a => FromEDN (Mon.First a) where
  fromEDN v = Mon.First <$> fromEDN v

instance ToEDN a => ToEDN (Mon.Last a) where
  toEDN = toEDN . Mon.getLast
  toEncoding = toEncoding . Mon.getLast

instance FromEDN a => FromEDN (Mon.Last a) where
  fromEDN v = Mon.Last <$> fromEDN v

instance ToEDN a => ToEDN (Semi.Min a) where
  toEDN = toEDN . Semi.getMin
  toEncoding = toEncoding . Semi.getMin

instance FromEDN a => FromEDN (Semi.Min a) where
  fromEDN v = Semi.Min <$> fromEDN v

instance ToEDN a => ToEDN (Semi.Max a) where
  toEDN = toEDN . Semi.getMax
  toEncoding = toEncoding . Semi.getMax

instance FromEDN a => FromEDN (Semi.Max a) where
  fromEDN v = Semi.Max <$> fromEDN v

instance ToEDN a => ToEDN (Semi.First a) where
  toEDN = toEDN . Semi.getFirst
  toEncoding = toEncoding . Semi.getFirst

instance FromEDN a => FromEDN (Semi.First a) where
  fromEDN v = Semi.First <$> fromEDN v

instance ToEDN a => ToEDN (Semi.Last a) where
  toEDN = toEDN . Semi.getLast
  toEncoding = toEncoding . Semi.getLast

instance FromEDN a => FromEDN (Semi.Last a) where
  fromEDN v = Semi.Last <$> fromEDN v

instance ToEDN a => ToEDN (Semi.WrappedMonoid a) where
  toEDN = toEDN . Semi.unwrapMonoid
  toEncoding = toEncoding . Semi.unwrapMonoid

instance FromEDN a => FromEDN (Semi.WrappedMonoid a) where
  fromEDN v = Semi.WrapMonoid <$> fromEDN v

instance (ToEDN a, ToEDN b) => ToEDN (Semi.Arg a b) where
  toEDN (Semi.Arg a b) = EV.Vector (V.fromList [toEDN a, toEDN b])

instance (FromEDN a, FromEDN b) => FromEDN (Semi.Arg a b) where
  fromEDN (EV.Vector vs)
    | V.length vs == 2 = Semi.Arg <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1)
  fromEDN _ = Left "FromEDN Arg: expected Vector of length 2"

instance ToEDN (f (g a)) => ToEDN (Compose f g a) where
  toEDN = toEDN . getCompose
  toEncoding = toEncoding . getCompose

instance FromEDN (f (g a)) => FromEDN (Compose f g a) where
  fromEDN v = Compose <$> fromEDN v

instance (ToEDN (f a), ToEDN (g a)) => ToEDN (FProduct.Product f g a) where
  toEDN (FProduct.Pair x y) = EV.Vector (V.fromList [toEDN x, toEDN y])

instance (FromEDN (f a), FromEDN (g a)) => FromEDN (FProduct.Product f g a) where
  fromEDN (EV.Vector vs)
    | V.length vs == 2 = FProduct.Pair <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1)
  fromEDN _ = Left "FromEDN Functor.Product: expected Vector of length 2"

instance (ToEDN (f a), ToEDN (g a)) => ToEDN (FSum.Sum f g a) where
  toEDN (FSum.InL x) = EV.Map (V.singleton (EV.Keyword Nothing "InL", toEDN x))
  toEDN (FSum.InR x) = EV.Map (V.singleton (EV.Keyword Nothing "InR", toEDN x))

instance (FromEDN (f a), FromEDN (g a)) => FromEDN (FSum.Sum f g a) where
  fromEDN (EV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (EV.Keyword _ "InL", v) -> FSum.InL <$> fromEDN v
        (EV.Keyword _ "InR", v) -> FSum.InR <$> fromEDN v
        (EV.String "InL", v)    -> FSum.InL <$> fromEDN v
        (EV.String "InR", v)    -> FSum.InR <$> fromEDN v
        _                       -> Left "FromEDN Functor.Sum: expected InL/InR key"
  fromEDN _ = Left "FromEDN Functor.Sum: expected single-key Map"

instance ToEDN EV.Value where
  toEDN = id

instance FromEDN EV.Value where
  fromEDN = Right

-- GHC.Generics support

class GToEDN f where
  gToEDN :: f p -> EV.Value

class GFromEDN f where
  gFromEDN :: EV.Value -> Either String (f p)

instance GToEDN f => GToEDN (M1 D c f) where
  gToEDN (M1 x) = gToEDN x

instance GFromEDN f => GFromEDN (M1 D c f) where
  gFromEDN v = M1 <$> gFromEDN v

instance (Constructor c, GToEDNFields f) => GToEDN (M1 C c f) where
  gToEDN (M1 x) =
    let fields = gToEDNFields x
    in EV.Map (V.fromList [(EV.Keyword Nothing k, v) | (k, v) <- fields])

instance (Constructor c, GFromEDNFields f) => GFromEDN (M1 C c f) where
  gFromEDN (EV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromEDNFields lkup
  gFromEDN _ = Left "GFromEDN: expected Map for record type"

lookupField :: Text -> Vector (EV.Value, EV.Value) -> Maybe EV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (EV.Keyword _ k, v) <- kvs V.! i, k == name = Just v
      | (EV.String k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToEDNFields f where
  gToEDNFields :: f p -> [(Text, EV.Value)]

class GFromEDNFields f where
  gFromEDNFields :: (Text -> Maybe EV.Value) -> Either String (f p)

instance (GToEDNFields a, GToEDNFields b) => GToEDNFields (a :*: b) where
  gToEDNFields (a :*: b) = gToEDNFields a ++ gToEDNFields b

instance (GFromEDNFields a, GFromEDNFields b) => GFromEDNFields (a :*: b) where
  gFromEDNFields lkup = (:*:) <$> gFromEDNFields lkup <*> gFromEDNFields lkup

instance (Selector s, ToEDN a) => GToEDNFields (M1 S s (K1 i a)) where
  gToEDNFields m@(M1 (K1 x)) = [(T.pack (selName m), toEDN x)]

instance (Selector s, FromEDN a) => GFromEDNFields (M1 S s (K1 i a)) where
  gFromEDNFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromEDN: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromEDN v

-- ---------------------------------------------------------------------------
-- Generic direct-to-text encoding.
-- ---------------------------------------------------------------------------

class GToEDNEncoding f where
  gToEncoding :: f p -> Encoding

class GToEDNEncodingFields f where
  gToEncodingFields :: f p -> [(Encoding, Encoding)]

instance GToEDNEncoding f => GToEDNEncoding (M1 D c f) where
  gToEncoding (M1 x) = gToEncoding x

instance (Constructor c, GToEDNEncodingFields f) => GToEDNEncoding (M1 C c f) where
  gToEncoding (M1 x) = Enc.mapList (gToEncodingFields x)

instance (GToEDNEncodingFields a, GToEDNEncodingFields b) => GToEDNEncodingFields (a :*: b) where
  gToEncodingFields (a :*: b) = gToEncodingFields a ++ gToEncodingFields b

instance (Selector s, ToEDN a) => GToEDNEncodingFields (M1 S s (K1 i a)) where
  gToEncodingFields m@(M1 (K1 x)) =
    [(Enc.keyword Nothing (T.pack (selName m)), toEncoding x)]
