{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based Bencode serialization with GHC Generics support.
module Bencode.Class
  ( ToBencode(..)
  , FromBencode(..)
  , encodeBencode
  , encodeBencodeDirect
  , decodeBencode
  , genericToEncoding
  , GToBencode(..)
  , GFromBencode(..)
  , GToBencodeEncoding(..)
  , GToBencodeEncodingFields(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.Product as FProduct
import qualified Data.Functor.Sum as FSum
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

import qualified Bencode.Value as B
import qualified Bencode.Encode as BE
import qualified Bencode.Decode as BD
import Bencode.Encoding (Encoding)
import qualified Bencode.Encoding as Enc

class ToBencode a where
  toBencode :: a -> B.Value
  default toBencode :: (Generic a, GToBencode (Rep a)) => a -> B.Value
  toBencode = gToBencode . from

  -- | Direct-to-bytes encoding. Default goes through 'toBencode'.
  toEncoding :: a -> Encoding
  toEncoding = valueToEncoding . toBencode

class FromBencode a where
  fromBencode :: B.Value -> Either String a
  default fromBencode :: (Generic a, GFromBencode (Rep a)) => B.Value -> Either String a
  fromBencode v = to <$> gFromBencode v

encodeBencode :: ToBencode a => a -> ByteString
encodeBencode = BE.encode . toBencode

encodeBencodeDirect :: ToBencode a => a -> ByteString
encodeBencodeDirect = Enc.encodingToByteString . toEncoding

decodeBencode :: FromBencode a => ByteString -> Either String a
decodeBencode bs = BD.decode bs >>= fromBencode

genericToEncoding :: (Generic a, GToBencodeEncoding (Rep a)) => a -> Encoding
genericToEncoding = gToEncoding . from

valueToEncoding :: B.Value -> Encoding
valueToEncoding v = case v of
  B.BString bs  -> Enc.bytes bs
  B.BInteger n  -> Enc.integer n
  B.BList xs    -> Enc.list (V.toList (V.map valueToEncoding xs))
  B.BDict kvs   -> Enc.dict (V.toList (V.map (\(k, val) -> (k, valueToEncoding val)) kvs))

instance ToBencode ByteString where
  toBencode = B.BString
  toEncoding = Enc.bytes

instance FromBencode ByteString where
  fromBencode (B.BString bs) = Right bs
  fromBencode _ = Left "FromBencode ByteString: expected BString"

instance ToBencode Text where
  toBencode = B.BString . TE.encodeUtf8
  toEncoding = Enc.text

instance FromBencode Text where
  fromBencode (B.BString bs) = case TE.decodeUtf8' bs of
    Left _ -> Left "FromBencode Text: invalid UTF-8"
    Right t -> Right t
  fromBencode _ = Left "FromBencode Text: expected BString"

instance ToBencode Integer where
  toBencode = B.BInteger
  toEncoding = Enc.integer

instance FromBencode Integer where
  fromBencode (B.BInteger n) = Right n
  fromBencode _ = Left "FromBencode Integer: expected BInteger"

instance ToBencode Int where
  toBencode = B.BInteger . fromIntegral
  toEncoding = Enc.int

instance FromBencode Int where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Int: expected BInteger"

instance ToBencode Int8 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Int8 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Int8: expected BInteger"

instance ToBencode Int16 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Int16 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Int16: expected BInteger"

instance ToBencode Int32 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Int32 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Int32: expected BInteger"

instance ToBencode Int64 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Int64 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Int64: expected BInteger"

instance ToBencode Word where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Word where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Word: expected BInteger"

instance ToBencode Word8 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Word8 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Word8: expected BInteger"

instance ToBencode Word16 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Word16 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Word16: expected BInteger"

instance ToBencode Word32 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Word32 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Word32: expected BInteger"

instance ToBencode Word64 where
  toBencode = B.BInteger . fromIntegral

instance FromBencode Word64 where
  fromBencode (B.BInteger n) = Right (fromIntegral n)
  fromBencode _ = Left "FromBencode Word64: expected BInteger"

instance ToBencode Bool where
  toBencode True = B.BInteger 1
  toBencode False = B.BInteger 0
  toEncoding = Enc.bool

instance FromBencode Bool where
  fromBencode (B.BInteger 0) = Right False
  fromBencode (B.BInteger _) = Right True
  fromBencode _ = Left "FromBencode Bool: expected BInteger"

instance ToBencode a => ToBencode [a] where
  toBencode xs = B.BList (V.fromList (map toBencode xs))
  toEncoding xs = Enc.listFromList (fmap toEncoding xs)

instance FromBencode a => FromBencode [a] where
  fromBencode (B.BList vs) = traverse fromBencode (V.toList vs)
  fromBencode _ = Left "FromBencode [a]: expected BList"

instance ToBencode a => ToBencode (Vector a) where
  toBencode xs = B.BList (V.map toBencode xs)
  toEncoding xs = Enc.list (V.toList (V.map toEncoding xs))

instance FromBencode a => FromBencode (Vector a) where
  fromBencode (B.BList vs) = V.mapM fromBencode vs
  fromBencode _ = Left "FromBencode Vector: expected BList"

instance ToBencode a => ToBencode (Maybe a) where
  toBencode Nothing = B.BList V.empty
  toBencode (Just x) = toBencode x

instance FromBencode a => FromBencode (Maybe a) where
  fromBencode (B.BList vs) | V.null vs = Right Nothing
  fromBencode v = Just <$> fromBencode v

-- Aeson-parity instances ---------------------------------------------------

instance ToBencode Char where
  toBencode c = B.BString (TE.encodeUtf8 (T.singleton c))

instance FromBencode Char where
  fromBencode (B.BString bs) = case TE.decodeUtf8' bs of
    Right t | T.length t == 1 -> Right (T.head t)
    _ -> Left "FromBencode Char: expected single-character BString"
  fromBencode _ = Left "FromBencode Char: expected BString"

instance ToBencode Natural where
  toBencode = B.BInteger . toInteger

instance FromBencode Natural where
  fromBencode (B.BInteger n) | n >= 0 = Right (fromInteger n)
  fromBencode _ = Left "FromBencode Natural: expected non-negative BInteger"

instance ToBencode TL.Text where
  toBencode = B.BString . TE.encodeUtf8 . TL.toStrict

instance FromBencode TL.Text where
  fromBencode v = TL.fromStrict <$> fromBencode v

instance ToBencode BSL.ByteString where
  toBencode = B.BString . BSL.toStrict

instance FromBencode BSL.ByteString where
  fromBencode v = BSL.fromStrict <$> fromBencode v

instance ToBencode a => ToBencode (NonEmpty a) where
  toBencode = toBencode . NE.toList

instance FromBencode a => FromBencode (NonEmpty a) where
  fromBencode v = do
    xs <- fromBencode v
    case xs of
      []     -> Left "FromBencode NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

-- | 'Either' encodes as a single-key BDict with @"Left"@ or @"Right"@.
instance (ToBencode a, ToBencode b) => ToBencode (Either a b) where
  toBencode (Left  x) = B.BDict (V.singleton ("Left",  toBencode x))
  toBencode (Right x) = B.BDict (V.singleton ("Right", toBencode x))

instance (FromBencode a, FromBencode b) => FromBencode (Either a b) where
  fromBencode (B.BDict kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("Left",  v) -> Left  <$> fromBencode v
        ("Right", v) -> Right <$> fromBencode v
        _            -> Left "FromBencode Either: expected Left/Right key"
  fromBencode _ = Left "FromBencode Either: expected single-key BDict"

instance (Ord a, ToBencode a) => ToBencode (Set a) where
  toBencode = B.BList . V.fromList . fmap toBencode . Set.toList

instance (Ord a, FromBencode a) => FromBencode (Set a) where
  fromBencode v = Set.fromList <$> fromBencode v

instance ToBencode a => ToBencode (Seq a) where
  toBencode s = B.BList (V.fromList (fmap toBencode (foldr (:) [] s)))

instance FromBencode a => FromBencode (Seq a) where
  fromBencode v = Seq.fromList <$> fromBencode v

instance ToBencode v => ToBencode (Map ByteString v) where
  toBencode m = B.BDict (V.fromList [(k, toBencode v) | (k, v) <- Map.toList m])

instance FromBencode v => FromBencode (Map ByteString v) where
  fromBencode (B.BDict kvs) = do
    pairs <- traverse (\(k, v) -> (,) k <$> fromBencode v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromBencode _ = Left "FromBencode (Map ByteString v): expected BDict"

instance ToBencode v => ToBencode (Map Text v) where
  toBencode m = B.BDict (V.fromList [(TE.encodeUtf8 k, toBencode v) | (k, v) <- Map.toList m])

instance FromBencode v => FromBencode (Map Text v) where
  fromBencode (B.BDict kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (Map.fromList pairs)
    where
      decodePair (k, v) = case TE.decodeUtf8' k of
        Right t -> (,) t <$> fromBencode v
        Left _  -> Left "FromBencode (Map Text v): non-UTF-8 key"
  fromBencode _ = Left "FromBencode (Map Text v): expected BDict"

instance ToBencode v => ToBencode (IntMap v) where
  toBencode m = B.BDict (V.fromList [(BS8.pack (show k), toBencode v) | (k, v) <- IntMap.toList m])

instance FromBencode v => FromBencode (IntMap v) where
  fromBencode (B.BDict kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case reads (BS8.unpack k) of
        [(i, "")] -> (,) i <$> fromBencode v
        _         -> Left "FromBencode IntMap: cannot parse Int key"
  fromBencode _ = Left "FromBencode IntMap: expected BDict"

instance ToBencode IntSet where
  toBencode = B.BList . V.fromList . fmap toBencode . IntSet.toList

instance FromBencode IntSet where
  fromBencode v = IntSet.fromList <$> fromBencode v

instance (ToBencode a, ToBencode b) => ToBencode (a, b) where
  toBencode (a, b) = B.BList (V.fromList [toBencode a, toBencode b])

instance (FromBencode a, FromBencode b) => FromBencode (a, b) where
  fromBencode (B.BList vs)
    | V.length vs == 2 = (,) <$> fromBencode (vs V.! 0) <*> fromBencode (vs V.! 1)
  fromBencode _ = Left "FromBencode (a,b): expected BList of length 2"

instance (ToBencode a, ToBencode b, ToBencode c) => ToBencode (a, b, c) where
  toBencode (a, b, c) = B.BList (V.fromList [toBencode a, toBencode b, toBencode c])

instance (FromBencode a, FromBencode b, FromBencode c) => FromBencode (a, b, c) where
  fromBencode (B.BList vs)
    | V.length vs == 3 =
        (,,) <$> fromBencode (vs V.! 0) <*> fromBencode (vs V.! 1) <*> fromBencode (vs V.! 2)
  fromBencode _ = Left "FromBencode (a,b,c): expected BList of length 3"

instance (ToBencode a, ToBencode b, ToBencode c, ToBencode d) => ToBencode (a, b, c, d) where
  toBencode (a, b, c, d) = B.BList (V.fromList [toBencode a, toBencode b, toBencode c, toBencode d])

instance (FromBencode a, FromBencode b, FromBencode c, FromBencode d) => FromBencode (a, b, c, d) where
  fromBencode (B.BList vs)
    | V.length vs == 4 =
        (,,,) <$> fromBencode (vs V.! 0) <*> fromBencode (vs V.! 1)
              <*> fromBencode (vs V.! 2) <*> fromBencode (vs V.! 3)
  fromBencode _ = Left "FromBencode (a,b,c,d): expected BList of length 4"

instance ToBencode () where
  toBencode () = B.BList V.empty

instance FromBencode () where
  fromBencode (B.BList vs) | V.null vs = Right ()
  fromBencode _ = Left "FromBencode (): expected empty BList"

instance ToBencode a => ToBencode (Identity a) where
  toBencode (Identity x) = toBencode x

instance FromBencode a => FromBencode (Identity a) where
  fromBencode v = Identity <$> fromBencode v

instance ToBencode a => ToBencode (Const a b) where
  toBencode (Const x) = toBencode x

instance FromBencode a => FromBencode (Const a b) where
  fromBencode v = Const <$> fromBencode v

instance ToBencode a => ToBencode (Down a) where
  toBencode (Down x) = toBencode x

instance FromBencode a => FromBencode (Down a) where
  fromBencode v = Down <$> fromBencode v

instance ToBencode Version where
  toBencode = toBencode . versionBranch

instance FromBencode Version where
  fromBencode v = makeVersion <$> fromBencode v

instance (Integral a, ToBencode a) => ToBencode (Ratio a) where
  toBencode r = B.BList (V.fromList [toBencode (numerator r), toBencode (denominator r)])

instance (Integral a, FromBencode a) => FromBencode (Ratio a) where
  fromBencode (B.BList vs)
    | V.length vs == 2 = do
        n <- fromBencode (vs V.! 0)
        d <- fromBencode (vs V.! 1)
        if d == 0
          then Left "FromBencode Ratio: zero denominator"
          else Right (n % d)
  fromBencode _ = Left "FromBencode Ratio: expected BList of length 2"

-- Functor / monoid newtype instances --------------------------------------

instance ToBencode a => ToBencode (Mon.Sum a) where
  toBencode = toBencode . Mon.getSum

instance FromBencode a => FromBencode (Mon.Sum a) where
  fromBencode v = Mon.Sum <$> fromBencode v

instance ToBencode a => ToBencode (Mon.Product a) where
  toBencode = toBencode . Mon.getProduct

instance FromBencode a => FromBencode (Mon.Product a) where
  fromBencode v = Mon.Product <$> fromBencode v

instance ToBencode a => ToBencode (Mon.Dual a) where
  toBencode = toBencode . Mon.getDual

instance FromBencode a => FromBencode (Mon.Dual a) where
  fromBencode v = Mon.Dual <$> fromBencode v

instance ToBencode Mon.All where
  toBencode = toBencode . Mon.getAll

instance FromBencode Mon.All where
  fromBencode v = Mon.All <$> fromBencode v

instance ToBencode Mon.Any where
  toBencode = toBencode . Mon.getAny

instance FromBencode Mon.Any where
  fromBencode v = Mon.Any <$> fromBencode v

instance ToBencode a => ToBencode (Mon.First a) where
  toBencode = toBencode . Mon.getFirst

instance FromBencode a => FromBencode (Mon.First a) where
  fromBencode v = Mon.First <$> fromBencode v

instance ToBencode a => ToBencode (Mon.Last a) where
  toBencode = toBencode . Mon.getLast

instance FromBencode a => FromBencode (Mon.Last a) where
  fromBencode v = Mon.Last <$> fromBencode v

instance ToBencode a => ToBencode (Semi.Min a) where
  toBencode = toBencode . Semi.getMin

instance FromBencode a => FromBencode (Semi.Min a) where
  fromBencode v = Semi.Min <$> fromBencode v

instance ToBencode a => ToBencode (Semi.Max a) where
  toBencode = toBencode . Semi.getMax

instance FromBencode a => FromBencode (Semi.Max a) where
  fromBencode v = Semi.Max <$> fromBencode v

instance ToBencode a => ToBencode (Semi.First a) where
  toBencode = toBencode . Semi.getFirst

instance FromBencode a => FromBencode (Semi.First a) where
  fromBencode v = Semi.First <$> fromBencode v

instance ToBencode a => ToBencode (Semi.Last a) where
  toBencode = toBencode . Semi.getLast

instance FromBencode a => FromBencode (Semi.Last a) where
  fromBencode v = Semi.Last <$> fromBencode v

instance ToBencode a => ToBencode (Semi.WrappedMonoid a) where
  toBencode = toBencode . Semi.unwrapMonoid

instance FromBencode a => FromBencode (Semi.WrappedMonoid a) where
  fromBencode v = Semi.WrapMonoid <$> fromBencode v

instance (ToBencode a, ToBencode b) => ToBencode (Semi.Arg a b) where
  toBencode (Semi.Arg a b) = B.BList (V.fromList [toBencode a, toBencode b])

instance (FromBencode a, FromBencode b) => FromBencode (Semi.Arg a b) where
  fromBencode (B.BList vs)
    | V.length vs == 2 = Semi.Arg <$> fromBencode (vs V.! 0) <*> fromBencode (vs V.! 1)
  fromBencode _ = Left "FromBencode Arg: expected BList of length 2"

instance ToBencode (f (g a)) => ToBencode (Compose f g a) where
  toBencode = toBencode . getCompose

instance FromBencode (f (g a)) => FromBencode (Compose f g a) where
  fromBencode v = Compose <$> fromBencode v

instance (ToBencode (f a), ToBencode (g a)) => ToBencode (FProduct.Product f g a) where
  toBencode (FProduct.Pair x y) = B.BList (V.fromList [toBencode x, toBencode y])

instance (FromBencode (f a), FromBencode (g a)) => FromBencode (FProduct.Product f g a) where
  fromBencode (B.BList vs)
    | V.length vs == 2 = FProduct.Pair <$> fromBencode (vs V.! 0) <*> fromBencode (vs V.! 1)
  fromBencode _ = Left "FromBencode Functor.Product: expected BList of length 2"

instance (ToBencode (f a), ToBencode (g a)) => ToBencode (FSum.Sum f g a) where
  toBencode (FSum.InL x) = B.BDict (V.singleton ("InL", toBencode x))
  toBencode (FSum.InR x) = B.BDict (V.singleton ("InR", toBencode x))

instance (FromBencode (f a), FromBencode (g a)) => FromBencode (FSum.Sum f g a) where
  fromBencode (B.BDict kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("InL", v) -> FSum.InL <$> fromBencode v
        ("InR", v) -> FSum.InR <$> fromBencode v
        _          -> Left "FromBencode Functor.Sum: expected InL/InR key"
  fromBencode _ = Left "FromBencode Functor.Sum: expected single-key BDict"

instance ToBencode B.Value where
  toBencode = id

instance FromBencode B.Value where
  fromBencode = Right

-- GHC.Generics support

class GToBencode f where
  gToBencode :: f p -> B.Value

class GFromBencode f where
  gFromBencode :: B.Value -> Either String (f p)

instance GToBencode f => GToBencode (M1 D c f) where
  gToBencode (M1 x) = gToBencode x

instance GFromBencode f => GFromBencode (M1 D c f) where
  gFromBencode v = M1 <$> gFromBencode v

instance (Constructor c, GToBencodeFields f) => GToBencode (M1 C c f) where
  gToBencode (M1 x) =
    let fields = gToBencodeFields x
    in B.BDict (V.fromList fields)

instance (Constructor c, GFromBencodeFields f) => GFromBencode (M1 C c f) where
  gFromBencode (B.BDict kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromBencodeFields lkup
  gFromBencode _ = Left "GFromBencode: expected BDict for record type"

lookupField :: ByteString -> Vector (ByteString, B.Value) -> Maybe B.Value
lookupField name kvs = go 0
  where
    !len = V.length kvs
    go !i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToBencodeFields f where
  gToBencodeFields :: f p -> [(ByteString, B.Value)]

class GFromBencodeFields f where
  gFromBencodeFields :: (ByteString -> Maybe B.Value) -> Either String (f p)

instance (GToBencodeFields a, GToBencodeFields b) => GToBencodeFields (a :*: b) where
  gToBencodeFields (a :*: b) = gToBencodeFields a ++ gToBencodeFields b

instance (GFromBencodeFields a, GFromBencodeFields b) => GFromBencodeFields (a :*: b) where
  gFromBencodeFields lkup = (:*:) <$> gFromBencodeFields lkup <*> gFromBencodeFields lkup

instance (Selector s, ToBencode a) => GToBencodeFields (M1 S s (K1 i a)) where
  gToBencodeFields m@(M1 (K1 x)) = [(BS8.pack (selName m), toBencode x)]

instance (Selector s, FromBencode a) => GFromBencodeFields (M1 S s (K1 i a)) where
  gFromBencodeFields lkup =
    let name = BS8.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromBencode: missing field " ++ BS8.unpack name
         Just v  -> M1 . K1 <$> fromBencode v

-- ---------------------------------------------------------------------------
-- Generic direct-to-bytes encoding.
-- ---------------------------------------------------------------------------

class GToBencodeEncoding f where
  gToEncoding :: f p -> Encoding

class GToBencodeEncodingFields f where
  gToEncodingFields :: f p -> [(ByteString, Encoding)]

instance GToBencodeEncoding f => GToBencodeEncoding (M1 D c f) where
  gToEncoding (M1 x) = gToEncoding x

instance (Constructor c, GToBencodeEncodingFields f) => GToBencodeEncoding (M1 C c f) where
  gToEncoding (M1 x) = Enc.dict (gToEncodingFields x)

instance (GToBencodeEncodingFields a, GToBencodeEncodingFields b) => GToBencodeEncodingFields (a :*: b) where
  gToEncodingFields (a :*: b) = gToEncodingFields a ++ gToEncodingFields b

instance (Selector s, ToBencode a) => GToBencodeEncodingFields (M1 S s (K1 i a)) where
  gToEncodingFields m@(M1 (K1 x)) = [(BS8.pack (selName m), toEncoding x)]
