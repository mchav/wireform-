{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based MessagePack serialization with GHC Generics support.
--
-- Provides 'ToMsgPack' and 'FromMsgPack' typeclasses that can be derived
-- automatically for record types via @DeriveGeneric@. Records are encoded
-- as MessagePack maps with field names as string keys.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import MsgPack.Class
--
-- data Person = Person { name :: Text, age :: Int } deriving (Generic)
-- instance ToMsgPack Person
-- instance FromMsgPack Person
--
-- let bytes = encodeMsgPack (Person \"Alice\" 30)
-- let Right person = decodeMsgPack bytes :: Either String Person
-- @
module MsgPack.Class
  ( ToMsgPack(..)
  , FromMsgPack(..)
  , encodeMsgPack
  , encodeMsgPackDirect
  , decodeMsgPack
  , genericToEncoding
  , GToMsgPack(..)
  , GFromMsgPack(..)
  , GToMsgPackEncoding(..)
  , GToMsgPackEncodingFields(..)
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

import qualified Data.ByteString.Builder as BB

import qualified MsgPack.Value as MV
import qualified MsgPack.Encode as ME
import qualified MsgPack.Decode as MD
import MsgPack.Encoding (Encoding)
import qualified MsgPack.Encoding as Enc

-- | Conversion to MessagePack.
--
-- Instances should provide 'toMsgPack' (the AST conversion). For
-- performance-sensitive types they /should/ also provide
-- 'toEncoding', which writes directly to a MsgPack builder without
-- constructing an intermediate 'MV.Value'. The default
-- 'toEncoding' falls back to 'toMsgPack' for source-level
-- compatibility.
class ToMsgPack a where
  toMsgPack :: a -> MV.Value
  default toMsgPack :: (Generic a, GToMsgPack (Rep a)) => a -> MV.Value
  toMsgPack = gToMsgPack . from

  toEncoding :: a -> Encoding
  toEncoding = valueToEncoding . toMsgPack

class FromMsgPack a where
  fromMsgPack :: MV.Value -> Either String a
  default fromMsgPack :: (Generic a, GFromMsgPack (Rep a)) => MV.Value -> Either String a
  fromMsgPack v = to <$> gFromMsgPack v

encodeMsgPack :: ToMsgPack a => a -> ByteString
encodeMsgPack = ME.encode . toMsgPack

-- | Encode directly via 'toEncoding'.
encodeMsgPackDirect :: ToMsgPack a => a -> ByteString
encodeMsgPackDirect = Enc.encodingToByteString . toEncoding

decodeMsgPack :: FromMsgPack a => ByteString -> Either String a
decodeMsgPack bs = MD.decode bs >>= fromMsgPack

-- | Generic 'toEncoding'. Use as
--
-- > instance ToMsgPack Foo where
-- >   toEncoding = genericToEncoding
genericToEncoding :: (Generic a, GToMsgPackEncoding (Rep a)) => a -> Encoding
genericToEncoding = gToEncoding . from

-- | Fallback used by the default 'toEncoding'. Walks a 'MV.Value'
-- tree and emits the corresponding builder. Anything we don't have a
-- direct primitive for goes through 'ME.encode' so the bytes still
-- match the AST encoder.
valueToEncoding :: MV.Value -> Encoding
valueToEncoding v = case v of
  MV.Nil           -> Enc.nil
  MV.Bool b        -> Enc.bool b
  MV.Int n         -> Enc.int64 n
  MV.Word n        -> Enc.word64 n
  MV.Float f       -> Enc.float f
  MV.Double d      -> Enc.double d
  MV.String t      -> Enc.string t
  MV.Binary bs     -> Enc.binary bs
  MV.Array xs      -> Enc.array (V.toList (V.map valueToEncoding xs))
  MV.Map kvs       -> Enc.map_ (V.toList (V.map (\(k, v') -> (valueToEncoding k, valueToEncoding v')) kvs))
  MV.Ext ty bs     -> Enc.ext ty bs
  MV.Timestamp{}   -> Enc.Encoding (BB.byteString (ME.encode v))

-- Instances for base types

instance ToMsgPack Bool where
  toMsgPack = MV.Bool
  toEncoding = Enc.bool

instance FromMsgPack Bool where
  fromMsgPack (MV.Bool b) = Right b
  fromMsgPack _ = Left "FromMsgPack Bool: expected Bool"

instance ToMsgPack Int where
  toMsgPack n
    | n >= 0    = MV.Word (fromIntegral n)
    | otherwise = MV.Int (fromIntegral n)
  toEncoding = Enc.int

instance FromMsgPack Int where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int: expected Int or Word"

instance ToMsgPack Int8 where
  toMsgPack n = MV.Int (fromIntegral n)
  toEncoding = Enc.int8

instance FromMsgPack Int8 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int8: expected Int or Word"

instance ToMsgPack Int16 where
  toMsgPack n = MV.Int (fromIntegral n)
  toEncoding = Enc.int16

instance FromMsgPack Int16 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int16: expected Int or Word"

instance ToMsgPack Int32 where
  toMsgPack n = MV.Int (fromIntegral n)
  toEncoding = Enc.int32

instance FromMsgPack Int32 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int32: expected Int or Word"

instance ToMsgPack Int64 where
  toMsgPack n
    | n >= 0    = MV.Word (fromIntegral n)
    | otherwise = MV.Int n
  toEncoding = Enc.int64

instance FromMsgPack Int64 where
  fromMsgPack (MV.Int n) = Right n
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int64: expected Int or Word"

instance ToMsgPack Word where
  toMsgPack n = MV.Word (fromIntegral n)
  toEncoding = Enc.word

instance FromMsgPack Word where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word: expected Word or Int"

instance ToMsgPack Word8 where
  toMsgPack n = MV.Word (fromIntegral n)
  toEncoding = Enc.word8

instance FromMsgPack Word8 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word8: expected Word or Int"

instance ToMsgPack Word16 where
  toMsgPack n = MV.Word (fromIntegral n)
  toEncoding = Enc.word16

instance FromMsgPack Word16 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word16: expected Word or Int"

instance ToMsgPack Word32 where
  toMsgPack n = MV.Word (fromIntegral n)
  toEncoding = Enc.word32

instance FromMsgPack Word32 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word32: expected Word or Int"

instance ToMsgPack Word64 where
  toMsgPack = MV.Word
  toEncoding = Enc.word64

instance FromMsgPack Word64 where
  fromMsgPack (MV.Word n) = Right n
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word64: expected Word or Int"

instance ToMsgPack Float where
  toMsgPack = MV.Float
  toEncoding = Enc.float

instance FromMsgPack Float where
  fromMsgPack (MV.Float f) = Right f
  fromMsgPack (MV.Double d) = Right (realToFrac d)
  fromMsgPack _ = Left "FromMsgPack Float: expected Float or Double"

instance ToMsgPack Double where
  toMsgPack = MV.Double
  toEncoding = Enc.double

instance FromMsgPack Double where
  fromMsgPack (MV.Double d) = Right d
  fromMsgPack (MV.Float f) = Right (realToFrac f)
  fromMsgPack _ = Left "FromMsgPack Double: expected Double or Float"

instance ToMsgPack Text where
  toMsgPack = MV.String
  toEncoding = Enc.string

instance FromMsgPack Text where
  fromMsgPack (MV.String t) = Right t
  fromMsgPack _ = Left "FromMsgPack Text: expected String"

instance ToMsgPack ByteString where
  toMsgPack = MV.Binary
  toEncoding = Enc.binary

instance FromMsgPack ByteString where
  fromMsgPack (MV.Binary bs) = Right bs
  fromMsgPack _ = Left "FromMsgPack ByteString: expected Binary"

instance ToMsgPack () where
  toMsgPack () = MV.Nil
  toEncoding () = Enc.nil

instance FromMsgPack () where
  fromMsgPack MV.Nil = Right ()
  fromMsgPack _ = Left "FromMsgPack (): expected Nil"

instance ToMsgPack a => ToMsgPack (Maybe a) where
  toMsgPack Nothing = MV.Nil
  toMsgPack (Just x) = toMsgPack x
  toEncoding Nothing  = Enc.nil
  toEncoding (Just x) = toEncoding x

instance FromMsgPack a => FromMsgPack (Maybe a) where
  fromMsgPack MV.Nil = Right Nothing
  fromMsgPack v = Just <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack [a] where
  toMsgPack xs = MV.Array (V.fromList (map toMsgPack xs))
  toEncoding xs = Enc.arrayList (fmap toEncoding xs)

instance FromMsgPack a => FromMsgPack [a] where
  fromMsgPack (MV.Array vs) = traverse fromMsgPack (V.toList vs)
  fromMsgPack _ = Left "FromMsgPack [a]: expected Array"

instance ToMsgPack a => ToMsgPack (Vector a) where
  toMsgPack xs = MV.Array (V.map toMsgPack xs)
  toEncoding xs = Enc.array (V.toList (V.map toEncoding xs))

instance FromMsgPack a => FromMsgPack (Vector a) where
  fromMsgPack (MV.Array vs) = V.mapM fromMsgPack vs
  fromMsgPack _ = Left "FromMsgPack Vector: expected Array"

instance (ToMsgPack a, ToMsgPack b) => ToMsgPack (a, b) where
  toMsgPack (a, b) = MV.Array (V.fromList [toMsgPack a, toMsgPack b])

instance (FromMsgPack a, FromMsgPack b) => FromMsgPack (a, b) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 2 = (,) <$> fromMsgPack (vs V.! 0) <*> fromMsgPack (vs V.! 1)
  fromMsgPack _ = Left "FromMsgPack (a,b): expected Array of length 2"

instance (ToMsgPack k, ToMsgPack v) => ToMsgPack (Map k v) where
  toMsgPack m = MV.Map (V.fromList [(toMsgPack k, toMsgPack v) | (k, v') <- Map.toList m, let v = v'])
  toEncoding m = Enc.mapList [(toEncoding k, toEncoding v') | (k, v') <- Map.toList m]

instance (Ord k, FromMsgPack k, FromMsgPack v) => FromMsgPack (Map k v) where
  fromMsgPack (MV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromMsgPack k <*> fromMsgPack v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromMsgPack _ = Left "FromMsgPack Map: expected Map"

-- Aeson-parity instances ---------------------------------------------------

instance ToMsgPack Integer where
  toMsgPack n
    | n >= 0    = MV.Word (fromInteger n)
    | otherwise = MV.Int  (fromInteger n)

instance FromMsgPack Integer where
  fromMsgPack (MV.Int n)  = Right (toInteger n)
  fromMsgPack (MV.Word n) = Right (toInteger n)
  fromMsgPack _ = Left "FromMsgPack Integer: expected Int or Word"

instance ToMsgPack Natural where
  toMsgPack = MV.Word . fromIntegral

instance FromMsgPack Natural where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) | n >= 0 = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Natural: expected non-negative integer"

instance ToMsgPack TL.Text where
  toMsgPack = MV.String . TL.toStrict

instance FromMsgPack TL.Text where
  fromMsgPack v = TL.fromStrict <$> fromMsgPack v

instance ToMsgPack BSL.ByteString where
  toMsgPack = MV.Binary . BSL.toStrict

instance FromMsgPack BSL.ByteString where
  fromMsgPack v = BSL.fromStrict <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (NonEmpty a) where
  toMsgPack = toMsgPack . NE.toList

instance FromMsgPack a => FromMsgPack (NonEmpty a) where
  fromMsgPack v = do
    xs <- fromMsgPack v
    case xs of
      []     -> Left "FromMsgPack NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance (ToMsgPack a, ToMsgPack b) => ToMsgPack (Either a b) where
  toMsgPack (Left  x) = MV.Map (V.singleton (MV.String "Left",  toMsgPack x))
  toMsgPack (Right x) = MV.Map (V.singleton (MV.String "Right", toMsgPack x))

instance (FromMsgPack a, FromMsgPack b) => FromMsgPack (Either a b) where
  fromMsgPack (MV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (MV.String "Left",  v) -> Left  <$> fromMsgPack v
        (MV.String "Right", v) -> Right <$> fromMsgPack v
        _                      -> Left "FromMsgPack Either: expected Left/Right key"
  fromMsgPack _ = Left "FromMsgPack Either: expected single-key Map"

instance (Ord a, ToMsgPack a) => ToMsgPack (Set a) where
  toMsgPack = MV.Array . V.fromList . fmap toMsgPack . Set.toList

instance (Ord a, FromMsgPack a) => FromMsgPack (Set a) where
  fromMsgPack v = Set.fromList <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Seq a) where
  toMsgPack s = MV.Array (V.fromList (fmap toMsgPack (foldr (:) [] s)))

instance FromMsgPack a => FromMsgPack (Seq a) where
  fromMsgPack v = Seq.fromList <$> fromMsgPack v

instance ToMsgPack v => ToMsgPack (IntMap v) where
  toMsgPack m = MV.Map (V.fromList (fmap (\(k, v) -> (toMsgPack k, toMsgPack v)) (IntMap.toList m)))

instance FromMsgPack v => FromMsgPack (IntMap v) where
  fromMsgPack (MV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromMsgPack k <*> fromMsgPack v) (V.toList kvs)
    Right (IntMap.fromList pairs)
  fromMsgPack _ = Left "FromMsgPack IntMap: expected Map"

instance ToMsgPack IntSet where
  toMsgPack = MV.Array . V.fromList . fmap toMsgPack . IntSet.toList

instance FromMsgPack IntSet where
  fromMsgPack v = IntSet.fromList <$> fromMsgPack v

instance (Hashable k, ToMsgPack k, ToMsgPack v) => ToMsgPack (HashMap k v) where
  toMsgPack m = MV.Map (V.fromList (fmap (\(k, v) -> (toMsgPack k, toMsgPack v)) (HM.toList m)))

instance (Eq k, Hashable k, FromMsgPack k, FromMsgPack v) => FromMsgPack (HashMap k v) where
  fromMsgPack (MV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromMsgPack k <*> fromMsgPack v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromMsgPack _ = Left "FromMsgPack HashMap: expected Map"

instance (Hashable a, ToMsgPack a) => ToMsgPack (HashSet a) where
  toMsgPack = MV.Array . V.fromList . fmap toMsgPack . HS.toList

instance (Eq a, Hashable a, FromMsgPack a) => FromMsgPack (HashSet a) where
  fromMsgPack v = HS.fromList <$> fromMsgPack v

instance (ToMsgPack a, ToMsgPack b, ToMsgPack c) => ToMsgPack (a, b, c) where
  toMsgPack (a, b, c) = MV.Array (V.fromList [toMsgPack a, toMsgPack b, toMsgPack c])

instance (FromMsgPack a, FromMsgPack b, FromMsgPack c) => FromMsgPack (a, b, c) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 3 =
        (,,) <$> fromMsgPack (vs V.! 0)
             <*> fromMsgPack (vs V.! 1)
             <*> fromMsgPack (vs V.! 2)
  fromMsgPack _ = Left "FromMsgPack (a,b,c): expected Array of length 3"

instance (ToMsgPack a, ToMsgPack b, ToMsgPack c, ToMsgPack d) => ToMsgPack (a, b, c, d) where
  toMsgPack (a, b, c, d) = MV.Array (V.fromList [toMsgPack a, toMsgPack b, toMsgPack c, toMsgPack d])

instance (FromMsgPack a, FromMsgPack b, FromMsgPack c, FromMsgPack d) => FromMsgPack (a, b, c, d) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 4 =
        (,,,) <$> fromMsgPack (vs V.! 0)
              <*> fromMsgPack (vs V.! 1)
              <*> fromMsgPack (vs V.! 2)
              <*> fromMsgPack (vs V.! 3)
  fromMsgPack _ = Left "FromMsgPack (a,b,c,d): expected Array of length 4"

instance ToMsgPack a => ToMsgPack (Identity a) where
  toMsgPack (Identity x) = toMsgPack x

instance FromMsgPack a => FromMsgPack (Identity a) where
  fromMsgPack v = Identity <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Const a b) where
  toMsgPack (Const x) = toMsgPack x

instance FromMsgPack a => FromMsgPack (Const a b) where
  fromMsgPack v = Const <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Down a) where
  toMsgPack (Down x) = toMsgPack x

instance FromMsgPack a => FromMsgPack (Down a) where
  fromMsgPack v = Down <$> fromMsgPack v

instance ToMsgPack Version where
  toMsgPack = toMsgPack . versionBranch

instance FromMsgPack Version where
  fromMsgPack v = makeVersion <$> fromMsgPack v

instance (Integral a, ToMsgPack a) => ToMsgPack (Ratio a) where
  toMsgPack r = MV.Array (V.fromList [toMsgPack (numerator r), toMsgPack (denominator r)])

instance (Integral a, FromMsgPack a) => FromMsgPack (Ratio a) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 2 = do
        n <- fromMsgPack (vs V.! 0)
        d <- fromMsgPack (vs V.! 1)
        if d == 0
          then Left "FromMsgPack Ratio: zero denominator"
          else Right (n % d)
  fromMsgPack _ = Left "FromMsgPack Ratio: expected Array of length 2"

-- Functor / monoid newtype instances --------------------------------------

instance ToMsgPack a => ToMsgPack (Mon.Sum a) where
  toMsgPack = toMsgPack . Mon.getSum
  toEncoding = toEncoding . Mon.getSum

instance FromMsgPack a => FromMsgPack (Mon.Sum a) where
  fromMsgPack v = Mon.Sum <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Mon.Product a) where
  toMsgPack = toMsgPack . Mon.getProduct
  toEncoding = toEncoding . Mon.getProduct

instance FromMsgPack a => FromMsgPack (Mon.Product a) where
  fromMsgPack v = Mon.Product <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Mon.Dual a) where
  toMsgPack = toMsgPack . Mon.getDual
  toEncoding = toEncoding . Mon.getDual

instance FromMsgPack a => FromMsgPack (Mon.Dual a) where
  fromMsgPack v = Mon.Dual <$> fromMsgPack v

instance ToMsgPack Mon.All where
  toMsgPack = toMsgPack . Mon.getAll
  toEncoding = toEncoding . Mon.getAll

instance FromMsgPack Mon.All where
  fromMsgPack v = Mon.All <$> fromMsgPack v

instance ToMsgPack Mon.Any where
  toMsgPack = toMsgPack . Mon.getAny
  toEncoding = toEncoding . Mon.getAny

instance FromMsgPack Mon.Any where
  fromMsgPack v = Mon.Any <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Mon.First a) where
  toMsgPack = toMsgPack . Mon.getFirst
  toEncoding = toEncoding . Mon.getFirst

instance FromMsgPack a => FromMsgPack (Mon.First a) where
  fromMsgPack v = Mon.First <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Mon.Last a) where
  toMsgPack = toMsgPack . Mon.getLast
  toEncoding = toEncoding . Mon.getLast

instance FromMsgPack a => FromMsgPack (Mon.Last a) where
  fromMsgPack v = Mon.Last <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Semi.Min a) where
  toMsgPack = toMsgPack . Semi.getMin
  toEncoding = toEncoding . Semi.getMin

instance FromMsgPack a => FromMsgPack (Semi.Min a) where
  fromMsgPack v = Semi.Min <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Semi.Max a) where
  toMsgPack = toMsgPack . Semi.getMax
  toEncoding = toEncoding . Semi.getMax

instance FromMsgPack a => FromMsgPack (Semi.Max a) where
  fromMsgPack v = Semi.Max <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Semi.First a) where
  toMsgPack = toMsgPack . Semi.getFirst
  toEncoding = toEncoding . Semi.getFirst

instance FromMsgPack a => FromMsgPack (Semi.First a) where
  fromMsgPack v = Semi.First <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Semi.Last a) where
  toMsgPack = toMsgPack . Semi.getLast
  toEncoding = toEncoding . Semi.getLast

instance FromMsgPack a => FromMsgPack (Semi.Last a) where
  fromMsgPack v = Semi.Last <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack (Semi.WrappedMonoid a) where
  toMsgPack = toMsgPack . Semi.unwrapMonoid
  toEncoding = toEncoding . Semi.unwrapMonoid

instance FromMsgPack a => FromMsgPack (Semi.WrappedMonoid a) where
  fromMsgPack v = Semi.WrapMonoid <$> fromMsgPack v

instance (ToMsgPack a, ToMsgPack b) => ToMsgPack (Semi.Arg a b) where
  toMsgPack (Semi.Arg a b) = MV.Array (V.fromList [toMsgPack a, toMsgPack b])
  toEncoding (Semi.Arg a b) = Enc.arrayList [toEncoding a, toEncoding b]

instance (FromMsgPack a, FromMsgPack b) => FromMsgPack (Semi.Arg a b) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 2 = Semi.Arg <$> fromMsgPack (vs V.! 0) <*> fromMsgPack (vs V.! 1)
  fromMsgPack _ = Left "FromMsgPack Arg: expected Array of length 2"

instance ToMsgPack (f (g a)) => ToMsgPack (Compose f g a) where
  toMsgPack = toMsgPack . getCompose
  toEncoding = toEncoding . getCompose

instance FromMsgPack (f (g a)) => FromMsgPack (Compose f g a) where
  fromMsgPack v = Compose <$> fromMsgPack v

instance (ToMsgPack (f a), ToMsgPack (g a)) => ToMsgPack (FProduct.Product f g a) where
  toMsgPack (FProduct.Pair x y) = MV.Array (V.fromList [toMsgPack x, toMsgPack y])
  toEncoding (FProduct.Pair x y) = Enc.arrayList [toEncoding x, toEncoding y]

instance (FromMsgPack (f a), FromMsgPack (g a)) => FromMsgPack (FProduct.Product f g a) where
  fromMsgPack (MV.Array vs)
    | V.length vs == 2 = FProduct.Pair <$> fromMsgPack (vs V.! 0) <*> fromMsgPack (vs V.! 1)
  fromMsgPack _ = Left "FromMsgPack Functor.Product: expected Array of length 2"

instance (ToMsgPack (f a), ToMsgPack (g a)) => ToMsgPack (FSum.Sum f g a) where
  toMsgPack (FSum.InL x) = MV.Map (V.singleton (MV.String "InL", toMsgPack x))
  toMsgPack (FSum.InR x) = MV.Map (V.singleton (MV.String "InR", toMsgPack x))
  toEncoding (FSum.InL x) = Enc.mapList [(Enc.string "InL", toEncoding x)]
  toEncoding (FSum.InR x) = Enc.mapList [(Enc.string "InR", toEncoding x)]

instance (FromMsgPack (f a), FromMsgPack (g a)) => FromMsgPack (FSum.Sum f g a) where
  fromMsgPack (MV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (MV.String "InL", v) -> FSum.InL <$> fromMsgPack v
        (MV.String "InR", v) -> FSum.InR <$> fromMsgPack v
        _                    -> Left "FromMsgPack Functor.Sum: expected InL/InR key"
  fromMsgPack _ = Left "FromMsgPack Functor.Sum: expected single-key Map"

instance ToMsgPack MV.Value where
  toMsgPack = id

instance FromMsgPack MV.Value where
  fromMsgPack = Right

-- GHC.Generics support

class GToMsgPack f where
  gToMsgPack :: f p -> MV.Value

class GFromMsgPack f where
  gFromMsgPack :: MV.Value -> Either String (f p)

-- Datatype metadata: unwrap
instance GToMsgPack f => GToMsgPack (M1 D c f) where
  gToMsgPack (M1 x) = gToMsgPack x

instance GFromMsgPack f => GFromMsgPack (M1 D c f) where
  gFromMsgPack v = M1 <$> gFromMsgPack v

-- Constructor metadata: encode as map
instance (Constructor c, GToMsgPackFields f) => GToMsgPack (M1 C c f) where
  gToMsgPack (M1 x) =
    let fields = gToMsgPackFields x
    in MV.Map (V.fromList [(MV.String k, v) | (k, v) <- fields])

instance (Constructor c, GFromMsgPackFields f) => GFromMsgPack (M1 C c f) where
  gFromMsgPack (MV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromMsgPackFields lkup
  gFromMsgPack _ = Left "GFromMsgPack: expected Map for record type"

lookupField :: Text -> Vector (MV.Value, MV.Value) -> Maybe MV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (MV.String k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToMsgPackFields f where
  gToMsgPackFields :: f p -> [(Text, MV.Value)]

class GFromMsgPackFields f where
  gFromMsgPackFields :: (Text -> Maybe MV.Value) -> Either String (f p)

-- Product: combine fields from both sides
instance (GToMsgPackFields a, GToMsgPackFields b) => GToMsgPackFields (a :*: b) where
  gToMsgPackFields (a :*: b) = gToMsgPackFields a ++ gToMsgPackFields b

instance (GFromMsgPackFields a, GFromMsgPackFields b) => GFromMsgPackFields (a :*: b) where
  gFromMsgPackFields lkup = (:*:) <$> gFromMsgPackFields lkup <*> gFromMsgPackFields lkup

-- Selector metadata: use field name as key
instance (Selector s, ToMsgPack a) => GToMsgPackFields (M1 S s (K1 i a)) where
  gToMsgPackFields m@(M1 (K1 x)) = [(T.pack (selName m), toMsgPack x)]

instance (Selector s, FromMsgPack a) => GFromMsgPackFields (M1 S s (K1 i a)) where
  gFromMsgPackFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromMsgPack: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromMsgPack v

-- ---------------------------------------------------------------------------
-- Generic direct-to-bytes encoding.
-- ---------------------------------------------------------------------------

class GToMsgPackEncoding f where
  gToEncoding :: f p -> Encoding

class GToMsgPackEncodingFields f where
  gToEncodingFields :: f p -> [(Encoding, Encoding)]

instance GToMsgPackEncoding f => GToMsgPackEncoding (M1 D c f) where
  gToEncoding (M1 x) = gToEncoding x

instance (Constructor c, GToMsgPackEncodingFields f) => GToMsgPackEncoding (M1 C c f) where
  gToEncoding (M1 x) = Enc.mapList (gToEncodingFields x)

instance (GToMsgPackEncodingFields a, GToMsgPackEncodingFields b) => GToMsgPackEncodingFields (a :*: b) where
  gToEncodingFields (a :*: b) = gToEncodingFields a ++ gToEncodingFields b

instance (Selector s, ToMsgPack a) => GToMsgPackEncodingFields (M1 S s (K1 i a)) where
  gToEncodingFields m@(M1 (K1 x)) = [(Enc.string (T.pack (selName m)), toEncoding x)]
