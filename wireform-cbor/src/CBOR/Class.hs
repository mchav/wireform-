{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based CBOR serialization with GHC Generics support.
--
-- Provides 'ToCBOR' and 'FromCBOR' typeclasses that can be derived
-- automatically for record types via @DeriveGeneric@. Records are encoded
-- as CBOR maps with field names as text-string keys.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import CBOR.Class
--
-- data Config = Config { host :: Text, port :: Int } deriving (Generic)
-- instance ToCBOR Config
-- instance FromCBOR Config
--
-- let bytes = encodeCBOR (Config \"localhost\" 8080)
-- let Right cfg = decodeCBOR bytes :: Either String Config
-- @
module CBOR.Class
  ( ToCBOR(..)
  , FromCBOR(..)
  , encodeCBOR
  , decodeCBOR
  , GToCBOR(..)
  , GFromCBOR(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
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

import qualified CBOR.Value as CV
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD

class ToCBOR a where
  toCBOR :: a -> CV.Value
  default toCBOR :: (Generic a, GToCBOR (Rep a)) => a -> CV.Value
  toCBOR = gToCBOR . from

class FromCBOR a where
  fromCBOR :: CV.Value -> Either String a
  default fromCBOR :: (Generic a, GFromCBOR (Rep a)) => CV.Value -> Either String a
  fromCBOR v = to <$> gFromCBOR v

encodeCBOR :: ToCBOR a => a -> ByteString
encodeCBOR = CE.encode . toCBOR

decodeCBOR :: FromCBOR a => ByteString -> Either String a
decodeCBOR bs = CD.decode bs >>= fromCBOR

-- Helper to convert a Haskell integer to CBOR UInt/NInt
intToCBOR :: Int64 -> CV.Value
intToCBOR n
  | n >= 0    = CV.UInt (fromIntegral n)
  | otherwise = CV.NInt (fromIntegral (negate n - 1))

cborToInt :: CV.Value -> Either String Int64
cborToInt (CV.UInt n) = Right (fromIntegral n)
cborToInt (CV.NInt n) = Right (negate (fromIntegral n) - 1)
cborToInt _ = Left "FromCBOR: expected UInt or NInt"

instance ToCBOR Bool where
  toCBOR = CV.Bool

instance FromCBOR Bool where
  fromCBOR (CV.Bool b) = Right b
  fromCBOR _ = Left "FromCBOR Bool: expected Bool"

instance ToCBOR Int where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int8 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int8 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int16 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int16 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int32 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int32 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int64 where
  toCBOR = intToCBOR

instance FromCBOR Int64 where
  fromCBOR = cborToInt

instance ToCBOR Word where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Word8 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word8 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word8: expected UInt"

instance ToCBOR Word16 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word16 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word16: expected UInt"

instance ToCBOR Word32 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word32 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word32: expected UInt"

instance ToCBOR Word64 where
  toCBOR = CV.UInt

instance FromCBOR Word64 where
  fromCBOR (CV.UInt n) = Right n
  fromCBOR _ = Left "FromCBOR Word64: expected UInt"

instance ToCBOR Float where
  toCBOR = CV.Float32

instance FromCBOR Float where
  fromCBOR (CV.Float32 f) = Right f
  fromCBOR (CV.Float16 f) = Right f
  fromCBOR (CV.Float64 d) = Right (realToFrac d)
  fromCBOR _ = Left "FromCBOR Float: expected Float"

instance ToCBOR Double where
  toCBOR = CV.Float64

instance FromCBOR Double where
  fromCBOR (CV.Float64 d) = Right d
  fromCBOR (CV.Float32 f) = Right (realToFrac f)
  fromCBOR (CV.Float16 f) = Right (realToFrac f)
  fromCBOR _ = Left "FromCBOR Double: expected Float"

instance ToCBOR Text where
  toCBOR = CV.TextString

instance FromCBOR Text where
  fromCBOR (CV.TextString t) = Right t
  fromCBOR _ = Left "FromCBOR Text: expected TextString"

instance ToCBOR ByteString where
  toCBOR = CV.ByteString

instance FromCBOR ByteString where
  fromCBOR (CV.ByteString bs) = Right bs
  fromCBOR _ = Left "FromCBOR ByteString: expected ByteString"

instance ToCBOR () where
  toCBOR () = CV.Null

instance FromCBOR () where
  fromCBOR CV.Null = Right ()
  fromCBOR _ = Left "FromCBOR (): expected Null"

instance ToCBOR a => ToCBOR (Maybe a) where
  toCBOR Nothing = CV.Null
  toCBOR (Just x) = toCBOR x

instance FromCBOR a => FromCBOR (Maybe a) where
  fromCBOR CV.Null = Right Nothing
  fromCBOR v = Just <$> fromCBOR v

instance ToCBOR a => ToCBOR [a] where
  toCBOR xs = CV.Array (V.fromList (map toCBOR xs))

instance FromCBOR a => FromCBOR [a] where
  fromCBOR (CV.Array vs) = traverse fromCBOR (V.toList vs)
  fromCBOR _ = Left "FromCBOR [a]: expected Array"

instance ToCBOR a => ToCBOR (Vector a) where
  toCBOR xs = CV.Array (V.map toCBOR xs)

instance FromCBOR a => FromCBOR (Vector a) where
  fromCBOR (CV.Array vs) = V.mapM fromCBOR vs
  fromCBOR _ = Left "FromCBOR Vector: expected Array"

instance (ToCBOR a, ToCBOR b) => ToCBOR (a, b) where
  toCBOR (a, b) = CV.Array (V.fromList [toCBOR a, toCBOR b])

instance (FromCBOR a, FromCBOR b) => FromCBOR (a, b) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = (,) <$> fromCBOR (vs V.! 0) <*> fromCBOR (vs V.! 1)
  fromCBOR _ = Left "FromCBOR (a,b): expected Array of length 2"

instance (ToCBOR k, ToCBOR v) => ToCBOR (Map k v) where
  toCBOR m = CV.Map (V.fromList [(toCBOR k, toCBOR v') | (k, v') <- Map.toList m])

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (Map k v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromCBOR _ = Left "FromCBOR Map: expected Map"

-- Aeson-parity instances ---------------------------------------------------

instance ToCBOR Integer where
  toCBOR n
    | n >= 0    = CV.UInt (fromInteger n)
    | otherwise = CV.NInt (fromInteger (negate n - 1))

instance FromCBOR Integer where
  fromCBOR (CV.UInt n) = Right (toInteger n)
  fromCBOR (CV.NInt n) = Right (negate (toInteger n) - 1)
  fromCBOR _ = Left "FromCBOR Integer: expected UInt or NInt"

instance ToCBOR Natural where
  toCBOR = CV.UInt . fromIntegral

instance FromCBOR Natural where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Natural: expected UInt"

instance ToCBOR TL.Text where
  toCBOR = CV.TextString . TL.toStrict

instance FromCBOR TL.Text where
  fromCBOR v = TL.fromStrict <$> fromCBOR v

instance ToCBOR BSL.ByteString where
  toCBOR = CV.ByteString . BSL.toStrict

instance FromCBOR BSL.ByteString where
  fromCBOR v = BSL.fromStrict <$> fromCBOR v

instance ToCBOR a => ToCBOR (NonEmpty a) where
  toCBOR = toCBOR . NE.toList

instance FromCBOR a => FromCBOR (NonEmpty a) where
  fromCBOR v = do
    xs <- fromCBOR v
    case xs of
      []     -> Left "FromCBOR NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

-- | 'Either' encodes as a tagged map with @"Left"@ or @"Right"@ keys
-- (mirrors aeson's Sum encoding).
instance (ToCBOR a, ToCBOR b) => ToCBOR (Either a b) where
  toCBOR (Left  x) = CV.Map (V.singleton (CV.TextString "Left",  toCBOR x))
  toCBOR (Right x) = CV.Map (V.singleton (CV.TextString "Right", toCBOR x))

instance (FromCBOR a, FromCBOR b) => FromCBOR (Either a b) where
  fromCBOR (CV.Map kvs)
    | V.length kvs == 1 = case V.head kvs of
        (CV.TextString "Left",  v) -> Left  <$> fromCBOR v
        (CV.TextString "Right", v) -> Right <$> fromCBOR v
        _                          -> Left "FromCBOR Either: expected Left/Right key"
  fromCBOR _ = Left "FromCBOR Either: expected single-key Map"

instance (Ord a, ToCBOR a) => ToCBOR (Set a) where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . Set.toList

instance (Ord a, FromCBOR a) => FromCBOR (Set a) where
  fromCBOR v = Set.fromList <$> fromCBOR v

instance ToCBOR a => ToCBOR (Seq a) where
  toCBOR s = CV.Array (V.fromList (fmap toCBOR (foldr (:) [] s)))

instance FromCBOR a => FromCBOR (Seq a) where
  fromCBOR v = Seq.fromList <$> fromCBOR v

instance ToCBOR v => ToCBOR (IntMap v) where
  toCBOR m = CV.Map (V.fromList (fmap (\(k, v) -> (toCBOR k, toCBOR v)) (IntMap.toList m)))

instance FromCBOR v => FromCBOR (IntMap v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (IntMap.fromList pairs)
  fromCBOR _ = Left "FromCBOR IntMap: expected Map"

instance ToCBOR IntSet where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . IntSet.toList

instance FromCBOR IntSet where
  fromCBOR v = IntSet.fromList <$> fromCBOR v

instance (Hashable k, ToCBOR k, ToCBOR v) => ToCBOR (HashMap k v) where
  toCBOR m = CV.Map (V.fromList (fmap (\(k, v) -> (toCBOR k, toCBOR v)) (HM.toList m)))

instance (Eq k, Hashable k, FromCBOR k, FromCBOR v) => FromCBOR (HashMap k v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromCBOR _ = Left "FromCBOR HashMap: expected Map"

instance (Hashable a, ToCBOR a) => ToCBOR (HashSet a) where
  toCBOR = CV.Array . V.fromList . fmap toCBOR . HS.toList

instance (Eq a, Hashable a, FromCBOR a) => FromCBOR (HashSet a) where
  fromCBOR v = HS.fromList <$> fromCBOR v

instance (ToCBOR a, ToCBOR b, ToCBOR c) => ToCBOR (a, b, c) where
  toCBOR (a, b, c) = CV.Array (V.fromList [toCBOR a, toCBOR b, toCBOR c])

instance (FromCBOR a, FromCBOR b, FromCBOR c) => FromCBOR (a, b, c) where
  fromCBOR (CV.Array vs)
    | V.length vs == 3 =
        (,,) <$> fromCBOR (vs V.! 0)
             <*> fromCBOR (vs V.! 1)
             <*> fromCBOR (vs V.! 2)
  fromCBOR _ = Left "FromCBOR (a,b,c): expected Array of length 3"

instance (ToCBOR a, ToCBOR b, ToCBOR c, ToCBOR d) => ToCBOR (a, b, c, d) where
  toCBOR (a, b, c, d) = CV.Array (V.fromList [toCBOR a, toCBOR b, toCBOR c, toCBOR d])

instance (FromCBOR a, FromCBOR b, FromCBOR c, FromCBOR d) => FromCBOR (a, b, c, d) where
  fromCBOR (CV.Array vs)
    | V.length vs == 4 =
        (,,,) <$> fromCBOR (vs V.! 0)
              <*> fromCBOR (vs V.! 1)
              <*> fromCBOR (vs V.! 2)
              <*> fromCBOR (vs V.! 3)
  fromCBOR _ = Left "FromCBOR (a,b,c,d): expected Array of length 4"

instance ToCBOR a => ToCBOR (Identity a) where
  toCBOR (Identity x) = toCBOR x

instance FromCBOR a => FromCBOR (Identity a) where
  fromCBOR v = Identity <$> fromCBOR v

instance ToCBOR a => ToCBOR (Const a b) where
  toCBOR (Const x) = toCBOR x

instance FromCBOR a => FromCBOR (Const a b) where
  fromCBOR v = Const <$> fromCBOR v

instance ToCBOR a => ToCBOR (Down a) where
  toCBOR (Down x) = toCBOR x

instance FromCBOR a => FromCBOR (Down a) where
  fromCBOR v = Down <$> fromCBOR v

instance ToCBOR Version where
  toCBOR = toCBOR . versionBranch

instance FromCBOR Version where
  fromCBOR v = makeVersion <$> fromCBOR v

instance (Integral a, ToCBOR a) => ToCBOR (Ratio a) where
  toCBOR r = CV.Array (V.fromList [toCBOR (numerator r), toCBOR (denominator r)])

instance (Integral a, FromCBOR a) => FromCBOR (Ratio a) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = do
        n <- fromCBOR (vs V.! 0)
        d <- fromCBOR (vs V.! 1)
        if d == 0
          then Left "FromCBOR Ratio: zero denominator"
          else Right (n % d)
  fromCBOR _ = Left "FromCBOR Ratio: expected Array of length 2"

instance ToCBOR CV.Value where
  toCBOR = id

instance FromCBOR CV.Value where
  fromCBOR = Right

-- GHC.Generics support

class GToCBOR f where
  gToCBOR :: f p -> CV.Value

class GFromCBOR f where
  gFromCBOR :: CV.Value -> Either String (f p)

instance GToCBOR f => GToCBOR (M1 D c f) where
  gToCBOR (M1 x) = gToCBOR x

instance GFromCBOR f => GFromCBOR (M1 D c f) where
  gFromCBOR v = M1 <$> gFromCBOR v

instance (Constructor c, GToCBORFields f) => GToCBOR (M1 C c f) where
  gToCBOR (M1 x) =
    let fields = gToCBORFields x
    in CV.Map (V.fromList [(CV.TextString k, v) | (k, v) <- fields])

instance (Constructor c, GFromCBORFields f) => GFromCBOR (M1 C c f) where
  gFromCBOR (CV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromCBORFields lkup
  gFromCBOR _ = Left "GFromCBOR: expected Map for record type"

lookupField :: Text -> Vector (CV.Value, CV.Value) -> Maybe CV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (CV.TextString k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToCBORFields f where
  gToCBORFields :: f p -> [(Text, CV.Value)]

class GFromCBORFields f where
  gFromCBORFields :: (Text -> Maybe CV.Value) -> Either String (f p)

instance (GToCBORFields a, GToCBORFields b) => GToCBORFields (a :*: b) where
  gToCBORFields (a :*: b) = gToCBORFields a ++ gToCBORFields b

instance (GFromCBORFields a, GFromCBORFields b) => GFromCBORFields (a :*: b) where
  gFromCBORFields lkup = (:*:) <$> gFromCBORFields lkup <*> gFromCBORFields lkup

instance (Selector s, ToCBOR a) => GToCBORFields (M1 S s (K1 i a)) where
  gToCBORFields m@(M1 (K1 x)) = [(T.pack (selName m), toCBOR x)]

instance (Selector s, FromCBOR a) => GFromCBORFields (M1 S s (K1 i a)) where
  gFromCBORFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromCBOR: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromCBOR v
