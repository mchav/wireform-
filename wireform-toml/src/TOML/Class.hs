{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based TOML serialization with GHC Generics support.
module TOML.Class
  ( ToTOML(..)
  , FromTOML(..)
  , encodeTOML
  , decodeTOML
  , GToTOML(..)
  , GFromTOML(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
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
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version, makeVersion, versionBranch)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

import qualified TOML.Value as TV
import qualified TOML.Encode as TE
import qualified TOML.Decode as TD

class ToTOML a where
  toTOML :: a -> TV.Value
  default toTOML :: (Generic a, GToTOML (Rep a)) => a -> TV.Value
  toTOML = gToTOML . from

class FromTOML a where
  fromTOML :: TV.Value -> Either String a
  default fromTOML :: (Generic a, GFromTOML (Rep a)) => TV.Value -> Either String a
  fromTOML v = to <$> gFromTOML v

encodeTOML :: ToTOML a => a -> Text
encodeTOML = TE.encode . toTOML

decodeTOML :: FromTOML a => Text -> Either String a
decodeTOML t = TD.decode t >>= fromTOML

instance ToTOML Text where
  toTOML = TV.TString

instance FromTOML Text where
  fromTOML (TV.TString t) = Right t
  fromTOML _ = Left "FromTOML Text: expected TString"

instance ToTOML Bool where
  toTOML = TV.TBool

instance FromTOML Bool where
  fromTOML (TV.TBool b) = Right b
  fromTOML _ = Left "FromTOML Bool: expected TBool"

instance ToTOML Int where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int: expected TInteger"

instance ToTOML Int8 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int8 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int8: expected TInteger"

instance ToTOML Int16 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int16 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int16: expected TInteger"

instance ToTOML Int32 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int32 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int32: expected TInteger"

instance ToTOML Int64 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int64 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int64: expected TInteger"

instance ToTOML Word where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word: expected TInteger"

instance ToTOML Word8 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word8 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word8: expected TInteger"

instance ToTOML Word16 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word16 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word16: expected TInteger"

instance ToTOML Word32 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word32 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word32: expected TInteger"

instance ToTOML Word64 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word64 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word64: expected TInteger"

instance ToTOML Integer where
  toTOML = TV.TInteger

instance FromTOML Integer where
  fromTOML (TV.TInteger n) = Right n
  fromTOML _ = Left "FromTOML Integer: expected TInteger"

instance ToTOML Double where
  toTOML = TV.TFloat

instance FromTOML Double where
  fromTOML (TV.TFloat d) = Right d
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Double: expected TFloat"

instance ToTOML Float where
  toTOML = TV.TFloat . realToFrac

instance FromTOML Float where
  fromTOML (TV.TFloat d) = Right (realToFrac d)
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Float: expected TFloat"

instance ToTOML a => ToTOML [a] where
  toTOML xs = TV.TArray (V.fromList (map toTOML xs))

instance FromTOML a => FromTOML [a] where
  fromTOML (TV.TArray vs) = traverse fromTOML (V.toList vs)
  fromTOML _ = Left "FromTOML [a]: expected TArray"

instance ToTOML a => ToTOML (Vector a) where
  toTOML xs = TV.TArray (V.map toTOML xs)

instance FromTOML a => FromTOML (Vector a) where
  fromTOML (TV.TArray vs) = V.mapM fromTOML vs
  fromTOML _ = Left "FromTOML Vector: expected TArray"

instance ToTOML a => ToTOML (Maybe a) where
  toTOML Nothing = TV.TString ""
  toTOML (Just x) = toTOML x

instance FromTOML a => FromTOML (Maybe a) where
  fromTOML (TV.TString t) | T.null t = Right Nothing
  fromTOML v = Just <$> fromTOML v

-- Aeson-parity instances ---------------------------------------------------

instance ToTOML Char where
  toTOML c = TV.TString (T.singleton c)

instance FromTOML Char where
  fromTOML (TV.TString t) | T.length t == 1 = Right (T.head t)
  fromTOML _ = Left "FromTOML Char: expected single-character TString"

instance ToTOML Natural where
  toTOML = TV.TInteger . toInteger

instance FromTOML Natural where
  fromTOML (TV.TInteger n) | n >= 0 = Right (fromInteger n)
  fromTOML _ = Left "FromTOML Natural: expected non-negative TInteger"

instance ToTOML TL.Text where
  toTOML = TV.TString . TL.toStrict

instance FromTOML TL.Text where
  fromTOML v = TL.fromStrict <$> fromTOML v

-- | TOML has no native binary type; bytes are encoded as their UTF-8
-- decoding when valid, otherwise rejected on decode.
instance ToTOML ByteString where
  toTOML = TV.TString . TEnc.decodeUtf8

instance FromTOML ByteString where
  fromTOML (TV.TString t) = Right (TEnc.encodeUtf8 t)
  fromTOML _ = Left "FromTOML ByteString: expected TString"

instance ToTOML BSL.ByteString where
  toTOML = TV.TString . TEnc.decodeUtf8 . BSL.toStrict

instance FromTOML BSL.ByteString where
  fromTOML v = BSL.fromStrict <$> fromTOML v

instance ToTOML a => ToTOML (NonEmpty a) where
  toTOML = toTOML . NE.toList

instance FromTOML a => FromTOML (NonEmpty a) where
  fromTOML v = do
    xs <- fromTOML v
    case xs of
      []     -> Left "FromTOML NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

-- | 'Either' encodes as a single-key TTable with @"Left"@ or @"Right"@.
instance (ToTOML a, ToTOML b) => ToTOML (Either a b) where
  toTOML (Left  x) = TV.TTable (V.singleton ("Left",  toTOML x))
  toTOML (Right x) = TV.TTable (V.singleton ("Right", toTOML x))

instance (FromTOML a, FromTOML b) => FromTOML (Either a b) where
  fromTOML (TV.TTable kvs)
    | V.length kvs == 1 = case V.head kvs of
        ("Left",  v) -> Left  <$> fromTOML v
        ("Right", v) -> Right <$> fromTOML v
        _            -> Left "FromTOML Either: expected Left/Right key"
  fromTOML _ = Left "FromTOML Either: expected single-key TTable"

instance (Ord a, ToTOML a) => ToTOML (Set a) where
  toTOML = TV.TArray . V.fromList . fmap toTOML . Set.toList

instance (Ord a, FromTOML a) => FromTOML (Set a) where
  fromTOML v = Set.fromList <$> fromTOML v

instance ToTOML a => ToTOML (Seq a) where
  toTOML s = TV.TArray (V.fromList (fmap toTOML (foldr (:) [] s)))

instance FromTOML a => FromTOML (Seq a) where
  fromTOML v = Seq.fromList <$> fromTOML v

instance ToTOML v => ToTOML (Map Text v) where
  toTOML m = TV.TTable (V.fromList [(k, toTOML v) | (k, v) <- Map.toList m])

instance FromTOML v => FromTOML (Map Text v) where
  fromTOML (TV.TTable kvs) = do
    pairs <- traverse (\(k, v) -> (,) k <$> fromTOML v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromTOML _ = Left "FromTOML (Map Text v): expected TTable"

instance ToTOML v => ToTOML (IntMap v) where
  toTOML m = TV.TTable (V.fromList [(T.pack (show k), toTOML v) | (k, v) <- IntMap.toList m])

instance FromTOML v => FromTOML (IntMap v) where
  fromTOML (TV.TTable kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (k, v) = case reads (T.unpack k) of
        [(i, "")] -> (,) i <$> fromTOML v
        _         -> Left "FromTOML IntMap: cannot parse Int key"
  fromTOML _ = Left "FromTOML IntMap: expected TTable"

instance ToTOML IntSet where
  toTOML = TV.TArray . V.fromList . fmap toTOML . IntSet.toList

instance FromTOML IntSet where
  fromTOML v = IntSet.fromList <$> fromTOML v

instance (ToTOML a, ToTOML b) => ToTOML (a, b) where
  toTOML (a, b) = TV.TArray (V.fromList [toTOML a, toTOML b])

instance (FromTOML a, FromTOML b) => FromTOML (a, b) where
  fromTOML (TV.TArray vs)
    | V.length vs == 2 = (,) <$> fromTOML (vs V.! 0) <*> fromTOML (vs V.! 1)
  fromTOML _ = Left "FromTOML (a,b): expected TArray of length 2"

instance (ToTOML a, ToTOML b, ToTOML c) => ToTOML (a, b, c) where
  toTOML (a, b, c) = TV.TArray (V.fromList [toTOML a, toTOML b, toTOML c])

instance (FromTOML a, FromTOML b, FromTOML c) => FromTOML (a, b, c) where
  fromTOML (TV.TArray vs)
    | V.length vs == 3 =
        (,,) <$> fromTOML (vs V.! 0) <*> fromTOML (vs V.! 1) <*> fromTOML (vs V.! 2)
  fromTOML _ = Left "FromTOML (a,b,c): expected TArray of length 3"

instance (ToTOML a, ToTOML b, ToTOML c, ToTOML d) => ToTOML (a, b, c, d) where
  toTOML (a, b, c, d) = TV.TArray (V.fromList [toTOML a, toTOML b, toTOML c, toTOML d])

instance (FromTOML a, FromTOML b, FromTOML c, FromTOML d) => FromTOML (a, b, c, d) where
  fromTOML (TV.TArray vs)
    | V.length vs == 4 =
        (,,,) <$> fromTOML (vs V.! 0) <*> fromTOML (vs V.! 1)
              <*> fromTOML (vs V.! 2) <*> fromTOML (vs V.! 3)
  fromTOML _ = Left "FromTOML (a,b,c,d): expected TArray of length 4"

instance ToTOML () where
  toTOML () = TV.TArray V.empty

instance FromTOML () where
  fromTOML (TV.TArray vs) | V.null vs = Right ()
  fromTOML _ = Left "FromTOML (): expected empty TArray"

instance ToTOML a => ToTOML (Identity a) where
  toTOML (Identity x) = toTOML x

instance FromTOML a => FromTOML (Identity a) where
  fromTOML v = Identity <$> fromTOML v

instance ToTOML a => ToTOML (Const a b) where
  toTOML (Const x) = toTOML x

instance FromTOML a => FromTOML (Const a b) where
  fromTOML v = Const <$> fromTOML v

instance ToTOML a => ToTOML (Down a) where
  toTOML (Down x) = toTOML x

instance FromTOML a => FromTOML (Down a) where
  fromTOML v = Down <$> fromTOML v

instance ToTOML Version where
  toTOML = toTOML . versionBranch

instance FromTOML Version where
  fromTOML v = makeVersion <$> fromTOML v

instance (Integral a, ToTOML a) => ToTOML (Ratio a) where
  toTOML r = TV.TArray (V.fromList [toTOML (numerator r), toTOML (denominator r)])

instance (Integral a, FromTOML a) => FromTOML (Ratio a) where
  fromTOML (TV.TArray vs)
    | V.length vs == 2 = do
        n <- fromTOML (vs V.! 0)
        d <- fromTOML (vs V.! 1)
        if d == 0
          then Left "FromTOML Ratio: zero denominator"
          else Right (n % d)
  fromTOML _ = Left "FromTOML Ratio: expected TArray of length 2"

instance ToTOML TV.Value where
  toTOML = id

instance FromTOML TV.Value where
  fromTOML = Right

-- GHC.Generics support

class GToTOML f where
  gToTOML :: f p -> TV.Value

class GFromTOML f where
  gFromTOML :: TV.Value -> Either String (f p)

instance GToTOML f => GToTOML (M1 D c f) where
  gToTOML (M1 x) = gToTOML x

instance GFromTOML f => GFromTOML (M1 D c f) where
  gFromTOML v = M1 <$> gFromTOML v

instance (Constructor c, GToTOMLFields f) => GToTOML (M1 C c f) where
  gToTOML (M1 x) =
    let fields = gToTOMLFields x
    in TV.TTable (V.fromList fields)

instance (Constructor c, GFromTOMLFields f) => GFromTOML (M1 C c f) where
  gFromTOML (TV.TTable kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromTOMLFields lkup
  gFromTOML _ = Left "GFromTOML: expected TTable for record type"

lookupField :: Text -> Vector (Text, TV.Value) -> Maybe TV.Value
lookupField name kvs = go 0
  where
    !len = V.length kvs
    go !i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToTOMLFields f where
  gToTOMLFields :: f p -> [(Text, TV.Value)]

class GFromTOMLFields f where
  gFromTOMLFields :: (Text -> Maybe TV.Value) -> Either String (f p)

instance (GToTOMLFields a, GToTOMLFields b) => GToTOMLFields (a :*: b) where
  gToTOMLFields (a :*: b) = gToTOMLFields a ++ gToTOMLFields b

instance (GFromTOMLFields a, GFromTOMLFields b) => GFromTOMLFields (a :*: b) where
  gFromTOMLFields lkup = (:*:) <$> gFromTOMLFields lkup <*> gFromTOMLFields lkup

instance (Selector s, ToTOML a) => GToTOMLFields (M1 S s (K1 i a)) where
  gToTOMLFields m@(M1 (K1 x)) = [(T.pack (selName m), toTOML x)]

instance (Selector s, FromTOML a) => GFromTOMLFields (M1 S s (K1 i a)) where
  gFromTOMLFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromTOML: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromTOML v
