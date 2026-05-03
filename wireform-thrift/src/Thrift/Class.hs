{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based Thrift serialization with GHC Generics support.
--
-- Provides 'ToThrift' and 'FromThrift' typeclasses for converting Haskell
-- values to\/from 'Thrift.Value.Value'. Records are encoded as Thrift
-- structs with field IDs assigned sequentially (1, 2, 3, ...).
-- Derive instances via @DeriveGeneric@.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import Thrift.Class
--
-- data LogEntry = LogEntry { level :: Text, message :: Text, code :: Int }
--   deriving (Generic)
-- instance ToThrift LogEntry
-- instance FromThrift LogEntry
--
-- let bytes = encodeThriftBinary (LogEntry \"ERROR\" \"disk full\" 507)
-- let Right entry = decodeThriftBinary bytes :: Either String LogEntry
-- @
module Thrift.Class
  ( ToThrift(..)
  , FromThrift(..)
  , encodeThriftBinary
  , encodeThriftBinaryDirect
  , decodeThriftBinary
  , encodeThriftCompact
  , encodeThriftCompactDirect
  , decodeThriftCompact
  , genericToEncoding
  , GToThrift(..)
  , GFromThrift(..)
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

import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType(..))
import qualified Thrift.Encode as TE
import qualified Thrift.Decode as TD
import Thrift.Encoding (Encoding)
import qualified Thrift.Encoding as Enc

class ToThrift a where
  toThrift :: a -> TV.Value
  default toThrift :: (Generic a, GToThrift (Rep a)) => a -> TV.Value
  toThrift = gToThrift . from

  -- | aeson-style direct encoder. Thrift's binary and compact wire
  -- formats need protocol commitment before bytes flow, so
  -- 'Encoding' wraps a fully-built 'TV.Value' and routes through
  -- 'TE.encodeBinary' / 'TE.encodeCompact' at run time.
  toEncoding :: a -> Encoding
  toEncoding = Enc.value . toThrift

class FromThrift a where
  fromThrift :: TV.Value -> Either String a
  default fromThrift :: (Generic a, GFromThrift (Rep a)) => TV.Value -> Either String a
  fromThrift v = to <$> gFromThrift v

encodeThriftBinary :: ToThrift a => a -> ByteString
encodeThriftBinary = TE.encodeBinary . toThrift

-- | Encode binary protocol via 'toEncoding'.
encodeThriftBinaryDirect :: ToThrift a => a -> ByteString
encodeThriftBinaryDirect = Enc.encodingToBinaryByteString . toEncoding

decodeThriftBinary :: FromThrift a => ByteString -> Either String a
decodeThriftBinary bs = TD.decodeBinary bs >>= fromThrift

encodeThriftCompact :: ToThrift a => a -> ByteString
encodeThriftCompact = TE.encodeCompact . toThrift

-- | Encode compact protocol via 'toEncoding'.
encodeThriftCompactDirect :: ToThrift a => a -> ByteString
encodeThriftCompactDirect = Enc.encodingToCompactByteString . toEncoding

genericToEncoding :: (Generic a, GToThrift (Rep a)) => a -> Encoding
genericToEncoding = Enc.value . gToThrift . from

decodeThriftCompact :: FromThrift a => ByteString -> Either String a
decodeThriftCompact bs = TD.decodeCompact bs >>= fromThrift

instance ToThrift Bool where
  toThrift = TV.Bool

instance FromThrift Bool where
  fromThrift (TV.Bool b) = Right b
  fromThrift _ = Left "FromThrift Bool: expected Bool"

instance ToThrift Int where
  toThrift n = TV.I64 (fromIntegral n)

instance FromThrift Int where
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Int: expected integer type"

instance ToThrift Int8 where
  toThrift = TV.Byte

instance FromThrift Int8 where
  fromThrift (TV.Byte n) = Right n
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Int8: expected integer type"

instance ToThrift Int16 where
  toThrift = TV.I16

instance FromThrift Int16 where
  fromThrift (TV.I16 n) = Right n
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Int16: expected integer type"

instance ToThrift Int32 where
  toThrift = TV.I32

instance FromThrift Int32 where
  fromThrift (TV.I32 n) = Right n
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Int32: expected integer type"

instance ToThrift Int64 where
  toThrift = TV.I64

instance FromThrift Int64 where
  fromThrift (TV.I64 n) = Right n
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Int64: expected integer type"

instance ToThrift Word where
  toThrift n = TV.I64 (fromIntegral n)

instance FromThrift Word where
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Word: expected integer type"

instance ToThrift Word8 where
  toThrift n = TV.Byte (fromIntegral n)

instance FromThrift Word8 where
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Word8: expected integer type"

instance ToThrift Word16 where
  toThrift n = TV.I16 (fromIntegral n)

instance FromThrift Word16 where
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Word16: expected integer type"

instance ToThrift Word32 where
  toThrift n = TV.I32 (fromIntegral n)

instance FromThrift Word32 where
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Word32: expected integer type"

instance ToThrift Word64 where
  toThrift n = TV.I64 (fromIntegral n)

instance FromThrift Word64 where
  fromThrift (TV.I64 n) = Right (fromIntegral n)
  fromThrift (TV.I32 n) = Right (fromIntegral n)
  fromThrift (TV.I16 n) = Right (fromIntegral n)
  fromThrift (TV.Byte n) = Right (fromIntegral n)
  fromThrift _ = Left "FromThrift Word64: expected integer type"

instance ToThrift Float where
  toThrift f = TV.Double (realToFrac f)

instance FromThrift Float where
  fromThrift (TV.Double d) = Right (realToFrac d)
  fromThrift _ = Left "FromThrift Float: expected Double"

instance ToThrift Double where
  toThrift = TV.Double

instance FromThrift Double where
  fromThrift (TV.Double d) = Right d
  fromThrift _ = Left "FromThrift Double: expected Double"

instance ToThrift Text where
  toThrift = TV.String

instance FromThrift Text where
  fromThrift (TV.String t) = Right t
  fromThrift _ = Left "FromThrift Text: expected String"

instance ToThrift ByteString where
  toThrift = TV.Binary

instance FromThrift ByteString where
  fromThrift (TV.Binary bs) = Right bs
  fromThrift (TV.UUID bs) = Right bs
  fromThrift _ = Left "FromThrift ByteString: expected Binary"

instance ToThrift a => ToThrift [a] where
  toThrift xs =
    let vals = V.fromList (map toThrift xs)
        tt = if V.null vals then TT_I64 else TV.thriftTypeOf (V.head vals)
    in TV.List tt vals

instance FromThrift a => FromThrift [a] where
  fromThrift (TV.List _ vs) = traverse fromThrift (V.toList vs)
  fromThrift (TV.Set _ vs) = traverse fromThrift (V.toList vs)
  fromThrift _ = Left "FromThrift [a]: expected List or Set"

instance ToThrift a => ToThrift (Vector a) where
  toThrift xs =
    let vals = V.map toThrift xs
        tt = if V.null vals then TT_I64 else TV.thriftTypeOf (V.head vals)
    in TV.List tt vals

instance FromThrift a => FromThrift (Vector a) where
  fromThrift (TV.List _ vs) = V.mapM fromThrift vs
  fromThrift (TV.Set _ vs) = V.mapM fromThrift vs
  fromThrift _ = Left "FromThrift Vector: expected List or Set"

instance (ToThrift a, ToThrift b) => ToThrift (a, b) where
  toThrift (a, b) =
    let vals = V.fromList [toThrift a, toThrift b]
        tt = TV.thriftTypeOf (toThrift a)
    in TV.List tt vals

instance (FromThrift a, FromThrift b) => FromThrift (a, b) where
  fromThrift (TV.List _ vs)
    | V.length vs == 2 = (,) <$> fromThrift (vs V.! 0) <*> fromThrift (vs V.! 1)
  fromThrift _ = Left "FromThrift (a,b): expected List of length 2"

-- Aeson-parity instances ---------------------------------------------------

thriftListOf :: ToThrift a => [a] -> TV.Value
thriftListOf xs =
  let vals = V.fromList (fmap toThrift xs)
      tt   = if V.null vals then TT_I64 else TV.thriftTypeOf (V.head vals)
  in TV.List tt vals

instance ToThrift Char where
  toThrift c = TV.String (T.singleton c)

instance FromThrift Char where
  fromThrift (TV.String t) | T.length t == 1 = Right (T.head t)
  fromThrift _ = Left "FromThrift Char: expected single-character String"

instance ToThrift Integer where
  toThrift = TV.I64 . fromInteger

instance FromThrift Integer where
  fromThrift (TV.I64 n)  = Right (toInteger n)
  fromThrift (TV.I32 n)  = Right (toInteger n)
  fromThrift (TV.I16 n)  = Right (toInteger n)
  fromThrift (TV.Byte n) = Right (toInteger n)
  fromThrift _ = Left "FromThrift Integer: expected integer type"

instance ToThrift Natural where
  toThrift = TV.I64 . fromIntegral

instance FromThrift Natural where
  fromThrift v = do
    n <- fromThrift v :: Either String Integer
    if n < 0
      then Left "FromThrift Natural: negative integer"
      else Right (fromInteger n)

instance ToThrift TL.Text where
  toThrift = TV.String . TL.toStrict

instance FromThrift TL.Text where
  fromThrift v = TL.fromStrict <$> fromThrift v

instance ToThrift BSL.ByteString where
  toThrift = TV.Binary . BSL.toStrict

instance FromThrift BSL.ByteString where
  fromThrift v = BSL.fromStrict <$> fromThrift v

instance ToThrift a => ToThrift (NonEmpty a) where
  toThrift = thriftListOf . NE.toList

instance FromThrift a => FromThrift (NonEmpty a) where
  fromThrift v = do
    xs <- fromThrift v
    case xs of
      []     -> Left "FromThrift NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

-- | 'Either' encodes as a single-field struct: field 1 = Left, field 2 = Right.
instance (ToThrift a, ToThrift b) => ToThrift (Either a b) where
  toThrift (Left  x) = TV.Struct (V.singleton (1, toThrift x))
  toThrift (Right x) = TV.Struct (V.singleton (2, toThrift x))

instance (FromThrift a, FromThrift b) => FromThrift (Either a b) where
  fromThrift (TV.Struct kvs)
    | V.length kvs == 1 = case V.head kvs of
        (1, v) -> Left  <$> fromThrift v
        (2, v) -> Right <$> fromThrift v
        _      -> Left "FromThrift Either: expected field id 1 or 2"
  fromThrift _ = Left "FromThrift Either: expected single-field Struct"

instance (Ord a, ToThrift a) => ToThrift (Set a) where
  toThrift xs =
    let vals = V.fromList (fmap toThrift (Set.toList xs))
        tt   = if V.null vals then TT_I64 else TV.thriftTypeOf (V.head vals)
    in TV.Set tt vals

instance (Ord a, FromThrift a) => FromThrift (Set a) where
  fromThrift v = Set.fromList <$> fromThrift v

instance ToThrift a => ToThrift (Seq a) where
  toThrift s = thriftListOf (foldr (:) [] s)

instance FromThrift a => FromThrift (Seq a) where
  fromThrift v = Seq.fromList <$> fromThrift v

instance (ToThrift k, ToThrift v) => ToThrift (Map k v) where
  toThrift m =
    let pairs = V.fromList [(toThrift k, toThrift v) | (k, v) <- Map.toList m]
        kt = if V.null pairs then TT_STRING else TV.thriftTypeOf (fst (V.head pairs))
        vt = if V.null pairs then TT_STRING else TV.thriftTypeOf (snd (V.head pairs))
    in TV.Map kt vt pairs

instance (Ord k, FromThrift k, FromThrift v) => FromThrift (Map k v) where
  fromThrift (TV.Map _ _ kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromThrift k <*> fromThrift v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromThrift _ = Left "FromThrift Map: expected Map"

instance (Hashable k, ToThrift k, ToThrift v) => ToThrift (HashMap k v) where
  toThrift m =
    let pairs = V.fromList [(toThrift k, toThrift v) | (k, v) <- HM.toList m]
        kt = if V.null pairs then TT_STRING else TV.thriftTypeOf (fst (V.head pairs))
        vt = if V.null pairs then TT_STRING else TV.thriftTypeOf (snd (V.head pairs))
    in TV.Map kt vt pairs

instance (Eq k, Hashable k, FromThrift k, FromThrift v) => FromThrift (HashMap k v) where
  fromThrift (TV.Map _ _ kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromThrift k <*> fromThrift v) (V.toList kvs)
    Right (HM.fromList pairs)
  fromThrift _ = Left "FromThrift HashMap: expected Map"

instance (Hashable a, ToThrift a) => ToThrift (HashSet a) where
  toThrift xs =
    let vals = V.fromList (fmap toThrift (HS.toList xs))
        tt   = if V.null vals then TT_I64 else TV.thriftTypeOf (V.head vals)
    in TV.Set tt vals

instance (Eq a, Hashable a, FromThrift a) => FromThrift (HashSet a) where
  fromThrift v = HS.fromList <$> fromThrift v

instance ToThrift v => ToThrift (IntMap v) where
  toThrift m =
    let pairs = V.fromList [(toThrift k, toThrift v) | (k, v) <- IntMap.toList m]
        vt = if V.null pairs then TT_STRING else TV.thriftTypeOf (snd (V.head pairs))
    in TV.Map TT_I64 vt pairs

instance FromThrift v => FromThrift (IntMap v) where
  fromThrift (TV.Map _ _ kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromThrift k <*> fromThrift v) (V.toList kvs)
    Right (IntMap.fromList pairs)
  fromThrift _ = Left "FromThrift IntMap: expected Map"

instance ToThrift IntSet where
  toThrift = thriftListOf . IntSet.toList

instance FromThrift IntSet where
  fromThrift v = IntSet.fromList <$> fromThrift v

instance (ToThrift a, ToThrift b, ToThrift c) => ToThrift (a, b, c) where
  toThrift (a, b, c) = TV.List TT_STRUCT (V.fromList [toThrift a, toThrift b, toThrift c])

instance (FromThrift a, FromThrift b, FromThrift c) => FromThrift (a, b, c) where
  fromThrift (TV.List _ vs)
    | V.length vs == 3 =
        (,,) <$> fromThrift (vs V.! 0) <*> fromThrift (vs V.! 1) <*> fromThrift (vs V.! 2)
  fromThrift _ = Left "FromThrift (a,b,c): expected List of length 3"

instance (ToThrift a, ToThrift b, ToThrift c, ToThrift d) => ToThrift (a, b, c, d) where
  toThrift (a, b, c, d) = TV.List TT_STRUCT (V.fromList [toThrift a, toThrift b, toThrift c, toThrift d])

instance (FromThrift a, FromThrift b, FromThrift c, FromThrift d) => FromThrift (a, b, c, d) where
  fromThrift (TV.List _ vs)
    | V.length vs == 4 =
        (,,,) <$> fromThrift (vs V.! 0) <*> fromThrift (vs V.! 1)
              <*> fromThrift (vs V.! 2) <*> fromThrift (vs V.! 3)
  fromThrift _ = Left "FromThrift (a,b,c,d): expected List of length 4"

instance ToThrift a => ToThrift (Identity a) where
  toThrift (Identity x) = toThrift x

instance FromThrift a => FromThrift (Identity a) where
  fromThrift v = Identity <$> fromThrift v

instance ToThrift a => ToThrift (Const a b) where
  toThrift (Const x) = toThrift x

instance FromThrift a => FromThrift (Const a b) where
  fromThrift v = Const <$> fromThrift v

instance ToThrift a => ToThrift (Down a) where
  toThrift (Down x) = toThrift x

instance FromThrift a => FromThrift (Down a) where
  fromThrift v = Down <$> fromThrift v

instance ToThrift Version where
  toThrift = toThrift . versionBranch

instance FromThrift Version where
  fromThrift v = makeVersion <$> fromThrift v

instance ToThrift () where
  toThrift () = TV.Struct V.empty

instance FromThrift () where
  fromThrift (TV.Struct kvs) | V.null kvs = Right ()
  fromThrift _ = Left "FromThrift (): expected empty Struct"

instance ToThrift a => ToThrift (Maybe a) where
  toThrift Nothing  = TV.Struct V.empty
  toThrift (Just x) = TV.Struct (V.singleton (1, toThrift x))

instance FromThrift a => FromThrift (Maybe a) where
  fromThrift (TV.Struct kvs)
    | V.null kvs = Right Nothing
    | V.length kvs == 1, (1, v) <- V.head kvs = Just <$> fromThrift v
  fromThrift v = Just <$> fromThrift v

instance (Integral a, ToThrift a) => ToThrift (Ratio a) where
  toThrift r = TV.List TT_STRUCT (V.fromList [toThrift (numerator r), toThrift (denominator r)])

instance (Integral a, FromThrift a) => FromThrift (Ratio a) where
  fromThrift (TV.List _ vs)
    | V.length vs == 2 = do
        n <- fromThrift (vs V.! 0)
        d <- fromThrift (vs V.! 1)
        if d == 0
          then Left "FromThrift Ratio: zero denominator"
          else Right (n % d)
  fromThrift _ = Left "FromThrift Ratio: expected List of length 2"

instance ToThrift TV.Value where
  toThrift = id

instance FromThrift TV.Value where
  fromThrift = Right

-- GHC.Generics support

class GToThrift f where
  gToThrift :: f p -> TV.Value

class GFromThrift f where
  gFromThrift :: TV.Value -> Either String (f p)

instance GToThrift f => GToThrift (M1 D c f) where
  gToThrift (M1 x) = gToThrift x

instance GFromThrift f => GFromThrift (M1 D c f) where
  gFromThrift v = M1 <$> gFromThrift v

instance (GToThriftFields f) => GToThrift (M1 C c f) where
  gToThrift (M1 x) =
    let fields = gToThriftFields 1 x
    in TV.Struct (V.fromList fields)

instance (GFromThriftFields f) => GFromThrift (M1 C c f) where
  gFromThrift (TV.Struct kvs) =
    let lkup fid = lookupField fid kvs
    in M1 <$> gFromThriftFields 1 lkup
  gFromThrift _ = Left "GFromThrift: expected Struct for record type"

lookupField :: Int16 -> Vector (Int16, TV.Value) -> Maybe TV.Value
lookupField fid kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == fid = Just v
      | otherwise = go (i + 1)

class GToThriftFields f where
  gToThriftFields :: Int16 -> f p -> [(Int16, TV.Value)]

class GFromThriftFields f where
  gFromThriftFields :: Int16 -> (Int16 -> Maybe TV.Value) -> Either String (f p)

instance (GToThriftFields a, GToThriftFields b, GFieldCount a) => GToThriftFields (a :*: b) where
  gToThriftFields fid (a :*: b) =
    gToThriftFields fid a ++ gToThriftFields (fid + gFieldCount (undefined :: a p)) b

instance (GFromThriftFields a, GFromThriftFields b, GFieldCount a) => GFromThriftFields (a :*: b) where
  gFromThriftFields fid lkup =
    (:*:) <$> gFromThriftFields fid lkup
          <*> gFromThriftFields (fid + gFieldCount (undefined :: a p)) lkup

instance (ToThrift a) => GToThriftFields (M1 S s (K1 i a)) where
  gToThriftFields fid (M1 (K1 x)) = [(fid, toThrift x)]

instance (FromThrift a) => GFromThriftFields (M1 S s (K1 i a)) where
  gFromThriftFields fid lkup =
    case lkup fid of
      Nothing -> Left $ "GFromThrift: missing field " ++ show fid
      Just v  -> M1 . K1 <$> fromThrift v

class GFieldCount f where
  gFieldCount :: f p -> Int16

instance GFieldCount (M1 S s (K1 i a)) where
  gFieldCount _ = 1

instance (GFieldCount a, GFieldCount b) => GFieldCount (a :*: b) where
  gFieldCount _ = gFieldCount (undefined :: a p) + gFieldCount (undefined :: b p)
