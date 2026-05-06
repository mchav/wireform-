{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based Apache Fory serialization with @GHC.Generics@
-- support.
--
-- The two classes are 'ToFury' and 'FromFury'. The default
-- generic deriver renders a record as a 'VV.StructVal' whose
-- namespace is the module name of the type and whose type name is
-- the simple constructor name; field names are passed through
-- unchanged (use the 'Fury.Derive' annotation deriver if you need
-- 'rename' / 'renameStyle' / @snake_case@ handling).
module Fury.Class
  ( ToFury (..)
  , FromFury (..)
  , encodeFury
  , decodeFury

    -- * Reference-tracked sharing
  , Shared (..)

    -- * Primitive array newtypes
    --
    -- $primitiveArrays
  , BoolArray (..)
  , Int8Array (..)
  , Int16Array (..)
  , Int32Array (..)
  , Int64Array (..)
  , Uint8Array (..)
  , Uint16Array (..)
  , Uint32Array (..)
  , Uint64Array (..)
  , Float32Array (..)
  , Float64Array (..)

    -- * Generic helpers
  , GToFury (..)
  , GFromFury (..)
  , GToFuryFields (..)
  , GFromFuryFields (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified Fury.Decode as D
import qualified Fury.Encode as E
import qualified Fury.Value as VV

-- ---------------------------------------------------------------------------
-- Public typeclasses
-- ---------------------------------------------------------------------------

class ToFury a where
  toFury :: a -> VV.Value
  default toFury :: (Generic a, GToFury (Rep a)) => a -> VV.Value
  toFury = gToFury . from

class FromFury a where
  fromFury :: VV.Value -> Either String a
  default fromFury :: (Generic a, GFromFury (Rep a)) => VV.Value -> Either String a
  fromFury v = to <$> gFromFury v

-- | Encode any 'ToFury' to its fory wire format.
encodeFury :: ToFury a => a -> ByteString
encodeFury = E.encode . toFury

-- | Decode any 'FromFury' from a fory-encoded byte string.
decodeFury :: FromFury a => ByteString -> Either String a
decodeFury bs = D.decode bs >>= fromFury

-- ---------------------------------------------------------------------------
-- Reference-tracked sharing
-- ---------------------------------------------------------------------------

-- | Wrap a value to opt into Fory\'s reference-tracking. The
-- 'sharingKey' field is a user-supplied 'Int' that tags logically
-- identical objects: every occurrence of the same @sharingKey@
-- under a single 'encodeFury' call after the first encodes as a
-- @REF_FLAG@ back-reference rather than a full payload.
--
-- @
-- let x = Shared 1 (StringVal \"hello\")
-- encodeFury (ListVal (V.fromList [toFury x, toFury x, toFury x]))
-- -- emits the string once + two back-refs
-- @
--
-- Round-trip note: the 'sharingKey' is opaque to the wire (only
-- the encoder\'s auto-assigned 0-based @ref_id@ travels). On
-- decode the recovered key is the wire id, not the original
-- user key. Use @Shared@ for sharing semantics, not for
-- preserving the integer key verbatim.
data Shared a = Shared
  { sharingKey :: !Int
  , unShared   :: !a
  } deriving (Eq, Show)

instance ToFury a => ToFury (Shared a) where
  toFury (Shared k a) = VV.RefVal k (toFury a)

instance FromFury a => FromFury (Shared a) where
  fromFury (VV.RefVal k inner) = Shared k <$> fromFury inner
  fromFury v = Shared 0 <$> fromFury v
  -- Bare values decode as @Shared 0 v@ for permissiveness — the
  -- user explicitly opted into ref-tracking on the encode side
  -- but the wire doesn\'t guarantee it.

-- ---------------------------------------------------------------------------
-- Primitive 1-D array newtypes
-- ---------------------------------------------------------------------------

-- $primitiveArrays
--
-- The default 'ToFury' instance for @'Vector' a@ produces a
-- 'VV.ListVal'. To opt into Fory\'s dense one-byte-per-element
-- (or fixed-width little-endian) array encoding, wrap in one of
-- the following newtypes:
--
-- @
-- toFury (Int32Array (V.fromList [1,2,3]))  -- 'VV.Int32ArrayVal'
-- toFury [1,2,3 :: Int32]                    -- 'VV.ListVal' of int32
-- @

newtype BoolArray = BoolArray { unBoolArray :: Vector Bool }
  deriving stock (Eq, Show)

newtype Int8Array = Int8Array { unInt8Array :: Vector Int8 }
  deriving stock (Eq, Show)

newtype Int16Array = Int16Array { unInt16Array :: Vector Int16 }
  deriving stock (Eq, Show)

newtype Int32Array = Int32Array { unInt32Array :: Vector Int32 }
  deriving stock (Eq, Show)

newtype Int64Array = Int64Array { unInt64Array :: Vector Int64 }
  deriving stock (Eq, Show)

newtype Uint8Array = Uint8Array { unUint8Array :: Vector Word8 }
  deriving stock (Eq, Show)

newtype Uint16Array = Uint16Array { unUint16Array :: Vector Word16 }
  deriving stock (Eq, Show)

newtype Uint32Array = Uint32Array { unUint32Array :: Vector Word32 }
  deriving stock (Eq, Show)

newtype Uint64Array = Uint64Array { unUint64Array :: Vector Word64 }
  deriving stock (Eq, Show)

newtype Float32Array = Float32Array { unFloat32Array :: Vector Float }
  deriving stock (Eq, Show)

newtype Float64Array = Float64Array { unFloat64Array :: Vector Double }
  deriving stock (Eq, Show)

instance ToFury BoolArray where
  toFury = VV.BoolArrayVal . unBoolArray
instance FromFury BoolArray where
  fromFury (VV.BoolArrayVal v) = Right (BoolArray v)
  fromFury _ = Left "FromFury BoolArray: expected BoolArrayVal"

instance ToFury Int8Array where
  toFury = VV.Int8ArrayVal . unInt8Array
instance FromFury Int8Array where
  fromFury (VV.Int8ArrayVal v) = Right (Int8Array v)
  fromFury _ = Left "FromFury Int8Array: expected Int8ArrayVal"

instance ToFury Int16Array where
  toFury = VV.Int16ArrayVal . unInt16Array
instance FromFury Int16Array where
  fromFury (VV.Int16ArrayVal v) = Right (Int16Array v)
  fromFury _ = Left "FromFury Int16Array: expected Int16ArrayVal"

instance ToFury Int32Array where
  toFury = VV.Int32ArrayVal . unInt32Array
instance FromFury Int32Array where
  fromFury (VV.Int32ArrayVal v) = Right (Int32Array v)
  fromFury _ = Left "FromFury Int32Array: expected Int32ArrayVal"

instance ToFury Int64Array where
  toFury = VV.Int64ArrayVal . unInt64Array
instance FromFury Int64Array where
  fromFury (VV.Int64ArrayVal v) = Right (Int64Array v)
  fromFury _ = Left "FromFury Int64Array: expected Int64ArrayVal"

instance ToFury Uint8Array where
  toFury = VV.Uint8ArrayVal . unUint8Array
instance FromFury Uint8Array where
  fromFury (VV.Uint8ArrayVal v) = Right (Uint8Array v)
  fromFury _ = Left "FromFury Uint8Array: expected Uint8ArrayVal"

instance ToFury Uint16Array where
  toFury = VV.Uint16ArrayVal . unUint16Array
instance FromFury Uint16Array where
  fromFury (VV.Uint16ArrayVal v) = Right (Uint16Array v)
  fromFury _ = Left "FromFury Uint16Array: expected Uint16ArrayVal"

instance ToFury Uint32Array where
  toFury = VV.Uint32ArrayVal . unUint32Array
instance FromFury Uint32Array where
  fromFury (VV.Uint32ArrayVal v) = Right (Uint32Array v)
  fromFury _ = Left "FromFury Uint32Array: expected Uint32ArrayVal"

instance ToFury Uint64Array where
  toFury = VV.Uint64ArrayVal . unUint64Array
instance FromFury Uint64Array where
  fromFury (VV.Uint64ArrayVal v) = Right (Uint64Array v)
  fromFury _ = Left "FromFury Uint64Array: expected Uint64ArrayVal"

instance ToFury Float32Array where
  toFury = VV.Float32ArrayVal . unFloat32Array
instance FromFury Float32Array where
  fromFury (VV.Float32ArrayVal v) = Right (Float32Array v)
  fromFury _ = Left "FromFury Float32Array: expected Float32ArrayVal"

instance ToFury Float64Array where
  toFury = VV.Float64ArrayVal . unFloat64Array
instance FromFury Float64Array where
  fromFury (VV.Float64ArrayVal v) = Right (Float64Array v)
  fromFury _ = Left "FromFury Float64Array: expected Float64ArrayVal"

-- ---------------------------------------------------------------------------
-- Base-type instances
-- ---------------------------------------------------------------------------

instance ToFury VV.Value where
  toFury = id

instance FromFury VV.Value where
  fromFury = Right

instance ToFury Bool where
  toFury = VV.BoolVal

instance FromFury Bool where
  fromFury (VV.BoolVal b) = Right b
  fromFury _ = Left "FromFury Bool: expected Bool"

instance ToFury Int8 where
  toFury = VV.Int8Val

instance FromFury Int8 where
  fromFury (VV.Int8Val n) = Right n
  fromFury _ = Left "FromFury Int8: expected Int8"

instance ToFury Int16 where
  toFury = VV.Int16Val

instance FromFury Int16 where
  fromFury (VV.Int16Val n) = Right n
  fromFury _ = Left "FromFury Int16: expected Int16"

instance ToFury Int32 where
  toFury = VV.Int32Val

instance FromFury Int32 where
  fromFury (VV.Int32Val n) = Right n
  fromFury _ = Left "FromFury Int32: expected Int32"

instance ToFury Int64 where
  toFury = VV.Int64Val

instance FromFury Int64 where
  fromFury (VV.Int64Val n) = Right n
  fromFury _ = Left "FromFury Int64: expected Int64"

instance ToFury Int where
  toFury = VV.Int64Val . fromIntegral

instance FromFury Int where
  fromFury (VV.Int64Val n) = Right (fromIntegral n)
  fromFury (VV.Int32Val n) = Right (fromIntegral n)
  fromFury (VV.Int16Val n) = Right (fromIntegral n)
  fromFury (VV.Int8Val  n) = Right (fromIntegral n)
  fromFury _ = Left "FromFury Int: expected an integer"

instance ToFury Word8 where
  toFury = VV.Uint8Val

instance FromFury Word8 where
  fromFury (VV.Uint8Val n) = Right n
  fromFury _ = Left "FromFury Word8: expected UInt8"

instance ToFury Word16 where
  toFury = VV.Uint16Val

instance FromFury Word16 where
  fromFury (VV.Uint16Val n) = Right n
  fromFury _ = Left "FromFury Word16: expected UInt16"

instance ToFury Word32 where
  toFury = VV.Uint32Val

instance FromFury Word32 where
  fromFury (VV.Uint32Val n) = Right n
  fromFury _ = Left "FromFury Word32: expected UInt32"

instance ToFury Word64 where
  toFury = VV.Uint64Val

instance FromFury Word64 where
  fromFury (VV.Uint64Val n) = Right n
  fromFury _ = Left "FromFury Word64: expected UInt64"

instance ToFury Word where
  toFury = VV.Uint64Val . fromIntegral

instance FromFury Word where
  fromFury (VV.Uint64Val n) = Right (fromIntegral n)
  fromFury (VV.Uint32Val n) = Right (fromIntegral n)
  fromFury (VV.Uint16Val n) = Right (fromIntegral n)
  fromFury (VV.Uint8Val  n) = Right (fromIntegral n)
  fromFury _ = Left "FromFury Word: expected an unsigned integer"

instance ToFury Float where
  toFury = VV.Float32Val

instance FromFury Float where
  fromFury (VV.Float32Val f) = Right f
  fromFury (VV.Float64Val d) = Right (realToFrac d)
  fromFury _ = Left "FromFury Float: expected Float32 or Float64"

instance ToFury Double where
  toFury = VV.Float64Val

instance FromFury Double where
  fromFury (VV.Float64Val d) = Right d
  fromFury (VV.Float32Val f) = Right (realToFrac f)
  fromFury _ = Left "FromFury Double: expected Float64 or Float32"

instance ToFury Text where
  toFury = VV.StringVal

instance FromFury Text where
  fromFury (VV.StringVal t) = Right t
  fromFury _ = Left "FromFury Text: expected String"

instance ToFury TL.Text where
  toFury = VV.StringVal . TL.toStrict

instance FromFury TL.Text where
  fromFury v = TL.fromStrict <$> fromFury v

instance ToFury Char where
  toFury c = VV.StringVal (T.singleton c)

instance FromFury Char where
  fromFury (VV.StringVal t) | T.length t == 1 = Right (T.head t)
  fromFury _ = Left "FromFury Char: expected single-character String"

instance ToFury ByteString where
  toFury = VV.BinaryVal

instance FromFury ByteString where
  fromFury (VV.BinaryVal bs) = Right bs
  fromFury _ = Left "FromFury ByteString: expected Binary"

instance ToFury BSL.ByteString where
  toFury = VV.BinaryVal . BSL.toStrict

instance FromFury BSL.ByteString where
  fromFury v = BSL.fromStrict <$> fromFury v

instance ToFury () where
  toFury () = VV.NoneVal

instance FromFury () where
  fromFury VV.NoneVal = Right ()
  fromFury _ = Left "FromFury (): expected None"

-- 'Maybe' lifts 'Nothing' to @None@ and 'Just' through to its
-- payload directly. Round-tripping a @Maybe (Maybe a)@ collapses
-- the two layers of 'Nothing' so this instance is /not/ injective
-- on nested optionals, mirroring what Fory's own xlang treatment
-- does in languages with implicit nullable types.
instance ToFury a => ToFury (Maybe a) where
  toFury Nothing  = VV.NoneVal
  toFury (Just x) = toFury x

instance FromFury a => FromFury (Maybe a) where
  fromFury VV.NoneVal = Right Nothing
  fromFury v          = Just <$> fromFury v

instance ToFury a => ToFury [a] where
  toFury xs = VV.ListVal (V.fromList (map toFury xs))

instance FromFury a => FromFury [a] where
  fromFury (VV.ListVal vs) = traverse fromFury (V.toList vs)
  fromFury _ = Left "FromFury [a]: expected List"

instance ToFury a => ToFury (Vector a) where
  toFury = VV.ListVal . V.map toFury

instance FromFury a => FromFury (Vector a) where
  fromFury (VV.ListVal vs) = V.mapM fromFury vs
  fromFury _ = Left "FromFury Vector: expected List"

instance ToFury a => ToFury (NonEmpty a) where
  toFury = toFury . NE.toList

instance FromFury a => FromFury (NonEmpty a) where
  fromFury v = do
    xs <- fromFury v
    case xs of
      []     -> Left "FromFury NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance ToFury a => ToFury (Seq a) where
  toFury s = VV.ListVal (V.fromList (fmap toFury (foldr (:) [] s)))

instance FromFury a => FromFury (Seq a) where
  fromFury v = Seq.fromList <$> fromFury v

instance ToFury a => ToFury (Set a) where
  toFury = VV.SetVal . V.fromList . fmap toFury . Set.toList

instance (Ord a, FromFury a) => FromFury (Set a) where
  fromFury (VV.SetVal vs) = Set.fromList <$> traverse fromFury (V.toList vs)
  fromFury (VV.ListVal vs) = Set.fromList <$> traverse fromFury (V.toList vs)
  fromFury _ = Left "FromFury Set: expected Set or List"

instance (ToFury k, ToFury v) => ToFury (Map k v) where
  toFury m =
    VV.MapVal
      (V.fromList
        [ (toFury k, toFury vv)
        | (k, vv) <- Map.toAscList m ])

instance (Ord k, FromFury k, FromFury v) => FromFury (Map k v) where
  fromFury (VV.MapVal kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (Map.fromList pairs)
    where
      decodePair (kv, vv) = (,) <$> fromFury kv <*> fromFury vv
  fromFury _ = Left "FromFury Map: expected Map"

instance ToFury v => ToFury (IntMap v) where
  toFury m =
    VV.MapVal
      (V.fromList
        [ (VV.Int64Val (fromIntegral k), toFury vv)
        | (k, vv) <- IntMap.toAscList m ])

instance FromFury v => FromFury (IntMap v) where
  fromFury (VV.MapVal kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (kv, vv) = do
        k' <- fromFury kv
        v' <- fromFury vv
        Right (k', v')
  fromFury _ = Left "FromFury IntMap: expected Map"

instance ToFury IntSet where
  toFury =
    VV.SetVal
      . V.fromList
      . fmap (VV.Int64Val . fromIntegral)
      . IntSet.toAscList

instance FromFury IntSet where
  fromFury v = IntSet.fromList <$> fromFury v

instance (ToFury a, ToFury b) => ToFury (a, b) where
  toFury (a, b) = VV.ListVal (V.fromList [toFury a, toFury b])

instance (FromFury a, FromFury b) => FromFury (a, b) where
  fromFury (VV.ListVal vs)
    | V.length vs == 2 = (,) <$> fromFury (vs V.! 0) <*> fromFury (vs V.! 1)
  fromFury _ = Left "FromFury (a,b): expected List of length 2"

instance (ToFury a, ToFury b, ToFury c) => ToFury (a, b, c) where
  toFury (a, b, c) =
    VV.ListVal (V.fromList [toFury a, toFury b, toFury c])

instance (FromFury a, FromFury b, FromFury c) => FromFury (a, b, c) where
  fromFury (VV.ListVal vs)
    | V.length vs == 3 =
        (,,) <$> fromFury (vs V.! 0)
             <*> fromFury (vs V.! 1)
             <*> fromFury (vs V.! 2)
  fromFury _ = Left "FromFury (a,b,c): expected List of length 3"

instance ToFury a => ToFury (Identity a) where
  toFury (Identity x) = toFury x

instance FromFury a => FromFury (Identity a) where
  fromFury v = Identity <$> fromFury v

instance ToFury a => ToFury (Const a b) where
  toFury (Const x) = toFury x

instance FromFury a => FromFury (Const a b) where
  fromFury v = Const <$> fromFury v

-- ---------------------------------------------------------------------------
-- Generic deriver
-- ---------------------------------------------------------------------------

class GToFury f where
  gToFury :: f p -> VV.Value

class GFromFury f where
  gFromFury :: VV.Value -> Either String (f p)

instance (Datatype d, GToFuryC f) => GToFury (M1 D d f) where
  gToFury m@(M1 x) = gToFuryC (T.pack (moduleName m)) (T.pack (datatypeName m)) x

instance GFromFuryC f => GFromFury (M1 D d f) where
  gFromFury v = M1 <$> gFromFuryC v

class GToFuryC f where
  gToFuryC :: Text -> Text -> f p -> VV.Value

class GFromFuryC f where
  gFromFuryC :: VV.Value -> Either String (f p)

instance (Constructor c, GToFuryFields f) => GToFuryC (M1 C c f) where
  gToFuryC ns _ m@(M1 x) =
    VV.StructVal ns (T.pack (conName m))
      (V.fromList (gToFuryFields x))

instance GFromFuryFields f => GFromFuryC (M1 C c f) where
  gFromFuryC (VV.StructVal _ _ fields) =
    let lkup name = lookupField name fields
    in M1 <$> gFromFuryFields lkup
  gFromFuryC _ = Left "GFromFury: expected NamedStruct for record type"

class GToFuryFields f where
  gToFuryFields :: f p -> [(Text, VV.Value)]

class GFromFuryFields f where
  gFromFuryFields :: (Text -> Maybe VV.Value) -> Either String (f p)

instance (GToFuryFields a, GToFuryFields b)
       => GToFuryFields (a :*: b) where
  gToFuryFields (a :*: b) = gToFuryFields a ++ gToFuryFields b

instance (GFromFuryFields a, GFromFuryFields b)
       => GFromFuryFields (a :*: b) where
  gFromFuryFields lkup =
    (:*:) <$> gFromFuryFields lkup <*> gFromFuryFields lkup

instance (Selector s, ToFury a) => GToFuryFields (M1 S s (K1 i a)) where
  gToFuryFields m@(M1 (K1 x)) = [(T.pack (selName m), toFury x)]

instance (Selector s, FromFury a) => GFromFuryFields (M1 S s (K1 i a)) where
  gFromFuryFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left ("GFromFury: missing field " ++ T.unpack name)
         Just v  -> M1 . K1 <$> fromFury v

lookupField :: Text -> Vector (Text, VV.Value) -> Maybe VV.Value
lookupField name fields = go 0
  where
    !len = V.length fields
    go !i
      | i >= len = Nothing
      | (k, v) <- fields V.! i, k == name = Just v
      | otherwise = go (i + 1)
