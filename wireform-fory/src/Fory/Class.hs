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
-- The two classes are 'ToFory' and 'FromFory'. The default
-- generic deriver renders a record as a 'VV.StructVal' whose
-- namespace is the module name of the type and whose type name is
-- the simple constructor name; field names are passed through
-- unchanged (use the 'Fory.Derive' annotation deriver if you need
-- 'rename' / 'renameStyle' / @snake_case@ handling).
module Fory.Class
  ( ToFory (..)
  , FromFory (..)
  , encodeFory
  , decodeFory

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
  , GToFory (..)
  , GFromFory (..)
  , GToForyFields (..)
  , GFromForyFields (..)
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
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified Fory.Decode as D
import qualified Fory.Encode as E
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Public typeclasses
-- ---------------------------------------------------------------------------

class ToFory a where
  toFory :: a -> VV.Value
  default toFory :: (Generic a, GToFory (Rep a)) => a -> VV.Value
  toFory = gToFory . from

class FromFory a where
  fromFory :: VV.Value -> Either String a
  default fromFory :: (Generic a, GFromFory (Rep a)) => VV.Value -> Either String a
  fromFory v = to <$> gFromFory v

-- | Encode any 'ToFory' to its fory wire format.
encodeFory :: ToFory a => a -> ByteString
encodeFory = E.encode . toFory

-- | Decode any 'FromFory' from a fory-encoded byte string.
decodeFory :: FromFory a => ByteString -> Either String a
decodeFory bs = D.decode bs >>= fromFory

-- ---------------------------------------------------------------------------
-- Reference-tracked sharing
-- ---------------------------------------------------------------------------

-- | Wrap a value to opt into Fory\'s reference-tracking. The
-- 'sharingKey' field is a user-supplied 'Int' that tags logically
-- identical objects: every occurrence of the same @sharingKey@
-- under a single 'encodeFory' call after the first encodes as a
-- @REF_FLAG@ back-reference rather than a full payload.
--
-- @
-- let x = Shared 1 (StringVal \"hello\")
-- encodeFory (ListVal (V.fromList [toFory x, toFory x, toFory x]))
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

instance ToFory a => ToFory (Shared a) where
  toFory (Shared k a) = VV.RefVal k (toFory a)

instance FromFory a => FromFory (Shared a) where
  fromFory (VV.RefVal k inner) = Shared k <$> fromFory inner
  fromFory v = Shared 0 <$> fromFory v
  -- Bare values decode as @Shared 0 v@ for permissiveness — the
  -- user explicitly opted into ref-tracking on the encode side
  -- but the wire doesn\'t guarantee it.

-- ---------------------------------------------------------------------------
-- Primitive 1-D array newtypes
-- ---------------------------------------------------------------------------

-- $primitiveArrays
--
-- The default 'ToFory' instance for @'Vector' a@ produces a
-- 'VV.ListVal'. To opt into Fory\'s dense one-byte-per-element
-- (or fixed-width little-endian) array encoding, wrap in one of
-- the following newtypes:
--
-- @
-- toFory (Int32Array (V.fromList [1,2,3]))  -- 'VV.Int32ArrayVal'
-- toFory [1,2,3 :: Int32]                    -- 'VV.ListVal' of int32
-- @

-- The primitive-array newtypes wrap 'Data.Vector.Storable'
-- vectors, so the wire-format payload is just a reinterpret
-- of the vector's underlying memory (see 'Fory.Bulk').
-- 'BoolArray' uses 'Word8' (0 / 1) because 'Bool' has no
-- single-byte 'Storable' instance — convert via 'fromIntegral
-- . fromEnum' and 'toEnum . fromIntegral' if you need to
-- bridge to '[Bool]'.
newtype BoolArray = BoolArray { unBoolArray :: VS.Vector Word8 }
  deriving stock (Eq, Show)

newtype Int8Array = Int8Array { unInt8Array :: VS.Vector Int8 }
  deriving stock (Eq, Show)

newtype Int16Array = Int16Array { unInt16Array :: VS.Vector Int16 }
  deriving stock (Eq, Show)

newtype Int32Array = Int32Array { unInt32Array :: VS.Vector Int32 }
  deriving stock (Eq, Show)

newtype Int64Array = Int64Array { unInt64Array :: VS.Vector Int64 }
  deriving stock (Eq, Show)

newtype Uint8Array = Uint8Array { unUint8Array :: VS.Vector Word8 }
  deriving stock (Eq, Show)

newtype Uint16Array = Uint16Array { unUint16Array :: VS.Vector Word16 }
  deriving stock (Eq, Show)

newtype Uint32Array = Uint32Array { unUint32Array :: VS.Vector Word32 }
  deriving stock (Eq, Show)

newtype Uint64Array = Uint64Array { unUint64Array :: VS.Vector Word64 }
  deriving stock (Eq, Show)

newtype Float32Array = Float32Array { unFloat32Array :: VS.Vector Float }
  deriving stock (Eq, Show)

newtype Float64Array = Float64Array { unFloat64Array :: VS.Vector Double }
  deriving stock (Eq, Show)

instance ToFory BoolArray where
  toFory = VV.BoolArrayVal . unBoolArray
instance FromFory BoolArray where
  fromFory (VV.BoolArrayVal v) = Right (BoolArray v)
  fromFory _ = Left "FromFory BoolArray: expected BoolArrayVal"

instance ToFory Int8Array where
  toFory = VV.Int8ArrayVal . unInt8Array
instance FromFory Int8Array where
  fromFory (VV.Int8ArrayVal v) = Right (Int8Array v)
  fromFory _ = Left "FromFory Int8Array: expected Int8ArrayVal"

instance ToFory Int16Array where
  toFory = VV.Int16ArrayVal . unInt16Array
instance FromFory Int16Array where
  fromFory (VV.Int16ArrayVal v) = Right (Int16Array v)
  fromFory _ = Left "FromFory Int16Array: expected Int16ArrayVal"

instance ToFory Int32Array where
  toFory = VV.Int32ArrayVal . unInt32Array
instance FromFory Int32Array where
  fromFory (VV.Int32ArrayVal v) = Right (Int32Array v)
  fromFory _ = Left "FromFory Int32Array: expected Int32ArrayVal"

instance ToFory Int64Array where
  toFory = VV.Int64ArrayVal . unInt64Array
instance FromFory Int64Array where
  fromFory (VV.Int64ArrayVal v) = Right (Int64Array v)
  fromFory _ = Left "FromFory Int64Array: expected Int64ArrayVal"

instance ToFory Uint8Array where
  toFory = VV.Uint8ArrayVal . unUint8Array
instance FromFory Uint8Array where
  fromFory (VV.Uint8ArrayVal v) = Right (Uint8Array v)
  fromFory _ = Left "FromFory Uint8Array: expected Uint8ArrayVal"

instance ToFory Uint16Array where
  toFory = VV.Uint16ArrayVal . unUint16Array
instance FromFory Uint16Array where
  fromFory (VV.Uint16ArrayVal v) = Right (Uint16Array v)
  fromFory _ = Left "FromFory Uint16Array: expected Uint16ArrayVal"

instance ToFory Uint32Array where
  toFory = VV.Uint32ArrayVal . unUint32Array
instance FromFory Uint32Array where
  fromFory (VV.Uint32ArrayVal v) = Right (Uint32Array v)
  fromFory _ = Left "FromFory Uint32Array: expected Uint32ArrayVal"

instance ToFory Uint64Array where
  toFory = VV.Uint64ArrayVal . unUint64Array
instance FromFory Uint64Array where
  fromFory (VV.Uint64ArrayVal v) = Right (Uint64Array v)
  fromFory _ = Left "FromFory Uint64Array: expected Uint64ArrayVal"

instance ToFory Float32Array where
  toFory = VV.Float32ArrayVal . unFloat32Array
instance FromFory Float32Array where
  fromFory (VV.Float32ArrayVal v) = Right (Float32Array v)
  fromFory _ = Left "FromFory Float32Array: expected Float32ArrayVal"

instance ToFory Float64Array where
  toFory = VV.Float64ArrayVal . unFloat64Array
instance FromFory Float64Array where
  fromFory (VV.Float64ArrayVal v) = Right (Float64Array v)
  fromFory _ = Left "FromFory Float64Array: expected Float64ArrayVal"

-- ---------------------------------------------------------------------------
-- Base-type instances
-- ---------------------------------------------------------------------------

instance ToFory VV.Value where
  toFory = id

instance FromFory VV.Value where
  fromFory = Right

instance ToFory Bool where
  toFory = VV.BoolVal

instance FromFory Bool where
  fromFory (VV.BoolVal b) = Right b
  fromFory _ = Left "FromFory Bool: expected Bool"

instance ToFory Int8 where
  toFory = VV.Int8Val

instance FromFory Int8 where
  fromFory (VV.Int8Val n) = Right n
  fromFory _ = Left "FromFory Int8: expected Int8"

instance ToFory Int16 where
  toFory = VV.Int16Val

instance FromFory Int16 where
  fromFory (VV.Int16Val n) = Right n
  fromFory _ = Left "FromFory Int16: expected Int16"

instance ToFory Int32 where
  toFory = VV.Int32Val

instance FromFory Int32 where
  fromFory (VV.Int32Val n) = Right n
  fromFory _ = Left "FromFory Int32: expected Int32"

instance ToFory Int64 where
  toFory = VV.Int64Val

instance FromFory Int64 where
  fromFory (VV.Int64Val n) = Right n
  fromFory _ = Left "FromFory Int64: expected Int64"

-- | The default encoding for Haskell @Int@ is xlang @VARINT64@,
-- matching what @pyfory@ does for Python @int@.
instance ToFory Int where
  toFory = VV.VarInt64Val . fromIntegral

instance FromFory Int where
  fromFory (VV.VarInt64Val n) = Right (fromIntegral n)
  fromFory (VV.VarInt32Val n) = Right (fromIntegral n)
  fromFory (VV.Int64Val n) = Right (fromIntegral n)
  fromFory (VV.Int32Val n) = Right (fromIntegral n)
  fromFory (VV.Int16Val n) = Right (fromIntegral n)
  fromFory (VV.Int8Val  n) = Right (fromIntegral n)
  fromFory _ = Left "FromFory Int: expected an integer"

instance ToFory Word8 where
  toFory = VV.Uint8Val

instance FromFory Word8 where
  fromFory (VV.Uint8Val n) = Right n
  fromFory _ = Left "FromFory Word8: expected UInt8"

instance ToFory Word16 where
  toFory = VV.Uint16Val

instance FromFory Word16 where
  fromFory (VV.Uint16Val n) = Right n
  fromFory _ = Left "FromFory Word16: expected UInt16"

instance ToFory Word32 where
  toFory = VV.Uint32Val

instance FromFory Word32 where
  fromFory (VV.Uint32Val n) = Right n
  fromFory _ = Left "FromFory Word32: expected UInt32"

instance ToFory Word64 where
  toFory = VV.Uint64Val

instance FromFory Word64 where
  fromFory (VV.Uint64Val n) = Right n
  fromFory _ = Left "FromFory Word64: expected UInt64"

instance ToFory Word where
  toFory = VV.Uint64Val . fromIntegral

instance FromFory Word where
  fromFory (VV.Uint64Val n) = Right (fromIntegral n)
  fromFory (VV.Uint32Val n) = Right (fromIntegral n)
  fromFory (VV.Uint16Val n) = Right (fromIntegral n)
  fromFory (VV.Uint8Val  n) = Right (fromIntegral n)
  fromFory _ = Left "FromFory Word: expected an unsigned integer"

instance ToFory Float where
  toFory = VV.Float32Val

instance FromFory Float where
  fromFory (VV.Float32Val f) = Right f
  fromFory (VV.Float64Val d) = Right (realToFrac d)
  fromFory _ = Left "FromFory Float: expected Float32 or Float64"

instance ToFory Double where
  toFory = VV.Float64Val

instance FromFory Double where
  fromFory (VV.Float64Val d) = Right d
  fromFory (VV.Float32Val f) = Right (realToFrac f)
  fromFory _ = Left "FromFory Double: expected Float64 or Float32"

instance ToFory Text where
  toFory = VV.StringVal

instance FromFory Text where
  fromFory (VV.StringVal t) = Right t
  fromFory _ = Left "FromFory Text: expected String"

instance ToFory TL.Text where
  toFory = VV.StringVal . TL.toStrict

instance FromFory TL.Text where
  fromFory v = TL.fromStrict <$> fromFory v

instance ToFory Char where
  toFory c = VV.StringVal (T.singleton c)

instance FromFory Char where
  fromFory (VV.StringVal t) | T.length t == 1 = Right (T.head t)
  fromFory _ = Left "FromFory Char: expected single-character String"

instance ToFory ByteString where
  toFory = VV.BinaryVal

instance FromFory ByteString where
  fromFory (VV.BinaryVal bs) = Right bs
  fromFory _ = Left "FromFory ByteString: expected Binary"

instance ToFory BSL.ByteString where
  toFory = VV.BinaryVal . BSL.toStrict

instance FromFory BSL.ByteString where
  fromFory v = BSL.fromStrict <$> fromFory v

instance ToFory () where
  toFory () = VV.NoneVal

instance FromFory () where
  fromFory VV.NoneVal = Right ()
  fromFory _ = Left "FromFory (): expected None"

-- 'Maybe' lifts 'Nothing' to @None@ and 'Just' through to its
-- payload directly. Round-tripping a @Maybe (Maybe a)@ collapses
-- the two layers of 'Nothing' so this instance is /not/ injective
-- on nested optionals, mirroring what Fory's own xlang treatment
-- does in languages with implicit nullable types.
instance ToFory a => ToFory (Maybe a) where
  toFory Nothing  = VV.NoneVal
  toFory (Just x) = toFory x

instance FromFory a => FromFory (Maybe a) where
  fromFory VV.NoneVal = Right Nothing
  fromFory v          = Just <$> fromFory v

instance ToFory a => ToFory [a] where
  toFory xs = VV.ListVal (V.fromList (map toFory xs))

instance FromFory a => FromFory [a] where
  fromFory (VV.ListVal vs) = traverse fromFory (V.toList vs)
  fromFory _ = Left "FromFory [a]: expected List"

instance ToFory a => ToFory (Vector a) where
  toFory = VV.ListVal . V.map toFory

instance FromFory a => FromFory (Vector a) where
  fromFory (VV.ListVal vs) = V.mapM fromFory vs
  fromFory _ = Left "FromFory Vector: expected List"

instance ToFory a => ToFory (NonEmpty a) where
  toFory = toFory . NE.toList

instance FromFory a => FromFory (NonEmpty a) where
  fromFory v = do
    xs <- fromFory v
    case xs of
      []     -> Left "FromFory NonEmpty: empty list"
      (y:ys) -> Right (y :| ys)

instance ToFory a => ToFory (Seq a) where
  toFory s = VV.ListVal (V.fromList (fmap toFory (foldr (:) [] s)))

instance FromFory a => FromFory (Seq a) where
  fromFory v = Seq.fromList <$> fromFory v

instance ToFory a => ToFory (Set a) where
  toFory = VV.SetVal . V.fromList . fmap toFory . Set.toList

instance (Ord a, FromFory a) => FromFory (Set a) where
  fromFory (VV.SetVal vs) = Set.fromList <$> traverse fromFory (V.toList vs)
  fromFory (VV.ListVal vs) = Set.fromList <$> traverse fromFory (V.toList vs)
  fromFory _ = Left "FromFory Set: expected Set or List"

instance (ToFory k, ToFory v) => ToFory (Map k v) where
  toFory m =
    VV.MapVal
      (V.fromList
        [ (toFory k, toFory vv)
        | (k, vv) <- Map.toAscList m ])

instance (Ord k, FromFory k, FromFory v) => FromFory (Map k v) where
  fromFory (VV.MapVal kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (Map.fromList pairs)
    where
      decodePair (kv, vv) = (,) <$> fromFory kv <*> fromFory vv
  fromFory _ = Left "FromFory Map: expected Map"

instance ToFory v => ToFory (IntMap v) where
  toFory m =
    VV.MapVal
      (V.fromList
        [ (VV.Int64Val (fromIntegral k), toFory vv)
        | (k, vv) <- IntMap.toAscList m ])

instance FromFory v => FromFory (IntMap v) where
  fromFory (VV.MapVal kvs) = do
    pairs <- traverse decodePair (V.toList kvs)
    Right (IntMap.fromList pairs)
    where
      decodePair (kv, vv) = do
        k' <- fromFory kv
        v' <- fromFory vv
        Right (k', v')
  fromFory _ = Left "FromFory IntMap: expected Map"

instance ToFory IntSet where
  toFory =
    VV.SetVal
      . V.fromList
      . fmap (VV.Int64Val . fromIntegral)
      . IntSet.toAscList

instance FromFory IntSet where
  fromFory v = IntSet.fromList <$> fromFory v

instance (ToFory a, ToFory b) => ToFory (a, b) where
  toFory (a, b) = VV.ListVal (V.fromList [toFory a, toFory b])

instance (FromFory a, FromFory b) => FromFory (a, b) where
  fromFory (VV.ListVal vs)
    | V.length vs == 2 = (,) <$> fromFory (vs V.! 0) <*> fromFory (vs V.! 1)
  fromFory _ = Left "FromFory (a,b): expected List of length 2"

instance (ToFory a, ToFory b, ToFory c) => ToFory (a, b, c) where
  toFory (a, b, c) =
    VV.ListVal (V.fromList [toFory a, toFory b, toFory c])

instance (FromFory a, FromFory b, FromFory c) => FromFory (a, b, c) where
  fromFory (VV.ListVal vs)
    | V.length vs == 3 =
        (,,) <$> fromFory (vs V.! 0)
             <*> fromFory (vs V.! 1)
             <*> fromFory (vs V.! 2)
  fromFory _ = Left "FromFory (a,b,c): expected List of length 3"

instance ToFory a => ToFory (Identity a) where
  toFory (Identity x) = toFory x

instance FromFory a => FromFory (Identity a) where
  fromFory v = Identity <$> fromFory v

instance ToFory a => ToFory (Const a b) where
  toFory (Const x) = toFory x

instance FromFory a => FromFory (Const a b) where
  fromFory v = Const <$> fromFory v

-- ---------------------------------------------------------------------------
-- Generic deriver
-- ---------------------------------------------------------------------------

class GToFory f where
  gToFory :: f p -> VV.Value

class GFromFory f where
  gFromFory :: VV.Value -> Either String (f p)

instance (Datatype d, GToForyC f) => GToFory (M1 D d f) where
  gToFory m@(M1 x) = gToForyC (T.pack (moduleName m)) (T.pack (datatypeName m)) x

instance GFromForyC f => GFromFory (M1 D d f) where
  gFromFory v = M1 <$> gFromForyC v

class GToForyC f where
  gToForyC :: Text -> Text -> f p -> VV.Value

class GFromForyC f where
  gFromForyC :: VV.Value -> Either String (f p)

instance (Constructor c, GToForyFields f) => GToForyC (M1 C c f) where
  gToForyC ns _ m@(M1 x) =
    VV.StructVal ns (T.pack (conName m))
      (V.fromList (gToForyFields x))

instance GFromForyFields f => GFromForyC (M1 C c f) where
  gFromForyC (VV.StructVal _ _ fields) =
    let lkup name = lookupField name fields
    in M1 <$> gFromForyFields lkup
  gFromForyC _ = Left "GFromFory: expected NamedStruct for record type"

class GToForyFields f where
  gToForyFields :: f p -> [(Text, VV.Value)]

class GFromForyFields f where
  gFromForyFields :: (Text -> Maybe VV.Value) -> Either String (f p)

instance (GToForyFields a, GToForyFields b)
       => GToForyFields (a :*: b) where
  gToForyFields (a :*: b) = gToForyFields a ++ gToForyFields b

instance (GFromForyFields a, GFromForyFields b)
       => GFromForyFields (a :*: b) where
  gFromForyFields lkup =
    (:*:) <$> gFromForyFields lkup <*> gFromForyFields lkup

instance (Selector s, ToFory a) => GToForyFields (M1 S s (K1 i a)) where
  gToForyFields m@(M1 (K1 x)) = [(T.pack (selName m), toFory x)]

instance (Selector s, FromFory a) => GFromForyFields (M1 S s (K1 i a)) where
  gFromForyFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left ("GFromFory: missing field " ++ T.unpack name)
         Just v  -> M1 . K1 <$> fromFory v

lookupField :: Text -> Vector (Text, VV.Value) -> Maybe VV.Value
lookupField name fields = go 0
  where
    !len = V.length fields
    go !i
      | i >= len = Nothing
      | (k, v) <- fields V.! i, k == name = Just v
      | otherwise = go (i + 1)
