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
  , decodeThriftBinary
  , encodeThriftCompact
  , decodeThriftCompact
  , GToThrift(..)
  , GFromThrift(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType(..))
import qualified Thrift.Encode as TE
import qualified Thrift.Decode as TD

class ToThrift a where
  toThrift :: a -> TV.Value
  default toThrift :: (Generic a, GToThrift (Rep a)) => a -> TV.Value
  toThrift = gToThrift . from

class FromThrift a where
  fromThrift :: TV.Value -> Either String a
  default fromThrift :: (Generic a, GFromThrift (Rep a)) => TV.Value -> Either String a
  fromThrift v = to <$> gFromThrift v

encodeThriftBinary :: ToThrift a => a -> ByteString
encodeThriftBinary = TE.encodeBinary . toThrift

decodeThriftBinary :: FromThrift a => ByteString -> Either String a
decodeThriftBinary bs = TD.decodeBinary bs >>= fromThrift

encodeThriftCompact :: ToThrift a => a -> ByteString
encodeThriftCompact = TE.encodeCompact . toThrift

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
