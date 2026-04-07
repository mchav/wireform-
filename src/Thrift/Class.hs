{-# LANGUAGE FlexibleInstances #-}
-- | Typeclass-based Thrift serialization.
--
-- Provides 'ToThrift' and 'FromThrift' typeclasses for converting Haskell
-- values to\/from 'Thrift.Value.Value'. Convenience functions serialize
-- directly to\/from binary or compact protocol wire format.
--
-- @
-- import Thrift.Class
-- import qualified Data.Vector as V
-- import Data.Int (Int16)
--
-- let bytes = encodeThriftBinary myThriftValue
-- let Right val = decodeThriftBinary bytes
-- @
module Thrift.Class
  ( ToThrift(..)
  , FromThrift(..)
  , encodeThriftBinary
  , decodeThriftBinary
  , encodeThriftCompact
  , decodeThriftCompact
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType(..))
import qualified Thrift.Encode as TE
import qualified Thrift.Decode as TD

class ToThrift a where
  toThrift :: a -> TV.Value

class FromThrift a where
  fromThrift :: TV.Value -> Either String a

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
