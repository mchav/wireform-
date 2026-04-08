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
  , decodeBencode
  , GToBencode(..)
  , GFromBencode(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified Bencode.Value as B
import qualified Bencode.Encode as BE
import qualified Bencode.Decode as BD

class ToBencode a where
  toBencode :: a -> B.Value
  default toBencode :: (Generic a, GToBencode (Rep a)) => a -> B.Value
  toBencode = gToBencode . from

class FromBencode a where
  fromBencode :: B.Value -> Either String a
  default fromBencode :: (Generic a, GFromBencode (Rep a)) => B.Value -> Either String a
  fromBencode v = to <$> gFromBencode v

encodeBencode :: ToBencode a => a -> ByteString
encodeBencode = BE.encode . toBencode

decodeBencode :: FromBencode a => ByteString -> Either String a
decodeBencode bs = BD.decode bs >>= fromBencode

instance ToBencode ByteString where
  toBencode = B.BString

instance FromBencode ByteString where
  fromBencode (B.BString bs) = Right bs
  fromBencode _ = Left "FromBencode ByteString: expected BString"

instance ToBencode Text where
  toBencode = B.BString . TE.encodeUtf8

instance FromBencode Text where
  fromBencode (B.BString bs) = case TE.decodeUtf8' bs of
    Left _ -> Left "FromBencode Text: invalid UTF-8"
    Right t -> Right t
  fromBencode _ = Left "FromBencode Text: expected BString"

instance ToBencode Integer where
  toBencode = B.BInteger

instance FromBencode Integer where
  fromBencode (B.BInteger n) = Right n
  fromBencode _ = Left "FromBencode Integer: expected BInteger"

instance ToBencode Int where
  toBencode = B.BInteger . fromIntegral

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

instance FromBencode Bool where
  fromBencode (B.BInteger 0) = Right False
  fromBencode (B.BInteger _) = Right True
  fromBencode _ = Left "FromBencode Bool: expected BInteger"

instance ToBencode a => ToBencode [a] where
  toBencode xs = B.BList (V.fromList (map toBencode xs))

instance FromBencode a => FromBencode [a] where
  fromBencode (B.BList vs) = traverse fromBencode (V.toList vs)
  fromBencode _ = Left "FromBencode [a]: expected BList"

instance ToBencode a => ToBencode (Vector a) where
  toBencode xs = B.BList (V.map toBencode xs)

instance FromBencode a => FromBencode (Vector a) where
  fromBencode (B.BList vs) = V.mapM fromBencode vs
  fromBencode _ = Left "FromBencode Vector: expected BList"

instance ToBencode a => ToBencode (Maybe a) where
  toBencode Nothing = B.BList V.empty
  toBencode (Just x) = toBencode x

instance FromBencode a => FromBencode (Maybe a) where
  fromBencode (B.BList vs) | V.null vs = Right Nothing
  fromBencode v = Just <$> fromBencode v

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
