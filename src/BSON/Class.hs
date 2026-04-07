{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module BSON.Class
  ( ToBSON(..)
  , FromBSON(..)
  , encodeBSON
  , decodeBSON
  , GToBSON(..)
  , GFromBSON(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified BSON.Value as BV
import qualified BSON.Encode as BE
import qualified BSON.Decode as BD

class ToBSON a where
  toBSON :: a -> BV.Value
  default toBSON :: (Generic a, GToBSON (Rep a)) => a -> BV.Value
  toBSON = gToBSON . from

class FromBSON a where
  fromBSON :: BV.Value -> Either String a
  default fromBSON :: (Generic a, GFromBSON (Rep a)) => BV.Value -> Either String a
  fromBSON v = to <$> gFromBSON v

encodeBSON :: ToBSON a => a -> ByteString
encodeBSON = BE.encode . toBSON

decodeBSON :: FromBSON a => ByteString -> Either String a
decodeBSON bs = BD.decode bs >>= fromBSON

instance ToBSON Bool where
  toBSON = BV.Bool

instance FromBSON Bool where
  fromBSON (BV.Bool b) = Right b
  fromBSON _ = Left "FromBSON Bool: expected Bool"

instance ToBSON Int where
  toBSON n
    | n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32)
      = BV.Int32 (fromIntegral n)
    | otherwise = BV.Int64 (fromIntegral n)

instance FromBSON Int where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Double d) = Right (round d)
  fromBSON _ = Left "FromBSON Int: expected Int32, Int64, or Double"

instance ToBSON Int8 where
  toBSON n = BV.Int32 (fromIntegral n)

instance FromBSON Int8 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int8: expected Int32 or Int64"

instance ToBSON Int16 where
  toBSON n = BV.Int32 (fromIntegral n)

instance FromBSON Int16 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int16: expected Int32 or Int64"

instance ToBSON Int32 where
  toBSON = BV.Int32

instance FromBSON Int32 where
  fromBSON (BV.Int32 n) = Right n
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int32: expected Int32 or Int64"

instance ToBSON Int64 where
  toBSON = BV.Int64

instance FromBSON Int64 where
  fromBSON (BV.Int64 n) = Right n
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Int64: expected Int64 or Int32"

instance ToBSON Word where
  toBSON n = BV.Int64 (fromIntegral n)

instance FromBSON Word where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word: expected Int64 or Int32"

instance ToBSON Word8 where
  toBSON n = BV.Int32 (fromIntegral n)

instance FromBSON Word8 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word8: expected Int32 or Int64"

instance ToBSON Word16 where
  toBSON n = BV.Int32 (fromIntegral n)

instance FromBSON Word16 where
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word16: expected Int32 or Int64"

instance ToBSON Word32 where
  toBSON n = BV.Int64 (fromIntegral n)

instance FromBSON Word32 where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word32: expected Int64 or Int32"

instance ToBSON Word64 where
  toBSON n = BV.Int64 (fromIntegral n)

instance FromBSON Word64 where
  fromBSON (BV.Int64 n) = Right (fromIntegral n)
  fromBSON (BV.Int32 n) = Right (fromIntegral n)
  fromBSON _ = Left "FromBSON Word64: expected Int64 or Int32"

instance ToBSON Float where
  toBSON f = BV.Double (realToFrac f)

instance FromBSON Float where
  fromBSON (BV.Double d) = Right (realToFrac d)
  fromBSON _ = Left "FromBSON Float: expected Double"

instance ToBSON Double where
  toBSON = BV.Double

instance FromBSON Double where
  fromBSON (BV.Double d) = Right d
  fromBSON _ = Left "FromBSON Double: expected Double"

instance ToBSON Text where
  toBSON = BV.String

instance FromBSON Text where
  fromBSON (BV.String t) = Right t
  fromBSON _ = Left "FromBSON Text: expected String"

instance ToBSON ByteString where
  toBSON = BV.Binary

instance FromBSON ByteString where
  fromBSON (BV.Binary bs) = Right bs
  fromBSON _ = Left "FromBSON ByteString: expected Binary"

instance ToBSON () where
  toBSON () = BV.Null

instance FromBSON () where
  fromBSON BV.Null = Right ()
  fromBSON _ = Left "FromBSON (): expected Null"

instance ToBSON a => ToBSON (Maybe a) where
  toBSON Nothing = BV.Null
  toBSON (Just x) = toBSON x

instance FromBSON a => FromBSON (Maybe a) where
  fromBSON BV.Null = Right Nothing
  fromBSON v = Just <$> fromBSON v

instance ToBSON a => ToBSON [a] where
  toBSON xs = BV.Array (V.fromList (map toBSON xs))

instance FromBSON a => FromBSON [a] where
  fromBSON (BV.Array vs) = traverse fromBSON (V.toList vs)
  fromBSON _ = Left "FromBSON [a]: expected Array"

instance ToBSON a => ToBSON (Vector a) where
  toBSON xs = BV.Array (V.map toBSON xs)

instance FromBSON a => FromBSON (Vector a) where
  fromBSON (BV.Array vs) = V.mapM fromBSON vs
  fromBSON _ = Left "FromBSON Vector: expected Array"

instance (ToBSON a, ToBSON b) => ToBSON (a, b) where
  toBSON (a, b) = BV.Array (V.fromList [toBSON a, toBSON b])

instance (FromBSON a, FromBSON b) => FromBSON (a, b) where
  fromBSON (BV.Array vs)
    | V.length vs == 2 = (,) <$> fromBSON (vs V.! 0) <*> fromBSON (vs V.! 1)
  fromBSON _ = Left "FromBSON (a,b): expected Array of length 2"

instance (ToBSON k, ToBSON v) => ToBSON (Map k v) where
  toBSON m = BV.Array (V.fromList [BV.Array (V.fromList [toBSON k, toBSON v']) | (k, v') <- Map.toList m])

instance (Ord k, FromBSON k, FromBSON v) => FromBSON (Map k v) where
  fromBSON (BV.Array vs) = do
    pairs <- traverse decodePair (V.toList vs)
    Right (Map.fromList pairs)
    where
      decodePair (BV.Array kv)
        | V.length kv == 2 = (,) <$> fromBSON (kv V.! 0) <*> fromBSON (kv V.! 1)
      decodePair _ = Left "FromBSON Map: expected Array of pairs"
  fromBSON _ = Left "FromBSON Map: expected Array"

instance ToBSON BV.Value where
  toBSON = id

instance FromBSON BV.Value where
  fromBSON = Right

-- GHC.Generics support

class GToBSON f where
  gToBSON :: f p -> BV.Value

class GFromBSON f where
  gFromBSON :: BV.Value -> Either String (f p)

instance GToBSON f => GToBSON (M1 D c f) where
  gToBSON (M1 x) = gToBSON x

instance GFromBSON f => GFromBSON (M1 D c f) where
  gFromBSON v = M1 <$> gFromBSON v

instance (Constructor c, GToBSONFields f) => GToBSON (M1 C c f) where
  gToBSON (M1 x) =
    let fields = gToBSONFields x
    in BV.Document (V.fromList fields)

instance (Constructor c, GFromBSONFields f) => GFromBSON (M1 C c f) where
  gFromBSON (BV.Document kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromBSONFields lkup
  gFromBSON _ = Left "GFromBSON: expected Document for record type"

lookupField :: Text -> Vector (Text, BV.Value) -> Maybe BV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToBSONFields f where
  gToBSONFields :: f p -> [(Text, BV.Value)]

class GFromBSONFields f where
  gFromBSONFields :: (Text -> Maybe BV.Value) -> Either String (f p)

instance (GToBSONFields a, GToBSONFields b) => GToBSONFields (a :*: b) where
  gToBSONFields (a :*: b) = gToBSONFields a ++ gToBSONFields b

instance (GFromBSONFields a, GFromBSONFields b) => GFromBSONFields (a :*: b) where
  gFromBSONFields lkup = (:*:) <$> gFromBSONFields lkup <*> gFromBSONFields lkup

instance (Selector s, ToBSON a) => GToBSONFields (M1 S s (K1 i a)) where
  gToBSONFields m@(M1 (K1 x)) = [(T.pack (selName m), toBSON x)]

instance (Selector s, FromBSON a) => GFromBSONFields (M1 S s (K1 i a)) where
  gFromBSONFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromBSON: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromBSON v
