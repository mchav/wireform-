{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module MsgPack.Class
  ( ToMsgPack(..)
  , FromMsgPack(..)
  , encodeMsgPack
  , decodeMsgPack
  , GToMsgPack(..)
  , GFromMsgPack(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified MsgPack.Value as MV
import qualified MsgPack.Encode as ME
import qualified MsgPack.Decode as MD

class ToMsgPack a where
  toMsgPack :: a -> MV.Value
  default toMsgPack :: (Generic a, GToMsgPack (Rep a)) => a -> MV.Value
  toMsgPack = gToMsgPack . from

class FromMsgPack a where
  fromMsgPack :: MV.Value -> Either String a
  default fromMsgPack :: (Generic a, GFromMsgPack (Rep a)) => MV.Value -> Either String a
  fromMsgPack v = to <$> gFromMsgPack v

encodeMsgPack :: ToMsgPack a => a -> ByteString
encodeMsgPack = ME.encode . toMsgPack

decodeMsgPack :: FromMsgPack a => ByteString -> Either String a
decodeMsgPack bs = MD.decode bs >>= fromMsgPack

-- Instances for base types

instance ToMsgPack Bool where
  toMsgPack = MV.Bool

instance FromMsgPack Bool where
  fromMsgPack (MV.Bool b) = Right b
  fromMsgPack _ = Left "FromMsgPack Bool: expected Bool"

instance ToMsgPack Int where
  toMsgPack n
    | n >= 0    = MV.Word (fromIntegral n)
    | otherwise = MV.Int (fromIntegral n)

instance FromMsgPack Int where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int: expected Int or Word"

instance ToMsgPack Int8 where
  toMsgPack n = MV.Int (fromIntegral n)

instance FromMsgPack Int8 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int8: expected Int or Word"

instance ToMsgPack Int16 where
  toMsgPack n = MV.Int (fromIntegral n)

instance FromMsgPack Int16 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int16: expected Int or Word"

instance ToMsgPack Int32 where
  toMsgPack n = MV.Int (fromIntegral n)

instance FromMsgPack Int32 where
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int32: expected Int or Word"

instance ToMsgPack Int64 where
  toMsgPack n
    | n >= 0    = MV.Word (fromIntegral n)
    | otherwise = MV.Int n

instance FromMsgPack Int64 where
  fromMsgPack (MV.Int n) = Right n
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Int64: expected Int or Word"

instance ToMsgPack Word where
  toMsgPack n = MV.Word (fromIntegral n)

instance FromMsgPack Word where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word: expected Word or Int"

instance ToMsgPack Word8 where
  toMsgPack n = MV.Word (fromIntegral n)

instance FromMsgPack Word8 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word8: expected Word or Int"

instance ToMsgPack Word16 where
  toMsgPack n = MV.Word (fromIntegral n)

instance FromMsgPack Word16 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word16: expected Word or Int"

instance ToMsgPack Word32 where
  toMsgPack n = MV.Word (fromIntegral n)

instance FromMsgPack Word32 where
  fromMsgPack (MV.Word n) = Right (fromIntegral n)
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word32: expected Word or Int"

instance ToMsgPack Word64 where
  toMsgPack = MV.Word

instance FromMsgPack Word64 where
  fromMsgPack (MV.Word n) = Right n
  fromMsgPack (MV.Int n) = Right (fromIntegral n)
  fromMsgPack _ = Left "FromMsgPack Word64: expected Word or Int"

instance ToMsgPack Float where
  toMsgPack = MV.Float

instance FromMsgPack Float where
  fromMsgPack (MV.Float f) = Right f
  fromMsgPack (MV.Double d) = Right (realToFrac d)
  fromMsgPack _ = Left "FromMsgPack Float: expected Float or Double"

instance ToMsgPack Double where
  toMsgPack = MV.Double

instance FromMsgPack Double where
  fromMsgPack (MV.Double d) = Right d
  fromMsgPack (MV.Float f) = Right (realToFrac f)
  fromMsgPack _ = Left "FromMsgPack Double: expected Double or Float"

instance ToMsgPack Text where
  toMsgPack = MV.String

instance FromMsgPack Text where
  fromMsgPack (MV.String t) = Right t
  fromMsgPack _ = Left "FromMsgPack Text: expected String"

instance ToMsgPack ByteString where
  toMsgPack = MV.Binary

instance FromMsgPack ByteString where
  fromMsgPack (MV.Binary bs) = Right bs
  fromMsgPack _ = Left "FromMsgPack ByteString: expected Binary"

instance ToMsgPack () where
  toMsgPack () = MV.Nil

instance FromMsgPack () where
  fromMsgPack MV.Nil = Right ()
  fromMsgPack _ = Left "FromMsgPack (): expected Nil"

instance ToMsgPack a => ToMsgPack (Maybe a) where
  toMsgPack Nothing = MV.Nil
  toMsgPack (Just x) = toMsgPack x

instance FromMsgPack a => FromMsgPack (Maybe a) where
  fromMsgPack MV.Nil = Right Nothing
  fromMsgPack v = Just <$> fromMsgPack v

instance ToMsgPack a => ToMsgPack [a] where
  toMsgPack xs = MV.Array (V.fromList (map toMsgPack xs))

instance FromMsgPack a => FromMsgPack [a] where
  fromMsgPack (MV.Array vs) = traverse fromMsgPack (V.toList vs)
  fromMsgPack _ = Left "FromMsgPack [a]: expected Array"

instance ToMsgPack a => ToMsgPack (Vector a) where
  toMsgPack xs = MV.Array (V.map toMsgPack xs)

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

instance (Ord k, FromMsgPack k, FromMsgPack v) => FromMsgPack (Map k v) where
  fromMsgPack (MV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromMsgPack k <*> fromMsgPack v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromMsgPack _ = Left "FromMsgPack Map: expected Map"

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
