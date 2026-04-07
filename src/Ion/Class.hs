{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ion.Class
  ( ToIon(..)
  , FromIon(..)
  , encodeIon
  , decodeIon
  , GToIon(..)
  , GFromIon(..)
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

import qualified Ion.Value as IV
import qualified Ion.Encode as IE
import qualified Ion.Decode as ID

class ToIon a where
  toIon :: a -> IV.Value
  default toIon :: (Generic a, GToIon (Rep a)) => a -> IV.Value
  toIon = gToIon . from

class FromIon a where
  fromIon :: IV.Value -> Either String a
  default fromIon :: (Generic a, GFromIon (Rep a)) => IV.Value -> Either String a
  fromIon v = to <$> gFromIon v

encodeIon :: ToIon a => a -> ByteString
encodeIon = IE.encode . toIon

decodeIon :: FromIon a => ByteString -> Either String a
decodeIon bs = ID.decode bs >>= fromIon

instance ToIon Bool where
  toIon = IV.Bool

instance FromIon Bool where
  fromIon (IV.Bool b) = Right b
  fromIon _ = Left "FromIon Bool: expected Bool"

instance ToIon Int where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Int where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int: expected Int"

instance ToIon Int8 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Int8 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int8: expected Int"

instance ToIon Int16 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Int16 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int16: expected Int"

instance ToIon Int32 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Int32 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Int32: expected Int"

instance ToIon Int64 where
  toIon = IV.Int

instance FromIon Int64 where
  fromIon (IV.Int n) = Right n
  fromIon _ = Left "FromIon Int64: expected Int"

instance ToIon Word where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Word where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word: expected Int"

instance ToIon Word8 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Word8 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word8: expected Int"

instance ToIon Word16 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Word16 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word16: expected Int"

instance ToIon Word32 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Word32 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word32: expected Int"

instance ToIon Word64 where
  toIon n = IV.Int (fromIntegral n)

instance FromIon Word64 where
  fromIon (IV.Int n) = Right (fromIntegral n)
  fromIon _ = Left "FromIon Word64: expected Int"

instance ToIon Float where
  toIon f = IV.Float (realToFrac f)

instance FromIon Float where
  fromIon (IV.Float d) = Right (realToFrac d)
  fromIon _ = Left "FromIon Float: expected Float"

instance ToIon Double where
  toIon = IV.Float

instance FromIon Double where
  fromIon (IV.Float d) = Right d
  fromIon _ = Left "FromIon Double: expected Float"

instance ToIon Text where
  toIon = IV.String

instance FromIon Text where
  fromIon (IV.String t) = Right t
  fromIon (IV.Symbol t) = Right t
  fromIon _ = Left "FromIon Text: expected String or Symbol"

instance ToIon ByteString where
  toIon = IV.Blob

instance FromIon ByteString where
  fromIon (IV.Blob bs) = Right bs
  fromIon (IV.Clob bs) = Right bs
  fromIon _ = Left "FromIon ByteString: expected Blob or Clob"

instance ToIon () where
  toIon () = IV.Null

instance FromIon () where
  fromIon IV.Null = Right ()
  fromIon _ = Left "FromIon (): expected Null"

instance ToIon a => ToIon (Maybe a) where
  toIon Nothing = IV.Null
  toIon (Just x) = toIon x

instance FromIon a => FromIon (Maybe a) where
  fromIon IV.Null = Right Nothing
  fromIon v = Just <$> fromIon v

instance ToIon a => ToIon [a] where
  toIon xs = IV.List (V.fromList (map toIon xs))

instance FromIon a => FromIon [a] where
  fromIon (IV.List vs) = traverse fromIon (V.toList vs)
  fromIon _ = Left "FromIon [a]: expected List"

instance ToIon a => ToIon (Vector a) where
  toIon xs = IV.List (V.map toIon xs)

instance FromIon a => FromIon (Vector a) where
  fromIon (IV.List vs) = V.mapM fromIon vs
  fromIon _ = Left "FromIon Vector: expected List"

instance (ToIon a, ToIon b) => ToIon (a, b) where
  toIon (a, b) = IV.List (V.fromList [toIon a, toIon b])

instance (FromIon a, FromIon b) => FromIon (a, b) where
  fromIon (IV.List vs)
    | V.length vs == 2 = (,) <$> fromIon (vs V.! 0) <*> fromIon (vs V.! 1)
  fromIon _ = Left "FromIon (a,b): expected List of length 2"

instance (ToIon k, ToIon v) => ToIon (Map k v) where
  toIon m = IV.List (V.fromList [IV.List (V.fromList [toIon k, toIon v']) | (k, v') <- Map.toList m])

instance (Ord k, FromIon k, FromIon v) => FromIon (Map k v) where
  fromIon (IV.List vs) = do
    pairs <- traverse decodePair (V.toList vs)
    Right (Map.fromList pairs)
    where
      decodePair (IV.List kv)
        | V.length kv == 2 = (,) <$> fromIon (kv V.! 0) <*> fromIon (kv V.! 1)
      decodePair _ = Left "FromIon Map: expected List of pairs"
  fromIon (IV.Struct kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromIon (IV.String k) <*> fromIon v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromIon _ = Left "FromIon Map: expected List or Struct"

instance ToIon IV.Value where
  toIon = id

instance FromIon IV.Value where
  fromIon = Right

-- GHC.Generics support

class GToIon f where
  gToIon :: f p -> IV.Value

class GFromIon f where
  gFromIon :: IV.Value -> Either String (f p)

instance GToIon f => GToIon (M1 D c f) where
  gToIon (M1 x) = gToIon x

instance GFromIon f => GFromIon (M1 D c f) where
  gFromIon v = M1 <$> gFromIon v

instance (Constructor c, GToIonFields f) => GToIon (M1 C c f) where
  gToIon (M1 x) =
    let fields = gToIonFields x
    in IV.Struct (V.fromList fields)

instance (Constructor c, GFromIonFields f) => GFromIon (M1 C c f) where
  gFromIon (IV.Struct kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromIonFields lkup
  gFromIon _ = Left "GFromIon: expected Struct for record type"

lookupField :: Text -> Vector (Text, IV.Value) -> Maybe IV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToIonFields f where
  gToIonFields :: f p -> [(Text, IV.Value)]

class GFromIonFields f where
  gFromIonFields :: (Text -> Maybe IV.Value) -> Either String (f p)

instance (GToIonFields a, GToIonFields b) => GToIonFields (a :*: b) where
  gToIonFields (a :*: b) = gToIonFields a ++ gToIonFields b

instance (GFromIonFields a, GFromIonFields b) => GFromIonFields (a :*: b) where
  gFromIonFields lkup = (:*:) <$> gFromIonFields lkup <*> gFromIonFields lkup

instance (Selector s, ToIon a) => GToIonFields (M1 S s (K1 i a)) where
  gToIonFields m@(M1 (K1 x)) = [(T.pack (selName m), toIon x)]

instance (Selector s, FromIon a) => GFromIonFields (M1 S s (K1 i a)) where
  gFromIonFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromIon: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromIon v
