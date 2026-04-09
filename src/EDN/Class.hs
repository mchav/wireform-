{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based EDN serialization with GHC Generics support.
--
-- Provides 'ToEDN' and 'FromEDN' typeclasses for converting Haskell
-- values to\/from EDN. Records are encoded as EDN maps with keyword keys.
-- Derive instances automatically via @DeriveGeneric@.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import EDN.Class
--
-- data Point = Point { x :: Double, y :: Double } deriving (Generic)
-- instance ToEDN Point
-- instance FromEDN Point
--
-- let bs = encodeEDN (Point 1.0 2.0)
-- let Right pt = decodeEDN bs :: Either String Point
-- @
module EDN.Class
  ( ToEDN(..)
  , FromEDN(..)
  , encodeEDN
  , decodeEDN
  , GToEDN(..)
  , GFromEDN(..)
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

import qualified EDN.Value as EV
import qualified EDN.Encode as EE
import qualified EDN.Decode as ED

class ToEDN a where
  toEDN :: a -> EV.Value
  default toEDN :: (Generic a, GToEDN (Rep a)) => a -> EV.Value
  toEDN = gToEDN . from

class FromEDN a where
  fromEDN :: EV.Value -> Either String a
  default fromEDN :: (Generic a, GFromEDN (Rep a)) => EV.Value -> Either String a
  fromEDN v = to <$> gFromEDN v

encodeEDN :: ToEDN a => a -> Text
encodeEDN = EE.encode . toEDN

decodeEDN :: FromEDN a => Text -> Either String a
decodeEDN t = ED.decode t >>= fromEDN

instance ToEDN Bool where
  toEDN = EV.Bool

instance FromEDN Bool where
  fromEDN (EV.Bool b) = Right b
  fromEDN _ = Left "FromEDN Bool: expected Bool"

instance ToEDN Int where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int: expected Integer"

instance ToEDN Int8 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int8 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int8: expected Integer"

instance ToEDN Int16 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int16 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int16: expected Integer"

instance ToEDN Int32 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int32 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int32: expected Integer"

instance ToEDN Int64 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Int64 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Int64: expected Integer"

instance ToEDN Word where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word: expected Integer"

instance ToEDN Word8 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word8 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word8: expected Integer"

instance ToEDN Word16 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word16 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word16: expected Integer"

instance ToEDN Word32 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word32 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word32: expected Integer"

instance ToEDN Word64 where
  toEDN n = EV.Integer (fromIntegral n)

instance FromEDN Word64 where
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Word64: expected Integer"

instance ToEDN Float where
  toEDN f = EV.Float (realToFrac f)

instance FromEDN Float where
  fromEDN (EV.Float d) = Right (realToFrac d)
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Float: expected Float"

instance ToEDN Double where
  toEDN = EV.Float

instance FromEDN Double where
  fromEDN (EV.Float d) = Right d
  fromEDN (EV.Integer n) = Right (fromIntegral n)
  fromEDN _ = Left "FromEDN Double: expected Float"

instance ToEDN Text where
  toEDN = EV.String

instance FromEDN Text where
  fromEDN (EV.String t) = Right t
  fromEDN _ = Left "FromEDN Text: expected String"

instance ToEDN ByteString where
  toEDN _ = EV.Nil

instance FromEDN ByteString where
  fromEDN _ = Left "FromEDN ByteString: EDN has no binary type"

instance ToEDN () where
  toEDN () = EV.Nil

instance FromEDN () where
  fromEDN EV.Nil = Right ()
  fromEDN _ = Left "FromEDN (): expected Nil"

instance ToEDN a => ToEDN (Maybe a) where
  toEDN Nothing = EV.Nil
  toEDN (Just x) = toEDN x

instance FromEDN a => FromEDN (Maybe a) where
  fromEDN EV.Nil = Right Nothing
  fromEDN v = Just <$> fromEDN v

instance ToEDN a => ToEDN [a] where
  toEDN xs = EV.Vector (V.fromList (map toEDN xs))

instance FromEDN a => FromEDN [a] where
  fromEDN (EV.Vector vs) = traverse fromEDN (V.toList vs)
  fromEDN (EV.List vs) = traverse fromEDN (V.toList vs)
  fromEDN _ = Left "FromEDN [a]: expected Vector or List"

instance ToEDN a => ToEDN (Vector a) where
  toEDN xs = EV.Vector (V.map toEDN xs)

instance FromEDN a => FromEDN (Vector a) where
  fromEDN (EV.Vector vs) = V.mapM fromEDN vs
  fromEDN (EV.List vs) = V.mapM fromEDN vs
  fromEDN _ = Left "FromEDN Vector: expected Vector or List"

instance (ToEDN a, ToEDN b) => ToEDN (a, b) where
  toEDN (a, b) = EV.Vector (V.fromList [toEDN a, toEDN b])

instance (FromEDN a, FromEDN b) => FromEDN (a, b) where
  fromEDN (EV.Vector vs)
    | V.length vs == 2 = (,) <$> fromEDN (vs V.! 0) <*> fromEDN (vs V.! 1)
  fromEDN _ = Left "FromEDN (a,b): expected Vector of length 2"

instance (ToEDN k, ToEDN v) => ToEDN (Map k v) where
  toEDN m = EV.Map (V.fromList [(toEDN k, toEDN v') | (k, v') <- Map.toList m])

instance (Ord k, FromEDN k, FromEDN v) => FromEDN (Map k v) where
  fromEDN (EV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromEDN k <*> fromEDN v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromEDN _ = Left "FromEDN Map: expected Map"

instance ToEDN EV.Value where
  toEDN = id

instance FromEDN EV.Value where
  fromEDN = Right

-- GHC.Generics support

class GToEDN f where
  gToEDN :: f p -> EV.Value

class GFromEDN f where
  gFromEDN :: EV.Value -> Either String (f p)

instance GToEDN f => GToEDN (M1 D c f) where
  gToEDN (M1 x) = gToEDN x

instance GFromEDN f => GFromEDN (M1 D c f) where
  gFromEDN v = M1 <$> gFromEDN v

instance (Constructor c, GToEDNFields f) => GToEDN (M1 C c f) where
  gToEDN (M1 x) =
    let fields = gToEDNFields x
    in EV.Map (V.fromList [(EV.Keyword Nothing k, v) | (k, v) <- fields])

instance (Constructor c, GFromEDNFields f) => GFromEDN (M1 C c f) where
  gFromEDN (EV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromEDNFields lkup
  gFromEDN _ = Left "GFromEDN: expected Map for record type"

lookupField :: Text -> Vector (EV.Value, EV.Value) -> Maybe EV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (EV.Keyword _ k, v) <- kvs V.! i, k == name = Just v
      | (EV.String k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToEDNFields f where
  gToEDNFields :: f p -> [(Text, EV.Value)]

class GFromEDNFields f where
  gFromEDNFields :: (Text -> Maybe EV.Value) -> Either String (f p)

instance (GToEDNFields a, GToEDNFields b) => GToEDNFields (a :*: b) where
  gToEDNFields (a :*: b) = gToEDNFields a ++ gToEDNFields b

instance (GFromEDNFields a, GFromEDNFields b) => GFromEDNFields (a :*: b) where
  gFromEDNFields lkup = (:*:) <$> gFromEDNFields lkup <*> gFromEDNFields lkup

instance (Selector s, ToEDN a) => GToEDNFields (M1 S s (K1 i a)) where
  gToEDNFields m@(M1 (K1 x)) = [(T.pack (selName m), toEDN x)]

instance (Selector s, FromEDN a) => GFromEDNFields (M1 S s (K1 i a)) where
  gFromEDNFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromEDN: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromEDN v
