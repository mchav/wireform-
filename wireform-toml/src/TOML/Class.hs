{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based TOML serialization with GHC Generics support.
module TOML.Class
  ( ToTOML(..)
  , FromTOML(..)
  , encodeTOML
  , decodeTOML
  , GToTOML(..)
  , GFromTOML(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

import qualified TOML.Value as TV
import qualified TOML.Encode as TE
import qualified TOML.Decode as TD

class ToTOML a where
  toTOML :: a -> TV.Value
  default toTOML :: (Generic a, GToTOML (Rep a)) => a -> TV.Value
  toTOML = gToTOML . from

class FromTOML a where
  fromTOML :: TV.Value -> Either String a
  default fromTOML :: (Generic a, GFromTOML (Rep a)) => TV.Value -> Either String a
  fromTOML v = to <$> gFromTOML v

encodeTOML :: ToTOML a => a -> Text
encodeTOML = TE.encode . toTOML

decodeTOML :: FromTOML a => Text -> Either String a
decodeTOML t = TD.decode t >>= fromTOML

instance ToTOML Text where
  toTOML = TV.TString

instance FromTOML Text where
  fromTOML (TV.TString t) = Right t
  fromTOML _ = Left "FromTOML Text: expected TString"

instance ToTOML Bool where
  toTOML = TV.TBool

instance FromTOML Bool where
  fromTOML (TV.TBool b) = Right b
  fromTOML _ = Left "FromTOML Bool: expected TBool"

instance ToTOML Int where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int: expected TInteger"

instance ToTOML Int8 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int8 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int8: expected TInteger"

instance ToTOML Int16 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int16 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int16: expected TInteger"

instance ToTOML Int32 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int32 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int32: expected TInteger"

instance ToTOML Int64 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Int64 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Int64: expected TInteger"

instance ToTOML Word where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word: expected TInteger"

instance ToTOML Word8 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word8 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word8: expected TInteger"

instance ToTOML Word16 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word16 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word16: expected TInteger"

instance ToTOML Word32 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word32 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word32: expected TInteger"

instance ToTOML Word64 where
  toTOML = TV.TInteger . fromIntegral

instance FromTOML Word64 where
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Word64: expected TInteger"

instance ToTOML Integer where
  toTOML = TV.TInteger

instance FromTOML Integer where
  fromTOML (TV.TInteger n) = Right n
  fromTOML _ = Left "FromTOML Integer: expected TInteger"

instance ToTOML Double where
  toTOML = TV.TFloat

instance FromTOML Double where
  fromTOML (TV.TFloat d) = Right d
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Double: expected TFloat"

instance ToTOML Float where
  toTOML = TV.TFloat . realToFrac

instance FromTOML Float where
  fromTOML (TV.TFloat d) = Right (realToFrac d)
  fromTOML (TV.TInteger n) = Right (fromIntegral n)
  fromTOML _ = Left "FromTOML Float: expected TFloat"

instance ToTOML a => ToTOML [a] where
  toTOML xs = TV.TArray (V.fromList (map toTOML xs))

instance FromTOML a => FromTOML [a] where
  fromTOML (TV.TArray vs) = traverse fromTOML (V.toList vs)
  fromTOML _ = Left "FromTOML [a]: expected TArray"

instance ToTOML a => ToTOML (Vector a) where
  toTOML xs = TV.TArray (V.map toTOML xs)

instance FromTOML a => FromTOML (Vector a) where
  fromTOML (TV.TArray vs) = V.mapM fromTOML vs
  fromTOML _ = Left "FromTOML Vector: expected TArray"

instance ToTOML a => ToTOML (Maybe a) where
  toTOML Nothing = TV.TString ""
  toTOML (Just x) = toTOML x

instance FromTOML a => FromTOML (Maybe a) where
  fromTOML (TV.TString t) | T.null t = Right Nothing
  fromTOML v = Just <$> fromTOML v

instance ToTOML TV.Value where
  toTOML = id

instance FromTOML TV.Value where
  fromTOML = Right

-- GHC.Generics support

class GToTOML f where
  gToTOML :: f p -> TV.Value

class GFromTOML f where
  gFromTOML :: TV.Value -> Either String (f p)

instance GToTOML f => GToTOML (M1 D c f) where
  gToTOML (M1 x) = gToTOML x

instance GFromTOML f => GFromTOML (M1 D c f) where
  gFromTOML v = M1 <$> gFromTOML v

instance (Constructor c, GToTOMLFields f) => GToTOML (M1 C c f) where
  gToTOML (M1 x) =
    let fields = gToTOMLFields x
    in TV.TTable (V.fromList fields)

instance (Constructor c, GFromTOMLFields f) => GFromTOML (M1 C c f) where
  gFromTOML (TV.TTable kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromTOMLFields lkup
  gFromTOML _ = Left "GFromTOML: expected TTable for record type"

lookupField :: Text -> Vector (Text, TV.Value) -> Maybe TV.Value
lookupField name kvs = go 0
  where
    !len = V.length kvs
    go !i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToTOMLFields f where
  gToTOMLFields :: f p -> [(Text, TV.Value)]

class GFromTOMLFields f where
  gFromTOMLFields :: (Text -> Maybe TV.Value) -> Either String (f p)

instance (GToTOMLFields a, GToTOMLFields b) => GToTOMLFields (a :*: b) where
  gToTOMLFields (a :*: b) = gToTOMLFields a ++ gToTOMLFields b

instance (GFromTOMLFields a, GFromTOMLFields b) => GFromTOMLFields (a :*: b) where
  gFromTOMLFields lkup = (:*:) <$> gFromTOMLFields lkup <*> gFromTOMLFields lkup

instance (Selector s, ToTOML a) => GToTOMLFields (M1 S s (K1 i a)) where
  gToTOMLFields m@(M1 (K1 x)) = [(T.pack (selName m), toTOML x)]

instance (Selector s, FromTOML a) => GFromTOMLFields (M1 S s (K1 i a)) where
  gFromTOMLFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromTOML: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromTOML v
