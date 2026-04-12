{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Typeclass-based CBOR serialization with GHC Generics support.
--
-- Provides 'ToCBOR' and 'FromCBOR' typeclasses that can be derived
-- automatically for record types via @DeriveGeneric@. Records are encoded
-- as CBOR maps with field names as text-string keys.
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
-- import CBOR.Class
--
-- data Config = Config { host :: Text, port :: Int } deriving (Generic)
-- instance ToCBOR Config
-- instance FromCBOR Config
--
-- let bytes = encodeCBOR (Config \"localhost\" 8080)
-- let Right cfg = decodeCBOR bytes :: Either String Config
-- @
module CBOR.Class
  ( ToCBOR(..)
  , FromCBOR(..)
  , encodeCBOR
  , decodeCBOR
  , GToCBOR(..)
  , GFromCBOR(..)
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

import qualified CBOR.Value as CV
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD

class ToCBOR a where
  toCBOR :: a -> CV.Value
  default toCBOR :: (Generic a, GToCBOR (Rep a)) => a -> CV.Value
  toCBOR = gToCBOR . from

class FromCBOR a where
  fromCBOR :: CV.Value -> Either String a
  default fromCBOR :: (Generic a, GFromCBOR (Rep a)) => CV.Value -> Either String a
  fromCBOR v = to <$> gFromCBOR v

encodeCBOR :: ToCBOR a => a -> ByteString
encodeCBOR = CE.encode . toCBOR

decodeCBOR :: FromCBOR a => ByteString -> Either String a
decodeCBOR bs = CD.decode bs >>= fromCBOR

-- Helper to convert a Haskell integer to CBOR UInt/NInt
intToCBOR :: Int64 -> CV.Value
intToCBOR n
  | n >= 0    = CV.UInt (fromIntegral n)
  | otherwise = CV.NInt (fromIntegral (negate n - 1))

cborToInt :: CV.Value -> Either String Int64
cborToInt (CV.UInt n) = Right (fromIntegral n)
cborToInt (CV.NInt n) = Right (negate (fromIntegral n) - 1)
cborToInt _ = Left "FromCBOR: expected UInt or NInt"

instance ToCBOR Bool where
  toCBOR = CV.Bool

instance FromCBOR Bool where
  fromCBOR (CV.Bool b) = Right b
  fromCBOR _ = Left "FromCBOR Bool: expected Bool"

instance ToCBOR Int where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int8 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int8 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int16 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int16 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int32 where
  toCBOR = intToCBOR . fromIntegral

instance FromCBOR Int32 where
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Int64 where
  toCBOR = intToCBOR

instance FromCBOR Int64 where
  fromCBOR = cborToInt

instance ToCBOR Word where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR v = fromIntegral <$> cborToInt v

instance ToCBOR Word8 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word8 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word8: expected UInt"

instance ToCBOR Word16 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word16 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word16: expected UInt"

instance ToCBOR Word32 where
  toCBOR n = CV.UInt (fromIntegral n)

instance FromCBOR Word32 where
  fromCBOR (CV.UInt n) = Right (fromIntegral n)
  fromCBOR _ = Left "FromCBOR Word32: expected UInt"

instance ToCBOR Word64 where
  toCBOR = CV.UInt

instance FromCBOR Word64 where
  fromCBOR (CV.UInt n) = Right n
  fromCBOR _ = Left "FromCBOR Word64: expected UInt"

instance ToCBOR Float where
  toCBOR = CV.Float32

instance FromCBOR Float where
  fromCBOR (CV.Float32 f) = Right f
  fromCBOR (CV.Float16 f) = Right f
  fromCBOR (CV.Float64 d) = Right (realToFrac d)
  fromCBOR _ = Left "FromCBOR Float: expected Float"

instance ToCBOR Double where
  toCBOR = CV.Float64

instance FromCBOR Double where
  fromCBOR (CV.Float64 d) = Right d
  fromCBOR (CV.Float32 f) = Right (realToFrac f)
  fromCBOR (CV.Float16 f) = Right (realToFrac f)
  fromCBOR _ = Left "FromCBOR Double: expected Float"

instance ToCBOR Text where
  toCBOR = CV.TextString

instance FromCBOR Text where
  fromCBOR (CV.TextString t) = Right t
  fromCBOR _ = Left "FromCBOR Text: expected TextString"

instance ToCBOR ByteString where
  toCBOR = CV.ByteString

instance FromCBOR ByteString where
  fromCBOR (CV.ByteString bs) = Right bs
  fromCBOR _ = Left "FromCBOR ByteString: expected ByteString"

instance ToCBOR () where
  toCBOR () = CV.Null

instance FromCBOR () where
  fromCBOR CV.Null = Right ()
  fromCBOR _ = Left "FromCBOR (): expected Null"

instance ToCBOR a => ToCBOR (Maybe a) where
  toCBOR Nothing = CV.Null
  toCBOR (Just x) = toCBOR x

instance FromCBOR a => FromCBOR (Maybe a) where
  fromCBOR CV.Null = Right Nothing
  fromCBOR v = Just <$> fromCBOR v

instance ToCBOR a => ToCBOR [a] where
  toCBOR xs = CV.Array (V.fromList (map toCBOR xs))

instance FromCBOR a => FromCBOR [a] where
  fromCBOR (CV.Array vs) = traverse fromCBOR (V.toList vs)
  fromCBOR _ = Left "FromCBOR [a]: expected Array"

instance ToCBOR a => ToCBOR (Vector a) where
  toCBOR xs = CV.Array (V.map toCBOR xs)

instance FromCBOR a => FromCBOR (Vector a) where
  fromCBOR (CV.Array vs) = V.mapM fromCBOR vs
  fromCBOR _ = Left "FromCBOR Vector: expected Array"

instance (ToCBOR a, ToCBOR b) => ToCBOR (a, b) where
  toCBOR (a, b) = CV.Array (V.fromList [toCBOR a, toCBOR b])

instance (FromCBOR a, FromCBOR b) => FromCBOR (a, b) where
  fromCBOR (CV.Array vs)
    | V.length vs == 2 = (,) <$> fromCBOR (vs V.! 0) <*> fromCBOR (vs V.! 1)
  fromCBOR _ = Left "FromCBOR (a,b): expected Array of length 2"

instance (ToCBOR k, ToCBOR v) => ToCBOR (Map k v) where
  toCBOR m = CV.Map (V.fromList [(toCBOR k, toCBOR v') | (k, v') <- Map.toList m])

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (Map k v) where
  fromCBOR (CV.Map kvs) = do
    pairs <- traverse (\(k, v) -> (,) <$> fromCBOR k <*> fromCBOR v) (V.toList kvs)
    Right (Map.fromList pairs)
  fromCBOR _ = Left "FromCBOR Map: expected Map"

instance ToCBOR CV.Value where
  toCBOR = id

instance FromCBOR CV.Value where
  fromCBOR = Right

-- GHC.Generics support

class GToCBOR f where
  gToCBOR :: f p -> CV.Value

class GFromCBOR f where
  gFromCBOR :: CV.Value -> Either String (f p)

instance GToCBOR f => GToCBOR (M1 D c f) where
  gToCBOR (M1 x) = gToCBOR x

instance GFromCBOR f => GFromCBOR (M1 D c f) where
  gFromCBOR v = M1 <$> gFromCBOR v

instance (Constructor c, GToCBORFields f) => GToCBOR (M1 C c f) where
  gToCBOR (M1 x) =
    let fields = gToCBORFields x
    in CV.Map (V.fromList [(CV.TextString k, v) | (k, v) <- fields])

instance (Constructor c, GFromCBORFields f) => GFromCBOR (M1 C c f) where
  gFromCBOR (CV.Map kvs) =
    let lkup name = lookupField name kvs
    in M1 <$> gFromCBORFields lkup
  gFromCBOR _ = Left "GFromCBOR: expected Map for record type"

lookupField :: Text -> Vector (CV.Value, CV.Value) -> Maybe CV.Value
lookupField name kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (CV.TextString k, v) <- kvs V.! i, k == name = Just v
      | otherwise = go (i + 1)

class GToCBORFields f where
  gToCBORFields :: f p -> [(Text, CV.Value)]

class GFromCBORFields f where
  gFromCBORFields :: (Text -> Maybe CV.Value) -> Either String (f p)

instance (GToCBORFields a, GToCBORFields b) => GToCBORFields (a :*: b) where
  gToCBORFields (a :*: b) = gToCBORFields a ++ gToCBORFields b

instance (GFromCBORFields a, GFromCBORFields b) => GFromCBORFields (a :*: b) where
  gFromCBORFields lkup = (:*:) <$> gFromCBORFields lkup <*> gFromCBORFields lkup

instance (Selector s, ToCBOR a) => GToCBORFields (M1 S s (K1 i a)) where
  gToCBORFields m@(M1 (K1 x)) = [(T.pack (selName m), toCBOR x)]

instance (Selector s, FromCBOR a) => GFromCBORFields (M1 S s (K1 i a)) where
  gFromCBORFields lkup =
    let name = T.pack (selName (undefined :: M1 S s (K1 i a) p))
    in case lkup name of
         Nothing -> Left $ "GFromCBOR: missing field " ++ T.unpack name
         Just v  -> M1 . K1 <$> fromCBOR v
