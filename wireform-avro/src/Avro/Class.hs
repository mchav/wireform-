{-# LANGUAGE FlexibleInstances #-}
-- | Typeclass-based Avro serialization.
--
-- Provides 'ToAvro' and 'FromAvro' typeclasses for converting Haskell
-- values to\/from 'Avro.Value.Value'. Instances are provided for common
-- types: 'Bool', 'Int', 'Int32', 'Int64', 'Float', 'Double', 'Text',
-- 'ByteString', 'Vector', and 'Maybe' (as Avro unions).
module Avro.Class
  ( ToAvro(..)
  , FromAvro(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Avro.Value as AV

class ToAvro a where
  toAvro :: a -> AV.Value

class FromAvro a where
  fromAvro :: AV.Value -> Either String a

instance ToAvro Bool where
  toAvro = AV.Bool

instance FromAvro Bool where
  fromAvro (AV.Bool b) = Right b
  fromAvro _ = Left "FromAvro Bool: expected Bool"

instance ToAvro Int where
  toAvro n = AV.Long (fromIntegral n)

instance FromAvro Int where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int: expected Int or Long"

instance ToAvro Int8 where
  toAvro n = AV.Int (fromIntegral n)

instance FromAvro Int8 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int8: expected Int or Long"

instance ToAvro Int16 where
  toAvro n = AV.Int (fromIntegral n)

instance FromAvro Int16 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int16: expected Int or Long"

instance ToAvro Int32 where
  toAvro = AV.Int

instance FromAvro Int32 where
  fromAvro (AV.Int n) = Right n
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int32: expected Int or Long"

instance ToAvro Int64 where
  toAvro = AV.Long

instance FromAvro Int64 where
  fromAvro (AV.Long n) = Right n
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Int64: expected Long or Int"

instance ToAvro Word where
  toAvro n = AV.Long (fromIntegral n)

instance FromAvro Word where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word: expected Long or Int"

instance ToAvro Word8 where
  toAvro n = AV.Int (fromIntegral n)

instance FromAvro Word8 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word8: expected Int or Long"

instance ToAvro Word16 where
  toAvro n = AV.Int (fromIntegral n)

instance FromAvro Word16 where
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word16: expected Int or Long"

instance ToAvro Word32 where
  toAvro n = AV.Long (fromIntegral n)

instance FromAvro Word32 where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word32: expected Long or Int"

instance ToAvro Word64 where
  toAvro n = AV.Long (fromIntegral n)

instance FromAvro Word64 where
  fromAvro (AV.Long n) = Right (fromIntegral n)
  fromAvro (AV.Int n) = Right (fromIntegral n)
  fromAvro _ = Left "FromAvro Word64: expected Long or Int"

instance ToAvro Float where
  toAvro = AV.Float

instance FromAvro Float where
  fromAvro (AV.Float f) = Right f
  fromAvro (AV.Double d) = Right (realToFrac d)
  fromAvro _ = Left "FromAvro Float: expected Float or Double"

instance ToAvro Double where
  toAvro = AV.Double

instance FromAvro Double where
  fromAvro (AV.Double d) = Right d
  fromAvro (AV.Float f) = Right (realToFrac f)
  fromAvro _ = Left "FromAvro Double: expected Double or Float"

instance ToAvro Text where
  toAvro = AV.String

instance FromAvro Text where
  fromAvro (AV.String t) = Right t
  fromAvro _ = Left "FromAvro Text: expected String"

instance ToAvro ByteString where
  toAvro = AV.Bytes

instance FromAvro ByteString where
  fromAvro (AV.Bytes bs) = Right bs
  fromAvro (AV.Fixed bs) = Right bs
  fromAvro _ = Left "FromAvro ByteString: expected Bytes or Fixed"

instance ToAvro () where
  toAvro () = AV.Null

instance FromAvro () where
  fromAvro AV.Null = Right ()
  fromAvro _ = Left "FromAvro (): expected Null"

instance ToAvro a => ToAvro (Maybe a) where
  toAvro Nothing = AV.Null
  toAvro (Just x) = toAvro x

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro AV.Null = Right Nothing
  fromAvro v = Just <$> fromAvro v

instance ToAvro a => ToAvro [a] where
  toAvro xs = AV.Array (V.fromList (map toAvro xs))

instance FromAvro a => FromAvro [a] where
  fromAvro (AV.Array vs) = traverse fromAvro (V.toList vs)
  fromAvro _ = Left "FromAvro [a]: expected Array"

instance ToAvro a => ToAvro (Vector a) where
  toAvro xs = AV.Array (V.map toAvro xs)

instance FromAvro a => FromAvro (Vector a) where
  fromAvro (AV.Array vs) = V.mapM fromAvro vs
  fromAvro _ = Left "FromAvro Vector: expected Array"

instance (ToAvro a, ToAvro b) => ToAvro (a, b) where
  toAvro (a, b) = AV.Array (V.fromList [toAvro a, toAvro b])

instance (FromAvro a, FromAvro b) => FromAvro (a, b) where
  fromAvro (AV.Array vs)
    | V.length vs == 2 = (,) <$> fromAvro (vs V.! 0) <*> fromAvro (vs V.! 1)
  fromAvro _ = Left "FromAvro (a,b): expected Array of length 2"

instance ToAvro AV.Value where
  toAvro = id

instance FromAvro AV.Value where
  fromAvro = Right
