{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Typeclass-based CSV serialization with Generic deriving.
module CSV.Class
  ( ToCSV(..)
  , FromCSV(..)
  , CSVField(..)
  , genericToCSVRow
  , genericFromCSVRow
  ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Generics
import Numeric.Natural (Natural)

class ToCSV a where
  toCSVRow :: a -> Vector Text

class FromCSV a where
  fromCSVRow :: Vector Text -> Either String a

-- Generic helpers for product types (records)

genericToCSVRow :: (Generic a, GToCSV (Rep a)) => a -> Vector Text
genericToCSVRow = V.fromList . gToCSV . from

genericFromCSVRow :: (Generic a, GFromCSV (Rep a)) => Vector Text -> Either String a
genericFromCSVRow vs = case gFromCSV (V.toList vs) of
  Right (a, _) -> Right (to a)
  Left err     -> Left err

class GToCSV f where
  gToCSV :: f p -> [Text]

instance GToCSV U1 where
  gToCSV U1 = []

instance GToCSV f => GToCSV (M1 i c f) where
  gToCSV (M1 x) = gToCSV x

instance (GToCSV a, GToCSV b) => GToCSV (a :*: b) where
  gToCSV (a :*: b) = gToCSV a ++ gToCSV b

instance CSVField a => GToCSV (K1 i a) where
  gToCSV (K1 x) = [toCSVField x]

class GFromCSV f where
  gFromCSV :: [Text] -> Either String (f p, [Text])

instance GFromCSV U1 where
  gFromCSV xs = Right (U1, xs)

instance GFromCSV f => GFromCSV (M1 i c f) where
  gFromCSV xs = case gFromCSV xs of
    Right (a, rest) -> Right (M1 a, rest)
    Left err        -> Left err

instance (GFromCSV a, GFromCSV b) => GFromCSV (a :*: b) where
  gFromCSV xs = do
    (a, xs') <- gFromCSV xs
    (b, xs'') <- gFromCSV xs'
    Right (a :*: b, xs'')

instance CSVField a => GFromCSV (K1 i a) where
  gFromCSV [] = Left "CSV.Class: not enough fields"
  gFromCSV (x:xs) = case fromCSVField x of
    Right a  -> Right (K1 a, xs)
    Left err -> Left err

class CSVField a where
  toCSVField :: a -> Text
  fromCSVField :: Text -> Either String a

instance CSVField Text where
  toCSVField = id
  fromCSVField = Right

instance CSVField String where
  toCSVField = T.pack
  fromCSVField = Right . T.unpack

instance CSVField Int where
  toCSVField = T.pack . show
  fromCSVField t = case reads (T.unpack t) of
    [(n, "")] -> Right n
    _         -> Left $ "CSV.Class: cannot parse Int from " ++ show t

instance CSVField Integer where
  toCSVField = T.pack . show
  fromCSVField t = case reads (T.unpack t) of
    [(n, "")] -> Right n
    _         -> Left $ "CSV.Class: cannot parse Integer from " ++ show t

instance CSVField Double where
  toCSVField = T.pack . show
  fromCSVField t = case reads (T.unpack t) of
    [(n, "")] -> Right n
    _         -> Left $ "CSV.Class: cannot parse Double from " ++ show t

instance CSVField Bool where
  toCSVField True  = "true"
  toCSVField False = "false"
  fromCSVField t
    | t' == "true"  || t' == "1" || t' == "yes" = Right True
    | t' == "false" || t' == "0" || t' == "no"  = Right False
    | otherwise = Left $ "CSV.Class: cannot parse Bool from " ++ show t
    where t' = T.toLower t

-- Aeson-parity field instances --------------------------------------------

readSignedDecimalCSV :: Integral a => String -> Text -> Either String a
readSignedDecimalCSV label t = case TR.signed TR.decimal t of
  Right (v, rest) | T.null rest -> Right v
  _ -> Left $ "CSV.Class: cannot parse " ++ label ++ " from " ++ show t

instance CSVField Char where
  toCSVField c = T.singleton c
  fromCSVField t = case T.length t of
    1 -> Right (T.head t)
    _ -> Left $ "CSV.Class: cannot parse Char from " ++ show t

instance CSVField Float where
  toCSVField = T.pack . show
  fromCSVField t = case reads (T.unpack t) of
    [(n, "")] -> Right n
    _         -> Left $ "CSV.Class: cannot parse Float from " ++ show t

instance CSVField Int8 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Int8"

instance CSVField Int16 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Int16"

instance CSVField Int32 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Int32"

instance CSVField Int64 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Int64"

instance CSVField Word where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Word"

instance CSVField Word8 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Word8"

instance CSVField Word16 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Word16"

instance CSVField Word32 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Word32"

instance CSVField Word64 where
  toCSVField = T.pack . show
  fromCSVField = readSignedDecimalCSV "Word64"

instance CSVField Natural where
  toCSVField = T.pack . show
  fromCSVField t = do
    n <- readSignedDecimalCSV "Natural" t :: Either String Integer
    if n < 0
      then Left $ "CSV.Class: negative Natural " ++ show t
      else Right (fromInteger n)

instance CSVField TL.Text where
  toCSVField = TL.toStrict
  fromCSVField = Right . TL.fromStrict

-- | Encodes 'Nothing' as the empty field.
instance CSVField a => CSVField (Maybe a) where
  toCSVField Nothing  = T.empty
  toCSVField (Just x) = toCSVField x
  fromCSVField t
    | T.null t  = Right Nothing
    | otherwise = Just <$> fromCSVField t

instance CSVField a => CSVField (Identity a) where
  toCSVField (Identity x) = toCSVField x
  fromCSVField t = Identity <$> fromCSVField t

instance CSVField a => CSVField (Const a b) where
  toCSVField (Const x) = toCSVField x
  fromCSVField t = Const <$> fromCSVField t

instance CSVField a => CSVField (Down a) where
  toCSVField (Down x) = toCSVField x
  fromCSVField t = Down <$> fromCSVField t

-- Functor / monoid newtype field instances (unwrap-only).

instance CSVField a => CSVField (Mon.Sum a) where
  toCSVField = toCSVField . Mon.getSum
  fromCSVField t = Mon.Sum <$> fromCSVField t

instance CSVField a => CSVField (Mon.Product a) where
  toCSVField = toCSVField . Mon.getProduct
  fromCSVField t = Mon.Product <$> fromCSVField t

instance CSVField a => CSVField (Mon.Dual a) where
  toCSVField = toCSVField . Mon.getDual
  fromCSVField t = Mon.Dual <$> fromCSVField t

instance CSVField Mon.All where
  toCSVField = toCSVField . Mon.getAll
  fromCSVField t = Mon.All <$> fromCSVField t

instance CSVField Mon.Any where
  toCSVField = toCSVField . Mon.getAny
  fromCSVField t = Mon.Any <$> fromCSVField t

instance CSVField a => CSVField (Mon.First a) where
  toCSVField = toCSVField . Mon.getFirst
  fromCSVField t = Mon.First <$> fromCSVField t

instance CSVField a => CSVField (Mon.Last a) where
  toCSVField = toCSVField . Mon.getLast
  fromCSVField t = Mon.Last <$> fromCSVField t

instance CSVField a => CSVField (Semi.Min a) where
  toCSVField = toCSVField . Semi.getMin
  fromCSVField t = Semi.Min <$> fromCSVField t

instance CSVField a => CSVField (Semi.Max a) where
  toCSVField = toCSVField . Semi.getMax
  fromCSVField t = Semi.Max <$> fromCSVField t

instance CSVField a => CSVField (Semi.First a) where
  toCSVField = toCSVField . Semi.getFirst
  fromCSVField t = Semi.First <$> fromCSVField t

instance CSVField a => CSVField (Semi.Last a) where
  toCSVField = toCSVField . Semi.getLast
  fromCSVField t = Semi.Last <$> fromCSVField t

instance CSVField a => CSVField (Semi.WrappedMonoid a) where
  toCSVField = toCSVField . Semi.unwrapMonoid
  fromCSVField t = Semi.WrapMonoid <$> fromCSVField t
