-- | Typeclass-based CSV serialization with Generic deriving.
module CSV.Class
  ( ToCSV(..)
  , FromCSV(..)
  , genericToCSVRow
  , genericFromCSVRow
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics

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
