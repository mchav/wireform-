{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-csv encode + decode hot paths.
Test fixture: a Sale record across small (10 rows) and medium (1000 rows)
inputs.
-}
module Main (main) where

import CSV.Class
import CSV.Decode (decodeRecords)
import CSV.Encode (encodeRecords)
import CSV.Value (defaultCSV)
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


data Sale = Sale
  { saleProduct :: !Text
  , saleUnits :: !Int
  , salePrice :: !Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance ToCSV Sale where
  toCSVRow (Sale p u r) =
    V.fromList [p, T.pack (show u), T.pack (show r)]


instance FromCSV Sale where
  fromCSVRow v
    | V.length v >= 3 =
        Right
          ( Sale
              (V.unsafeIndex v 0)
              (read (T.unpack (V.unsafeIndex v 1)))
              (read (T.unpack (V.unsafeIndex v 2)))
          )
    | otherwise = Left "Sale: expected 3 fields"


small :: Vector Sale
small =
  V.fromList
    [ Sale (T.pack ("p-" <> show i)) (i `mod` 50) (fromIntegral i * 1.5)
    | i <- [1 .. 10 :: Int]
    ]


medium :: Vector Sale
medium =
  V.fromList
    [ Sale (T.pack ("p-" <> show i)) (i `mod` 50) (fromIntegral i * 1.5)
    | i <- [1 .. 1000 :: Int]
    ]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "10 rows" $ nf (encodeRecords defaultCSV) small
        , bench "1000 rows" $ nf (encodeRecords defaultCSV) medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeRecords defaultCSV small)) $ \bs ->
            bench "10 rows" $ nf (decodeRecords defaultCSV :: ByteString -> Either String (Vector Sale)) bs
        , env (pure (encodeRecords defaultCSV medium)) $ \bs ->
            bench "1000 rows" $ nf (decodeRecords defaultCSV :: ByteString -> Either String (Vector Sale)) bs
        ]
    ]
