{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Microbench for wireform-bson encode + decode hot paths.
module Main (main) where

import BSON.Class
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToBSON, FromBSON, NFData)


small :: Person
small = Person "Alice" 30 "alice@example.com"


medium :: Vector Person
medium =
  V.fromList
    [ Person
      (T.pack ("user-" <> show i))
      (20 + i `mod` 50)
      (T.pack ("user" <> show i <> "@example.com"))
    | i <- [1 .. 100 :: Int]
    ]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person" $ nf encodeBSON small
        , bench "[Person] x 100" $ nf encodeBSON medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeBSON small)) $ \bs ->
            bench "Person" $ nf (decodeBSON :: ByteString -> Either String Person) bs
        , env (pure (encodeBSON medium)) $ \bs ->
            bench "[Person] x 100" $ nf (decodeBSON :: ByteString -> Either String (Vector Person)) bs
        ]
    ]
