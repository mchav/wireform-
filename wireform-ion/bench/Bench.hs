{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Ion.Class


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToIon, FromIon, NFData)


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
        [ bench "Person" $ nf encodeIon small
        , bench "[Person] x 100" $ nf encodeIon medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeIon small)) $ \bs ->
            bench "Person" $ nf (decodeIon :: ByteString -> Either String Person) bs
        , env (pure (encodeIon medium)) $ \bs ->
            bench "[Person] x 100" $ nf (decodeIon :: ByteString -> Either String (Vector Person)) bs
        ]
    ]
