{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-toml encode + decode hot paths.
TOML is a text-line format; the test fixture is a Person record
and an array-of-tables of 100 such records.
-}
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import TOML.Class


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToTOML, FromTOML, NFData)


newtype People = People {people :: Vector Person}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToTOML, FromTOML, NFData)


small :: Person
small = Person "Alice" 30 "alice@example.com"


medium :: People
medium =
  People $
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
        [ bench "Person" $ nf encodeTOML small
        , bench "[Person] x 100" $ nf encodeTOML medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeTOML small)) $ \t ->
            bench "Person" $ nf (decodeTOML :: Text -> Either String Person) t
        , env (pure (encodeTOML medium)) $ \t ->
            bench "[Person] x 100" $ nf (decodeTOML :: Text -> Either String People) t
        ]
    ]
