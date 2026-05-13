{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-edn encode + decode hot paths.
EDN is a text format, so encode produces 'Text' and decode
consumes it.
-}
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import EDN.Class
import GHC.Generics (Generic)


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToEDN, FromEDN, NFData)


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
        [ bench "Person" $ nf encodeEDN small
        , bench "[Person] x 100" $ nf encodeEDN medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeEDN small)) $ \t ->
            bench "Person" $ nf (decodeEDN :: Text -> Either String Person) t
        , env (pure (encodeEDN medium)) $ \t ->
            bench "[Person] x 100" $ nf (decodeEDN :: Text -> Either String (Vector Person)) t
        ]
    ]
