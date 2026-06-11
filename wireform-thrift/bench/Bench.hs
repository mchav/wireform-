{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-thrift's encode + decode hot paths
across the binary and compact wire protocols.

Test fixtures: a small @Person@ record and a 100-element vector of
the same. Same shape as every other per-package bench in the
monorepo so cross-format comparisons in a unified dashboard stay
comparable.
-}
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Thrift.Class


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToThrift, FromThrift, NFData)


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
        "binary"
        [ bgroup
            "encode"
            [ bench "Person" $ nf encodeThriftBinary small
            , bench "[Person] x 100" $ nf encodeThriftBinary medium
            ]
        , bgroup
            "decode"
            [ env (pure (encodeThriftBinary small)) $ \bs ->
                bench "Person" $ nf (decodeThriftBinary :: ByteString -> Either String Person) bs
            , env (pure (encodeThriftBinary medium)) $ \bs ->
                bench "[Person] x 100" $ nf (decodeThriftBinary :: ByteString -> Either String (Vector Person)) bs
            ]
        ]
    , bgroup
        "compact"
        [ bgroup
            "encode"
            [ bench "Person" $ nf encodeThriftCompact small
            , bench "[Person] x 100" $ nf encodeThriftCompact medium
            ]
        , bgroup
            "decode"
            [ env (pure (encodeThriftCompact small)) $ \bs ->
                bench "Person" $ nf (decodeThriftCompact :: ByteString -> Either String Person) bs
            , env (pure (encodeThriftCompact medium)) $ \bs ->
                bench "[Person] x 100" $ nf (decodeThriftCompact :: ByteString -> Either String (Vector Person)) bs
            ]
        ]
    ]
