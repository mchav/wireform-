{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Microbench for wireform-asn1's typed encode + decode hot paths.
Test fixture: a small Subject record + a 100-element vector.
-}
module Main (main) where

import ASN1.Derive qualified as DASN1
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


data Subject = Subject
  { subjectCN :: !Text
  , subjectO :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


DASN1.deriveASN1 ''Subject


small :: Subject
small = Subject "example.com" "Example Corp"


medium :: Vector Subject
medium =
  V.fromList
    [ Subject
        (T.pack ("host-" <> show i <> ".example.com"))
        (T.pack ("Org-" <> show i))
    | i <- [1 .. 100 :: Int]
    ]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Subject" $ nf DASN1.encodeASN1 small
        , bench "[Subject] x 100" $ nf DASN1.encodeASN1 medium
        ]
    , bgroup
        "decode"
        [ env (pure (DASN1.encodeASN1 small)) $ \bs ->
            bench "Subject" $ nf (DASN1.decodeASN1 :: ByteString -> Either String Subject) bs
        , env (pure (DASN1.encodeASN1 medium)) $ \bs ->
            bench "[Subject] x 100" $ nf (DASN1.decodeASN1 :: ByteString -> Either String (Vector Subject)) bs
        ]
    ]
