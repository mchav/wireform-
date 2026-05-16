{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Microbench for wireform-bond's typed encode + decode hot paths.
Goes through the @Bond.Derive@ typeclass machinery (the user-facing
entry point) instead of constructing 'Bond.Value' by hand.
-}
module Main (main) where

import Bond.Decode qualified as BD
import Bond.Derive qualified as DBond
import Bond.Encode qualified as BE
import Bond.Value qualified as BV
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


-- Bond's ToBond / FromBond don't ship instances for the unsized
-- 'Int'; use 'Int32' for the age field.
data Person = Person
  { personName :: !Text
  , personAge :: !Int32
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


DBond.deriveBond ''Person


encodeBond :: DBond.ToBond a => a -> ByteString
encodeBond = BE.encode . DBond.toBond
{-# NOINLINE encodeBond #-}


decodeBond :: DBond.FromBond a => ByteString -> Either String a
decodeBond bs = case BD.decode BV.BT_STRUCT bs of
  Right v -> DBond.fromBond v
  Left e -> Left e
{-# NOINLINE decodeBond #-}


small :: Person
small = Person "Alice" 30 "alice@example.com"


medium :: Vector Person
medium =
  V.fromList
    [ Person
      (T.pack ("user-" <> show i))
      (fromIntegral (20 + i `mod` 50))
      (T.pack ("user" <> show i <> "@example.com"))
    | i <- [1 .. 100 :: Int]
    ]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person" $ nf encodeBond small
        , bench "[Person] x 100" $ nf encodeBond medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeBond small)) $ \bs ->
            bench "Person" $ nf (decodeBond :: ByteString -> Either String Person) bs
        , env (pure (encodeBond medium)) $ \bs ->
            bench "[Person] x 100" $ nf (decodeBond :: ByteString -> Either String (Vector Person)) bs
        ]
    ]
